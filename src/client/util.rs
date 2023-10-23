use super::RequestError;
use indenter::indented;
use mime::{Mime, JSON};
use reqwest::{header::HeaderMap, Method, Response, StatusCode};
use serde_json::{to_string_pretty, value::Value};
use std::fmt::{self, Write};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use url::Url;

// Retry configuration:
const RETRIES: i32 = 10;
const BACKOFF_FACTOR: f64 = 1.0;
const BACKOFF_BASE: f64 = 1.25;
const BACKOFF_MAX: f64 = 120.0;
const TOTAL_WAIT: Duration = Duration::from_secs(300);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct Retrier {
    method: Method,
    url: Url,
    attempts: i32,
    stop_time: Instant,
}

impl Retrier {
    pub(super) fn new(method: Method, url: Url) -> Retrier {
        Retrier {
            method,
            url,
            attempts: 0,
            stop_time: Instant::now() + TOTAL_WAIT,
        }
    }

    // Takes the return value of a call to `RequestBuilder::send()` that has
    // not been run through `Response::error_for_status()`.
    //
    // - If the request was successful (status code and everything), returns
    //   `Ok(RetryDecision::Success(response))`.
    //
    // - If the request should be retried, returns
    //   `Ok(RetryDecision::Retry(delay))`.
    //
    // - If the request was a failure (possibly due to status code) and should
    //   not be retried (possibly due to all retries having been exhausted),
    //   returns an `Err`.
    pub(super) async fn handle(
        &mut self,
        resp: Result<Response, reqwest::Error>,
    ) -> Result<RetryDecision, RequestError> {
        self.attempts += 1;
        if self.attempts > RETRIES {
            log::trace!("Retries exhausted");
            return self.finalize(resp).await;
        }
        let now = Instant::now();
        if now > self.stop_time {
            log::trace!("Maximum total retry wait time exceeded");
            return self.finalize(resp).await;
        }
        let backoff = if self.attempts < 2 {
            // urllib3 says "most errors are resolved immediately by a second
            // try without a delay" and thus doesn't sleep on the first retry,
            // but that seems irresponsible
            BACKOFF_FACTOR * 0.1
        } else {
            (BACKOFF_FACTOR * BACKOFF_BASE.powi(self.attempts - 1)).clamp(0.0, BACKOFF_MAX)
        };
        let backoff = Duration::from_secs_f64(backoff);
        let delay = match resp {
            Err(ref e) if e.is_builder() => return self.finalize(resp).await,
            Err(_) => backoff,
            Ok(r) if r.status() == StatusCode::FORBIDDEN => {
                let parts = ResponseParts::from_response(r).await;
                if let Some(v) = parts.headers.get("Retry-After") {
                    let secs = v
                        .to_str()
                        .ok()
                        .and_then(|s| s.parse::<u64>().ok())
                        .map(|n| n + 1);
                    if secs.is_some() {
                        log::trace!("Server responded with 403 and Retry-After header");
                    }
                    Duration::from_secs(secs.unwrap_or_default())
                } else if parts
                    .text
                    .as_ref()
                    .is_some_and(|s| s.contains("rate limit"))
                {
                    if parts
                        .headers
                        .get("x-ratelimit-remaining")
                        .and_then(|v| v.to_str().ok())
                        == Some("0")
                    {
                        if let Some(reset) = parts
                            .headers
                            .get("x-ratelimit-reset")
                            .and_then(|v| v.to_str().ok())
                            .and_then(|s| s.parse::<u64>().ok())
                        {
                            log::trace!("Primary rate limit exceeded; waiting for reset");
                            time_till_timestamp(reset).unwrap_or_default() + Duration::from_secs(1)
                        } else {
                            Duration::ZERO
                        }
                    } else {
                        log::trace!("Secondary rate limit triggered");
                        backoff
                    }
                } else {
                    return self.finalize_parts(parts);
                }
            }
            Ok(r) if r.status().is_server_error() => backoff,
            _ => return self.finalize(resp).await,
        };
        let delay = delay.max(backoff);
        let time_left = self.stop_time.saturating_duration_since(Instant::now());
        Ok(RetryDecision::Retry(delay.clamp(Duration::ZERO, time_left)))
    }

    async fn finalize(
        &self,
        resp: Result<Response, reqwest::Error>,
    ) -> Result<RetryDecision, RequestError> {
        match resp {
            Ok(r) if r.status().is_client_error() || r.status().is_server_error() => Err(
                RequestError::Status(Box::new(PrettyHttpError::new(self.method.clone(), r).await)),
            ),
            Ok(r) => Ok(RetryDecision::Success(r)),
            Err(source) => Err(RequestError::Send {
                method: self.method.clone(),
                url: self.url.clone(),
                source,
            }),
        }
    }

    fn finalize_parts<T>(&self, parts: ResponseParts) -> Result<T, RequestError> {
        Err(RequestError::Status(Box::new(PrettyHttpError::from_parts(
            self.method.clone(),
            self.url.clone(),
            parts,
        ))))
    }
}

#[derive(Debug)]
pub(super) enum RetryDecision {
    Success(Response),
    Retry(Duration),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ResponseParts {
    status: StatusCode,
    headers: HeaderMap,
    text: Option<String>,
}

impl ResponseParts {
    async fn from_response(r: Response) -> ResponseParts {
        let status = r.status();
        let headers = r.headers().clone();
        let text = r.text().await.ok();
        ResponseParts {
            status,
            headers,
            text,
        }
    }
}

/// Error raised for a 4xx or 5xx HTTP response that includes the response body
/// — and, if that body is JSON, it's pretty-printed
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PrettyHttpError {
    method: Method,
    url: Url,
    status: StatusCode,
    body: Option<String>,
}

impl PrettyHttpError {
    pub(crate) async fn new(method: Method, r: reqwest::Response) -> PrettyHttpError {
        let url = r.url().clone();
        let status = r.status();
        // If the response body is JSON, pretty-print it.
        let body = if is_json_response(&r) {
            r.json::<Value>().await.ok().map(|v| {
                to_string_pretty(&v).expect("Re-JSONifying a JSON response should not fail")
            })
        } else {
            r.text().await.ok()
        };
        PrettyHttpError {
            method,
            url,
            status,
            body,
        }
    }

    fn from_parts(method: Method, url: Url, parts: ResponseParts) -> PrettyHttpError {
        let status = parts.status;
        // If the response body is JSON, pretty-print it.
        let body = if has_json_content_type(&parts.headers) {
            parts
                .text
                .and_then(|s| serde_json::from_str::<Value>(&s).ok())
                .map(|v| {
                    to_string_pretty(&v).expect("Re-JSONifying a JSON response should not fail")
                })
        } else {
            parts.text
        };
        PrettyHttpError {
            method,
            url,
            status,
            body,
        }
    }
}

impl fmt::Display for PrettyHttpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} request to {} returned {}",
            self.method, self.url, self.status
        )?;
        if let Some(text) = &self.body {
            write!(indented(f).with_str("    "), "\n\n{text}\n")?;
        }
        Ok(())
    }
}

impl std::error::Error for PrettyHttpError {}

/// Return the `rel="next"` URL, if any, from the response's "Link" header
pub(crate) fn get_next_link(r: &Response) -> Option<Url> {
    let header_value = r.headers().get(reqwest::header::LINK)?.to_str().ok()?;
    parse_link_header::parse_with_rel(header_value)
        .ok()?
        .get("next")
        .map(|link| link.uri.clone())
}

/// Returns `true` iff the response's Content-Type header indicates the body is
/// JSON
fn is_json_response(r: &Response) -> bool {
    has_json_content_type(r.headers())
}

fn has_json_content_type(headers: &HeaderMap) -> bool {
    headers
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse::<Mime>().ok())
        .is_some_and(|ct| {
            ct.type_() == "application" && (ct.subtype() == "json" || ct.suffix() == Some(JSON))
        })
}

pub(super) fn urljoin<I>(url: &Url, segments: I) -> Url
where
    I: IntoIterator,
    I::Item: AsRef<str>,
{
    let mut url = url.clone();
    url.path_segments_mut()
        .expect("API URL should be able to be a base")
        .pop_if_empty()
        .extend(segments);
    url
}

fn time_till_timestamp(ts: u64) -> Option<Duration> {
    (UNIX_EPOCH + Duration::from_secs(ts))
        .duration_since(SystemTime::now())
        .ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("https://api.github.com")]
    #[case("https://api.github.com/")]
    fn test_urljoin_nopath(#[case] base: Url) {
        let u = urljoin(&base, ["foo"]);
        assert_eq!(u.as_str(), "https://api.github.com/foo");
        let u = urljoin(&base, ["foo", "bar"]);
        assert_eq!(u.as_str(), "https://api.github.com/foo/bar");
    }

    #[rstest]
    #[case("https://api.github.com/foo/bar")]
    #[case("https://api.github.com/foo/bar/")]
    fn test_urljoin_path(#[case] base: Url) {
        let u = urljoin(&base, ["gnusto"]);
        assert_eq!(u.as_str(), "https://api.github.com/foo/bar/gnusto");
        let u = urljoin(&base, ["gnusto", "cleesh"]);
        assert_eq!(u.as_str(), "https://api.github.com/foo/bar/gnusto/cleesh");
    }

    #[rstest]
    #[case("foo#bar", "https://api.github.com/foo%23bar")]
    #[case("foo%bar", "https://api.github.com/foo%25bar")]
    #[case("foo/bar", "https://api.github.com/foo%2Fbar")]
    #[case("foo?bar", "https://api.github.com/foo%3Fbar")]
    fn test_urljoin_special_chars(#[case] path: &str, #[case] expected: &str) {
        let base = Url::parse("https://api.github.com").unwrap();
        let u = urljoin(&base, [path]);
        assert_eq!(u.as_str(), expected);
    }
}
