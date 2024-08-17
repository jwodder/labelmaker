use super::RequestError;
use indenter::indented;
use mime::{Mime, JSON};
use serde_json::{to_string_pretty, value::Value};
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use ureq::Response;
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

    // Takes the return value of a call to `Request::call()` or similar.
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
    pub(super) fn handle(
        &mut self,
        resp: Result<Response, ureq::Error>,
    ) -> Result<RetryDecision, RequestError> {
        self.attempts += 1;
        if self.attempts > RETRIES {
            log::trace!("Retries exhausted");
            return self.finalize(resp);
        }
        let now = Instant::now();
        if now > self.stop_time {
            log::trace!("Maximum total retry wait time exceeded");
            return self.finalize(resp);
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
            Err(ureq::Error::Status(403, r)) => {
                let parts = ResponseParts::from_response(r);
                if let Some(v) = parts.header("retry-after") {
                    let secs = v.parse::<u64>().ok().map(|n| n + 1);
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
                        .header("x-ratelimit-remaining")
                        .is_some_and(|v| v == "0")
                    {
                        if let Some(reset) = parts
                            .header("x-ratelimit-reset")
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
            Err(ureq::Error::Status(code, _)) if code >= 500 => backoff,
            Err(_) => backoff,
            Ok(_) => return self.finalize(resp),
        };
        let delay = delay.max(backoff);
        let time_left = self.stop_time.saturating_duration_since(Instant::now());
        Ok(RetryDecision::Retry(delay.clamp(Duration::ZERO, time_left)))
    }

    fn finalize(&self, resp: Result<Response, ureq::Error>) -> Result<RetryDecision, RequestError> {
        match resp {
            Ok(r) => Ok(RetryDecision::Success(r)),
            Err(ureq::Error::Status(_, r)) => {
                Err(RequestError::Status(PrettyHttpError::new(self.method, r)))
            }
            Err(ureq::Error::Transport(source)) => Err(RequestError::Send {
                method: self.method,
                url: self.url.clone(),
                source: Box::new(source),
            }),
        }
    }

    fn finalize_parts<T>(&self, parts: ResponseParts) -> Result<T, RequestError> {
        Err(RequestError::Status(PrettyHttpError::from_parts(
            self.method,
            self.url.clone(),
            parts,
        )))
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub(super) enum RetryDecision {
    Success(Response),
    Retry(Duration),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum Method {
    Get,
    Post,
    Patch,
    //Put,
    //Delete,
}

impl Method {
    pub(super) fn is_mutating(&self) -> bool {
        matches!(
            self,
            Method::Post | Method::Patch /*| Method::Put | Method::Delete*/
        )
    }

    pub(super) fn as_str(&self) -> &'static str {
        match self {
            Method::Get => "GET",
            Method::Post => "POST",
            Method::Patch => "PATCH",
            //Method::Put => "PUT",
            //Method::Delete => "DELETE",
        }
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ResponseParts {
    status: u16,
    // Keys are lowercased:
    headers: HashMap<String, String>,
    text: Option<String>,
}

impl ResponseParts {
    fn from_response(r: Response) -> ResponseParts {
        let status = r.status();
        let mut headers = HashMap::new();
        for name in r.headers_names() {
            if let Some(value) = r.header(&name) {
                headers.insert(name, value.to_owned());
            }
        }
        let text = r.into_string().ok();
        ResponseParts {
            status,
            headers,
            text,
        }
    }

    fn header(&self, s: &str) -> Option<&str> {
        self.headers
            .get(&s.to_ascii_lowercase())
            .map(String::as_str)
    }
}

/// Error raised for a 4xx or 5xx HTTP response that includes the response body
/// — and, if that body is JSON, it's pretty-printed
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PrettyHttpError {
    method: Method,
    url: String,
    status: u16,
    body: Option<String>,
}

impl PrettyHttpError {
    pub(super) fn new(method: Method, r: Response) -> PrettyHttpError {
        let url = r.get_url().to_owned();
        let status = r.status();
        // If the response body is JSON, pretty-print it.
        let body = if is_json_response(&r) {
            r.into_json::<Value>().ok().map(|v| {
                to_string_pretty(&v).expect("Re-JSONifying a JSON response should not fail")
            })
        } else {
            r.into_string().ok()
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
        let body = if parts
            .headers
            .get("content-type")
            .is_some_and(|v| is_json_content_type(v))
        {
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
            url: url.into(),
            status,
            body,
        }
    }
}

impl fmt::Display for PrettyHttpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    let header_value = r.header("Link")?;
    parse_link_header::parse_with_rel(header_value)
        .ok()?
        .get("next")
        .map(|link| link.uri.clone())
}

/// Returns `true` iff the response's Content-Type header indicates the body is
/// JSON
fn is_json_response(r: &Response) -> bool {
    r.header("Content-Type").is_some_and(is_json_content_type)
}

fn is_json_content_type(ct_value: &str) -> bool {
    ct_value.parse::<Mime>().ok().is_some_and(|ct| {
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
    #[case("foo#bar", "https://api.github.com/base/foo%23bar")]
    #[case("foo%bar", "https://api.github.com/base/foo%25bar")]
    #[case("foo/bar", "https://api.github.com/base/foo%2Fbar")]
    #[case("foo?bar", "https://api.github.com/base/foo%3Fbar")]
    #[ignore]
    #[case(".", "https://api.github.com/base/%2E")]
    #[ignore]
    #[case("..", "https://api.github.com/base/%2E%2E")]
    fn test_urljoin_special_chars(#[case] path: &str, #[case] expected: &str) {
        let base = Url::parse("https://api.github.com/base").unwrap();
        let u = urljoin(&base, [path]);
        assert_eq!(u.as_str(), expected);
    }
}
