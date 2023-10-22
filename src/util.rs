use crate::client::ResponseParts;
use indenter::indented;
use mime::{Mime, JSON};
use reqwest::{header::HeaderMap, Method, Response, StatusCode};
use serde_json::{to_string_pretty, value::Value};
use std::fmt::{self, Write};
use url::Url;

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

    pub(crate) fn from_parts(method: Method, url: Url, parts: ResponseParts) -> PrettyHttpError {
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
        write!(f, "Request to {} returned {}", self.url, self.status)?;
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
    Some(
        parse_link_header::parse_with_rel(header_value)
            .ok()?
            .get("next")?
            .uri
            .clone(),
    )
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
