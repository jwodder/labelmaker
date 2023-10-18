use csscolorparser::Color;
use indenter::indented;
use mime::{Mime, JSON};
use reqwest::{Method, Response, StatusCode};
use serde::{Serialize, Serializer};
use std::fmt::{self, Write};
use url::Url;

pub(crate) type ICaseStr = unicase::UniCase<String>;

/// Error raised for a 4xx or 5xx HTTP response that includes the response body
/// — and, if that body is JSON, it's pretty-printed
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PrettyHttpError {
    pub(crate) method: Method,
    pub(crate) url: Url,
    pub(crate) status: StatusCode,
    pub(crate) body: Option<String>,
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
pub(crate) fn is_json_response(r: &Response) -> bool {
    r.headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse::<Mime>().ok())
        .map(|ct| {
            ct.type_() == "application" && (ct.subtype() == "json" || ct.suffix() == Some(JSON))
        })
        .unwrap_or(false)
}

pub(crate) fn color2rgbhex(color: &Color) -> String {
    let [r, g, b, _] = color.to_rgba8();
    format!("{:02x}{:02x}{:02x}", r, g, b)
}

pub(crate) fn serialize_color<S: Serializer>(
    color: &Color,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    color2rgbhex(color).serialize(serializer)
}

pub(crate) fn serialize_option_color<S: Serializer>(
    color: &Option<Color>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    color.as_ref().map(color2rgbhex).serialize(serializer)
}
