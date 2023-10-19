use crate::labels::*;
use crate::util::*;
use csscolorparser::Color;
use ghrepo::GHRepo;
use reqwest::{
    header::{self, HeaderMap, HeaderValue, InvalidHeaderValue},
    Client, ClientBuilder, Method, Response,
};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::{to_string_pretty, value::Value};
use serde_with::skip_serializing_none;
use std::borrow::Borrow;
use thiserror::Error;
use url::Url;

static USER_AGENT: &str = concat!(
    env!("CARGO_PKG_NAME"),
    "/",
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("CARGO_PKG_REPOSITORY"),
    ")",
);

static GITHUB_API_URL: &str = "https://api.github.com";

#[derive(Clone, Debug)]
pub(crate) struct GitHub {
    client: Client,
    api_url: Url,
}

impl GitHub {
    pub(crate) fn new(token: &str) -> Result<GitHub, BuildClientError> {
        let api_url = Url::parse(GITHUB_API_URL).expect("GITHUB_API_URL should be a valid URL");
        let mut headers = HeaderMap::new();
        let mut auth = header::HeaderValue::try_from(format!("Bearer {token}"))
            .map_err(BuildClientError::BadAuthHeader)?;
        auth.set_sensitive(true);
        headers.insert(header::AUTHORIZATION, auth);
        headers.insert(
            header::ACCEPT,
            HeaderValue::from_static("application/vnd.github+json"),
        );
        headers.insert(
            "X-GitHub-Api-Version",
            HeaderValue::from_static("2022-11-28"),
        );
        let client = ClientBuilder::new()
            .user_agent(USER_AGENT)
            .default_headers(headers)
            .https_only(true)
            .build()
            .map_err(BuildClientError::Init)?;
        Ok(GitHub { client, api_url })
    }

    async fn raw_request<T: Serialize>(
        &self,
        method: reqwest::Method,
        url: Url,
        payload: Option<&T>,
    ) -> Result<Response, RequestError> {
        log::trace!("{} {}", method, url);
        let mut req = self.client.request(method.clone(), url.clone());
        if let Some(p) = payload {
            req = req.json(p);
        }
        match req.send().await {
            Ok(r) if r.status().is_client_error() || r.status().is_server_error() => {
                let status = r.status();
                // If the response body is JSON, pretty-print it.
                let body = if is_json_response(&r) {
                    r.json::<Value>().await.ok().map(|v| {
                        to_string_pretty(&v).expect("Re-JSONifying a JSON response should not fail")
                    })
                } else {
                    r.text().await.ok()
                };
                Err(RequestError::Status(Box::new(PrettyHttpError {
                    method,
                    url,
                    status,
                    body,
                })))
            }
            Ok(r) => Ok(r),
            Err(source) => Err(RequestError::Send {
                method,
                url,
                source,
            }),
        }
    }

    async fn request<T: Serialize, U: DeserializeOwned>(
        &self,
        method: reqwest::Method,
        url: Url,
        payload: Option<&T>,
    ) -> Result<U, RequestError> {
        let r = self
            .raw_request::<T>(method.clone(), url.clone(), payload)
            .await?;
        match r.json::<U>().await {
            Ok(val) => Ok(val),
            Err(source) => Err(RequestError::Deserialize {
                method,
                url,
                source,
            }),
        }
    }

    async fn get<T: DeserializeOwned>(&self, url: Url) -> Result<T, RequestError> {
        self.request::<(), T>(Method::GET, url, None).await
    }

    async fn post<T: Serialize, U: DeserializeOwned>(
        &self,
        url: Url,
        payload: &T,
    ) -> Result<U, RequestError> {
        self.request::<T, U>(Method::POST, url, Some(payload)).await
    }

    async fn patch<T: Serialize, U: DeserializeOwned>(
        &self,
        url: Url,
        payload: &T,
    ) -> Result<U, RequestError> {
        self.request::<T, U>(Method::PATCH, url, Some(payload))
            .await
    }

    async fn paginate<T: DeserializeOwned>(&self, mut url: Url) -> Result<Vec<T>, RequestError> {
        let mut items = Vec::new();
        loop {
            let r = self
                .raw_request::<()>(Method::GET, url.clone(), None)
                .await?;
            let next_url = get_next_link(&r);
            match r.json::<Vec<T>>().await {
                Ok(page) => items.extend(page),
                Err(source) => {
                    return Err(RequestError::Deserialize {
                        method: Method::GET,
                        url,
                        source,
                    })
                }
            }
            match next_url {
                Some(u) => url = u,
                None => return Ok(items),
            }
        }
    }

    pub(crate) async fn whoami(&self) -> Result<String, RequestError> {
        Ok(self
            .get::<User>(urljoin(&self.api_url, ["user"]))
            .await?
            .login)
    }

    pub(crate) async fn get_label_maker<R: rand::Rng>(
        &self,
        repo: GHRepo,
        rng: R,
        dry_run: bool,
    ) -> Result<LabelMaker<'_, R>, RequestError> {
        log::debug!("Fetching current labels for {repo} ...");
        let labels_url = urljoin(&self.api_url, [repo.owner(), repo.name(), "labels"]);
        let mut labels = LabelSet::new(rng);
        labels.extend(self.paginate::<Label>(labels_url.clone()).await?);
        Ok(LabelMaker {
            client: self,
            repo,
            labels,
            labels_url,
            dry_run,
        })
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
struct User {
    login: String,
}

#[derive(Debug, Error)]
pub(crate) enum BuildClientError {
    #[error("could not construct Authorization header value from token")]
    BadAuthHeader(#[source] InvalidHeaderValue),
    #[error("failed to initialize HTTP client")]
    Init(#[source] reqwest::Error),
}

#[derive(Debug, Error)]
pub(crate) enum RequestError {
    #[error("failed to deserialize response body from {method} request to {url}")]
    Deserialize {
        method: Method,
        url: Url,
        source: reqwest::Error,
    },
    #[error("failed to make {method} request to {url}")]
    Send {
        method: Method,
        url: Url,
        source: reqwest::Error,
    },
    #[error(transparent)]
    Status(#[from] Box<PrettyHttpError>),
}

#[derive(Clone, Debug)]
pub(crate) struct LabelMaker<'a, R: rand::Rng> {
    client: &'a GitHub,
    repo: GHRepo,
    labels: LabelSet<R>,
    labels_url: Url,
    dry_run: bool,
}

impl<'a, R: rand::Rng> LabelMaker<'a, R> {
    pub(crate) async fn make<I>(&mut self, specs: I) -> Result<(), LabelMakerError>
    where
        I: IntoIterator,
        I::Item: Borrow<LabelSpec>,
    {
        let mut res = Vec::new();
        for s in specs {
            res.extend(self.resolve(s.borrow())?);
        }
        for r in res {
            match r {
                LabelResolution::Operation(op) => {
                    log::info!("{}", op.as_log_message(&self.repo, self.dry_run));
                    if !self.dry_run {
                        self.execute(op).await?;
                    }
                }
                LabelResolution::Warning(wrn) => log::warn!("{wrn}"),
            }
        }
        Ok(())
    }

    pub(crate) fn resolve(&mut self, spec: &LabelSpec) -> Result<Vec<LabelResolution>, LabelError> {
        self.labels.resolve(spec)
    }

    pub(crate) async fn execute(&mut self, op: LabelOperation) -> Result<(), RequestError> {
        match op {
            LabelOperation::Create(label) => {
                let created = self
                    .client
                    .post::<Label, Label>(self.labels_url.clone(), &label)
                    .await?;
                // TODO: Should this do anything if any of the new label's
                // values are different from what was submitted?
                self.labels.add(created);
            }
            LabelOperation::Update {
                name,
                new_name,
                color,
                description,
            } => {
                let url = urljoin(&self.labels_url, [name.as_ref()]);
                let payload = UpdateLabel {
                    new_name,
                    color,
                    description,
                };
                let updated = self
                    .client
                    .patch::<UpdateLabel, Label>(url, &payload)
                    .await?;
                // TODO: Should this do anything if any of the new values are
                // different from what was submitted?
                self.labels.add(updated);
            }
        }
        Ok(())
    }
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Serialize)]
struct UpdateLabel {
    new_name: Option<LabelName>,
    #[serde(serialize_with = "serialize_option_color")]
    color: Option<Color>,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
}

#[derive(Debug, Error)]
pub(crate) enum LabelMakerError {
    #[error(transparent)]
    Label(#[from] LabelError),
    #[error("GitHub API request failed")]
    Request(#[from] RequestError),
}

fn urljoin<I>(url: &Url, segments: I) -> Url
where
    I: IntoIterator,
    I::Item: AsRef<str>,
{
    let mut url = url.clone();
    // We have to convert to an owned String so that we're not trying to modify
    // `url` with something immutably borrowed from it.
    if let Some(p) = url
        .path()
        .strip_suffix('/')
        .filter(|s| !s.is_empty())
        .map(String::from)
    {
        url.set_path(&p);
    }
    url.path_segments_mut()
        .expect("API URL should be able to be a base")
        .extend(segments);
    url
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

    #[test]
    fn test_serialize_update_label_color() {
        let update = UpdateLabel {
            new_name: None,
            color: Some("red".parse().unwrap()),
            description: None,
        };
        assert_eq!(
            serde_json::to_string(&update).unwrap(),
            r#"{"color":"ff0000"}"#
        );
    }

    #[test]
    fn test_serialize_update_label_no_color() {
        let update = UpdateLabel {
            new_name: Some("foo".parse().unwrap()),
            color: None,
            description: None,
        };
        assert_eq!(
            serde_json::to_string(&update).unwrap(),
            r#"{"new_name":"foo"}"#
        );
    }
}
