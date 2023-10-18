use crate::http_utils::*;
use crate::labels::{LabelOperation, LabelSpec};
use ghrepo::GHRepo;
use reqwest::{
    header::{self, HeaderMap, HeaderValue, InvalidHeaderValue},
    Client, ClientBuilder, Method, Response,
};
use serde::{de::DeserializeOwned, Serialize};
use serde_json::{to_string_pretty, value::Value};
use thiserror::Error;
use url::{ParseError, Url};

static USER_AGENT: &str = concat!(
    env!("CARGO_PKG_NAME"),
    "/",
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("CARGO_PKG_REPOSITORY"),
    ")",
);

#[derive(Clone, Debug)]
pub(crate) struct GitHub {
    client: Client,
    api_url: Url,
}

impl GitHub {
    pub(crate) fn new(api_url: Url, token: &str) -> Result<GitHub, BuildClientError> {
        if api_url.scheme() != "https" {
            return Err(BuildClientError::NotHttps);
        }
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

    fn mkurl(&self, path: &str) -> Result<Url, RequestError> {
        self.api_url.join(path).map_err(|source| RequestError::Url {
            path: path.to_string(),
            source,
        })
    }

    async fn request<T: Serialize>(
        &self,
        method: reqwest::Method,
        url: Url,
        payload: Option<&T>,
    ) -> Result<Response, RequestError> {
        log::debug!("{} {}", method, url);
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

    async fn get<T: DeserializeOwned>(&self, url: Url) -> Result<T, RequestError> {
        let r = self.request::<()>(Method::GET, url.clone(), None).await?;
        match r.json::<T>().await {
            Ok(val) => Ok(val),
            Err(source) => Err(RequestError::Deserialize {
                method: Method::GET,
                url,
                source,
            }),
        }
    }

    async fn post<T: Serialize, U: DeserializeOwned>(
        &self,
        url: Url,
        payload: &T,
    ) -> Result<U, RequestError> {
        let r = self
            .request::<T>(Method::POST, url.clone(), Some(payload))
            .await?;
        match r.json::<U>().await {
            Ok(val) => Ok(val),
            Err(source) => Err(RequestError::Deserialize {
                method: Method::POST,
                url,
                source,
            }),
        }
    }

    async fn paginate<T: DeserializeOwned>(&self, mut url: Url) -> Result<Vec<T>, RequestError> {
        let mut items = Vec::new();
        loop {
            let r = self.request::<()>(Method::GET, url.clone(), None).await?;
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
        todo!()
    }

    pub(crate) async fn get_label_maker(
        &self,
        repo: GHRepo,
        dry_run: bool,
    ) -> Result<LabelMaker, RequestError> {
        todo!()
    }
}

pub(crate) struct LabelMaker;

impl LabelMaker {
    pub(crate) fn resolve(&self, spec: &LabelSpec) -> Option<LabelOperation> {
        todo!()
    }

    pub(crate) async fn execute(&self, op: LabelOperation) -> Result<(), RequestError> {
        todo!()
    }
}

#[derive(Debug, Error)]
pub(crate) enum BuildClientError {
    #[error("API URL is not HTTPS")]
    NotHttps,
    #[error("could not construct Authorization header value from token")]
    BadAuthHeader(#[source] InvalidHeaderValue),
    #[error("failed to initialize HTTP client")]
    Init(#[source] reqwest::Error),
}

#[derive(Debug, Error)]
pub(crate) enum RequestError {
    #[error("failed to construct API URL with path {path:?}")]
    Url { path: String, source: ParseError },
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

#[derive(Debug)]
pub(crate) struct Error;

pub(crate) fn get_github_token(api_url: &Url) -> Result<String, Error> {
    todo!()
}
