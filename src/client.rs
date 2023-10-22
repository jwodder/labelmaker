use crate::config::Profile;
use crate::labels::*;
use crate::util::*;
use csscolorparser::Color;
use ghrepo::GHRepo;
use reqwest::{
    header::{self, HeaderMap, HeaderValue, InvalidHeaderValue},
    Client, ClientBuilder, Method, Response, StatusCode,
};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_with::{serde_as, skip_serializing_none};
use std::cell::Cell;
use std::fmt;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use thiserror::Error;
use tokio::time::sleep;
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

static MUTATING_METHODS: &[Method] = &[Method::POST, Method::PATCH, Method::PUT, Method::DELETE];

const MUTATION_DELAY: Duration = Duration::from_secs(1);

// Retry configuration:
const RETRIES: i32 = 10;
const BACKOFF_FACTOR: f64 = 1.0;
const BACKOFF_BASE: f64 = 1.25;
const BACKOFF_MAX: f64 = 120.0;
const TOTAL_WAIT: Duration = Duration::from_secs(300);

#[derive(Clone, Debug)]
pub(crate) struct GitHub {
    client: Client,
    api_url: Url,
    last_mutation: Cell<Option<Instant>>,
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
        Ok(GitHub {
            client,
            api_url,
            last_mutation: Cell::new(None),
        })
    }

    async fn raw_request<T: Serialize>(
        &self,
        method: Method,
        url: Url,
        payload: Option<&T>,
    ) -> Result<Response, RequestError> {
        log::trace!("{} {}", method, url);
        if MUTATING_METHODS.contains(&method) {
            if let Some(lastmut) = self.last_mutation.get() {
                let delay = MUTATION_DELAY
                    .saturating_sub(Instant::now().saturating_duration_since(lastmut));
                if !delay.is_zero() {
                    log::trace!("Sleeping for {delay:?} between mutating requests");
                    sleep(delay).await;
                }
            }
        }
        let mut req = self.client.request(method.clone(), url.clone());
        if let Some(p) = payload {
            req = req.json(p);
        }
        let mut retrier = Retrier::new(method.clone(), url.clone());
        loop {
            if MUTATING_METHODS.contains(&method) {
                self.last_mutation.set(Some(Instant::now()));
            }
            let req = req
                .try_clone()
                .expect("our non-streaming requests should be clonable");
            let error = match req.send().await {
                Ok(r) if r.status().is_client_error() || r.status().is_server_error() => {
                    RetryCandidate::Status(r)
                }
                Ok(r) => return Ok(r),
                Err(source) if source.is_builder() => {
                    return Err(RequestError::Send {
                        method,
                        url,
                        source,
                    })
                }
                Err(e) => RetryCandidate::Transport(e),
            };
            let msg = error.to_string();
            let delay = retrier.handle(error).await?;
            log::warn!("{msg}; waiting {delay:?} and retrying");
            sleep(delay).await;
        }
    }

    async fn request<T: Serialize, U: DeserializeOwned>(
        &self,
        method: Method,
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
        let repo = Repository::new(self, repo);
        let mut labels = LabelSet::new(rng);
        labels.extend(repo.get_labels().await?);
        Ok(LabelMaker {
            repo,
            labels,
            dry_run,
        })
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Repository<'a> {
    client: &'a GitHub,
    repo: GHRepo,
    labels_url: Url,
}

impl<'a> Repository<'a> {
    pub(crate) fn new(client: &'a GitHub, repo: GHRepo) -> Repository<'a> {
        let labels_url = urljoin(
            &client.api_url,
            ["repos", repo.owner(), repo.name(), "labels"],
        );
        Repository {
            client,
            repo,
            labels_url,
        }
    }

    pub(crate) async fn get_labels(&self) -> Result<Vec<Label>, RequestError> {
        self.client.paginate::<Label>(self.labels_url.clone()).await
    }

    pub(crate) async fn create_label(&self, label: Label) -> Result<Label, RequestError> {
        self.client
            .post::<Label, Label>(self.labels_url.clone(), &label)
            .await
    }

    pub(crate) async fn update_label(
        &self,
        label: LabelName,
        payload: UpdateLabel,
    ) -> Result<Label, RequestError> {
        let url = urljoin(&self.labels_url, [label.as_ref()]);
        self.client.patch::<UpdateLabel, Label>(url, &payload).await
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
    #[error("failed to make {method} request to {url}")]
    Send {
        method: Method,
        url: Url,
        source: reqwest::Error,
    },
    #[error(transparent)]
    Status(Box<PrettyHttpError>),
    #[error("failed to deserialize response body from {method} request to {url}")]
    Deserialize {
        method: Method,
        url: Url,
        source: reqwest::Error,
    },
}

#[derive(Debug, Error)]
enum RetryCandidate {
    Status(Response),
    Transport(reqwest::Error),
}

impl RetryCandidate {
    async fn into_error(self, method: Method, url: Url) -> RequestError {
        match self {
            RetryCandidate::Status(r) => {
                RequestError::Status(Box::new(PrettyHttpError::new(method, r).await))
            }
            RetryCandidate::Transport(source) => RequestError::Send {
                method,
                url,
                source,
            },
        }
    }
}

impl fmt::Display for RetryCandidate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RetryCandidate::Status(r) => write!(f, "Server returned {} response", r.status()),
            RetryCandidate::Transport(e) => write!(f, "Request failed: {e}"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ResponseParts {
    pub(crate) status: StatusCode,
    pub(crate) headers: HeaderMap,
    pub(crate) text: Option<String>,
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

#[derive(Clone, Debug, Eq, PartialEq)]
struct Retrier {
    method: Method,
    url: Url,
    attempts: i32,
    stop_time: Instant,
}

impl Retrier {
    fn new(method: Method, url: Url) -> Retrier {
        Retrier {
            method,
            url,
            attempts: 0,
            stop_time: Instant::now() + TOTAL_WAIT,
        }
    }

    async fn handle(&mut self, error: RetryCandidate) -> Result<Duration, RequestError> {
        self.attempts += 1;
        if self.attempts > RETRIES {
            log::trace!("Retries exhausted");
            return self.raise(error).await;
        }
        let now = Instant::now();
        if now > self.stop_time {
            log::trace!("Maximum total retry wait time exceeded");
            return self.raise(error).await;
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
        let delay = match error {
            RetryCandidate::Transport(_) => backoff,
            RetryCandidate::Status(r) if r.status() == StatusCode::FORBIDDEN => {
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
                            time_till_timestamp(reset)
                                .map(|d| d + Duration::from_secs(1))
                                .unwrap_or_default()
                        } else {
                            Duration::ZERO
                        }
                    } else {
                        log::trace!("Secondary rate limit triggered");
                        backoff
                    }
                } else {
                    return self.raise_parts(parts);
                }
            }
            RetryCandidate::Status(r) if r.status().is_server_error() => backoff,
            _ => return self.raise(error).await,
        };
        let delay = delay.max(backoff);
        let time_left = self.stop_time.saturating_duration_since(Instant::now());
        Ok(delay.clamp(Duration::ZERO, time_left))
    }

    async fn raise<T>(&self, error: RetryCandidate) -> Result<T, RequestError> {
        Err(error
            .into_error(self.method.clone(), self.url.clone())
            .await)
    }

    fn raise_parts<T>(&self, parts: ResponseParts) -> Result<T, RequestError> {
        Err(RequestError::Status(Box::new(PrettyHttpError::from_parts(
            self.method.clone(),
            self.url.clone(),
            parts,
        ))))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct LabelMaker<'a, R: rand::Rng> {
    repo: Repository<'a>,
    labels: LabelSet<R>,
    dry_run: bool,
}

impl<'a, R: rand::Rng> LabelMaker<'a, R> {
    pub(crate) async fn make(&mut self, profile: &Profile) -> Result<(), LabelMakerError> {
        let mut res = Vec::new();
        for s in profile.specs() {
            res.extend(self.labels.resolve(s)?);
        }
        for r in res {
            match r {
                LabelResolution::Operation(op) => {
                    log::info!("{}", op.as_log_message(&self.repo.repo, self.dry_run));
                    if !self.dry_run {
                        self.execute(op).await?;
                    }
                }
                LabelResolution::Warning(wrn) => log::warn!("{wrn}"),
            }
        }
        Ok(())
    }

    async fn execute(&mut self, op: LabelOperation) -> Result<(), RequestError> {
        match op {
            LabelOperation::Create(label) => {
                let created = self.repo.create_label(label).await?;
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
                let payload = UpdateLabel {
                    new_name,
                    color,
                    description,
                };
                let updated = self.repo.update_label(name, payload).await?;
                // TODO: Should this do anything if any of the new values are
                // different from what was submitted?
                self.labels.add(updated);
            }
        }
        Ok(())
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct UpdateLabel {
    new_name: Option<LabelName>,
    #[serde_as(as = "Option<AsHashlessRgb>")]
    color: Option<Color>,
    description: Option<String>,
}

#[derive(Debug, Error)]
pub(crate) enum LabelMakerError {
    #[error(transparent)]
    Resolve(#[from] SpecResolveError),
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
