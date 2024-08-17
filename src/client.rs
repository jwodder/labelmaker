mod util;
use self::util::*;
use crate::labels::*;
use crate::profile::Profile;
use ghrepo::GHRepo;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::borrow::Cow;
use std::cell::Cell;
use std::thread::sleep;
use std::time::{Duration, Instant};
use thiserror::Error;
use ureq::{Agent, AgentBuilder, Response};
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

const MUTATION_DELAY: Duration = Duration::from_secs(1);

#[derive(Clone, Debug)]
pub(crate) struct GitHub {
    client: Agent,
    api_url: Url,
    last_mutation: Cell<Option<Instant>>,
}

impl GitHub {
    pub(crate) fn new(token: &str) -> GitHub {
        let api_url = Url::parse(GITHUB_API_URL).expect("GITHUB_API_URL should be a valid URL");
        let auth = format!("Bearer {token}");
        let client = AgentBuilder::new()
            .user_agent(USER_AGENT)
            .https_only(true)
            .middleware(move |req: ureq::Request, next: ureq::MiddlewareNext<'_>| {
                next.handle(
                    req.set("Authorization", &auth)
                        .set("Accept", "application/vnd.github+json")
                        .set("X-GitHub-Api-Version", "2022-11-28"),
                )
            })
            .build();
        GitHub {
            client,
            api_url,
            last_mutation: Cell::new(None),
        }
    }

    fn raw_request<T: Serialize>(
        &self,
        method: Method,
        url: Url,
        payload: Option<&T>,
    ) -> Result<Response, RequestError> {
        if method.is_mutating() {
            if let Some(lastmut) = self.last_mutation.get() {
                let delay = MUTATION_DELAY
                    .saturating_sub(Instant::now().saturating_duration_since(lastmut));
                if !delay.is_zero() {
                    log::trace!("Sleeping for {delay:?} between mutating requests");
                    sleep(delay);
                }
            }
        }
        let req = self.client.request_url(method.as_str(), &url);
        let mut retrier = Retrier::new(method, url.clone());
        loop {
            if method.is_mutating() {
                self.last_mutation.set(Some(Instant::now()));
            }
            let req = req.clone();
            log::trace!("{} {}", method.as_str(), url);
            let resp = if let Some(p) = payload {
                req.send_json(p)
            } else {
                req.call()
            };
            let desc = match &resp {
                Ok(_) => Cow::from("Request succeeded"),
                Err(ureq::Error::Status(code, _)) => {
                    Cow::from(format!("Server returned {code} response"))
                }
                Err(e) => Cow::from(format!("Request failed: {e}")),
            };
            match retrier.handle(resp)? {
                RetryDecision::Success(r) => return Ok(r),
                RetryDecision::Retry(delay) => {
                    log::warn!("{desc}; waiting {delay:?} and retrying");
                    sleep(delay);
                }
            }
        }
    }

    fn request<T: Serialize, U: DeserializeOwned>(
        &self,
        method: Method,
        url: Url,
        payload: Option<&T>,
    ) -> Result<U, RequestError> {
        let r = self.raw_request::<T>(method, url.clone(), payload)?;
        match r.into_json::<U>() {
            Ok(val) => Ok(val),
            Err(source) => Err(RequestError::Deserialize {
                method,
                url,
                source,
            }),
        }
    }

    fn get<T: DeserializeOwned>(&self, url: Url) -> Result<T, RequestError> {
        self.request::<(), T>(Method::Get, url, None)
    }

    fn post<T: Serialize, U: DeserializeOwned>(
        &self,
        url: Url,
        payload: &T,
    ) -> Result<U, RequestError> {
        self.request::<T, U>(Method::Post, url, Some(payload))
    }

    fn patch<T: Serialize, U: DeserializeOwned>(
        &self,
        url: Url,
        payload: &T,
    ) -> Result<U, RequestError> {
        self.request::<T, U>(Method::Patch, url, Some(payload))
    }

    fn paginate<T: DeserializeOwned>(&self, mut url: Url) -> Result<Vec<T>, RequestError> {
        let mut items = Vec::new();
        loop {
            let r = self.raw_request::<()>(Method::Get, url.clone(), None)?;
            let next_url = get_next_link(&r);
            match r.into_json::<Vec<T>>() {
                Ok(page) => items.extend(page),
                Err(source) => {
                    return Err(RequestError::Deserialize {
                        method: Method::Get,
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

    pub(crate) fn whoami(&self) -> Result<String, RequestError> {
        Ok(self.get::<User>(urljoin(&self.api_url, ["user"]))?.login)
    }

    pub(crate) fn get_label_maker<R: rand::Rng>(
        &self,
        repo: GHRepo,
        rng: R,
        dry_run: bool,
    ) -> Result<LabelMaker<'_, R>, RequestError> {
        log::debug!("Fetching current labels for {repo} ...");
        let repo = Repository::new(self, repo);
        let mut labels = LabelSet::new(rng);
        labels.extend(repo.get_labels()?);
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

    pub(crate) fn get_labels(&self) -> Result<Vec<Label>, RequestError> {
        self.client.paginate::<Label>(self.labels_url.clone())
    }

    fn create_label(&self, label: Label) -> Result<Label, RequestError> {
        self.client
            .post::<Label, Label>(self.labels_url.clone(), &label)
    }

    fn update_label(&self, label: LabelName, payload: UpdateLabel) -> Result<Label, RequestError> {
        let url = urljoin(&self.labels_url, [label]);
        self.client.patch::<UpdateLabel, Label>(url, &payload)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
struct User {
    login: String,
}

#[derive(Debug, Error)]
pub(crate) enum RequestError {
    #[error("failed to make {method} request to {url}")]
    Send {
        method: Method,
        url: Url,
        source: Box<ureq::Transport>,
    },
    #[error(transparent)]
    Status(PrettyHttpError),
    #[error("failed to deserialize response body from {method} request to {url}")]
    Deserialize {
        method: Method,
        url: Url,
        source: std::io::Error,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct LabelMaker<'a, R: rand::Rng> {
    repo: Repository<'a>,
    labels: LabelSet<R>,
    dry_run: bool,
}

impl<R: rand::Rng> LabelMaker<'_, R> {
    pub(crate) fn make(&mut self, profile: &Profile) -> Result<(), LabelMakerError> {
        let mut res = Vec::new();
        for s in profile.specs() {
            res.extend(self.labels.resolve(s)?);
        }
        if res.is_empty() {
            log::info!("No changes to {}", self.repo.repo);
            return Ok(());
        }
        for r in res {
            match r {
                LabelResolution::Operation(op) => {
                    log::info!("{}", op.as_log_message(&self.repo.repo, self.dry_run));
                    if !self.dry_run {
                        self.execute(op)?;
                    }
                }
                LabelResolution::Warning(wrn) => log::warn!("{wrn}"),
            }
        }
        Ok(())
    }

    fn execute(&mut self, op: LabelOperation) -> Result<(), RequestError> {
        match op {
            LabelOperation::Create(label) => {
                let created = self.repo.create_label(label)?;
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
                let updated = self.repo.update_label(name, payload)?;
                // TODO: Should this do anything if any of the new values are
                // different from what was submitted?
                self.labels.add(updated);
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
struct UpdateLabel {
    #[serde(skip_serializing_if = "Option::is_none")]
    new_name: Option<LabelName>,
    #[serde(skip_serializing_if = "Option::is_none")]
    color: Option<Color>,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<Description>,
}

#[derive(Debug, Error)]
pub(crate) enum LabelMakerError {
    #[error(transparent)]
    Resolve(#[from] SpecResolveError),
    #[error("GitHub API request failed")]
    Request(#[from] RequestError),
}

#[cfg(test)]
mod tests {
    use super::*;

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
