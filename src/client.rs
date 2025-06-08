use crate::labels::*;
use crate::profile::Profile;
use ghrepo::GHRepo;
use minigh::RequestError;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/* <https://github.com/jwodder/minigh/issues/17>
static USER_AGENT: &str = concat!(
    env!("CARGO_PKG_NAME"),
    "/",
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("CARGO_PKG_REPOSITORY"),
    ")",
);
*/

#[derive(Clone, Debug)]
pub(crate) struct GitHub(minigh::Client);

impl GitHub {
    pub(crate) fn new(token: &str) -> Result<GitHub, minigh::BuildClientError> {
        Ok(GitHub(minigh::Client::new(token)?))
    }

    pub(crate) fn whoami(&self) -> Result<String, RequestError> {
        Ok(self.0.get::<User>("user")?.login)
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
    labels_path: String,
}

impl<'a> Repository<'a> {
    pub(crate) fn new(client: &'a GitHub, repo: GHRepo) -> Repository<'a> {
        let labels_path = format!("/repos/{}/{}/labels", repo.owner(), repo.name());
        Repository {
            client,
            repo,
            labels_path,
        }
    }

    pub(crate) fn get_labels(&self) -> Result<Vec<Label>, RequestError> {
        self.client.0.paginate::<Label>(&self.labels_path).collect()
    }

    fn create_label(&self, label: Label) -> Result<Label, RequestError> {
        self.client
            .0
            .post::<Label, Label>(&self.labels_path, &label)
    }

    fn update_label(&self, label: LabelName, payload: UpdateLabel) -> Result<Label, RequestError> {
        let path = format!("{}/{}", self.labels_path, label);
        self.client.0.patch::<UpdateLabel, Label>(&path, &payload)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
struct User {
    login: String,
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
