use crate::labels::{LabelOperation, LabelSpec};
use ghrepo::GHRepo;
use serde::{de::DeserializeOwned, Serialize};

pub(crate) struct Client;

impl Client {
    pub(crate) fn new(api_url: url::Url, token: &str) -> Client {
        todo!()
    }

    fn get<T: DeserializeOwned>(&self, urlpath: &str) -> Result<T, Error> {
        todo!()
    }

    fn post<T: Serialize, U: DeserializeOwned>(
        &self,
        urlpath: &str,
        payload: T,
    ) -> Result<U, Error> {
        todo!()
    }

    fn paginate<T: DeserializeOwned>(&self, urlpath: &str) -> Result<Vec<T>, Error> {
        todo!()
    }

    pub(crate) fn whoami(&self) -> Result<String, Error> {
        todo!()
    }

    pub(crate) fn get_label_maker(&self, repo: GHRepo, dry_run: bool) -> Result<LabelMaker, Error> {
        todo!()
    }
}

pub(crate) struct LabelMaker;

impl LabelMaker {
    pub(crate) fn resolve(&self, spec: &LabelSpec) -> Result<Option<LabelOperation>, Error> {
        todo!()
    }

    pub(crate) fn execute(&self, op: LabelOperation) -> Result<(), Error> {
        todo!()
    }
}

#[derive(Debug)]
pub(crate) struct Error;

pub(crate) fn get_github_token(api_url: &url::Url) -> Result<String, Error> {
    todo!()
}
