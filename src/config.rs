use crate::labels::{ColorSpec, LabelSpec, OnRenameClash};
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use std::collections::HashMap;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Config {
    defaults: TopOptions,
    profile: HashMap<String, RawProfile>,
}

#[derive(Debug)]
pub(crate) struct Error;

impl Config {
    pub(crate) fn load(path: patharg::InputArg) -> Result<Config, Error> {
        todo!()
    }

    pub(crate) fn get_profile(&self, name: &str) -> Result<Profile, Error> {
        todo!()
    }

    pub(crate) fn get_default_profile(&self) -> Result<Profile, Error> {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Profile {
    pub(crate) name: String,
    pub(crate) specs: Vec<LabelSpec>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct TopOptions {
    #[serde(default = "default_profile")]
    profile: String,
    #[serde(flatten)]
    options: PartialLabelOptions,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct PartialLabelOptions {
    create: Option<bool>,
    update: Option<bool>,
    color: Option<ColorSpec>,
    description: Option<String>,
    on_rename_clash: Option<OnRenameClash>,
    enforce_case: Option<bool>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct RawProfile {
    defaults: PartialLabelOptions,
    label: Vec<PartialLabelSpec>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct PartialLabelSpec {
    name: String,
    #[serde(default)]
    rename_from: Vec<String>,
    #[serde(flatten)]
    options: PartialLabelOptions,
}

fn default_profile() -> String {
    String::from("default")
}
