use crate::labels::{ColorSpec, OnRenameClash};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

static DEFAULT_PROFILE: &str = "default";

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Config {
    defaults: TopOptions,
    profile: HashMap<String, RawProfile>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct TopOptions {
    // TODO: Default this to DEFAULT_PROFILE
    profile: Option<String>,
    #[serde(flatten)]
    options: PartialLabelOptions,
}

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
