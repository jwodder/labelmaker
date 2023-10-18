use crate::labels::{ColorSpec, LabelName, LabelOptions, LabelSpec, OnRenameClash};
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Config {
    #[serde(default)]
    defaults: TopOptions,
    #[serde(default)]
    profile: HashMap<String, RawProfile>,
}

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub(crate) enum ConfigError {
    #[error("profile not found in config file: {0:?}")]
    NoSuchProfile(String),
}

impl Config {
    pub(crate) fn load(path: patharg::InputArg) -> Result<Config, ConfigError> {
        todo!()
    }

    pub(crate) fn get_profile(&self, name: &str) -> Result<Profile, ConfigError> {
        let Some(raw) = self.profile.get(name) else {
            return Err(ConfigError::NoSuchProfile(name.into()));
        };
        let settings = LabelOptions::default()
            .with_overrides(&self.defaults.options)
            .with_overrides(&raw.defaults);
        // TODO: Check for inter-label conflicts!!!
        let mut specs = Vec::with_capacity(raw.label.len());
        for lbl in &raw.label {
            let PartialLabelSpec {
                name,
                rename_from,
                options,
            } = lbl;
            specs.push(LabelSpec {
                name: LabelName::new(name.to_string()),
                rename_from: rename_from
                    .iter()
                    .map(|s| LabelName::new(s.to_string()))
                    .collect(),
                options: settings.with_overrides(options),
            });
        }
        Ok(Profile {
            name: name.into(),
            specs,
        })
    }

    pub(crate) fn get_default_profile(&self) -> Result<Profile, ConfigError> {
        self.get_profile(&self.defaults.profile)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Profile {
    pub(crate) name: String,
    pub(crate) specs: Vec<LabelSpec>,
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub(crate) struct TopOptions {
    #[serde(default = "default_profile")]
    profile: String,
    #[serde(default, flatten)]
    options: PartialLabelOptions,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
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
    #[serde(default)]
    defaults: PartialLabelOptions,
    #[serde(default)]
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
