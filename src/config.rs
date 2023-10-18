use crate::labels::{ColorSpec, LabelName, LabelOptions, LabelSpec, OnRenameClash};
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use std::collections::{hash_map::Entry, HashMap, HashSet};
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
    #[error("profile not found: {0:?}")]
    NoSuchProfile(String),
    #[error("multiple definitions for label {0:?} found in profile")]
    RepeatedLabel(String),
    #[error("label {label:?} defined but also would be renamed to {renamer:?}")]
    LabelRenamed { label: String, renamer: String },
    #[error("{renamed:?} would be renamed to both {label1:?} and {label2:?}")]
    RenameConflict {
        renamed: String,
        label1: String,
        label2: String,
    },
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
        let mut defined_labels = HashSet::<LabelName>::with_capacity(raw.label.len());
        let mut renamed_from_to = HashMap::<LabelName, LabelName>::new();
        for lbl in &raw.label {
            let PartialLabelSpec {
                name,
                rename_from,
                options,
            } = lbl;
            let name = LabelName::new(name.to_string());
            if let Some(renamer) = renamed_from_to.remove(&name) {
                return Err(ConfigError::LabelRenamed {
                    label: name.into_inner(),
                    renamer: renamer.into_inner(),
                });
            }
            let mut ln_rename_from = Vec::with_capacity(rename_from.len());
            for n in rename_from {
                let key = LabelName::new(n.to_string());
                if defined_labels.contains(&key) {
                    return Err(ConfigError::LabelRenamed {
                        label: key.into_inner(),
                        renamer: name.into_inner(),
                    });
                }
                match renamed_from_to.entry(key.clone()) {
                    Entry::Occupied(oc) => {
                        return Err(ConfigError::RenameConflict {
                            renamed: key.into_inner(),
                            label1: name.into_inner(),
                            label2: oc.remove().into_inner(),
                        });
                    }
                    Entry::Vacant(vac) => {
                        vac.insert(name.clone());
                    }
                }
                ln_rename_from.push(key);
            }
            // Do this check after the rename-from checks so that a label
            // that's renamed from itself or a variant casing doesn't get a
            // false positive.
            if !defined_labels.insert(name.clone()) {
                return Err(ConfigError::RepeatedLabel(name.into_inner()));
            }
            specs.push(LabelSpec {
                name,
                rename_from: ln_rename_from,
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
