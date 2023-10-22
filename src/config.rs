use crate::labels::*;
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Config {
    #[serde(default)]
    defaults: TopOptions,
    #[serde(default)]
    profiles: HashMap<String, RawProfile>,
}

impl Config {
    pub(crate) fn load<P: AsRef<Path>>(path: P) -> Result<Config, ConfigError> {
        match std::fs::read_to_string(&path) {
            Ok(s) => Config::from_toml(&s),
            Err(source) => Err(ConfigError::Read {
                path: path.as_ref().to_owned(),
                source,
            }),
        }
    }

    pub(crate) fn from_toml(s: &str) -> Result<Config, ConfigError> {
        toml::from_str::<Config>(s).map_err(ConfigError::from)
    }

    pub(crate) fn from_labels(profile: String, mut labels: Vec<Label>) -> Config {
        labels.sort_unstable_by(|lb1, lb2| lb1.name.cmp(&lb2.name));
        let labels = labels
            .into_iter()
            .map(|lbl| PartialLabelSpec {
                name: lbl.name,
                rename_from: Vec::new(),
                options: PartialLabelOptions {
                    color: Some(ColorSpec::Fixed(lbl.color)),
                    description: lbl.description,
                    ..PartialLabelOptions::default()
                },
            })
            .collect();
        let p = RawProfile {
            defaults: PartialLabelOptions::default(),
            labels,
        };
        let defaults = TopOptions {
            profile: profile.clone(),
            options: PartialLabelOptions {
                color: Some(ColorSpec::default()),
                ..PartialLabelOptions::default()
            },
        };
        Config {
            defaults,
            profiles: HashMap::from([(profile, p)]),
        }
    }

    pub(crate) fn to_toml(&self) -> Result<String, ConfigError> {
        toml::to_string(self).map_err(Into::into)
    }

    pub(crate) fn dump(&self, outfile: patharg::OutputArg) -> Result<(), ConfigError> {
        match outfile.write(self.to_toml()?) {
            Ok(()) => Ok(()),
            Err(source) => Err(ConfigError::Write {
                path: outfile,
                source,
            }),
        }
    }

    pub(crate) fn get_profile(&self, name: &str) -> Result<Profile, ConfigError> {
        let Some(raw) = self.profiles.get(name) else {
            return Err(ConfigError::NoSuchProfile(name.into()));
        };
        let settings = LabelOptions::default()
            .with_overrides(&self.defaults.options)
            .with_overrides(&raw.defaults);
        let mut specs = Vec::with_capacity(raw.labels.len());
        let mut defined_labels = HashSet::<ICaseName>::with_capacity(raw.labels.len());
        let mut renamed_from_to = HashMap::<ICaseName, LabelName>::new();
        for lbl in &raw.labels {
            let PartialLabelSpec {
                name,
                rename_from,
                options,
            } = lbl;
            let name = name.clone();
            let iname = name.to_icase();
            if !defined_labels.insert(iname.clone()) {
                return Err(ConfigError::RepeatedLabel(name));
            }
            if let Some(renamer) = renamed_from_to.remove(&iname) {
                return Err(ConfigError::LabelRenamed {
                    label: name,
                    renamer,
                });
            }
            let mut rename_from2 = Vec::with_capacity(rename_from.len());
            let mut renamed_here = HashSet::<ICaseName>::new();
            for n in rename_from {
                let src = n.clone();
                let isrc = src.to_icase();
                if isrc == iname {
                    return Err(ConfigError::SelfRename(name));
                }
                if defined_labels.contains(&isrc) {
                    return Err(ConfigError::LabelRenamed {
                        label: src,
                        renamer: name,
                    });
                }
                if !renamed_here.insert(isrc.clone()) {
                    continue;
                }
                match renamed_from_to.entry(isrc.clone()) {
                    Entry::Occupied(oc) => {
                        return Err(ConfigError::RenameConflict {
                            renamed: src,
                            label1: oc.remove(),
                            label2: name,
                        });
                    }
                    Entry::Vacant(vac) => {
                        vac.insert(name.clone());
                    }
                }
                rename_from2.push(src);
            }
            specs.push(LabelSpec {
                name,
                rename_from: rename_from2,
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
    name: String,
    // Invariant (enforced on creation by Config::get_profile): The various
    // LabelSpecs do not step on each others' toes
    specs: Vec<LabelSpec>,
}

impl Profile {
    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn specs(&self) -> &[LabelSpec] {
        &self.specs
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct TopOptions {
    #[serde(default = "default_profile")]
    profile: String,
    #[serde(default, flatten)]
    options: PartialLabelOptions,
}

impl Default for TopOptions {
    fn default() -> TopOptions {
        TopOptions {
            profile: default_profile(),
            options: PartialLabelOptions::default(),
        }
    }
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct PartialLabelOptions {
    pub(crate) create: Option<bool>,
    pub(crate) update: Option<bool>,
    pub(crate) color: Option<ColorSpec>,
    pub(crate) description: Option<String>,
    pub(crate) on_rename_clash: Option<OnRenameClash>,
    pub(crate) enforce_case: Option<bool>,
}

impl PartialLabelOptions {
    fn is_all_none(&self) -> bool {
        self == &PartialLabelOptions::default()
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct RawProfile {
    #[serde(default, skip_serializing_if = "PartialLabelOptions::is_all_none")]
    defaults: PartialLabelOptions,
    #[serde(default)]
    labels: Vec<PartialLabelSpec>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct PartialLabelSpec {
    name: LabelName,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    rename_from: Vec<LabelName>,
    #[serde(flatten)]
    options: PartialLabelOptions,
}

#[derive(Debug, Error)]
pub(crate) enum ConfigError {
    #[error("failed to read file {}", .path.display())]
    Read {
        path: PathBuf,
        source: std::io::Error,
    },
    #[error(transparent)]
    Parse(#[from] toml::de::Error),
    #[error("profile not found: {0:?}")]
    NoSuchProfile(String),
    #[error("multiple definitions for label {0:?} in profile")]
    RepeatedLabel(LabelName),
    #[error("label {0:?} cannot be renamed from itself")]
    SelfRename(LabelName),
    #[error("label {label:?} defined but also would be renamed to {renamer:?}")]
    LabelRenamed {
        label: LabelName,
        renamer: LabelName,
    },
    #[error("label {renamed:?} would be renamed to both {label1:?} and {label2:?}")]
    RenameConflict {
        renamed: LabelName,
        label1: LabelName,
        label2: LabelName,
    },
    #[error("failed serializing configuration")]
    Serialize(#[from] toml::ser::Error),
    #[error("failed to write to {path:#}")]
    Write {
        path: patharg::OutputArg,
        source: std::io::Error,
    },
}

fn default_profile() -> String {
    String::from("default")
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use indoc::indoc;

    #[test]
    fn test_simple_config() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.default.labels]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_profile("mine");
        assert_matches!(r, Err(ConfigError::NoSuchProfile(name)) => {
            assert_eq!(name, "mine");
        });
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "foo".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("red".parse().unwrap()),
                        description: Some(String::from("Foo all the bars")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "bar".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("blue".parse().unwrap()),
                        description: Some(String::from("Bar all the foos")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_custom_default_profile() {
        let s = indoc! {r#"
            [defaults]
            profile = "mine"

            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.default.labels]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"

            [[profiles.mine.labels]]
            name = "gnusto"
            color = "yellow"
            description = "Gnu all the stos"

            [[profiles.mine.labels]]
            name = "cleesh"
            color = "green"
            description = "Clee all the shs"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "mine");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "gnusto".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("yellow".parse().unwrap()),
                        description: Some(String::from("Gnu all the stos")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "cleesh".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("green".parse().unwrap()),
                        description: Some(String::from("Clee all the shs")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_custom_profile_no_default() {
        let s = indoc! {r#"
            [[profiles.mine.labels]]
            name = "gnusto"
            color = "yellow"
            description = "Gnu all the stos"

            [[profiles.mine.labels]]
            name = "cleesh"
            color = "green"
            description = "Clee all the shs"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::NoSuchProfile(name)) => {
            assert_eq!(name, "default");
        });
        let profile = cfg.get_profile("mine").unwrap();
        assert_eq!(profile.name, "mine");
        assert_eq!(
            profile.specs,
            [
                LabelSpec {
                    name: "gnusto".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("yellow".parse().unwrap()),
                        description: Some(String::from("Gnu all the stos")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "cleesh".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("green".parse().unwrap()),
                        description: Some(String::from("Clee all the shs")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_repeated_label() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.default.labels]]
            name = "Foo"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::RepeatedLabel(name)) => {
            assert_eq!(name, "Foo");
        });
    }

    #[test]
    fn test_repeated_labels_in_different_profiles() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.custom.labels]]
            name = "Foo"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name, "default");
        assert_eq!(
            default.specs,
            [LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::default(),
                    enforce_case: true,
                }
            }]
        );
        let custom = cfg.get_profile("custom").unwrap();
        assert_eq!(custom.name, "custom");
        assert_eq!(
            custom.specs,
            [LabelSpec {
                name: "Foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("blue".parse().unwrap()),
                    description: Some(String::from("Bar all the foos")),
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::default(),
                    enforce_case: true,
                }
            }]
        );
    }

    #[test]
    fn test_label_previously_renamed() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["bar"]

            [[profiles.default.labels]]
            name = "BAR"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::LabelRenamed { label, renamer }) => {
            assert_eq!(label, "BAR");
            assert_eq!(renamer, "foo");
        });
    }

    #[test]
    fn test_label_later_renamed() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.default.labels]]
            name = "BAR"
            color = "blue"
            description = "Bar all the foos"
            rename-from = ["foo"]
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::LabelRenamed { label, renamer }) => {
            assert_eq!(label, "foo");
            assert_eq!(renamer, "BAR");
        });
    }

    #[test]
    fn test_label_renamed_twice() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["quux"]

            [[profiles.default.labels]]
            name = "BAR"
            color = "blue"
            description = "Bar all the foos"
            rename-from = ["quux"]
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::RenameConflict {
            renamed,
            label1,
            label2,
        }) => {
            assert_eq!(renamed, "quux");
            assert_eq!(label1, "foo");
            assert_eq!(label2, "BAR");
        });
    }

    #[test]
    fn test_duplicate_rename_from() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["food", "drink", "Food"]

            [[profiles.default.labels]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "foo".parse().unwrap(),
                    rename_from: vec!["food".parse().unwrap(), "drink".parse().unwrap()],
                    options: LabelOptions {
                        color: ColorSpec::Fixed("red".parse().unwrap()),
                        description: Some(String::from("Foo all the bars")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "bar".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("blue".parse().unwrap()),
                        description: Some(String::from("Bar all the foos")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_rename_from_self() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["Foo"]

            [[profiles.default.labels]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::SelfRename(name)) => {
            assert_eq!(name, "foo");
        });
    }

    #[test]
    fn test_more_options() {
        let s = indoc! {r#"
            [[profiles.default.labels]]
            name = "foo"
            description = "Foo all the bars"
            create = false
            on-rename-clash = "warn"

            [[profiles.default.labels]]
            name = "bar"
            color = "blue"
            update = false
            enforce-case = false
            on-rename-clash = "ignore"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "foo".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::default(),
                        description: Some(String::from("Foo all the bars")),
                        create: false,
                        update: true,
                        on_rename_clash: OnRenameClash::Warn,
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "bar".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("blue".parse().unwrap()),
                        description: None,
                        create: true,
                        update: false,
                        on_rename_clash: OnRenameClash::Ignore,
                        enforce_case: false,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_defaults() {
        let s = indoc! {r#"
            [defaults]
            color = "cccccc"
            create = false

            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.default.labels]]
            name = "bar"
            description = "Bar all the foos"
            create = true
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "foo".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("red".parse().unwrap()),
                        description: Some(String::from("Foo all the bars")),
                        create: false,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "bar".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("#cccccc".parse().unwrap()),
                        description: Some(String::from("Bar all the foos")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_profile_defaults() {
        let s = indoc! {r#"
            [profiles.default.defaults]
            color = "cccccc"
            create = false

            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profiles.default.labels]]
            name = "bar"
            description = "Bar all the foos"
            create = true
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "foo".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("red".parse().unwrap()),
                        description: Some(String::from("Foo all the bars")),
                        create: false,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "bar".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("#cccccc".parse().unwrap()),
                        description: Some(String::from("Bar all the foos")),
                        create: true,
                        update: true,
                        on_rename_clash: OnRenameClash::default(),
                        enforce_case: true,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_defaults_and_profile_defaults() {
        let s = indoc! {r#"
            [defaults]
            color = ["red", "green", "blue"]
            description = "This is a label."
            on-rename-clash = "error"
            enforce-case = false

            [profiles.default.defaults]
            enforce-case = true
            update = false

            [[profiles.default.labels]]
            name = "foo"
            description = "Foo all the bars"
            create = false
            update = true

            [[profiles.default.labels]]
            name = "bar"
            color = "orange"
            on-rename-clash = "warn"
            enforce-case = false
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: "foo".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Random(vec![
                            "red".parse().unwrap(),
                            "green".parse().unwrap(),
                            "blue".parse().unwrap(),
                        ]),
                        description: Some(String::from("Foo all the bars")),
                        create: false,
                        update: true,
                        on_rename_clash: OnRenameClash::Error,
                        enforce_case: true,
                    }
                },
                LabelSpec {
                    name: "bar".parse().unwrap(),
                    rename_from: Vec::new(),
                    options: LabelOptions {
                        color: ColorSpec::Fixed("orange".parse().unwrap()),
                        description: Some(String::from("This is a label.")),
                        create: true,
                        update: false,
                        on_rename_clash: OnRenameClash::Warn,
                        enforce_case: false,
                    }
                },
            ]
        );
    }

    #[test]
    fn test_different_profiles_different_defaults() {
        let s = indoc! {r#"
            [defaults]
            color = ["red", "green", "blue"]
            update = false
            enforce-case = false

            [profiles.default.defaults]
            color = "white"
            update = true

            [[profiles.default.labels]]
            name = "foo"
            color = ["blue", "yellow", "purple"]
            description = "Foo all the bars"

            [profiles.custom.defaults]
            create = false

            [[profiles.custom.labels]]
            name = "Foo"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name, "default");
        assert_eq!(
            default.specs,
            [LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Random(vec![
                        "blue".parse().unwrap(),
                        "yellow".parse().unwrap(),
                        "purple".parse().unwrap(),
                    ]),
                    description: Some(String::from("Foo all the bars")),
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::default(),
                    enforce_case: false,
                }
            }]
        );
        let custom = cfg.get_profile("custom").unwrap();
        assert_eq!(custom.name, "custom");
        assert_eq!(
            custom.specs,
            [LabelSpec {
                name: "Foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Random(vec![
                        "red".parse().unwrap(),
                        "green".parse().unwrap(),
                        "blue".parse().unwrap(),
                    ]),
                    description: Some(String::from("Bar all the foos")),
                    create: false,
                    update: false,
                    on_rename_clash: OnRenameClash::default(),
                    enforce_case: false,
                }
            }]
        );
    }

    #[test]
    fn test_ignored_defaults() {
        let s = indoc! {r#"
            [profiles.default.defaults]
            profile = "custom"
            rename-from = ["bar"]

            [[profiles.default.labels]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
        "#};
        let cfg = Config::from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::default(),
                    enforce_case: true,
                }
            }]
        );
    }

    #[test]
    fn test_labels2toml() {
        let labels = vec![
            Label {
                name: "Foo".parse().unwrap(),
                color: "red".parse().unwrap(),
                description: Some(String::from("Foo all the bars")),
            },
            Label {
                name: "bar".parse().unwrap(),
                color: "blue".parse().unwrap(),
                description: Some(String::from("Bar all the foos")),
            },
            Label {
                name: "no-desc".parse().unwrap(),
                color: "green".parse().unwrap(),
                description: None,
            },
            Label {
                name: "empty-desc".parse().unwrap(),
                color: "yellow".parse().unwrap(),
                description: Some(String::new()),
            },
        ];
        let cfg = Config::from_labels(String::from("test"), labels);
        let toml = cfg.to_toml().unwrap();
        eprintln!("{toml}");
        assert_eq!(
            toml,
            indoc! {r##"
            [defaults]
            profile = "test"
            color = ["#0052cc", "#006b75", "#0e8a16", "#1d76db", "#5319e7", "#b60205", "#bfd4f2", "#bfdadc", "#c2e0c6", "#c5def5", "#d4c5f9", "#d93f0b", "#e99695", "#f9d0c4", "#fbca04", "#fef2c0"]

            [[profiles.test.labels]]
            name = "Foo"
            color = "#ff0000"
            description = "Foo all the bars"

            [[profiles.test.labels]]
            name = "bar"
            color = "#0000ff"
            description = "Bar all the foos"

            [[profiles.test.labels]]
            name = "empty-desc"
            color = "#ffff00"
            description = ""

            [[profiles.test.labels]]
            name = "no-desc"
            color = "#008000"
        "##}
        );
    }
}
