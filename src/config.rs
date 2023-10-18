use crate::labels::*;
use crate::util::ICaseStr;
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

#[derive(Debug, Error)]
pub(crate) enum ConfigError {
    #[error("failed to read {path:#}")]
    Read {
        path: patharg::InputArg,
        source: std::io::Error,
    },
    #[error(transparent)]
    Parse(#[from] toml::de::Error),
    #[error("profile not found: {0:?}")]
    NoSuchProfile(String),
    #[error("multiple definitions for label {0:?} in profile")]
    RepeatedLabel(String),
    #[error("label {0:?} cannot be renamed from itself")]
    SelfRename(String),
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
        match path.read_to_string() {
            Ok(s) => Config::from_toml_string(&s),
            Err(source) => Err(ConfigError::Read { path, source }),
        }
    }

    pub(crate) fn from_toml_string(s: &str) -> Result<Config, ConfigError> {
        toml::from_str::<Config>(s).map_err(ConfigError::from)
    }

    pub(crate) fn get_profile(&self, name: &str) -> Result<Profile, ConfigError> {
        let Some(raw) = self.profile.get(name) else {
            return Err(ConfigError::NoSuchProfile(name.into()));
        };
        let settings = LabelOptions::default()
            .with_overrides(&self.defaults.options)
            .with_overrides(&raw.defaults);
        let mut specs = Vec::with_capacity(raw.label.len());
        let mut defined_labels = HashSet::<ICaseStr>::with_capacity(raw.label.len());
        let mut renamed_from_to = HashMap::<ICaseStr, String>::new();
        for lbl in &raw.label {
            let PartialLabelSpec {
                name,
                rename_from,
                options,
            } = lbl;
            let name = name.to_string();
            let iname = ICaseStr::new(name.clone());
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
            let mut renamed_here = HashSet::<ICaseStr>::new();
            for n in rename_from {
                let src = n.to_string();
                let isrc = ICaseStr::new(src.clone());
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
    pub(crate) name: String,
    pub(crate) specs: Vec<LabelSpec>,
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

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use indoc::indoc;

    #[test]
    fn test_simple_config() {
        let s = indoc! {r#"
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.default.label]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
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
                    name: String::from("foo"),
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
                    name: String::from("bar"),
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

            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.default.label]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"

            [[profile.mine.label]]
            name = "gnusto"
            color = "yellow"
            description = "Gnu all the stos"

            [[profile.mine.label]]
            name = "cleesh"
            color = "green"
            description = "Clee all the shs"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "mine");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: String::from("gnusto"),
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
                    name: String::from("cleesh"),
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
            [[profile.mine.label]]
            name = "gnusto"
            color = "yellow"
            description = "Gnu all the stos"

            [[profile.mine.label]]
            name = "cleesh"
            color = "green"
            description = "Clee all the shs"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
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
                    name: String::from("gnusto"),
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
                    name: String::from("cleesh"),
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
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.default.label]]
            name = "Foo"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::RepeatedLabel(name)) => {
            assert_eq!(name, "Foo");
        });
    }

    #[test]
    fn test_repeated_labels_in_different_profiles() {
        let s = indoc! {r#"
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.custom.label]]
            name = "Foo"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name, "default");
        assert_eq!(
            default.specs,
            [LabelSpec {
                name: String::from("foo"),
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
                name: String::from("Foo"),
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
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["bar"]

            [[profile.default.label]]
            name = "BAR"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::LabelRenamed { label, renamer }) => {
            assert_eq!(label, "BAR");
            assert_eq!(renamer, "foo");
        });
    }

    #[test]
    fn test_label_later_renamed() {
        let s = indoc! {r#"
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.default.label]]
            name = "BAR"
            color = "blue"
            description = "Bar all the foos"
            rename-from = ["foo"]
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::LabelRenamed { label, renamer }) => {
            assert_eq!(label, "foo");
            assert_eq!(renamer, "BAR");
        });
    }

    #[test]
    fn test_label_renamed_twice() {
        let s = indoc! {r#"
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["quux"]

            [[profile.default.label]]
            name = "BAR"
            color = "blue"
            description = "Bar all the foos"
            rename-from = ["quux"]
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
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
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["food", "drink", "Food"]

            [[profile.default.label]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: String::from("foo"),
                    rename_from: vec![String::from("food"), String::from("drink")],
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
                    name: String::from("bar"),
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
            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
            rename-from = ["Foo"]

            [[profile.default.label]]
            name = "bar"
            color = "blue"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::SelfRename(name)) => {
            assert_eq!(name, "foo");
        });
    }

    #[test]
    fn test_more_options() {
        let s = indoc! {r#"
            [[profile.default.label]]
            name = "foo"
            description = "Foo all the bars"
            create = false
            on-rename-clash = "warn"

            [[profile.default.label]]
            name = "bar"
            color = "blue"
            update = false
            enforce-case = false
            on-rename-clash = "ignore"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: String::from("foo"),
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
                    name: String::from("bar"),
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

            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.default.label]]
            name = "bar"
            description = "Bar all the foos"
            create = true
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: String::from("foo"),
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
                    name: String::from("bar"),
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
            [profile.default.defaults]
            color = "cccccc"
            create = false

            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"

            [[profile.default.label]]
            name = "bar"
            description = "Bar all the foos"
            create = true
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: String::from("foo"),
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
                    name: String::from("bar"),
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

            [profile.default.defaults]
            enforce-case = true
            update = false

            [[profile.default.label]]
            name = "foo"
            description = "Foo all the bars"
            create = false
            update = true

            [[profile.default.label]]
            name = "bar"
            color = "orange"
            on-rename-clash = "warn"
            enforce-case = false
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [
                LabelSpec {
                    name: String::from("foo"),
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
                    name: String::from("bar"),
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

            [profile.default.defaults]
            color = "white"
            update = true

            [[profile.default.label]]
            name = "foo"
            color = ["blue", "yellow", "purple"]
            description = "Foo all the bars"

            [profile.custom.defaults]
            create = false

            [[profile.custom.label]]
            name = "Foo"
            description = "Bar all the foos"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name, "default");
        assert_eq!(
            default.specs,
            [LabelSpec {
                name: String::from("foo"),
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
                name: String::from("Foo"),
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
            [profile.default.defaults]
            profile = "custom"
            rename-from = ["bar"]

            [[profile.default.label]]
            name = "foo"
            color = "red"
            description = "Foo all the bars"
        "#};
        let cfg = Config::from_toml_string(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name, "default");
        assert_eq!(
            defprofile.specs,
            [LabelSpec {
                name: String::from("foo"),
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
}
