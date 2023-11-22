use crate::labels::*;
use crate::profile::*;
use patharg::OutputArg;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
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
        cfgfifo::load::<Config, _>(&path).map_err(Into::into)
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

    pub(crate) fn dump(&self, outfile: OutputArg) -> Result<(), ConfigError> {
        match outfile {
            OutputArg::Stdout => {
                let writer = std::io::stdout().lock();
                cfgfifo::Format::Json
                    .dump_to_writer(writer, self)
                    .map_err(ConfigError::DumpStdout)
            }
            OutputArg::Path(p) => cfgfifo::dump(p, self).map_err(Into::into),
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
        for lbl in &raw.labels {
            let PartialLabelSpec {
                name,
                rename_from,
                options,
            } = lbl;
            let sp = LabelSpec::new(
                name.clone(),
                rename_from.iter().cloned(),
                settings.with_overrides(options),
            )
            .map_err(|e| ConfigError::Profile(ProfileError::Spec(e)))?;
            specs.push(sp);
        }
        Profile::new(name, specs).map_err(Into::into)
    }

    pub(crate) fn get_default_profile(&self) -> Result<Profile, ConfigError> {
        self.get_profile(&self.defaults.profile)
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

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct PartialLabelOptions {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) create: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) update: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) color: Option<ColorSpec>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) description: Option<Description>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) on_rename_clash: Option<OnRenameClash>,
    #[serde(skip_serializing_if = "Option::is_none")]
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
    #[error("failed to load configuration")]
    Load(#[from] cfgfifo::LoadError),
    #[error("failed to write configuration to file")]
    Dump(#[from] cfgfifo::DumpError),
    #[error("failed to write configuration to stdout")]
    DumpStdout(#[source] cfgfifo::SerializeError),
    #[error("profile not found: {0:?}")]
    NoSuchProfile(String),
    #[error(transparent)]
    Profile(#[from] ProfileError),
}

fn default_profile() -> String {
    String::from("default")
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use indoc::indoc;

    fn from_toml(s: &str) -> Result<Config, cfgfifo::DeserializeError> {
        cfgfifo::Format::Toml.load_from_str::<Config>(s)
    }

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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_profile("mine");
        assert_matches!(r, Err(ConfigError::NoSuchProfile(name)) => {
            assert_eq!(name, "mine");
        });
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("red".parse().unwrap()),
                description: Some("Foo all the bars".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "bar");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("blue".parse().unwrap()),
                description: Some("Bar all the foos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "mine");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "gnusto");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("yellow".parse().unwrap()),
                description: Some("Gnu all the stos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "cleesh");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("green".parse().unwrap()),
                description: Some("Clee all the shs".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::NoSuchProfile(name)) => {
            assert_eq!(name, "default");
        });
        let profile = cfg.get_profile("mine").unwrap();
        assert_eq!(profile.name(), "mine");
        assert_eq!(profile.specs().len(), 2);
        assert_eq!(profile.specs()[0].name(), "gnusto");
        assert!(profile.specs()[0].rename_from().is_empty());
        assert_eq!(
            profile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("yellow".parse().unwrap()),
                description: Some("Gnu all the stos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        assert_eq!(profile.specs()[1].name(), "cleesh");
        assert!(profile.specs()[1].rename_from().is_empty());
        assert_eq!(
            profile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("green".parse().unwrap()),
                description: Some("Clee all the shs".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::Profile(ProfileError::RepeatedLabel(name))) => {
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
        let cfg = from_toml(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name(), "default");
        assert_eq!(default.specs().len(), 1);
        assert_eq!(default.specs()[0].name(), "foo");
        assert!(default.specs()[0].rename_from().is_empty());
        assert_eq!(
            default.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("red".parse().unwrap()),
                description: Some("Foo all the bars".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        let custom = cfg.get_profile("custom").unwrap();
        assert_eq!(custom.name(), "custom");
        assert_eq!(custom.specs()[0].name(), "Foo");
        assert!(custom.specs()[0].rename_from().is_empty());
        assert_eq!(
            custom.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("blue".parse().unwrap()),
                description: Some("Bar all the foos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::Profile(ProfileError::LabelRenamed { label, renamer })) => {
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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::Profile(ProfileError::LabelRenamed { label, renamer })) => {
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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::Profile(ProfileError::RenameConflict {
            renamed,
            label1,
            label2,
        })) => {
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert_eq!(defprofile.specs()[0].rename_from(), ["food", "drink"]);
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("red".parse().unwrap()),
                description: Some("Foo all the bars".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "bar");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("blue".parse().unwrap()),
                description: Some("Bar all the foos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let r = cfg.get_default_profile();
        assert_matches!(r, Err(ConfigError::Profile(ProfileError::Spec(LabelSpecError::SelfRename(name)))) => {
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::default(),
                description: Some("Foo all the bars".parse().unwrap()),
                create: false,
                update: true,
                on_rename_clash: OnRenameClash::Warn,
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "bar");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("blue".parse().unwrap()),
                description: None,
                create: true,
                update: false,
                on_rename_clash: OnRenameClash::Ignore,
                enforce_case: false,
            }
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("red".parse().unwrap()),
                description: Some("Foo all the bars".parse().unwrap()),
                create: false,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "bar");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("#cccccc".parse().unwrap()),
                description: Some("Bar all the foos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("red".parse().unwrap()),
                description: Some("Foo all the bars".parse().unwrap()),
                create: false,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "bar");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("#cccccc".parse().unwrap()),
                description: Some("Bar all the foos".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 2);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Random(vec![
                    "red".parse().unwrap(),
                    "green".parse().unwrap(),
                    "blue".parse().unwrap(),
                ]),
                description: Some("Foo all the bars".parse().unwrap()),
                create: false,
                update: true,
                on_rename_clash: OnRenameClash::Error,
                enforce_case: true,
            }
        );
        assert_eq!(defprofile.specs()[1].name(), "bar");
        assert!(defprofile.specs()[1].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[1].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("orange".parse().unwrap()),
                description: Some("This is a label.".parse().unwrap()),
                create: true,
                update: false,
                on_rename_clash: OnRenameClash::Warn,
                enforce_case: false,
            }
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
        let cfg = from_toml(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name(), "default");
        assert_eq!(default.specs().len(), 1);
        assert_eq!(default.specs()[0].name(), "foo");
        assert!(default.specs()[0].rename_from().is_empty());
        assert_eq!(
            default.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Random(vec![
                    "blue".parse().unwrap(),
                    "yellow".parse().unwrap(),
                    "purple".parse().unwrap(),
                ]),
                description: Some("Foo all the bars".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: false,
            }
        );
        let custom = cfg.get_profile("custom").unwrap();
        assert_eq!(custom.name(), "custom");
        assert_eq!(custom.specs().len(), 1);
        assert_eq!(custom.specs()[0].name(), "Foo");
        assert!(custom.specs()[0].rename_from().is_empty());
        assert_eq!(
            custom.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Random(vec![
                    "red".parse().unwrap(),
                    "green".parse().unwrap(),
                    "blue".parse().unwrap(),
                ]),
                description: Some("Bar all the foos".parse().unwrap()),
                create: false,
                update: false,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: false,
            }
        );
    }

    #[test]
    fn test_different_profiles_different_defaults_yaml() {
        let s = indoc! {"
            defaults:
              color: [red, green, blue]
              update: false
              enforce-case: false
            profiles:
              default:
                defaults:
                  color: white
                  update: true
                labels:
                  - name: foo
                    color: [blue, yellow, purple]
                    description: Foo all the bars
              custom:
                defaults:
                  create: false
                labels:
                  - name: Foo
                    description: Bar all the foos
        "};
        let cfg = cfgfifo::Format::Yaml.load_from_str::<Config>(s).unwrap();
        let default = cfg.get_profile("default").unwrap();
        assert_eq!(default.name(), "default");
        assert_eq!(default.specs().len(), 1);
        assert_eq!(default.specs()[0].name(), "foo");
        assert!(default.specs()[0].rename_from().is_empty());
        assert_eq!(
            default.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Random(vec![
                    "blue".parse().unwrap(),
                    "yellow".parse().unwrap(),
                    "purple".parse().unwrap(),
                ]),
                description: Some("Foo all the bars".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: false,
            }
        );
        let custom = cfg.get_profile("custom").unwrap();
        assert_eq!(custom.name(), "custom");
        assert_eq!(custom.specs().len(), 1);
        assert_eq!(custom.specs()[0].name(), "Foo");
        assert!(custom.specs()[0].rename_from().is_empty());
        assert_eq!(
            custom.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Random(vec![
                    "red".parse().unwrap(),
                    "green".parse().unwrap(),
                    "blue".parse().unwrap(),
                ]),
                description: Some("Bar all the foos".parse().unwrap()),
                create: false,
                update: false,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: false,
            }
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
        let cfg = from_toml(s).unwrap();
        let defprofile = cfg.get_default_profile().unwrap();
        assert_eq!(defprofile.name(), "default");
        assert_eq!(defprofile.specs().len(), 1);
        assert_eq!(defprofile.specs()[0].name(), "foo");
        assert!(defprofile.specs()[0].rename_from().is_empty());
        assert_eq!(
            defprofile.specs()[0].options(),
            &LabelOptions {
                color: ColorSpec::Fixed("red".parse().unwrap()),
                description: Some("Foo all the bars".parse().unwrap()),
                create: true,
                update: true,
                on_rename_clash: OnRenameClash::default(),
                enforce_case: true,
            }
        );
    }

    #[test]
    fn test_labels2toml() {
        let labels = vec![
            Label {
                name: "Foo".parse().unwrap(),
                color: "red".parse().unwrap(),
                description: Some("Foo all the bars".parse().unwrap()),
            },
            Label {
                name: "bar".parse().unwrap(),
                color: "blue".parse().unwrap(),
                description: Some("Bar all the foos".parse().unwrap()),
            },
            Label {
                name: "no-desc".parse().unwrap(),
                color: "green".parse().unwrap(),
                description: None,
            },
            Label {
                name: "empty-desc".parse().unwrap(),
                color: "yellow".parse().unwrap(),
                description: Some("".parse().unwrap()),
            },
        ];
        let cfg = Config::from_labels(String::from("test"), labels);
        let toml = cfgfifo::Format::Toml.dump_to_string(&cfg).unwrap();
        eprintln!("{toml}");
        assert_eq!(
            toml,
            indoc! {r#"
            [defaults]
            profile = "test"
            color = [
                "0052cc",
                "006b75",
                "0e8a16",
                "1d76db",
                "5319e7",
                "b60205",
                "bfd4f2",
                "bfdadc",
                "c2e0c6",
                "c5def5",
                "d4c5f9",
                "d93f0b",
                "e99695",
                "f9d0c4",
                "fbca04",
                "fef2c0",
            ]

            [[profiles.test.labels]]
            name = "Foo"
            color = "ff0000"
            description = "Foo all the bars"

            [[profiles.test.labels]]
            name = "bar"
            color = "0000ff"
            description = "Bar all the foos"

            [[profiles.test.labels]]
            name = "empty-desc"
            color = "ffff00"
            description = ""

            [[profiles.test.labels]]
            name = "no-desc"
            color = "008000"
        "#}
        );
    }

    #[test]
    fn test_labels2yaml() {
        let labels = vec![
            Label {
                name: "Foo".parse().unwrap(),
                color: "red".parse().unwrap(),
                description: Some("Foo all the bars".parse().unwrap()),
            },
            Label {
                name: "bar".parse().unwrap(),
                color: "blue".parse().unwrap(),
                description: Some("Bar all the foos".parse().unwrap()),
            },
            Label {
                name: "no-desc".parse().unwrap(),
                color: "green".parse().unwrap(),
                description: None,
            },
            Label {
                name: "empty-desc".parse().unwrap(),
                color: "yellow".parse().unwrap(),
                description: Some("".parse().unwrap()),
            },
        ];
        let cfg = Config::from_labels(String::from("test"), labels);
        let yaml = cfgfifo::Format::Yaml.dump_to_string(&cfg).unwrap();
        eprintln!("{yaml}");
        assert_eq!(
            yaml,
            indoc! {"
            defaults:
              profile: test
              color:
              - 0052cc
              - 006b75
              - 0e8a16
              - 1d76db
              - '5319e7'
              - b60205
              - bfd4f2
              - bfdadc
              - c2e0c6
              - c5def5
              - d4c5f9
              - d93f0b
              - e99695
              - f9d0c4
              - fbca04
              - fef2c0
            profiles:
              test:
                labels:
                - name: Foo
                  color: ff0000
                  description: Foo all the bars
                - name: bar
                  color: 0000ff
                  description: Bar all the foos
                - name: empty-desc
                  color: ffff00
                  description: ''
                - name: no-desc
                  color: '008000'
        "}
        );
    }
}
