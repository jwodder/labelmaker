use crate::config::PartialLabelOptions;
use crate::labels::*;
use clap::ValueEnum;
use rand::{seq::SliceRandom, Rng};
use serde::{Deserialize, Serialize};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use thiserror::Error;

// These are the "default colors" listed when creating a label via GitHub's web
// UI as of 2023-09-24:
static DEFAULT_COLORS: &[(u8, u8, u8)] = &[
    (0x00, 0x52, 0xCC),
    (0x00, 0x6B, 0x75),
    (0x0E, 0x8A, 0x16),
    (0x1D, 0x76, 0xDB),
    (0x53, 0x19, 0xE7),
    (0xB6, 0x02, 0x05),
    (0xBF, 0xD4, 0xF2),
    (0xBF, 0xDA, 0xDC),
    (0xC2, 0xE0, 0xC6),
    (0xC5, 0xDE, 0xF5),
    (0xD4, 0xC5, 0xF9),
    (0xD9, 0x3F, 0x0B),
    (0xE9, 0x96, 0x95),
    (0xF9, 0xD0, 0xC4),
    (0xFB, 0xCA, 0x04),
    (0xFE, 0xF2, 0xC0),
];

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Profile {
    name: String,
    // Invariant (enforced on creation): The various LabelSpecs do not step on
    // each others' toes
    specs: Vec<LabelSpec>,
}

impl Profile {
    pub(crate) fn new<I>(name: &str, specs: I) -> Result<Profile, ProfileError>
    where
        I: IntoIterator<Item = LabelSpec>,
    {
        let mut defined_labels = HashSet::<ICaseName>::new();
        let mut renamed_from_to = HashMap::<ICaseName, LabelName>::new();
        let specs = specs.into_iter();
        let mut specs2 = Vec::with_capacity(specs.size_hint().0);
        for sp in specs {
            let name = sp.name().clone();
            let iname = name.to_icase();
            if !defined_labels.insert(iname.clone()) {
                return Err(ProfileError::RepeatedLabel(name));
            }
            if let Some(renamer) = renamed_from_to.remove(&iname) {
                return Err(ProfileError::LabelRenamed {
                    label: name,
                    renamer,
                });
            }
            for n in sp.rename_from() {
                let src = n.clone();
                let isrc = src.to_icase();
                if defined_labels.contains(&isrc) {
                    return Err(ProfileError::LabelRenamed {
                        label: src,
                        renamer: name,
                    });
                }
                match renamed_from_to.entry(isrc.clone()) {
                    Entry::Occupied(oc) => {
                        return Err(ProfileError::RenameConflict {
                            renamed: src,
                            label1: oc.remove(),
                            label2: name,
                        });
                    }
                    Entry::Vacant(vac) => {
                        vac.insert(name.clone());
                    }
                }
            }
            specs2.push(sp);
        }
        Ok(Profile {
            name: name.to_owned(),
            specs: specs2,
        })
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn specs(&self) -> &[LabelSpec] {
        &self.specs
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub(crate) enum ProfileError {
    #[error(transparent)]
    Spec(#[from] LabelSpecError),
    #[error("multiple definitions for label {0:?} in profile")]
    RepeatedLabel(LabelName),
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
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelSpec {
    name: LabelName,
    // Invariant (enforced on creation): rename_from contains neither
    // duplicates (*modulo* case) nor the same string as `name` (*modulo* case)
    rename_from: Vec<LabelName>,
    options: LabelOptions,
}

impl LabelSpec {
    pub(crate) fn new<I>(
        name: LabelName,
        rename_from: I,
        options: LabelOptions,
    ) -> Result<LabelSpec, LabelSpecError>
    where
        I: IntoIterator<Item = LabelName>,
    {
        let mut seen = HashSet::from([name.to_icase()]);
        let rename_from = rename_from.into_iter();
        let mut rename_from2 = Vec::with_capacity(rename_from.size_hint().0);
        for n in rename_from {
            if unicase::eq(&name, &n) {
                return Err(LabelSpecError::SelfRename(name));
            } else if seen.insert(n.to_icase()) {
                rename_from2.push(n);
            }
        }
        Ok(LabelSpec {
            name,
            rename_from: rename_from2,
            options,
        })
    }

    pub(crate) fn name(&self) -> &LabelName {
        &self.name
    }

    pub(crate) fn rename_from(&self) -> &[LabelName] {
        &self.rename_from
    }

    pub(crate) fn options(&self) -> &LabelOptions {
        &self.options
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub(crate) enum LabelSpecError {
    #[error("label {0:?} cannot be renamed from itself")]
    SelfRename(LabelName),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelOptions {
    pub(crate) color: ColorSpec,
    pub(crate) description: Option<Description>,
    pub(crate) create: bool,
    pub(crate) update: bool,
    pub(crate) on_rename_clash: OnRenameClash,
    pub(crate) enforce_case: bool,
}

impl LabelOptions {
    pub(crate) fn with_overrides(&self, popt: &PartialLabelOptions) -> LabelOptions {
        LabelOptions {
            color: popt.color.as_ref().unwrap_or(&self.color).clone(),
            description: popt
                .description
                .as_ref()
                .or(self.description.as_ref())
                .cloned(),
            create: *popt.create.as_ref().unwrap_or(&self.create),
            update: *popt.update.as_ref().unwrap_or(&self.update),
            on_rename_clash: *popt
                .on_rename_clash
                .as_ref()
                .unwrap_or(&self.on_rename_clash),
            enforce_case: *popt.enforce_case.as_ref().unwrap_or(&self.enforce_case),
        }
    }
}

impl Default for LabelOptions {
    fn default() -> LabelOptions {
        LabelOptions {
            color: ColorSpec::default(),
            description: None,
            create: true,
            update: true,
            on_rename_clash: OnRenameClash::default(),
            enforce_case: true,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize, ValueEnum)]
#[serde(rename_all = "lowercase")]
pub(crate) enum OnRenameClash {
    /// Do nothing
    Ignore,
    /// Emit a warning
    #[default]
    Warn,
    /// Fail with an error
    Error,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum ColorSpec {
    Fixed(Color),
    Random(Vec<Color>),
}

impl ColorSpec {
    pub(crate) fn pick<R: Rng>(&self, rng: &mut R) -> Color {
        match self {
            ColorSpec::Fixed(c) => c.clone(),
            ColorSpec::Random(cs) => cs.choose(rng).cloned().unwrap_or_default(),
        }
    }
}

impl Default for ColorSpec {
    fn default() -> ColorSpec {
        ColorSpec::Random(DEFAULT_COLORS.iter().map(|&rgb| Color::from(rgb)).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_color_spec() {
        let ColorSpec::Random(colors) = ColorSpec::default() else {
            panic!("ColorSpec::default() was not Random");
        };
        assert_eq!(colors.len(), DEFAULT_COLORS.len());
    }

    mod label_spec {
        use super::*;
        use assert_matches::assert_matches;

        #[test]
        fn simple() {
            let opts = LabelOptions {
                color: ColorSpec::Fixed("turquoise".parse().unwrap()),
                description: Some("A label for labelling".parse().unwrap()),
                ..LabelOptions::default()
            };
            let spec = LabelSpec::new(
                "foo".parse().unwrap(),
                ["bar".parse().unwrap(), "baz".parse().unwrap()],
                opts.clone(),
            )
            .unwrap();
            assert_eq!(spec.name(), "foo");
            assert_eq!(spec.rename_from(), ["bar", "baz"]);
            assert_eq!(spec.options, opts);
        }

        #[test]
        fn duplicate_rename_from() {
            let opts = LabelOptions {
                color: ColorSpec::Fixed("turquoise".parse().unwrap()),
                description: Some("A label for labelling".parse().unwrap()),
                ..LabelOptions::default()
            };
            let spec = LabelSpec::new(
                "foo".parse().unwrap(),
                [
                    "BAR".parse().unwrap(),
                    "baz".parse().unwrap(),
                    "Bar".parse().unwrap(),
                ],
                opts.clone(),
            )
            .unwrap();
            assert_eq!(spec.name(), "foo");
            assert_eq!(spec.rename_from(), ["BAR", "baz"]);
            assert_eq!(spec.options, opts);
        }

        #[test]
        fn rename_from_self() {
            let r = LabelSpec::new(
                "foo".parse().unwrap(),
                [
                    "BAR".parse().unwrap(),
                    "baz".parse().unwrap(),
                    "Foo".parse().unwrap(),
                ],
                LabelOptions::default(),
            );
            assert_matches!(r, Err(LabelSpecError::SelfRename(name)) => {
                assert_eq!(name, "foo");
            });
        }
    }
}
