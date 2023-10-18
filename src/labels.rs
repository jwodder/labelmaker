use crate::config::{PartialLabelOptions, PartialLabelSpec};
use crate::util::{color2rgbhex, serialize_color, ICaseStr};
use csscolorparser::Color;
use ghrepo::GHRepo;
use itertools::Itertools;
use serde::{Deserialize, Serialize, Serializer};
use std::collections::HashMap;
use std::fmt;
use thiserror::Error; // format()

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

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Label {
    pub(crate) name: String,
    #[serde(serialize_with = "serialize_color")]
    pub(crate) color: Color,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) description: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LabelOperation {
    Create(Label),
    Update {
        name: String,
        new_name: Option<String>,
        color: Option<Color>,
        description: Option<String>,
    },
}

impl LabelOperation {
    pub(crate) fn as_log_message<'a>(
        &'a self,
        repo: &'a GHRepo,
        dry_run: bool,
    ) -> LabelOperationMessage<'a> {
        LabelOperationMessage {
            op: self,
            repo,
            dry_run,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct LabelOperationMessage<'a> {
    op: &'a LabelOperation,
    repo: &'a GHRepo,
    dry_run: bool,
}

impl<'a> fmt::Display for LabelOperationMessage<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            LabelOperation::Create(label) => {
                if self.dry_run {
                    write!(f, "Would create ")?;
                } else {
                    write!(f, "Creating ")?;
                }
                write!(
                    f,
                    "label {:?} in {} (color: {:?}, description: {:?})",
                    label.name,
                    self.repo,
                    color2rgbhex(&label.color),
                    label.description.as_deref().unwrap_or_default()
                )?;
            }
            LabelOperation::Update {
                name,
                new_name,
                color,
                description,
            } => {
                if self.dry_run {
                    write!(f, "Would update ")?;
                } else {
                    write!(f, "Updating ")?;
                }
                write!(f, "label {:?} in {} (", name, self.repo)?;
                let mut first = true;
                if let Some(n) = new_name.as_ref() {
                    write!(f, "new name: {n:?}")?;
                    first = false;
                }
                if let Some(c) = color.as_ref() {
                    if !std::mem::replace(&mut first, false) {
                        write!(f, ", ")?;
                    }
                    write!(f, "color: {:?}", color2rgbhex(c))?;
                }
                if let Some(d) = description.as_ref() {
                    if !std::mem::replace(&mut first, false) {
                        write!(f, ", ")?;
                    }
                    write!(f, "description: {d:?}")?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelSpec {
    pub(crate) name: String,
    // Invariant: rename_from contains neither duplicates (*modulo* name
    // casing) nor the same (*modulo* case) string as `name`
    pub(crate) rename_from: Vec<String>,
    pub(crate) options: LabelOptions,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelOptions {
    pub(crate) color: ColorSpec,
    pub(crate) description: Option<String>,
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

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum OnRenameClash {
    Ignore,
    #[default]
    Warn,
    Error,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum ColorSpec {
    Fixed(Color),
    Random(Vec<Color>),
}

impl Default for ColorSpec {
    fn default() -> ColorSpec {
        ColorSpec::Random(DEFAULT_COLORS.iter().map(|&rgb| Color::from(rgb)).collect())
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct LabelSet(HashMap<ICaseStr, Label>);

impl LabelSet {
    pub(crate) fn new() -> LabelSet {
        LabelSet(HashMap::new())
    }

    pub(crate) fn add(&mut self, label: Label) {
        self.0.insert(ICaseStr::new(label.name.clone()), label);
    }

    pub(crate) fn resolve(&self, spec: &LabelSpec) -> Result<Vec<LabelResolution>, LabelError> {
        todo!()
    }
}

impl FromIterator<Label> for LabelSet {
    fn from_iter<I>(iter: I) -> LabelSet
    where
        I: IntoIterator<Item = Label>,
    {
        LabelSet(
            iter.into_iter()
                .map(|lbl| (ICaseStr::new(lbl.name.clone()), lbl))
                .collect(),
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LabelResolution {
    Operation(LabelOperation),
    Warning(LabelWarning),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum LabelWarning {
    RenameClash { label: String, candidate: String },
}

impl fmt::Display for LabelWarning {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelWarning::RenameClash { label, candidate } => write!(
                f,
                "label {label:?} exists and so does rename-from candidate {candidate:?}"
            ),
        }
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub(crate) enum LabelError {
    #[error("label {label:?} has multiple rename-from candidates: {:?}", .candidates.iter().format(", "))]
    MultipleRenameCandidates {
        label: String,
        candidates: Vec<String>,
    },
    #[error("label {label:?} exists and so does rename-from candidate {candidate:?}")]
    RenameClash { label: String, candidate: String },
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[derive(Clone, Debug, PartialEq, Serialize)]
    struct ColorContainer {
        #[serde(serialize_with = "serialize_color")]
        color: Color,
    }

    #[rstest]
    #[case("black", "000000")]
    #[case("transparent", "000000")]
    #[case("#CCCCCC", "cccccc")]
    #[case("#D90DAD80", "d90dad")]
    #[case("CCC", "cccccc")]
    #[case("c0c0c0", "c0c0c0")]
    fn test_color2rgbhex(#[case] color: Color, #[case] s: &str) {
        let obj = ColorContainer { color };
        let expected = format!(r#"{{"color":"{s}"}}"#);
        assert_eq!(serde_json::to_string(&obj).unwrap(), expected);
    }

    #[test]
    fn test_default_color_spec() {
        let ColorSpec::Random(colors) = ColorSpec::default() else {
            panic!("ColorSpec::default() was not Random");
        };
        assert_eq!(colors.len(), DEFAULT_COLORS.len());
    }

    mod resolve {
        use super::*;
        use assert_matches::assert_matches;

        fn sample_label_set() -> LabelSet {
            LabelSet::from_iter([
                Label {
                    name: String::from("foo"),
                    color: "red".parse().unwrap(),
                    description: Some(String::from("Foo all the bars")),
                },
                Label {
                    name: String::from("BAR"),
                    color: "blue".parse().unwrap(),
                    description: Some(String::from("Bar all the foos")),
                },
                Label {
                    name: String::from("no-desc"),
                    color: "blue".parse().unwrap(),
                    description: None,
                },
                Label {
                    name: String::from("empty-desc"),
                    color: "blue".parse().unwrap(),
                    description: Some(String::new()),
                },
            ])
        }

        #[test]
        fn create_new_label() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("green".parse().unwrap()),
                    description: Some(String::from("Quux you")),
                    create: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Create(Label {
                    name: String::from("quux"),
                    color: "green".parse().unwrap(),
                    description: Some(String::from("Quux you")),
                }))]
            );
        }

        #[test]
        fn create_new_label_random_color() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::default(),
                    create: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Create(Label {
                    name: String::from("quux"),
                    color: "UPDATE WITH REPRODUCEABLE TEST OUTPUT".parse().unwrap(),
                    description: None,
                }))]
            );
        }

        #[test]
        fn no_create_new_label() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("green".parse().unwrap()),
                    description: Some(String::from("Quux you")),
                    create: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn no_change_needed() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn update_color() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("purple".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: None,
                    color: Some("purple".parse().unwrap()),
                    description: None,
                })]
            );
        }

        #[test]
        fn update_description() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Just what is a \"foo\", anyway?")),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: None,
                    color: None,
                    description: Some(String::from("Just what is a \"foo\", anyway?")),
                })]
            );
        }

        #[test]
        fn update_color_and_description() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("silver".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: None,
                    color: Some("silver".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                })]
            );
        }

        #[test]
        fn differs_update_false() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("silver".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                    update: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn enforce_case_difference(#[case] update: bool) {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    update,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("Foo")),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn enforce_case_difference_and_update_description() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                    update: true,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("Foo")),
                    color: None,
                    description: Some(String::from("What is a foo without its bar?")),
                })]
            );
        }

        #[test]
        fn enforce_case_difference_and_no_update_description() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                    update: false,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("Foo")),
                    color: None,
                    description: None,
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn update_does_not_imply_enforce_case_no_change(#[case] update: bool) {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    update,
                    enforce_case: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn update_does_not_imply_enforce_case_still_change() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                    update: true,
                    enforce_case: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: None,
                    color: None,
                    description: Some(String::from("What is a foo without its bar?")),
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn rename(#[case] create: bool) {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: vec![String::from("foo"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    create,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("quux")),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn rename_and_update() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: vec![String::from("foo"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("magenta".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("quux")),
                    color: Some("magenta".parse().unwrap()),
                    description: None,
                })]
            );
        }

        #[test]
        fn rename_and_skip_update() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: vec![String::from("foo"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("magenta".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    update: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("quux")),
                    color: None,
                    description: None,
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn rename_across_case(#[case] enforce_case: bool) {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: vec![String::from("FOO"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    enforce_case,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("quux")),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn multiple_rename_candidates() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("quux"),
                rename_from: vec![String::from("Foo"), String::from("BAR")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("yellow".parse().unwrap()),
                    description: None,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::MultipleRenameCandidates {ref label, ref candidates}) => {
                assert_eq!(label, "quux");
                assert_eq!(candidates, &vec![String::from("foo"), String::from("BAR")]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "quux" has multiple rename-from matches: "foo", "BAR""#
            );
        }

        #[rstest]
        #[case(OnRenameClash::Ignore)]
        #[case(OnRenameClash::Warn)]
        #[case(OnRenameClash::Error)]
        fn label_exists_and_multiple_rename_candidates(#[case] on_rename_clash: OnRenameClash) {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: vec![
                    String::from("no-desc"),
                    String::from("nexists"),
                    String::from("BAR"),
                ],
                options: LabelOptions {
                    color: ColorSpec::Fixed("yellow".parse().unwrap()),
                    description: None,
                    on_rename_clash,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::MultipleRenameCandidates {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &vec![String::from("no-desc"), String::from("BAR")]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" has multiple rename-from candidates: "no-desc", "BAR""#
            );
        }

        #[test]
        fn rename_clash_ignore() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: vec![String::from("bar"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Ignore,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn rename_clash_warn() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: vec![String::from("bar"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Warn,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Warning(LabelWarning::RenameClash {
                    label: String::from("foo"),
                    candidate: String::from("BAR"),
                })]
            );
            let LabelResolution::Warning(ref warn) = res[0] else {
                unreachable!();
            };
            assert_eq!(
                warn.to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn rename_clash_error() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: vec![String::from("bar"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Error,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::RenameClash {ref label, ref candidate}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidate, "BAR");
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_ignore() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: vec![String::from("bar"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Ignore,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: String::from("foo"),
                    new_name: Some(String::from("Foo")),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_warn() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: vec![String::from("bar"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                vec![
                    LabelResolution::Operation(LabelOperation::Update {
                        name: String::from("foo"),
                        new_name: Some(String::from("Foo")),
                        color: None,
                        description: None,
                    }),
                    LabelResolution::Warning(LabelWarning::RenameClash {
                        label: String::from("foo"),
                        candidate: String::from("BAR"),
                    }),
                ]
            );
            let LabelResolution::Warning(ref warn) = res[1] else {
                unreachable!();
            };
            assert_eq!(
                warn.to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_error() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("Foo"),
                rename_from: vec![String::from("bar"), String::from("nexists")],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Error,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::RenameClash {ref label, ref candidate}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidate, "BAR");
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn dont_update_null_desc_to_empty() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("no-desc"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    description: Some(String::new()),
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn dont_update_empty_desc_to_null() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("empty-desc"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    description: None,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn dont_update_color_outside_of_random_list() {
            let labels = sample_label_set();
            let spec = LabelSpec {
                name: String::from("foo"),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Random(vec![
                        "purple".parse().unwrap(),
                        "orange".parse().unwrap(),
                        "yellow".parse().unwrap(),
                    ]),
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }
    }
}
