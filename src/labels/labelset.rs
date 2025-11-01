use super::*;
use crate::profile::*;
use ghrepo::GHRepo;
use itertools::Itertools; // format()
use rand::Rng;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use thiserror::Error;

/// A set of labels in a repository, against which `LabelSpec`s are resolved to
/// obtain `LabelResolution`s
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelSet<R: Rng> {
    data: HashMap<ICaseName, Label>,
    rng: R,
}

impl<R: Rng> LabelSet<R> {
    pub(crate) fn new(rng: R) -> LabelSet<R> {
        LabelSet {
            data: HashMap::new(),
            rng,
        }
    }

    pub(crate) fn add(&mut self, label: Label) {
        self.data.insert(label.name.to_icase(), label);
    }

    pub(crate) fn resolve(
        &mut self,
        spec: &LabelSpec,
    ) -> Result<Vec<LabelResolution>, SpecResolveError> {
        let mut res = Vec::with_capacity(2);
        let iname = spec.name().to_icase();
        let mut rename_candidates = spec
            .rename_from()
            .iter()
            .filter_map(|name| self.data.get(&name.to_icase()))
            .collect::<Vec<_>>();
        let updating = if let Some(extant) = self.data.get(&iname) {
            let mut builder = UpdateBuilder::new(&extant.name);
            if spec.options().enforce_case && spec.name() != &extant.name {
                builder.new_name(spec.name());
            }
            if !rename_candidates.is_empty() {
                match spec.options().on_rename_clash {
                    OnRenameClash::Ignore => (),
                    OnRenameClash::Warn => {
                        res.push(LabelResolution::Warning(LabelWarning::RenameClash {
                            label: extant.name.clone(),
                            candidates: rename_candidates
                                .into_iter()
                                .map(|c| c.name.clone())
                                .collect(),
                        }));
                    }
                    OnRenameClash::Error => {
                        return Err(SpecResolveError::RenameClash {
                            label: extant.name.clone(),
                            candidates: rename_candidates
                                .into_iter()
                                .map(|c| c.name.clone())
                                .collect(),
                        });
                    }
                }
            }
            Some((extant, builder))
        } else if !rename_candidates.is_empty() {
            if rename_candidates.len() > 1 {
                return Err(SpecResolveError::MultipleRenameCandidates {
                    label: spec.name().clone(),
                    candidates: rename_candidates
                        .into_iter()
                        .map(|c| c.name.clone())
                        .collect(),
                });
            }
            let candidate = rename_candidates
                .pop()
                .expect("nonempty Vec should pop Some");
            let mut builder = UpdateBuilder::new(&candidate.name);
            builder.new_name(spec.name());
            Some((candidate, builder))
        } else {
            None
        };
        if let Some((extant, mut builder)) = updating {
            if spec.options().update {
                if let ColorSpec::Fixed(ref c) = spec.options().color
                    && c != &extant.color
                {
                    builder.color(c);
                }
                if let Some(ref desc) = spec.options().description
                    && desc.deref() != extant.description.as_deref().unwrap_or_default()
                {
                    builder.description(desc);
                }
            }
            res.extend(builder.build());
        } else if spec.options().create {
            let label = Label {
                name: spec.name().clone(),
                color: spec.options().color.pick(&mut self.rng),
                description: spec.options().description.clone(),
            };
            res.push(LabelResolution::Operation(LabelOperation::Create(label)));
        }
        Ok(res)
    }
}

impl<R: Rng> Extend<Label> for LabelSet<R> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Label>,
    {
        self.data
            .extend(iter.into_iter().map(|lbl| (lbl.name.to_icase(), lbl)));
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LabelResolution {
    Operation(LabelOperation),
    Warning(LabelWarning),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LabelOperation {
    Create(Label),
    Update {
        name: LabelName,
        new_name: Option<LabelName>,
        color: Option<Color>,
        description: Option<Description>,
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

impl fmt::Display for LabelOperationMessage<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                    label.color,
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
                    write!(f, "new color: {c:?}")?;
                }
                if let Some(d) = description.as_ref() {
                    if !std::mem::replace(&mut first, false) {
                        write!(f, ", ")?;
                    }
                    write!(f, "new description: {d:?}")?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum LabelWarning {
    RenameClash {
        label: LabelName,
        candidates: Vec<LabelName>,
    },
}

impl fmt::Display for LabelWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LabelWarning::RenameClash { label, candidates } => write!(
                f,
                "label {label:?} exists and so {}",
                display_rename_clash_candidates(candidates)
            ),
        }
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub(crate) enum SpecResolveError {
    #[error("multiple rename-from candidates exist for label {label:?}: {:?}", .candidates.iter().format(", "))]
    MultipleRenameCandidates {
        label: LabelName,
        candidates: Vec<LabelName>,
    },
    #[error("label {label:?} exists and so {}", display_rename_clash_candidates(.candidates))]
    RenameClash {
        label: LabelName,
        candidates: Vec<LabelName>,
    },
}

fn display_rename_clash_candidates(candidates: &[LabelName]) -> String {
    if candidates.len() == 1 {
        format!("does rename-from candidate {:?}", candidates[0])
    } else {
        format!(
            "do rename-from candidates {:?}",
            candidates.iter().format(", ")
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
struct UpdateBuilder<'a> {
    name: &'a LabelName,
    new_name: Option<&'a LabelName>,
    color: Option<&'a Color>,
    description: Option<&'a Description>,
}

impl<'a> UpdateBuilder<'a> {
    fn new(name: &'a LabelName) -> UpdateBuilder<'a> {
        UpdateBuilder {
            name,
            new_name: None,
            color: None,
            description: None,
        }
    }

    fn new_name(&mut self, name: &'a LabelName) {
        self.new_name = Some(name);
    }

    fn color(&mut self, color: &'a Color) {
        self.color = Some(color);
    }

    fn description(&mut self, desc: &'a Description) {
        self.description = Some(desc);
    }

    fn build(self) -> Option<LabelResolution> {
        match self {
            UpdateBuilder {
                new_name: None,
                color: None,
                description: None,
                ..
            } => None,
            UpdateBuilder {
                name,
                new_name,
                color,
                description,
            } => Some(LabelResolution::Operation(LabelOperation::Update {
                name: name.clone(),
                new_name: new_name.cloned(),
                color: color.cloned(),
                description: description.cloned(),
            })),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod resolve {
        use super::*;
        use assert_matches::assert_matches;
        use rand::SeedableRng;
        use rand_chacha::ChaCha12Rng;
        use rstest::rstest;

        macro_rules! spec {
            {$name:literal, $rename_from:expr, $options:expr $(,)?} => {
                LabelSpec::new($name.parse().unwrap(), $rename_from, $options).unwrap()
            }
        }

        fn sample_label_set() -> LabelSet<ChaCha12Rng> {
            let mut labels = LabelSet::new(ChaCha12Rng::seed_from_u64(0x0123456789ABCDEF));
            labels.extend([
                Label {
                    name: "foo".parse().unwrap(),
                    color: "red".parse().unwrap(),
                    description: Some("Foo all the bars".parse().unwrap()),
                },
                Label {
                    name: "BAR".parse().unwrap(),
                    color: "blue".parse().unwrap(),
                    description: Some("Bar all the foos".parse().unwrap()),
                },
                Label {
                    name: "no-desc".parse().unwrap(),
                    color: "blue".parse().unwrap(),
                    description: None,
                },
                Label {
                    name: "empty-desc".parse().unwrap(),
                    color: "blue".parse().unwrap(),
                    description: Some("".parse().unwrap()),
                },
            ]);
            labels
        }

        #[test]
        fn create_new_label() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("green".parse().unwrap()),
                    description: Some("Quux you".parse().unwrap()),
                    create: true,
                    ..LabelOptions::default()
                }
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Create(Label {
                    name: "quux".parse().unwrap(),
                    color: "green".parse().unwrap(),
                    description: Some("Quux you".parse().unwrap()),
                }))]
            );
        }

        #[test]
        fn create_new_label_random_color() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::default(),
                    create: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Create(Label {
                    name: "quux".parse().unwrap(),
                    color: "E99695".parse().unwrap(),
                    description: None,
                }))]
            );
        }

        #[test]
        fn create_new_label_empty_color_list() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Random(Vec::new()),
                    create: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Create(Label {
                    name: "quux".parse().unwrap(),
                    color: "000000".parse().unwrap(),
                    description: None,
                }))]
            );
        }

        #[test]
        fn no_create_new_label() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("green".parse().unwrap()),
                    description: Some("Quux you".parse().unwrap()),
                    create: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn no_change_needed() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn update_color() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("purple".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: Some("purple".parse().unwrap()),
                    description: None,
                })]
            );
        }

        #[test]
        fn update_description() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Just what is a \"foo\", anyway?".parse().unwrap()),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: None,
                    description: Some("Just what is a \"foo\", anyway?".parse().unwrap()),
                })]
            );
        }

        #[test]
        fn dont_update_to_null_description() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: None,
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn update_color_and_description() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("silver".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: Some("silver".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                })]
            );
        }

        #[test]
        fn differs_update_false() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("silver".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
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
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    update,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("Foo".parse().unwrap()),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn enforce_case_difference_and_update_description() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                    update: true,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("Foo".parse().unwrap()),
                    color: None,
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                })]
            );
        }

        #[test]
        fn enforce_case_difference_and_no_update_description() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                    update: false,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("Foo".parse().unwrap()),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn no_enforce_case_difference_and_update_description() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                    update: true,
                    enforce_case: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: None,
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn update_does_not_imply_enforce_case_no_change(#[case] update: bool) {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
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
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                    update: true,
                    enforce_case: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: None,
                    description: Some("What is a foo without its bar?".parse().unwrap()),
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn rename(#[case] create: bool) {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                vec!["foo".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    create,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("quux".parse().unwrap()),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn rename_and_update() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                vec!["foo".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("magenta".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("quux".parse().unwrap()),
                    color: Some("magenta".parse().unwrap()),
                    description: None,
                })]
            );
        }

        #[test]
        fn rename_and_skip_update() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                vec!["foo".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("magenta".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    update: false,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("quux".parse().unwrap()),
                    color: None,
                    description: None,
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn rename_across_case(#[case] enforce_case: bool) {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                vec!["FOO".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    enforce_case,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("quux".parse().unwrap()),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn multiple_rename_candidates() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "quux",
                vec!["Foo".parse().unwrap(), "bar".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("yellow".parse().unwrap()),
                    description: None,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(SpecResolveError::MultipleRenameCandidates {ref label, ref candidates}) => {
                assert_eq!(label, "quux");
                assert_eq!(candidates, &["foo", "BAR"]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"multiple rename-from candidates exist for label "quux": "foo", "BAR""#
            );
        }

        #[test]
        fn rename_clash_ignore() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Ignore,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn rename_clash_warn() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Warn,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Warning(LabelWarning::RenameClash {
                    label: "foo".parse().unwrap(),
                    candidates: vec!["BAR".parse().unwrap()],
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
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Error,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(SpecResolveError::RenameClash {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &["BAR"]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_ignore() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Ignore,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Operation(LabelOperation::Update {
                    name: "foo".parse().unwrap(),
                    new_name: Some("Foo".parse().unwrap()),
                    color: None,
                    description: None,
                })]
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_warn() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [
                    LabelResolution::Warning(LabelWarning::RenameClash {
                        label: "foo".parse().unwrap(),
                        candidates: vec!["BAR".parse().unwrap()],
                    }),
                    LabelResolution::Operation(LabelOperation::Update {
                        name: "foo".parse().unwrap(),
                        new_name: Some("Foo".parse().unwrap()),
                        color: None,
                        description: None,
                    }),
                ]
            );
            let Some(LabelResolution::Warning(warn)) = res.first() else {
                unreachable!();
            };
            assert_eq!(
                warn.to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_error() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "Foo",
                vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Error,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(SpecResolveError::RenameClash {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &["BAR"]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn multiple_rename_clash_ignore() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                vec![
                    "no-desc".parse().unwrap(),
                    "nexists".parse().unwrap(),
                    "bar".parse().unwrap(),
                ],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Ignore,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty(), "{res:?}");
        }

        #[test]
        fn multiple_rename_clash_warn() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                vec![
                    "no-desc".parse().unwrap(),
                    "nexists".parse().unwrap(),
                    "bar".parse().unwrap(),
                ],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Warn,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert_eq!(
                res,
                [LabelResolution::Warning(LabelWarning::RenameClash {
                    label: "foo".parse().unwrap(),
                    candidates: vec![
                        "no-desc".parse::<LabelName>().unwrap(),
                        "BAR".parse().unwrap()
                    ],
                })]
            );
            let LabelResolution::Warning(ref warn) = res[0] else {
                unreachable!();
            };
            assert_eq!(
                warn.to_string(),
                r#"label "foo" exists and so do rename-from candidates "no-desc", "BAR""#
            );
        }

        #[test]
        fn multiple_rename_clash_error() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                vec![
                    "no-desc".parse().unwrap(),
                    "nexists".parse().unwrap(),
                    "bar".parse().unwrap(),
                ],
                LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some("Foo all the bars".parse().unwrap()),
                    on_rename_clash: OnRenameClash::Error,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(SpecResolveError::RenameClash {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &["no-desc", "BAR"]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so do rename-from candidates "no-desc", "BAR""#
            );
        }

        #[test]
        fn dont_update_null_desc_to_empty() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "no-desc",
                Vec::new(),
                LabelOptions {
                    description: Some("".parse().unwrap()),
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty());
        }

        #[test]
        fn dont_update_color_outside_of_random_list() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Random(vec![
                        "purple".parse().unwrap(),
                        "orange".parse().unwrap(),
                        "yellow".parse().unwrap(),
                    ]),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty(), "{res:?}");
        }

        #[test]
        fn dont_update_color_outside_of_empty_random_list() {
            let mut labels = sample_label_set();
            let spec = spec! {
                "foo",
                Vec::new(),
                LabelOptions {
                    color: ColorSpec::Random(Vec::new()),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty(), "{res:?}");
        }
    }

    mod as_log_message {
        use super::*;

        #[test]
        fn create() {
            let op = LabelOperation::Create(Label {
                name: "foo".parse().unwrap(),
                color: "red".parse().unwrap(),
                description: Some("Foo all the bars".parse().unwrap()),
            });
            let repo = GHRepo::new("octocat", "repo").unwrap();
            assert_eq!(
                op.as_log_message(&repo, false).to_string(),
                r#"Creating label "foo" in octocat/repo (color: "ff0000", description: "Foo all the bars")"#
            );
            assert_eq!(
                op.as_log_message(&repo, true).to_string(),
                r#"Would create label "foo" in octocat/repo (color: "ff0000", description: "Foo all the bars")"#
            );
        }

        #[test]
        fn create_none_description() {
            let op = LabelOperation::Create(Label {
                name: "foo".parse().unwrap(),
                color: "red".parse().unwrap(),
                description: None,
            });
            let repo = GHRepo::new("octocat", "repo").unwrap();
            assert_eq!(
                op.as_log_message(&repo, false).to_string(),
                r#"Creating label "foo" in octocat/repo (color: "ff0000", description: "")"#
            );
            assert_eq!(
                op.as_log_message(&repo, true).to_string(),
                r#"Would create label "foo" in octocat/repo (color: "ff0000", description: "")"#
            );
        }

        #[test]
        fn update_name() {
            let op = LabelOperation::Update {
                name: "foo".parse().unwrap(),
                new_name: Some("bar".parse().unwrap()),
                color: None,
                description: None,
            };
            let repo = GHRepo::new("octocat", "repo").unwrap();
            assert_eq!(
                op.as_log_message(&repo, false).to_string(),
                r#"Updating label "foo" in octocat/repo (new name: "bar")"#
            );
            assert_eq!(
                op.as_log_message(&repo, true).to_string(),
                r#"Would update label "foo" in octocat/repo (new name: "bar")"#
            );
        }

        #[test]
        fn update_color() {
            let op = LabelOperation::Update {
                name: "foo".parse().unwrap(),
                new_name: None,
                color: Some("blue".parse().unwrap()),
                description: None,
            };
            let repo = GHRepo::new("octocat", "repo").unwrap();
            assert_eq!(
                op.as_log_message(&repo, false).to_string(),
                r#"Updating label "foo" in octocat/repo (new color: "0000ff")"#
            );
            assert_eq!(
                op.as_log_message(&repo, true).to_string(),
                r#"Would update label "foo" in octocat/repo (new color: "0000ff")"#
            );
        }

        #[test]
        fn update_description() {
            let op = LabelOperation::Update {
                name: "foo".parse().unwrap(),
                new_name: None,
                color: None,
                description: Some("What is a foo without its bar?".parse().unwrap()),
            };
            let repo = GHRepo::new("octocat", "repo").unwrap();
            assert_eq!(
                op.as_log_message(&repo, false).to_string(),
                r#"Updating label "foo" in octocat/repo (new description: "What is a foo without its bar?")"#
            );
            assert_eq!(
                op.as_log_message(&repo, true).to_string(),
                r#"Would update label "foo" in octocat/repo (new description: "What is a foo without its bar?")"#
            );
        }

        #[test]
        fn message_update_all() {
            let op = LabelOperation::Update {
                name: "foo".parse().unwrap(),
                new_name: Some("bar".parse().unwrap()),
                color: Some("blue".parse().unwrap()),
                description: Some("What is a foo without its bar?".parse().unwrap()),
            };
            let repo = GHRepo::new("octocat", "repo").unwrap();
            assert_eq!(
                op.as_log_message(&repo, false).to_string(),
                r#"Updating label "foo" in octocat/repo (new name: "bar", new color: "0000ff", new description: "What is a foo without its bar?")"#
            );
            assert_eq!(
                op.as_log_message(&repo, true).to_string(),
                r#"Would update label "foo" in octocat/repo (new name: "bar", new color: "0000ff", new description: "What is a foo without its bar?")"#
            );
        }
    }
}
