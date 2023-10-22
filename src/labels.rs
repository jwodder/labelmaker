use crate::config::PartialLabelOptions;
use csscolorparser::Color;
use ghrepo::GHRepo;
use itertools::Itertools;
use rand::{seq::SliceRandom, Rng};
use serde::{
    de::{Deserializer, Unexpected, Visitor},
    ser::Serializer,
    Deserialize, Serialize,
};
use serde_with::{serde_as, DeserializeAs, SerializeAs};
use smartstring::alias::CompactString;
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

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct LabelName(CompactString);

pub(crate) type ICaseName = unicase::UniCase<LabelName>;

impl LabelName {
    pub(crate) fn to_icase(&self) -> ICaseName {
        ICaseName::new(self.clone())
    }
}

impl fmt::Debug for LabelName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for LabelName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<str> for LabelName {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<&'a str> for LabelName {
    fn eq(&self, other: &&'a str) -> bool {
        &self.0 == other
    }
}

impl std::str::FromStr for LabelName {
    type Err = ParseLabelNameError;

    fn from_str(s: &str) -> Result<LabelName, ParseLabelNameError> {
        let s = s.trim();
        if s.is_empty() {
            Err(ParseLabelNameError)
        } else {
            Ok(LabelName(s.into()))
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Error, PartialEq)]
#[error("label names cannot be empty or all-whitespace")]
pub(crate) struct ParseLabelNameError;

impl AsRef<str> for LabelName {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Serialize for LabelName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}

impl<'de> Deserialize<'de> for LabelName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct LabelNameVisitor;

        impl<'de> Visitor<'de> for LabelNameVisitor {
            type Value = LabelName;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a label name (neither empty nor all-whitespace)")
            }

            fn visit_str<E>(self, input: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                input
                    .parse::<LabelName>()
                    .map_err(|_| E::invalid_value(Unexpected::Str(input), &self))
            }
        }

        deserializer.deserialize_str(LabelNameVisitor)
    }
}

#[serde_as]
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Label {
    pub(crate) name: LabelName,
    #[serde_as(as = "AsHashlessRgb")]
    pub(crate) color: Color,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) description: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LabelOperation {
    Create(Label),
    Update {
        name: LabelName,
        new_name: Option<LabelName>,
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
                    hashless_rgb(&label.color),
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
                    write!(f, "new color: {:?}", hashless_rgb(c))?;
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

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelSpec {
    pub(crate) name: LabelName,
    // Invariant (enforced on creation by Config::get_profile): rename_from
    // contains neither duplicates (*modulo* name casing) nor the same string
    // as `name` (*modulo* case)
    pub(crate) rename_from: Vec<LabelName>,
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

    pub(crate) fn resolve(&mut self, spec: &LabelSpec) -> Result<Vec<LabelResolution>, LabelError> {
        let mut res = Vec::with_capacity(2);
        let iname = spec.name.to_icase();
        let mut rename_candidates = spec
            .rename_from
            .iter()
            .filter_map(|name| self.data.get(&name.to_icase()))
            .collect::<Vec<_>>();
        let updating = if let Some(extant) = self.data.get(&iname) {
            let mut builder = UpdateBuilder::new(&extant.name);
            if spec.options.enforce_case && spec.name != extant.name {
                builder.new_name(&spec.name);
            }
            if !rename_candidates.is_empty() {
                match spec.options.on_rename_clash {
                    OnRenameClash::Ignore => (),
                    OnRenameClash::Warn => {
                        res.push(LabelResolution::Warning(LabelWarning::RenameClash {
                            label: extant.name.clone(),
                            candidates: rename_candidates
                                .into_iter()
                                .map(|c| c.name.clone())
                                .collect(),
                        }))
                    }
                    OnRenameClash::Error => {
                        return Err(LabelError::RenameClash {
                            label: extant.name.clone(),
                            candidates: rename_candidates
                                .into_iter()
                                .map(|c| c.name.clone())
                                .collect(),
                        })
                    }
                }
            }
            Some((extant, builder))
        } else if !rename_candidates.is_empty() {
            if rename_candidates.len() > 1 {
                return Err(LabelError::MultipleRenameCandidates {
                    label: spec.name.clone(),
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
            builder.new_name(&spec.name);
            Some((candidate, builder))
        } else {
            None
        };
        if let Some((extant, mut builder)) = updating {
            if spec.options.update {
                if let ColorSpec::Fixed(ref c) = spec.options.color {
                    if c != &extant.color {
                        builder.color(c);
                    }
                }
                if let Some(ref desc) = spec.options.description {
                    if desc != extant.description.as_deref().unwrap_or_default() {
                        builder.description(desc);
                    }
                }
            }
            res.extend(builder.build());
        } else if spec.options.create {
            let label = Label {
                name: spec.name.clone(),
                color: spec.options.color.pick(&mut self.rng),
                description: spec.options.description.clone(),
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
            .extend(iter.into_iter().map(|lbl| (lbl.name.to_icase(), lbl)))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LabelResolution {
    Operation(LabelOperation),
    Warning(LabelWarning),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum LabelWarning {
    RenameClash {
        label: LabelName,
        candidates: Vec<LabelName>,
    },
}

impl fmt::Display for LabelWarning {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
pub(crate) enum LabelError {
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
    description: Option<&'a str>,
}

impl<'a> UpdateBuilder<'a> {
    fn new(name: &'a LabelName) -> UpdateBuilder {
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

    fn description(&mut self, desc: &'a str) {
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
                description: description.map(String::from),
            })),
        }
    }
}

fn hashless_rgb(color: &Color) -> String {
    let [r, g, b, _] = color.to_rgba8();
    format!("{:02x}{:02x}{:02x}", r, g, b)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct AsHashlessRgb;

impl SerializeAs<Color> for AsHashlessRgb {
    fn serialize_as<S: Serializer>(color: &Color, serializer: S) -> Result<S::Ok, S::Error> {
        hashless_rgb(color).serialize(serializer)
    }
}

impl<'de> DeserializeAs<'de, Color> for AsHashlessRgb {
    fn deserialize_as<D>(deserializer: D) -> Result<Color, D::Error>
    where
        D: Deserializer<'de>,
    {
        Color::deserialize(deserializer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    struct NameContainer {
        name: LabelName,
    }

    #[test]
    fn test_normal_label_name() {
        let name = "foo".parse::<LabelName>().unwrap();
        assert_eq!(name, "foo");
        assert_eq!(name.to_string(), "foo");
        assert_eq!(name.as_ref(), "foo");
        assert_eq!(format!("{name:?}"), r#""foo""#);
        let cntr = NameContainer { name };
        assert_eq!(serde_json::to_string(&cntr).unwrap(), r#"{"name":"foo"}"#);
        assert_eq!(
            serde_json::from_str::<NameContainer>(r#"{"name":"foo"}"#).unwrap(),
            cntr
        );
    }

    #[test]
    fn test_trimmed_label_name() {
        let name = " foo ".parse::<LabelName>().unwrap();
        assert_eq!(name, "foo");
        assert_eq!(name.to_string(), "foo");
        assert_eq!(name.as_ref(), "foo");
        assert_eq!(format!("{name:?}"), r#""foo""#);
        let cntr = NameContainer { name };
        assert_eq!(serde_json::to_string(&cntr).unwrap(), r#"{"name":"foo"}"#);
        assert_eq!(
            serde_json::from_str::<NameContainer>(r#"{"name":"foo"}"#).unwrap(),
            cntr
        );
        assert_eq!(
            serde_json::from_str::<NameContainer>(r#"{"name":" foo "}"#).unwrap(),
            cntr
        );
    }

    #[test]
    fn test_empty_label_name() {
        let r = "".parse::<LabelName>();
        assert_eq!(r, Err(ParseLabelNameError));
        assert!(serde_json::from_str::<NameContainer>(r#"{"name":""}"#).is_err());
    }

    #[test]
    fn test_whitespace_label_name() {
        let r = " ".parse::<LabelName>();
        assert_eq!(r, Err(ParseLabelNameError));
        assert!(serde_json::from_str::<NameContainer>(r#"{"name":" "}"#).is_err());
    }

    #[serde_as]
    #[derive(Clone, Debug, PartialEq, Serialize)]
    struct ColorContainer {
        #[serde_as(as = "AsHashlessRgb")]
        color: Color,
    }

    #[rstest]
    #[case("black", "000000")]
    #[case("transparent", "000000")]
    #[case("#CCCCCC", "cccccc")]
    #[case("#D90DAD80", "d90dad")]
    #[case("CCC", "cccccc")]
    #[case("c0c0c0", "c0c0c0")]
    fn test_as_hashless_rgb(#[case] color: Color, #[case] s: &str) {
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
        use rand::SeedableRng;
        use rand_chacha::ChaCha12Rng;

        fn sample_label_set() -> LabelSet<ChaCha12Rng> {
            let mut labels = LabelSet::new(ChaCha12Rng::seed_from_u64(0x0123456789ABCDEF));
            labels.extend([
                Label {
                    name: "foo".parse().unwrap(),
                    color: "red".parse().unwrap(),
                    description: Some(String::from("Foo all the bars")),
                },
                Label {
                    name: "BAR".parse().unwrap(),
                    color: "blue".parse().unwrap(),
                    description: Some(String::from("Bar all the foos")),
                },
                Label {
                    name: "no-desc".parse().unwrap(),
                    color: "blue".parse().unwrap(),
                    description: None,
                },
                Label {
                    name: "empty-desc".parse().unwrap(),
                    color: "blue".parse().unwrap(),
                    description: Some(String::new()),
                },
            ]);
            labels
        }

        #[test]
        fn create_new_label() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
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
                    name: "quux".parse().unwrap(),
                    color: "green".parse().unwrap(),
                    description: Some(String::from("Quux you")),
                }))]
            );
        }

        #[test]
        fn create_new_label_random_color() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
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
                    name: "quux".parse().unwrap(),
                    color: "006B75".parse().unwrap(),
                    description: None,
                }))]
            );
        }

        #[test]
        fn create_new_label_empty_color_list() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
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
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
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
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
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
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
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
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
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
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: None,
                    description: Some(String::from("Just what is a \"foo\", anyway?")),
                })]
            );
        }

        #[test]
        fn dont_update_to_null_description() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
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
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
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
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: Some("silver".parse().unwrap()),
                    description: Some(String::from("What is a foo without its bar?")),
                })]
            );
        }

        #[test]
        fn differs_update_false() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
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
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
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
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
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
                    name: "foo".parse().unwrap(),
                    new_name: Some("Foo".parse().unwrap()),
                    color: None,
                    description: Some(String::from("What is a foo without its bar?")),
                })]
            );
        }

        #[test]
        fn enforce_case_difference_and_no_update_description() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
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
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
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
                    name: "foo".parse().unwrap(),
                    new_name: None,
                    color: None,
                    description: Some(String::from("What is a foo without its bar?")),
                })]
            );
        }

        #[rstest]
        #[case(true)]
        #[case(false)]
        fn update_does_not_imply_enforce_case_no_change(#[case] update: bool) {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
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
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
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
                    name: "foo".parse().unwrap(),
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
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
                rename_from: vec!["foo".parse().unwrap(), "nexists".parse().unwrap()],
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
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
                rename_from: vec!["foo".parse().unwrap(), "nexists".parse().unwrap()],
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
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
                rename_from: vec!["foo".parse().unwrap(), "nexists".parse().unwrap()],
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
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
                rename_from: vec!["FOO".parse().unwrap(), "nexists".parse().unwrap()],
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
            let spec = LabelSpec {
                name: "quux".parse().unwrap(),
                rename_from: vec!["Foo".parse().unwrap(), "bar".parse().unwrap()],
                options: LabelOptions {
                    color: ColorSpec::Fixed("yellow".parse().unwrap()),
                    description: None,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::MultipleRenameCandidates {ref label, ref candidates}) => {
                assert_eq!(label, "quux");
                assert_eq!(candidates, &["foo".parse::<LabelName>().unwrap(), "BAR".parse().unwrap()]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"multiple rename-from candidates exist for label "quux": "foo", "BAR""#
            );
        }

        #[test]
        fn rename_clash_ignore() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
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
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
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
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Error,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::RenameClash {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &["BAR".parse::<LabelName>().unwrap()]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[test]
        fn enforce_case_and_rename_clash_ignore() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
                rename_from: vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
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
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
                rename_from: vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
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
            let Some(LabelResolution::Warning(ref warn)) = res.first() else {
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
            let spec = LabelSpec {
                name: "Foo".parse().unwrap(),
                rename_from: vec!["bar".parse().unwrap(), "nexists".parse().unwrap()],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Error,
                    enforce_case: true,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::RenameClash {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &["BAR".parse::<LabelName>().unwrap()]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so does rename-from candidate "BAR""#
            );
        }

        #[rstest]
        fn multiple_rename_clash_ignore() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: vec![
                    "no-desc".parse().unwrap(),
                    "nexists".parse().unwrap(),
                    "bar".parse().unwrap(),
                ],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
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
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: vec![
                    "no-desc".parse().unwrap(),
                    "nexists".parse().unwrap(),
                    "bar".parse().unwrap(),
                ],
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
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: vec![
                    "no-desc".parse().unwrap(),
                    "nexists".parse().unwrap(),
                    "bar".parse().unwrap(),
                ],
                options: LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: Some(String::from("Foo all the bars")),
                    on_rename_clash: OnRenameClash::Error,
                    ..LabelOptions::default()
                },
            };
            let r = labels.resolve(&spec);
            assert_matches!(r, Err(LabelError::RenameClash {ref label, ref candidates}) => {
                assert_eq!(label, "foo");
                assert_eq!(candidates, &["no-desc".parse::<LabelName>().unwrap(), "BAR".parse().unwrap()]);
            });
            assert_eq!(
                r.unwrap_err().to_string(),
                r#"label "foo" exists and so do rename-from candidates "no-desc", "BAR""#
            );
        }

        #[test]
        fn dont_update_null_desc_to_empty() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "no-desc".parse().unwrap(),
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
        fn dont_update_color_outside_of_random_list() {
            let mut labels = sample_label_set();
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
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
            let spec = LabelSpec {
                name: "foo".parse().unwrap(),
                rename_from: Vec::new(),
                options: LabelOptions {
                    color: ColorSpec::Random(Vec::new()),
                    update: true,
                    ..LabelOptions::default()
                },
            };
            let res = labels.resolve(&spec).unwrap();
            assert!(res.is_empty(), "{res:?}");
        }
    }

    #[test]
    fn test_log_message_create() {
        let op = LabelOperation::Create(Label {
            name: "foo".parse().unwrap(),
            color: "red".parse().unwrap(),
            description: Some(String::from("Foo all the bars")),
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
    fn test_log_message_create_none_description() {
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
    fn test_log_message_update_name() {
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
    fn test_log_message_update_color() {
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
    fn test_log_message_update_description() {
        let op = LabelOperation::Update {
            name: "foo".parse().unwrap(),
            new_name: None,
            color: None,
            description: Some(String::from("What is a foo without its bar?")),
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
    fn test_log_message_update_all() {
        let op = LabelOperation::Update {
            name: "foo".parse().unwrap(),
            new_name: Some("bar".parse().unwrap()),
            color: Some("blue".parse().unwrap()),
            description: Some(String::from("What is a foo without its bar?")),
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
