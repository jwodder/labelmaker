use crate::config::{PartialLabelOptions, PartialLabelSpec};
use csscolorparser::Color;
use serde::{Deserialize, Serialize};

// These are the "default colors" listed when creating a label via GitHub's web
// UI as of 2023-09-24:
static DEFAULT_COLORS: &[&str] = &[
    "0052cc", "006b75", "0e8a16", "1d76db", "5319e7", "b60205", "bfd4f2", "bfdadc", "c2e0c6",
    "c5def5", "d4c5f9", "d93f0b", "e99695", "f9d0c4", "fbca04", "fef2c0",
];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Label {
    name: String,
    // TODO: Serialize as "rrggbb" (no alpha, no octothorpe):
    color: Color,
    description: Option<String>,
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

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelSpec {
    name: String,
    rename_from: Vec<String>,
    options: LabelOptions,
}

impl LabelSpec {
    fn merged_with(&self, pspec: &PartialLabelSpec) -> LabelSpec {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelOptions {
    create: bool,
    update: bool,
    color: ColorSpec,
    description: String,
    on_rename_clash: OnRenameClash,
    enforce_case: bool,
}

impl LabelOptions {
    fn merged_with(&self, popt: &PartialLabelOptions) -> LabelOptions {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum OnRenameClash {
    Ignore,
    Warn,
    Error,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum ColorSpec {
    Fixed(Color),
    Random(Vec<Color>),
}
