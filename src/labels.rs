use crate::config::{PartialLabelOptions, PartialLabelSpec};
use csscolorparser::Color;
use serde::{Deserialize, Serialize, Serializer};
use serde_with::{serde_as, DisplayFromStr};

// These are the "default colors" listed when creating a label via GitHub's web
// UI as of 2023-09-24:
static DEFAULT_COLORS: &[&str] = &[
    "0052cc", "006b75", "0e8a16", "1d76db", "5319e7", "b60205", "bfd4f2", "bfdadc", "c2e0c6",
    "c5def5", "d4c5f9", "d93f0b", "e99695", "f9d0c4", "fbca04", "fef2c0",
];

type LabelName = unicase::UniCase<String>;

#[serde_as]
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Label {
    #[serde_as(as = "DisplayFromStr")]
    name: LabelName,
    #[serde(serialize_with = "color2rgbhex")]
    color: Color,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
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

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelSpec {
    name: LabelName,
    rename_from: Vec<LabelName>,
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

fn color2rgbhex<S: Serializer>(color: &Color, serializer: S) -> Result<S::Ok, S::Error> {
    let [r, g, b, _] = color.to_rgba8();
    let s = format!("{:02x}{:02x}{:02x}", r, g, b);
    s.serialize(serializer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[serde_as]
    #[derive(Clone, Debug, PartialEq, Serialize)]
    struct ColorContainer {
        #[serde(serialize_with = "color2rgbhex")]
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
}
