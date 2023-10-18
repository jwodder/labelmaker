use crate::config::{PartialLabelOptions, PartialLabelSpec};
use csscolorparser::Color;
use serde::{Deserialize, Serialize, Serializer};
use serde_with::{serde_as, DisplayFromStr};

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

pub(crate) type LabelName = unicase::UniCase<String>;

pub(crate) trait LabelNameExt {
    fn from_str(s: &str) -> Self;
}

impl LabelNameExt for LabelName {
    fn from_str(s: &str) -> Self {
        LabelName::new(s.to_string())
    }
}

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
    pub(crate) name: LabelName,
    pub(crate) rename_from: Vec<LabelName>,
    pub(crate) options: LabelOptions,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LabelOptions {
    pub(crate) color: ColorSpec,
    pub(crate) description: String,
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
                .unwrap_or(&self.description)
                .clone(),
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
            description: String::new(),
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

    #[test]
    fn test_default_color_spec() {
        let ColorSpec::Random(colors) = ColorSpec::default() else {
            panic!("ColorSpec::default() was not Random");
        };
        assert_eq!(colors.len(), DEFAULT_COLORS.len());
    }
}
