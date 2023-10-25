mod labelset;
pub(crate) use self::labelset::*;
use csscolorparser::Color;
use serde::{
    de::{Deserializer, Unexpected, Visitor},
    ser::Serializer,
    Deserialize, Serialize,
};
use serde_with::{serde_as, DeserializeAs, SerializeAs};
use smartstring::alias::CompactString;
use std::fmt;
use std::ops::Deref;
use thiserror::Error;

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
        // GitHub normalizes label names by removing leading & trailing TAB,
        // LF, VT, FF, CR, and SP and converting internal LF to SP.
        let s = s.trim_matches(['\t', '\n', '\x0B', '\x0C', '\r', ' '].as_slice());
        if s.is_empty() {
            Err(ParseLabelNameError)
        } else {
            Ok(LabelName(
                s.chars().map(|c| if c == '\n' { ' ' } else { c }).collect(),
            ))
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

#[derive(Clone, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct Description(String);

impl fmt::Debug for Description {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for Description {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<str> for Description {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<&'a str> for Description {
    fn eq(&self, other: &&'a str) -> bool {
        &self.0 == other
    }
}

impl std::str::FromStr for Description {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Description, std::convert::Infallible> {
        // GitHub normalizes descriptions by removing leading & trailing NUL,
        // TAB, LF, VT, FF, CR, and SP and converting internal LF to SP.
        let s = s.trim_matches(['\0', '\t', '\n', '\x0B', '\x0C', '\r', ' '].as_slice());
        Ok(Description(s.replace('\n', " ")))
    }
}

impl AsRef<str> for Description {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Deref for Description {
    type Target = str;

    fn deref(&self) -> &str {
        self.0.deref()
    }
}

impl Serialize for Description {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}

impl<'de> Deserialize<'de> for Description {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(s.parse::<Description>().unwrap())
    }
}

#[serde_as]
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct Label {
    pub(crate) name: LabelName,
    #[serde_as(as = "AsHashlessRgb")]
    pub(crate) color: Color,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) description: Option<Description>,
}

pub(crate) fn hashless_rgb(color: &Color) -> String {
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

    mod label_name {
        use super::*;
        use rstest::rstest;

        #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
        struct NameContainer {
            name: LabelName,
        }

        #[test]
        fn normal() {
            let name = "foo".parse::<LabelName>().unwrap();
            assert_eq!(name, "foo");
            assert_ne!(name, "Foo");
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

        #[rstest]
        #[case(" foo ", "foo")]
        #[case("foo ", "foo")]
        #[case(" foo", "foo")]
        #[case("\t\n\x0B\x0C\r foo", "foo")]
        #[case("foo\t\n\x0B\x0C\r ", "foo")]
        #[case("foo\nbar", "foo bar")]
        #[case("foo \nbar", "foo  bar")]
        #[case("foo\tbar", "foo\tbar")]
        #[case("foo \tbar", "foo \tbar")]
        #[case("\0", "\0")]
        #[case("\0foo", "\0foo")]
        fn normalized(#[case] before: &str, #[case] after: &str) {
            let name = before.parse::<LabelName>().unwrap();
            assert_eq!(name, after);
            assert_eq!(name.to_string(), after);
            assert_eq!(name.as_ref(), after);
            assert_eq!(format!("{name:?}"), format!("{after:?}"));
            let cntr = NameContainer { name: name.clone() };
            let before_json = serde_json::json!({"name": before}).to_string();
            assert_eq!(
                serde_json::from_str::<NameContainer>(&before_json).unwrap(),
                cntr
            );
            let after_json = serde_json::json!({"name": after}).to_string();
            assert_eq!(serde_json::to_string(&cntr).unwrap(), after_json);
        }

        #[rstest]
        #[case("")]
        #[case("\t")]
        #[case("\n")]
        #[case("\x0B")]
        #[case("\x0C")]
        #[case("\r")]
        #[case(" ")]
        #[case("\t\n\x0B\x0C\r ")]
        fn error(#[case] name: &str) {
            let r = name.parse::<LabelName>();
            assert_eq!(r, Err(ParseLabelNameError));
            let json = serde_json::json!({"name": name}).to_string();
            assert!(serde_json::from_str::<NameContainer>(&json).is_err());
        }
    }

    mod color {
        use super::*;
        use rstest::rstest;

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
        fn as_hashless_rgb(#[case] color: Color, #[case] s: &str) {
            let obj = ColorContainer { color };
            let expected = format!(r#"{{"color":"{s}"}}"#);
            assert_eq!(serde_json::to_string(&obj).unwrap(), expected);
        }
    }

    mod description {
        use super::*;
        use rstest::rstest;

        #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
        struct DescContainer {
            description: Description,
        }

        #[rstest]
        #[case("foo", "foo")]
        #[case(" foo ", "foo")]
        #[case("foo ", "foo")]
        #[case(" foo", "foo")]
        #[case("", "")]
        #[case("\0", "")]
        #[case("\t", "")]
        #[case("\n", "")]
        #[case("\x0B", "")]
        #[case("\x0C", "")]
        #[case("\r", "")]
        #[case(" ", "")]
        #[case("\0\t\n\x0B\x0C\r ", "")]
        #[case("\0\t\n\x0B\x0C\r foo", "foo")]
        #[case("foo\0\t\n\x0B\x0C\r ", "foo")]
        #[case("foo\nbar", "foo bar")]
        #[case("foo \nbar", "foo  bar")]
        #[case("foo\tbar", "foo\tbar")]
        #[case("foo \tbar", "foo \tbar")]
        fn test(#[case] before: &str, #[case] after: &str) {
            let desc = before.parse::<Description>().unwrap();
            assert_eq!(desc, after);
            assert_eq!(desc.to_string(), after);
            assert_eq!(desc.as_ref(), after);
            assert_eq!(desc.deref(), after);
            assert_eq!(format!("{desc:?}"), format!("{after:?}"));
            let cntr = DescContainer {
                description: desc.clone(),
            };
            let before_json = serde_json::json!({"description": before}).to_string();
            assert_eq!(
                serde_json::from_str::<DescContainer>(&before_json).unwrap(),
                cntr
            );
            let after_json = serde_json::json!({"description": after}).to_string();
            assert_eq!(serde_json::to_string(&cntr).unwrap(), after_json);
        }
    }
}
