use std::{borrow::Cow, hash::Hash as _, marker::PhantomData};

use serde::Deserialize;

use crate::{parser::Span, SemanticHash};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StringExpr<'a> {
    pub span: Span,
    pub fragments: Vec<StringFragment<'a>>,
}

impl serde::Serialize for StringExpr<'_> {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq as _;
        if self.fragments.len() == 1 {
            self.fragments[0].serialize(ser)
        } else {
            let mut seq = ser.serialize_seq(Some(self.fragments.len()))?;
            for fragment in &self.fragments {
                seq.serialize_element(fragment)?;
            }
            seq.end()
        }
    }
}

impl<'de> serde::Deserialize<'de> for StringExpr<'_> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<'a>(PhantomData<&'a ()>);
        impl<'a, 'de> serde::de::Visitor<'de> for Visitor<'a> {
            type Value = StringExpr<'a>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "string literal, list of fragments, or map")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(StringExpr {
                    span: Span::ignore(),
                    fragments: vec![StringFragment::Literal(Cow::Owned(v.into()))],
                })
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let fragments =
                    Vec::deserialize(serde::de::value::SeqAccessDeserializer::new(seq))?;
                Ok(StringExpr {
                    span: Span::ignore(),
                    fragments,
                })
            }

            fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let fragment =
                    StringFragment::deserialize(serde::de::value::MapAccessDeserializer::new(map))?;
                Ok(StringExpr {
                    span: Span::ignore(),
                    fragments: vec![fragment],
                })
            }
        }

        deserializer.deserialize_any(Visitor(PhantomData))
    }
}

impl SemanticHash for StringExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.as_slice().semantic_hash(state);
    }
}

impl<'a> StringExpr<'a> {
    pub fn literal(span: impl Into<Span>, s: impl Into<Cow<'a, str>>) -> Self {
        Self {
            span: span.into(),
            fragments: vec![StringFragment::Literal(s.into())],
        }
    }
}

/// Interpolated string fragment.
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum StringFragment<'a> {
    Literal(Cow<'a, str>),
    /// `{...}`
    Interpolation(Interpolation<'a>),
}

impl SemanticHash for StringFragment<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            StringFragment::Literal(s) => s.hash(state),
            StringFragment::Interpolation(i) => i.hash(state),
        }
    }
}

impl Default for StringFragment<'_> {
    #[inline]
    fn default() -> Self {
        StringFragment::Literal(Cow::Borrowed(""))
    }
}

#[derive(Clone, Debug, Default, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct PatternExpr<'a> {
    #[serde(skip, default)]
    pub span: Span,
    pub fragments: Vec<PatternFragment<'a>>,
}

impl SemanticHash for PatternExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.as_slice().semantic_hash(state);
    }
}

/// Interpolated pattern fragment (i.e., can have capture patterns like `%` and `(a|b|c)`).
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum PatternFragment<'a> {
    Literal(Cow<'a, str>),
    /// `%`
    PatternStem,
    /// `(a|b|c)`
    OneOf(Vec<Cow<'a, str>>),
    /// `{...}`
    Interpolation(Interpolation<'a>),
}

impl SemanticHash for PatternFragment<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            PatternFragment::Literal(s) => s.hash(state),
            PatternFragment::PatternStem => (),
            PatternFragment::OneOf(v) => v.hash(state),
            PatternFragment::Interpolation(i) => i.hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Interpolation<'a> {
    pub stem: InterpolationStem<'a>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub options: Option<Box<InterpolationOptions<'a>>>,
}

impl SemanticHash for Interpolation<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.stem.semantic_hash(state);
        self.options.hash(state);
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct InterpolationOptions<'a> {
    /// `{stem:operation}`
    pub ops: Vec<InterpolationOp<'a>>,
    /// `{...*}` - This is not a normal operation because we need to treat it
    /// specially when building shell commands.
    pub join: Option<Cow<'a, str>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum InterpolationStem<'a> {
    /// Empty stem; inherit output type from the interpolated value.
    Implied,
    /// `{%}` - output is string.
    PatternCapture,
    /// `{1}` - output is string.
    CaptureGroup(u32),
    /// `{ident}` - output is string.
    Ident(Cow<'a, str>),
}

impl SemanticHash for InterpolationStem<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            InterpolationStem::PatternCapture | InterpolationStem::Implied => (),
            InterpolationStem::CaptureGroup(i) => i.hash(state),
            InterpolationStem::Ident(s) => s.hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type")]
pub enum InterpolationOp<'a> {
    /// Replace extension - input must be path.
    ReplaceExtension {
        from: Cow<'a, str>,
        to: Cow<'a, str>,
    },
    PrependEach(Cow<'a, str>),
    AppendEach(Cow<'a, str>),
    RegexReplace(RegexInterpolationOp<'a>),
    // Interpret the string as an OS path and resolve it. This is the `<..>`
    // interpolation syntax.
    ResolveOsPath,
}

impl SemanticHash for InterpolationOp<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            InterpolationOp::ReplaceExtension { from, to } => {
                from.hash(state);
                to.hash(state);
            }
            InterpolationOp::PrependEach(s) | InterpolationOp::AppendEach(s) => s.hash(state),
            InterpolationOp::RegexReplace(r) => r.hash(state),
            InterpolationOp::ResolveOsPath => (),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct RegexInterpolationOp<'a> {
    #[serde(
        serialize_with = "serialize_regex",
        deserialize_with = "deserialize_regex"
    )]
    pub regex: regex::Regex,
    pub replacer: Cow<'a, str>,
}

impl PartialEq for RegexInterpolationOp<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.replacer == other.replacer
    }
}

impl Eq for RegexInterpolationOp<'_> {}

impl std::hash::Hash for RegexInterpolationOp<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.regex.as_str().hash(state);
        self.replacer.hash(state);
    }
}

fn serialize_regex<S: serde::Serializer>(regex: &regex::Regex, ser: S) -> Result<S::Ok, S::Error> {
    ser.serialize_str(regex.as_str())
}

fn deserialize_regex<'de, D: serde::Deserializer<'de>>(de: D) -> Result<regex::Regex, D::Error> {
    let string: String = serde::Deserialize::deserialize(de)?;
    regex::Regex::new(&string).map_err(serde::de::Error::custom)
}
