use std::{borrow::Cow, hash::Hash as _};

use crate::{parser::Span, SemanticHash};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StringExpr<'a> {
    pub span: Span,
    pub fragments: Vec<StringFragment<'a>>,
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct PatternExpr<'a> {
    pub span: Span,
    pub fragments: Vec<PatternFragment<'a>>,
}

impl SemanticHash for PatternExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.as_slice().semantic_hash(state);
    }
}

/// Interpolated pattern fragment (i.e., can have capture patterns like `%` and `(a|b|c)`).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Interpolation<'a> {
    pub stem: InterpolationStem<'a>,
    pub options: Option<Box<InterpolationOptions<'a>>>,
}

impl SemanticHash for Interpolation<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.stem.semantic_hash(state);
        self.options.hash(state);
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct InterpolationOptions<'a> {
    /// `{stem:operation}`
    pub ops: Vec<InterpolationOp<'a>>,
    /// `{...*}` - This is not a normal operation because we need to treat it
    /// specially when building shell commands.
    pub join: Option<Cow<'a, str>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolationStem<'a> {
    /// Empty stem; inherit output type from the interpolated value.
    Implied,
    /// `{%}` - output is string.
    PatternCapture,
    /// `{1}` - output is string.
    CaptureGroup(usize),
    /// `{ident}` - output is string.
    Ident(Cow<'a, str>),
}

impl SemanticHash for InterpolationStem<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            InterpolationStem::Implied => (),
            InterpolationStem::PatternCapture => (),
            InterpolationStem::CaptureGroup(i) => i.hash(state),
            InterpolationStem::Ident(s) => s.hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolationOp<'a> {
    /// Replace extension - input must be path.
    ReplaceExtension(Cow<'a, str>, Cow<'a, str>),
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
            InterpolationOp::ReplaceExtension(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            InterpolationOp::PrependEach(s) | InterpolationOp::AppendEach(s) => s.hash(state),
            InterpolationOp::RegexReplace(r) => r.hash(state),
            InterpolationOp::ResolveOsPath => (),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RegexInterpolationOp<'a> {
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
