use std::{borrow::Cow, fmt::Write, hash::Hash as _};

use werk_util::Symbol;

use crate::{
    parser::{parse_pattern_expr_unquoted, parse_string_expr_unquoted, Escape, Span},
    SemanticHash,
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StringExpr<'a> {
    pub span: Span,
    pub fragments: Vec<StringFragment<'a>>,
}

impl StringExpr<'_> {
    #[inline]
    pub fn into_static(self) -> StringExpr<'static> {
        StringExpr {
            span: self.span,
            fragments: self
                .fragments
                .into_iter()
                .map(StringFragment::into_static)
                .collect(),
        }
    }
}

impl std::fmt::Display for StringExpr<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            match fragment {
                StringFragment::Literal(s) => Escape::<false>(s).fmt(f)?,
                StringFragment::Interpolation(interp) => interp.fmt(f)?,
            }
        }
        Ok(())
    }
}

impl serde::Serialize for StringExpr<'_> {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&self.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for StringExpr<'_> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        parse_string_expr_unquoted(&s)
            .map(|mut expr| {
                expr.span = Span::ignore();
                expr.into_static()
            })
            .map_err(serde::de::Error::custom)
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringFragment<'a> {
    Literal(Cow<'a, str>),
    /// `{...}`
    Interpolation(Interpolation<'a>),
}

impl StringFragment<'_> {
    #[must_use]
    pub fn into_static(self) -> StringFragment<'static> {
        match self {
            StringFragment::Literal(s) => StringFragment::Literal(s.into_owned().into()),
            StringFragment::Interpolation(interp) => {
                StringFragment::Interpolation(interp.into_static())
            }
        }
    }
}

impl SemanticHash for StringFragment<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            StringFragment::Literal(s) => s.hash(state),
            StringFragment::Interpolation(i) => i.semantic_hash(state),
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

impl PatternExpr<'_> {
    #[inline]
    #[must_use]
    pub fn into_static(self) -> PatternExpr<'static> {
        PatternExpr {
            span: self.span,
            fragments: self
                .fragments
                .into_iter()
                .map(PatternFragment::into_static)
                .collect(),
        }
    }
}

impl std::fmt::Display for PatternExpr<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(s) => Escape::<true>(s).fmt(f)?,
                PatternFragment::Interpolation(interp) => interp.fmt(f)?,
                PatternFragment::PatternStem => f.write_char('%')?,
                PatternFragment::OneOf(vec) => {
                    f.write_char('(')?;
                    for (index, pattern) in vec.iter().enumerate() {
                        if index != 0 {
                            f.write_char('|')?;
                        }
                        f.write_str(pattern)?;
                    }
                    f.write_char(')')?;
                }
            }
        }
        Ok(())
    }
}

impl serde::Serialize for PatternExpr<'_> {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&self.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for PatternExpr<'_> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        parse_pattern_expr_unquoted(&s)
            .map(|mut expr| {
                expr.span = Span::ignore();
                expr.into_static()
            })
            .map_err(serde::de::Error::custom)
    }
}

impl SemanticHash for PatternExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.as_slice().semantic_hash(state);
    }
}

/// Interpolated pattern fragment (i.e., can have capture patterns like `%` and `(a|b|c)`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternFragment<'a> {
    Literal(Cow<'a, str>),
    /// `%`
    PatternStem,
    /// `(a|b|c)`
    OneOf(Vec<Cow<'a, str>>),
    /// `{...}`
    Interpolation(Interpolation<'a>),
}

impl PatternFragment<'_> {
    #[must_use]
    pub fn into_static(self) -> PatternFragment<'static> {
        match self {
            PatternFragment::Literal(s) => PatternFragment::Literal(s.into_owned().into()),
            PatternFragment::PatternStem => PatternFragment::PatternStem,
            PatternFragment::OneOf(v) => {
                PatternFragment::OneOf(v.into_iter().map(Cow::into_owned).map(Cow::Owned).collect())
            }
            PatternFragment::Interpolation(interp) => {
                PatternFragment::Interpolation(interp.into_static())
            }
        }
    }
}

impl SemanticHash for PatternFragment<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            PatternFragment::Literal(s) => s.hash(state),
            PatternFragment::PatternStem => (),
            PatternFragment::OneOf(v) => v.hash(state),
            PatternFragment::Interpolation(i) => i.semantic_hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Interpolation<'a> {
    pub stem: InterpolationStem,
    pub options: Option<Box<InterpolationOptions<'a>>>,
}

impl Interpolation<'_> {
    #[must_use]
    pub fn into_static(self) -> Interpolation<'static> {
        Interpolation {
            stem: self.stem,
            options: self.options.map(|o| Box::new(o.into_static())),
        }
    }

    #[inline]
    #[must_use]
    pub fn is_path_interpolation(&self) -> bool {
        if let Some(ref options) = self.options {
            if options
                .ops
                .iter()
                .any(|op| matches!(&op, InterpolationOp::ResolveOsPath))
            {
                return true;
            }
        }

        false
    }

    #[inline]
    #[must_use]
    pub fn join(&self) -> Option<&str> {
        if let Some(ref options) = self.options {
            options.join.as_deref()
        } else {
            None
        }
    }
}

impl SemanticHash for Interpolation<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.stem.semantic_hash(state);
        self.options.semantic_hash(state);
    }
}

impl std::fmt::Display for Interpolation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let is_path = self.is_path_interpolation();

        f.write_char(if is_path { '<' } else { '{' })?;

        match &self.stem {
            InterpolationStem::Implied => (),
            InterpolationStem::PatternCapture => f.write_char('%')?,
            InterpolationStem::CaptureGroup(i) => write!(f, "{i}")?,
            InterpolationStem::Ident(ident) => write!(f, "{ident}")?,
        }

        if let Some(join) = self.join() {
            if join == " " {
                // Elide the separator when it is a single space.
                f.write_char('*')?;
            } else {
                write!(f, "{join}*")?;
            }
        }

        if let Some(options) = self.options.as_deref() {
            let mut has_colon = false;
            let mut is_first = true;
            for op in &options.ops {
                if let InterpolationOp::ResolveOsPath = op {
                    continue;
                }
                if !has_colon {
                    f.write_char(':')?;
                    has_colon = true;
                }
                if is_first {
                    is_first = false;
                } else {
                    f.write_char(',')?;
                }

                match op {
                    InterpolationOp::ReplaceExtension { from, to } => write!(f, "{from}={to}")?,
                    InterpolationOp::PrependEach(_) => todo!(),
                    InterpolationOp::AppendEach(_) => todo!(),
                    InterpolationOp::RegexReplace(regex_interpolation_op) => write!(
                        f,
                        "s/{}/{}/",
                        // TODO: Escape
                        regex_interpolation_op.regex,
                        regex_interpolation_op.replacer
                    )?,
                    InterpolationOp::ResolveOsPath => unreachable!(),
                }
            }
        }

        f.write_char(if is_path { '>' } else { '}' })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InterpolationOptions<'a> {
    /// `{stem:operation}`
    pub ops: Vec<InterpolationOp<'a>>,
    /// `{...*}` - This is not a normal operation because we need to treat it
    /// specially when building shell commands.
    pub join: Option<Cow<'a, str>>,
}

impl InterpolationOptions<'_> {
    pub fn into_static(self) -> InterpolationOptions<'static> {
        InterpolationOptions {
            ops: self
                .ops
                .into_iter()
                .map(InterpolationOp::into_static)
                .collect(),
            join: self.join.map(Cow::into_owned).map(Cow::Owned),
        }
    }
}

impl SemanticHash for InterpolationOptions<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ops.as_slice().semantic_hash(state);
        self.join.as_deref().semantic_hash(state);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InterpolationStem {
    /// Empty stem; inherit output type from the interpolated value.
    Implied,
    /// `{%}` - output is string.
    PatternCapture,
    /// `{1}` - output is string.
    CaptureGroup(u32),
    /// `{ident}` - output is string.
    Ident(Symbol),
}

impl SemanticHash for InterpolationStem {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            InterpolationStem::PatternCapture | InterpolationStem::Implied => (),
            InterpolationStem::CaptureGroup(i) => i.hash(state),
            InterpolationStem::Ident(s) => s.as_str().hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

impl InterpolationOp<'_> {
    #[inline]
    #[must_use]
    pub fn into_static(self) -> InterpolationOp<'static> {
        match self {
            InterpolationOp::ReplaceExtension { from, to } => InterpolationOp::ReplaceExtension {
                from: from.into_owned().into(),
                to: to.into_owned().into(),
            },
            InterpolationOp::PrependEach(s) => InterpolationOp::PrependEach(s.into_owned().into()),
            InterpolationOp::AppendEach(s) => InterpolationOp::AppendEach(s.into_owned().into()),
            InterpolationOp::RegexReplace(r) => InterpolationOp::RegexReplace(r.into_static()),
            InterpolationOp::ResolveOsPath => InterpolationOp::ResolveOsPath,
        }
    }
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

#[derive(Clone, Debug)]
pub struct RegexInterpolationOp<'a> {
    pub regex: regex::Regex,
    pub replacer: Cow<'a, str>,
}

impl RegexInterpolationOp<'_> {
    #[inline]
    #[must_use]
    pub fn into_static(self) -> RegexInterpolationOp<'static> {
        RegexInterpolationOp {
            regex: self.regex,
            replacer: self.replacer.into_owned().into(),
        }
    }
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
