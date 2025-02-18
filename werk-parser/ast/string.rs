use std::{fmt::Write, hash::Hash as _};

use werk_util::{SemanticHash, Span, Symbol};

use crate::parser::{
    escape_pattern_literal, escape_string_literal, parse_pattern_expr_unquoted,
    parse_string_expr_unquoted,
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StringExpr {
    pub span: Span,
    pub fragments: Vec<StringFragment>,
}

impl std::fmt::Display for StringExpr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            match fragment {
                StringFragment::Literal(s) => escape_string_literal(s).fmt(f)?,
                StringFragment::PatternStem => f.write_char('%')?,
                StringFragment::Interpolation(interp) => interp.fmt(f)?,
            }
        }
        Ok(())
    }
}

impl serde::Serialize for StringExpr {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&self.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for StringExpr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        parse_string_expr_unquoted(&s)
            .map(|mut expr| {
                expr.span = Span::ignore();
                expr
            })
            .map_err(serde::de::Error::custom)
    }
}

impl SemanticHash for StringExpr {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.as_slice().semantic_hash(state);
    }
}

impl StringExpr {
    pub fn literal(span: impl Into<Span>, s: impl Into<String>) -> Self {
        Self {
            span: span.into(),
            fragments: vec![StringFragment::Literal(s.into())],
        }
    }
}

/// Interpolated string fragment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringFragment {
    Literal(String),
    /// `%`, shorthand for `{%}`
    PatternStem,
    /// `{...}` or `<...>`
    Interpolation(Interpolation),
}

impl SemanticHash for StringFragment {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            StringFragment::Literal(s) => s.hash(state),
            StringFragment::PatternStem => (), // covered by discriminant
            StringFragment::Interpolation(i) => i.semantic_hash(state),
        }
    }
}

impl Default for StringFragment {
    #[inline]
    fn default() -> Self {
        StringFragment::Literal(String::new())
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct PatternExpr {
    pub span: Span,
    pub fragments: Vec<PatternFragment>,
}

impl std::fmt::Display for PatternExpr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(s) => escape_pattern_literal(s).fmt(f)?,
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

impl serde::Serialize for PatternExpr {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&self.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for PatternExpr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        parse_pattern_expr_unquoted(&s)
            .map(|mut expr| {
                expr.span = Span::ignore();
                expr
            })
            .map_err(serde::de::Error::custom)
    }
}

impl SemanticHash for PatternExpr {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.as_slice().semantic_hash(state);
    }
}

/// Interpolated pattern fragment (i.e., can have capture patterns like `%` and `(a|b|c)`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternFragment {
    Literal(String),
    /// `%`
    PatternStem,
    /// `(a|b|c)`
    OneOf(Vec<String>),
    /// `{...}`
    Interpolation(Interpolation),
}

impl SemanticHash for PatternFragment {
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
pub struct Interpolation {
    pub stem: InterpolationStem,
    pub index: Option<InterpolationIndex>,
    pub options: Option<Box<InterpolationOptions>>,
}

impl Interpolation {
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

impl SemanticHash for Interpolation {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.stem.semantic_hash(state);
        self.options.semantic_hash(state);
    }
}

impl std::fmt::Display for Interpolation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let is_path = self.is_path_interpolation();

        f.write_char(if is_path { '<' } else { '{' })?;

        match &self.stem {
            InterpolationStem::Implied => (),
            InterpolationStem::PatternCapture => f.write_char('%')?,
            InterpolationStem::CaptureGroup(i) => write!(f, "{i}")?,
            InterpolationStem::Ident(ident) => write!(f, "{ident}")?,
        }

        match &self.index {
            Some(InterpolationIndex::Const(i)) => write!(f, "[{i}]")?,
            Some(InterpolationIndex::Ident(ident)) => write!(f, "[{ident}]")?,
            None => (),
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
                    InterpolationOp::Dedup => f.write_str("dedup")?,
                    InterpolationOp::Filename => f.write_str("filename")?,
                    InterpolationOp::Dirname => f.write_str("dir")?,
                    InterpolationOp::Ext => f.write_str("ext")?,
                    InterpolationOp::ResolveOutDir => f.write_str("out-dir")?,
                    InterpolationOp::ResolveWorkspace => f.write_str("workspace")?,
                }
            }
        }

        f.write_char(if is_path { '>' } else { '}' })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InterpolationOptions {
    /// `{stem:operation}`
    pub ops: Vec<InterpolationOp>,
    /// `{...*}` - This is not a normal operation because we need to treat it
    /// specially when building shell commands.
    pub join: Option<String>,
}

impl SemanticHash for InterpolationOptions {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InterpolationIndex {
    /// `{ident[index]}` - output is string
    Const(i32),
    /// `{ident[ident]}` - output is string
    Ident(Symbol),
}

impl SemanticHash for InterpolationIndex {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            InterpolationIndex::Const(i) => i.hash(state),
            InterpolationIndex::Ident(s) => s.as_str().hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolationOp {
    Dedup,
    /// Get the filename part of a path.
    Filename,
    /// Get the directory part of a path (wihout a final path separator).
    Dirname,
    /// Get the file extension of a path (without the dot).
    Ext,
    /// Replace extension - input must be path.
    ReplaceExtension {
        from: String,
        to: String,
    },
    PrependEach(String),
    AppendEach(String),
    RegexReplace(RegexInterpolationOp),
    // Interpret the string as an OS path and resolve it. This is the `<..>`
    // interpolation syntax.
    ResolveOsPath,
    ResolveOutDir,
    ResolveWorkspace,
}

impl SemanticHash for InterpolationOp {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            InterpolationOp::ReplaceExtension { from, to } => {
                from.hash(state);
                to.hash(state);
            }
            InterpolationOp::PrependEach(s) | InterpolationOp::AppendEach(s) => s.hash(state),
            InterpolationOp::RegexReplace(r) => r.hash(state),
            // Covered by discriminant.
            InterpolationOp::Dedup
            | InterpolationOp::Filename
            | InterpolationOp::Dirname
            | InterpolationOp::Ext
            | InterpolationOp::ResolveOsPath
            | InterpolationOp::ResolveOutDir
            | InterpolationOp::ResolveWorkspace => (),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RegexInterpolationOp {
    pub regex: regex::Regex,
    pub replacer: String,
}

impl PartialEq for RegexInterpolationOp {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.replacer == other.replacer
    }
}

impl Eq for RegexInterpolationOp {}

impl std::hash::Hash for RegexInterpolationOp {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.regex.as_str().hash(state);
        self.replacer.hash(state);
    }
}
