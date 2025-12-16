use std::hash::Hash;

use crate::{DiagnosticFileId, SemanticHash};

#[derive(Clone, Copy, Default)]
pub struct Span {
    pub start: Offset,
    pub end: Offset,
}

impl Span {
    #[must_use]
    #[inline]
    pub const fn ignore() -> Self {
        Self {
            start: Offset::ignore(),
            end: Offset::ignore(),
        }
    }

    #[must_use]
    #[inline]
    #[allow(clippy::cast_possible_truncation)]
    pub fn from_offset_and_len(offset: Offset, len: usize) -> Self {
        if offset.is_ignored() {
            Self::ignore()
        } else {
            Self {
                start: offset,
                end: Offset(offset.0 + len as u32),
            }
        }
    }

    #[must_use]
    #[inline]
    pub const fn is_ignored(&self) -> bool {
        self.start.is_ignored()
    }

    #[must_use]
    #[inline]
    pub const fn is_offset(&self) -> bool {
        !self.is_ignored() && self.start.0 == self.end.0
    }

    #[inline]
    #[must_use]
    pub fn merge(self, other: Span) -> Span {
        let min = self.start.0.min(other.start.0);
        let max = self.end.0.max(other.end.0);
        Span {
            start: Offset(min),
            end: Offset(max),
        }
    }

    #[inline]
    #[must_use]
    pub fn within(self, other: Span) -> Span {
        Span {
            start: Offset(self.start.0 + other.start.0),
            end: Offset(self.end.0 + other.start.0),
        }
    }

    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    #[must_use]
    #[inline]
    pub fn with_file(self, file: DiagnosticFileId) -> DiagnosticSpan {
        DiagnosticSpan { file, span: self }
    }
}

impl PartialEq for Span {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if self.is_ignored() || other.is_ignored() {
            return true;
        }

        self.start == other.start && self.end == other.end
    }
}

impl std::fmt::Debug for Span {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_ignored() {
            return f.write_str("ignored");
        }

        write!(f, "{}..{}", self.start.0, self.end.0)
    }
}

impl From<Offset> for Span {
    #[inline]
    fn from(value: Offset) -> Self {
        Self::from_offset_and_len(value, 0)
    }
}

#[derive(Clone, Copy)]
pub struct Offset(pub u32);

impl Offset {
    #[inline]
    #[must_use]
    pub fn to_span(self) -> Span {
        self.into()
    }
}

impl Default for Offset {
    #[inline]
    fn default() -> Self {
        Self::ignore()
    }
}

impl std::fmt::Debug for Offset {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_ignored() {
            return f.write_str("ignored");
        }

        write!(f, "{}", self.0)
    }
}

impl PartialEq for Offset {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.is_ignored() || other.is_ignored() || self.0 == other.0
    }
}

impl PartialOrd for Offset {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.is_ignored() || other.is_ignored() {
            return Some(std::cmp::Ordering::Equal);
        }

        self.0.partial_cmp(&other.0)
    }
}

impl Offset {
    #[must_use]
    pub const fn ignore() -> Self {
        Self(u32::MAX)
    }

    #[must_use]
    pub const fn is_ignored(self) -> bool {
        self.0 == u32::MAX
    }
}

impl From<std::ops::Range<u32>> for Span {
    #[inline]
    fn from(value: std::ops::Range<u32>) -> Self {
        Self {
            start: Offset(value.start),
            end: Offset(value.end),
        }
    }
}

impl From<std::ops::Range<Offset>> for Span {
    #[inline]
    fn from(value: std::ops::Range<Offset>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<std::ops::Range<usize>> for Span {
    #[allow(clippy::cast_possible_truncation)]
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: Offset(value.start as u32),
            end: Offset(value.end as u32),
        }
    }
}

impl From<Option<std::ops::Range<usize>>> for Span {
    fn from(value: Option<std::ops::Range<usize>>) -> Self {
        match value {
            Some(range) => range.into(),
            None => Span::default(),
        }
    }
}

impl From<Span> for std::ops::Range<u32> {
    #[inline]
    fn from(value: Span) -> Self {
        if value.is_ignored() {
            0..0
        } else {
            value.start.0..value.end.0
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    #[inline]
    fn from(value: Span) -> Self {
        if value.is_ignored() {
            0..0
        } else {
            value.start.0 as usize..value.end.0 as usize
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DiagnosticSpan {
    pub file: DiagnosticFileId,
    pub span: Span,
}

impl DiagnosticSpan {
    #[inline]
    #[must_use]
    pub fn annotation(
        self,
        level: annotate_snippets::Level<'static>,
        message: impl std::fmt::Display,
    ) -> crate::DiagnosticAnnotation {
        crate::DiagnosticAnnotation {
            span: self,
            level,
            message: message.to_string(),
        }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

#[must_use]
pub fn span(span: std::ops::Range<u32>) -> Span {
    span.into()
}

pub fn spanned<T>(span: impl Into<Span>, value: T) -> SpannedValue<T> {
    SpannedValue::new(span, value)
}

#[derive(Debug, Clone)]
pub struct SpannedValue<T> {
    pub span: Span,
    pub value: T,
}

impl<T> SpannedValue<T> {
    pub fn new(span: impl Into<Span>, value: T) -> Self {
        Self {
            span: span.into(),
            value,
        }
    }
}

impl<T> std::ops::Deref for SpannedValue<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: PartialEq> PartialEq for SpannedValue<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: PartialEq> PartialEq<T> for SpannedValue<T> {
    fn eq(&self, other: &T) -> bool {
        self.value == *other
    }
}

impl<T: Hash> SemanticHash for SpannedValue<T> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}
