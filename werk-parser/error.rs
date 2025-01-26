use std::sync::Arc;

use winnow::stream::Location;

use crate::{
    parse_toml::{ExprType, RunExprType},
    parser::{Input, Offset, Span, SpannedValue},
};

pub trait DisplayError {
    fn title(&self) -> impl std::fmt::Display;
    fn snippets(&self) -> impl IntoIterator<Item: DisplaySnippet>;
}

pub trait DisplaySnippet {
    fn annotations(&self) -> impl IntoIterator<Item: DisplayAnnotation>;
}

pub trait DisplayAnnotation {
    fn level(&self) -> annotate_snippets::Level;
    fn span(&self) -> Span;
    fn label(&self) -> impl std::fmt::Display;
}

#[derive(thiserror::Error, Debug)]
pub enum TomlError {
    #[error(transparent)]
    Toml(Box<toml_edit::TomlError>),
    #[error("unknown config key")]
    UnknownConfigKey(Span),
    #[error("invalid key")]
    InvalidKey(Span),
    #[error("expected table")]
    ExpectedTable(Span),
    #[error("expected string")]
    ExpectedString(Span),
    #[error("expected boolean")]
    ExpectedBool(Span),
    #[error("expected string or table")]
    ExpectedStringOrTable(Span),
    #[error("expected string or array")]
    ExpectedStringOrArray(Span),
    #[error("expected integer")]
    ExpectedInteger(Span),
    #[error("expected key '{1}' in table expression")]
    ExpectedKey(Span, &'static &'static str),
    #[error("expression table contain a root expression, one of: {}", ExprType::all_strs().join(", "))]
    ExpectedMainExpression(Span),
    #[error("expression table can only contain one root expression, found: {} and {}", &**.0, &**.1)]
    AmbiguousMainExpression(SpannedValue<ExprType>, SpannedValue<ExprType>),
    #[error("expression table can only contain one root expression, found: {} and {}", &**.0, &**.1)]
    AmbiguousRunExpression(SpannedValue<RunExprType>, SpannedValue<RunExprType>),
    #[error("unknown chaining expression")]
    UnknownExpressionChain(Span),
    #[error("invalid identifier: {1}")]
    InvalidIdent(Span, TomlParseError),
    #[error("invalid string expression: {1}")]
    InvalidStringExpr(Span, TomlParseError),
    #[error("invalid pattern expression: {1}")]
    InvalidPatternExpr(Span, TomlParseError),
}

impl DisplayError for TomlError {
    fn title(&self) -> impl std::fmt::Display {
        "error parsing TOML manifest"
    }

    fn snippets(&self) -> impl IntoIterator<Item: DisplaySnippet> {
        [self]
    }
}

impl DisplaySnippet for &TomlError {
    fn annotations(&self) -> impl IntoIterator<Item: DisplayAnnotation> {
        [*self]
    }
}

impl DisplayAnnotation for &TomlError {
    fn level(&self) -> annotate_snippets::Level {
        annotate_snippets::Level::Error
    }

    fn span(&self) -> Span {
        (*self).span()
    }

    fn label(&self) -> impl std::fmt::Display {
        self
    }
}

impl From<toml_edit::TomlError> for TomlError {
    #[inline]
    fn from(value: toml_edit::TomlError) -> Self {
        Self::Toml(Box::new(value))
    }
}

impl TomlError {
    pub fn span(&self) -> Span {
        match self {
            Self::Toml(toml_error) => toml_error.span().map_or(Span::ignore(), Into::into),
            Self::InvalidKey(span)
            | Self::UnknownConfigKey(span)
            | Self::ExpectedTable(span)
            | Self::ExpectedString(span)
            | Self::ExpectedBool(span)
            | Self::ExpectedStringOrTable(span)
            | Self::ExpectedStringOrArray(span)
            | Self::ExpectedInteger(span)
            | Self::ExpectedKey(span, _)
            | Self::ExpectedMainExpression(span)
            | Self::UnknownExpressionChain(span)
            | Self::InvalidIdent(span, ..)
            | Self::InvalidStringExpr(span, ..)
            | Self::InvalidPatternExpr(span, ..)
            | Self::AmbiguousMainExpression(_, SpannedValue { span, .. })
            | Self::AmbiguousRunExpression(_, SpannedValue { span, .. }) => *span,
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Toml(#[from] TomlError),
    #[error(transparent)]
    Werk(#[from] ParseError),
}

impl Error {
    #[must_use]
    pub fn with_location<'a>(
        self,
        file_name: &'a std::path::Path,
        source_code: &'a str,
    ) -> LocatedError<'a, Self> {
        LocatedError {
            file_name,
            source_code,
            error: self,
        }
    }

    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Error::Toml(toml_error) => toml_error.span(),
            Error::Werk(werk_error) => Span::from_offset_and_len(werk_error.offset, 0),
        }
    }
}

impl From<toml_edit::TomlError> for Error {
    #[inline]
    fn from(value: toml_edit::TomlError) -> Self {
        Self::Toml(value.into())
    }
}

enum Either<A, B> {
    Left(A),
    Right(B),
}

impl<A: Iterator, B: Iterator> Iterator for Either<A, B> {
    type Item = Either<A::Item, B::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Either::Left(left) => left.next().map(Either::Left),
            Either::Right(right) => right.next().map(Either::Right),
        }
    }
}

impl<A: DisplaySnippet, B: DisplaySnippet> DisplaySnippet for Either<A, B> {
    fn annotations(&self) -> impl IntoIterator<Item: DisplayAnnotation> {
        let either = match self {
            Either::Left(left) => Either::Left(left.annotations().into_iter()),
            Either::Right(right) => Either::Right(right.annotations().into_iter()),
        };
        either
    }
}

impl<A: DisplayAnnotation, B: DisplayAnnotation> DisplayAnnotation for Either<A, B> {
    fn level(&self) -> annotate_snippets::Level {
        match self {
            Either::Left(left) => left.level(),
            Either::Right(right) => right.level(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Either::Left(left) => left.span(),
            Either::Right(right) => right.span(),
        }
    }

    fn label(&self) -> impl std::fmt::Display {
        match self {
            Either::Left(left) => Either::Left(left.label()),
            Either::Right(right) => Either::Right(right.label()),
        }
    }
}

impl<A: std::fmt::Display, B: std::fmt::Display> std::fmt::Display for Either<A, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Either::Left(left) => left.fmt(f),
            Either::Right(right) => right.fmt(f),
        }
    }
}

impl DisplayError for Error {
    fn title(&self) -> impl std::fmt::Display {
        self
    }

    fn snippets(&self) -> impl IntoIterator<Item: DisplaySnippet> {
        let snippets = match self {
            Error::Toml(toml_error) => Either::Left(toml_error.snippets().into_iter()),
            Error::Werk(parse_error) => Either::Right(parse_error.snippets().into_iter()),
        };
        snippets
    }
}

#[derive(Debug)]
pub struct LocatedError<'a, E> {
    pub file_name: &'a std::path::Path,
    pub source_code: &'a str,
    pub error: E,
}

impl<E: DisplayError> std::fmt::Display for LocatedError<'_, E> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use annotate_snippets::Level;

        // Collect all annotations into an owned structure, because
        // `annotate-snippets` requires references, but we want to operate in
        // terms of `Display`.
        struct Annotation {
            level: Level,
            span: Span,
            label: String,
        }
        let mut annotations = vec![];
        for snippet in self.error.snippets() {
            for annotation in snippet.annotations() {
                annotations.push(Annotation {
                    level: annotation.level(),
                    span: annotation.span(),
                    label: annotation.label().to_string(),
                });
            }
        }

        let title = self.error.title().to_string();
        let origin = self.file_name.to_string_lossy();
        let mut message = Level::Error.title(&title);
        if !annotations.is_empty() {
            message = message.snippet(
                annotate_snippets::Snippet::source(self.source_code)
                    .origin(&origin)
                    .fold(true)
                    .annotations(
                        annotations
                            .iter()
                            .map(|a| a.level.span(a.span.into()).label(&a.label)),
                    ),
            );
        }

        let renderer = annotate_snippets::Renderer::styled();
        let render = renderer.render(message);
        render.fmt(f)
    }
}

impl<E: DisplayError + std::fmt::Debug> std::error::Error for LocatedError<'_, E> {}

impl LocatedError<'_, Error> {
    #[must_use]
    pub fn render(&self) -> String {
        self.to_string()
    }
}

impl<E> std::ops::Deref for LocatedError<'_, E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        &self.error
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TomlParseError {
    #[error("empty identifier")]
    EmptyIdentifier,
    #[error("invalid char in identifier: {0}")]
    InvalidIdentifier(char),
    #[error(transparent)]
    InvalidExpr(StringExprParseError),
}

#[derive(Debug)]
pub struct StringExprParseError {
    pub offset: usize,
    pub error: ParseError,
}

impl std::fmt::Display for StringExprParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "string expression parse error at offset {}: {}",
            self.offset, self.error
        )
    }
}

impl std::error::Error for StringExprParseError {}

impl<T> From<winnow::error::ParseError<T, ParseError>> for StringExprParseError {
    fn from(value: winnow::error::ParseError<T, ParseError>) -> Self {
        Self {
            offset: value.offset(),
            error: value.into_inner(),
        }
    }
}

impl<T> From<winnow::error::ParseError<T, ParseError>> for TomlParseError {
    fn from(value: winnow::error::ParseError<T, ParseError>) -> Self {
        Self::InvalidExpr(value.into())
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub context: Option<Box<Vec<ErrContext>>>,
    pub fail: Failure,
    pub offset: Offset,
}

impl ParseError {
    #[inline]
    #[must_use]
    pub fn new(offset: Offset, fail: Failure) -> Self {
        Self {
            context: None,
            fail,
            offset,
        }
    }

    #[inline]
    #[must_use]
    pub fn context(&self) -> &[ErrContext] {
        if let Some(stack) = self.context.as_deref() {
            stack
        } else {
            &[]
        }
    }

    #[inline]
    pub(crate) fn push(&mut self, entry: ErrContext) {
        if let Some(ref mut stack) = self.context {
            stack.push(entry);
        } else {
            self.context = Some(Box::new(vec![entry]));
        }
    }
}

impl DisplayError for ParseError {
    fn title(&self) -> impl std::fmt::Display {
        self
    }

    fn snippets(&self) -> impl IntoIterator<Item: DisplaySnippet> {
        self.context()
    }
}

impl DisplaySnippet for &ErrContext {
    fn annotations(&self) -> impl IntoIterator<Item: DisplayAnnotation> {
        Some(*self)
    }
}

impl DisplayAnnotation for &ErrContext {
    fn level(&self) -> annotate_snippets::Level {
        match self {
            ErrContext::Error(..) => annotate_snippets::Level::Error,
            ErrContext::WhileParsing(..) => annotate_snippets::Level::Info,
            ErrContext::Hint(..) => annotate_snippets::Level::Help,
            ErrContext::Note(..) => annotate_snippets::Level::Note,
        }
    }

    fn span(&self) -> Span {
        let offset = match **self {
            ErrContext::Error(offset, _)
            | ErrContext::Hint(offset, _)
            | ErrContext::Note(offset, _)
            | ErrContext::WhileParsing(offset, _) => offset,
        };
        Span::from_offset_and_len(offset, 0)
    }

    fn label(&self) -> impl std::fmt::Display {
        self
    }
}

impl winnow::error::ParserError<Input<'_>> for ParseError {
    fn from_error_kind(input: &Input<'_>, kind: winnow::error::ErrorKind) -> Self {
        let offset = Offset(input.location() as u32);
        Self {
            context: None,
            fail: Failure::Internal(kind),
            offset,
        }
    }

    fn append(
        self,
        _input: &Input<'_>,
        _token_start: &<Input<'_> as winnow::stream::Stream>::Checkpoint,
        _kind: winnow::error::ErrorKind,
    ) -> Self {
        self
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fail)
    }
}

impl std::error::Error for ParseError {}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Failure {
    #[error("{0}")]
    Internal(winnow::error::ErrorKind),
    /// "expected ..."
    #[error("expected {0}")]
    Expected(&'static &'static str),
    #[error("expected keyword `{0}`")]
    ExpectedKeyword(&'static &'static str),
    #[error("invalid escape sequence: {0:?}")]
    InvalidEscapeChar(char),
    #[error("expected character {0}")]
    ExpectedChar(char),
    #[error(transparent)]
    ValidRegex(Arc<regex::Error>),
    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
}

impl winnow::error::FromExternalError<Input<'_>, std::num::ParseIntError> for ParseError {
    fn from_external_error(
        input: &Input<'_>,
        _kind: winnow::error::ErrorKind,
        e: std::num::ParseIntError,
    ) -> Self {
        let location = input.location();
        ParseError {
            context: None,
            fail: Failure::ParseInt(e),
            offset: Offset(location as u32),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrContext {
    #[error("{1}")]
    Error(Offset, &'static str),
    #[error("while parsing {}", .1)]
    WhileParsing(Offset, &'static str),
    #[error("{1}")]
    Hint(Offset, &'static str),
    #[error("{1}")]
    Note(Offset, &'static str),
}

pub(crate) fn fatal<'a, O>(failure: Failure) -> impl winnow::Parser<Input<'a>, O, ParseError> {
    let mut failure = Some(failure);
    fatal_with(move || failure.take().expect("fatal parser invoked multiple times"))
}

pub(crate) fn fatal_with<'a, O, F>(with: F) -> impl winnow::Parser<Input<'a>, O, ParseError>
where
    F: FnOnce() -> Failure,
{
    let mut with = Some(with);
    move |input: &mut Input<'a>| {
        let location = input.location();
        Err(winnow::error::ErrMode::Cut(ParseError {
            context: None,
            fail: with.take().expect("fatal parser invoked multiple times")(),
            offset: Offset(location as u32),
        }))
    }
}
