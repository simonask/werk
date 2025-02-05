use std::sync::Arc;

use winnow::{error::AddContext, stream::Location};

use crate::parser::{Input, Offset, Parser, Span};

/// Owned version of `annotate_snippets::Message`.
pub trait DisplayError: std::fmt::Display {
    fn annotations(&self) -> Vec<DisplayAnnotation>;
}

/// Owned version of `annotate_snippets::Annotation`.
///
/// Note that parser errors pertain to a specific location in the input, so the
/// parser will only ever need one snippet per error.
pub struct DisplayAnnotation {
    pub level: annotate_snippets::Level,
    pub message: String,
    pub span: Span,
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
        Span::from_offset_and_len(self.offset, 0)
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
        let annotations = self.error.annotations();

        let title = self.error.to_string();
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
                            .map(|a| a.level.span(a.span.into()).label(&a.message)),
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

#[derive(Debug)]
pub struct Error {
    pub context: Option<Box<Vec<(Offset, ErrContext)>>>,
    pub fail: Failure,
    pub offset: Offset,
}

impl Error {
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
    pub fn context(&self) -> &[(Offset, ErrContext)] {
        if let Some(stack) = self.context.as_deref() {
            stack
        } else {
            &[]
        }
    }

    #[inline]
    pub(crate) fn push(&mut self, offset: Offset, entry: ErrContext) {
        if let Some(ref mut stack) = self.context {
            stack.push((offset, entry));
        } else {
            self.context = Some(Box::new(vec![(offset, entry)]));
        }
    }
}

impl DisplayError for Error {
    fn annotations(&self) -> Vec<DisplayAnnotation> {
        let mut annotations = self.context().iter().map(Into::into).collect::<Vec<_>>();
        annotations.push(DisplayAnnotation {
            level: annotate_snippets::Level::Error,
            message: self.to_string(),
            span: self.offset.into(),
        });
        annotations
    }
}

impl From<&(Offset, ErrContext)> for DisplayAnnotation {
    fn from((offset, context): &(Offset, ErrContext)) -> Self {
        use annotate_snippets::Level;
        let level = match *context {
            ErrContext::Error(_) => Level::Error,
            ErrContext::WhileParsing(_) => Level::Info,
            ErrContext::Hint(_) => Level::Help,
            ErrContext::Note(_) => Level::Note,
        };
        DisplayAnnotation {
            level,
            message: context.to_string(),
            span: Span::from_offset_and_len(*offset, 0),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fail)
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Failure {
    #[error("(no error message; this is a parser bug)")]
    Unknown,
    /// "expected ..."
    #[error("expected {0}")]
    Expected(&'static &'static str),
    #[error("expected keyword `{0}`")]
    ExpectedKeyword(&'static &'static str),
    #[error("invalid escape sequence: {0:?}")]
    InvalidEscapeChar(char),
    #[error("invalid interpolation operator")]
    InvalidInterpolationOp,
    #[error("expected character {0}")]
    ExpectedChar(char),
    #[error(transparent)]
    ValidRegex(Arc<regex::Error>),
    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
}

impl winnow::error::FromExternalError<Input<'_>, std::num::ParseIntError> for ModalErr {
    fn from_external_error(input: &Input<'_>, e: std::num::ParseIntError) -> Self {
        let offset = Offset(input.current_token_start() as u32);
        ModalErr::Backtrack(offset, Failure::ParseInt(e))
    }
}

#[derive(Clone, Copy, Debug, thiserror::Error)]
pub enum ErrContext {
    #[error("{0}")]
    Error(&'static str),
    #[error("while parsing {0}")]
    WhileParsing(&'static str),
    #[error("{0}")]
    Hint(&'static str),
    #[error("{0}")]
    Note(&'static str),
}

pub enum ModalErr {
    /// The parser could not match the input. Backtrack and try the next
    /// alternative.
    Backtrack(Offset, Failure),
    /// The parser failed with a hard error.
    Error(Error),
}

impl winnow::error::ParserError<Input<'_>> for ModalErr {
    type Inner = Error;

    #[inline]
    fn from_input(input: &Input<'_>) -> Self {
        let offset = Offset(input.current_token_start() as u32);
        ModalErr::Backtrack(offset, Failure::Unknown)
    }

    #[inline]
    fn into_inner(self) -> Result<Self::Inner, Self> {
        match self {
            ModalErr::Backtrack(offset, fail) => Ok(Error::new(offset, fail)),
            ModalErr::Error(error) => Ok(error),
        }
    }

    #[inline]
    fn assert(input: &Input<'_>, message: &'static str) -> Self {
        std::panic!("assert `{message}` failed at {input:#?}");
    }

    #[inline]
    fn incomplete(_input: &Input<'_>, _needed: winnow::error::Needed) -> Self {
        unreachable!("incomplete parsing not supported")
    }

    #[inline]
    fn or(self, other: Self) -> Self {
        match (self, other) {
            (ModalErr::Backtrack(lhs, lhs_fail), ModalErr::Backtrack(rhs, rhs_fail)) => {
                if lhs < rhs {
                    ModalErr::Backtrack(rhs, rhs_fail)
                } else {
                    ModalErr::Backtrack(lhs, lhs_fail)
                }
            }
            (ModalErr::Error(lhs), ModalErr::Error(rhs)) => ModalErr::Error(lhs.or(rhs)),
            (ModalErr::Error(error), ModalErr::Backtrack(..))
            | (ModalErr::Backtrack(..), ModalErr::Error(error)) => ModalErr::Error(error),
        }
    }

    #[inline]
    fn is_backtrack(&self) -> bool {
        matches!(self, Self::Backtrack(..))
    }

    #[inline]
    fn is_incomplete(&self) -> bool {
        false
    }

    #[inline]
    fn needed(&self) -> Option<winnow::error::Needed> {
        None
    }
}

impl winnow::error::ModalError for ModalErr {
    #[inline]
    fn cut(self) -> Self {
        match self {
            ModalErr::Backtrack(offset, fail) => ModalErr::Error(Error::new(offset, fail)),
            ModalErr::Error(_) => self,
        }
    }

    #[inline]
    fn backtrack(self) -> Self {
        match self {
            ModalErr::Backtrack(..) => self,
            ModalErr::Error(error) => ModalErr::Backtrack(error.offset, error.fail),
        }
    }
}

impl winnow::error::ParserError<Input<'_>> for Error {
    type Inner = Self;

    #[inline]
    fn from_input(input: &Input<'_>) -> Self {
        Error {
            context: None,
            fail: Failure::Unknown,
            offset: Offset(input.current_token_start() as _),
        }
    }

    #[inline]
    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }

    #[inline]
    fn is_backtrack(&self) -> bool {
        false
    }

    #[inline]
    fn assert(input: &Input<'_>, message: &'static str) -> Self {
        std::panic!("assert `{message}` failed at {input:#?}");
    }

    #[inline]
    fn incomplete(input: &Input<'_>, _needed: winnow::error::Needed) -> Self {
        unreachable!("incomplete parsing not supported at {input:#?}");
    }

    #[inline]
    fn or(self, other: Self) -> Self {
        // Choose the error that made it furthest.
        if self.offset < other.offset {
            other
        } else {
            self
        }
    }

    #[inline]
    fn is_incomplete(&self) -> bool {
        false
    }

    #[inline]
    fn needed(&self) -> Option<winnow::error::Needed> {
        None
    }
}

impl<'a> AddContext<Input<'a>, ErrContext> for Error {
    fn add_context(
        mut self,
        input: &Input<'a>,
        _token_start: &<Input<'a> as winnow::stream::Stream>::Checkpoint,
        context: ErrContext,
    ) -> Self {
        let offset = Offset(input.current_token_start() as u32);
        self.push(offset, context);
        self
    }
}

impl<'a> AddContext<Input<'a>, ErrContext> for ModalErr {
    fn add_context(
        mut self,
        input: &Input<'a>,
        token_start: &<Input<'a> as winnow::stream::Stream>::Checkpoint,
        context: ErrContext,
    ) -> Self {
        use winnow::stream::Offset;
        match self {
            ModalErr::Backtrack(..) => self,
            ModalErr::Error(ref mut error) => {
                let start = input.offset_from(token_start);
                let location = input.current_token_start() - start;
                error.push(Offset(location as u32), context);
                self
            }
        }
    }
}

pub(crate) fn fatal<'a, O>(failure: Failure) -> impl Parser<'a, O> {
    winnow::combinator::cut_err(winnow::combinator::fail.or_fail(failure))
}
