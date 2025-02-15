use std::sync::Arc;

use werk_util::{AnnotateLevelExt, DiagnosticFileId, Level};
use winnow::{error::AddContext, stream::Location};

use crate::parser::{Input, Offset, Parser, Span};

impl Error {
    #[must_use]
    pub fn span(&self) -> Span {
        Span::from_offset_and_len(self.offset, 0)
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

impl werk_util::AsDiagnostic for Error {
    fn as_diagnostic(&self) -> werk_util::Diagnostic {
        let level = Level::Error;
        let file_id = DiagnosticFileId::default(); // TODO

        let context = self
            .context
            .as_ref()
            .map(|context| {
                context.iter().filter_map(|(offset, c)| match c {
                    ErrContext::WhileParsing(thing) => Some(werk_util::DiagnosticAnnotation {
                        span: offset.0 as usize..offset.0 as usize,
                        level: Level::Info,
                        message: format!("while parsing {thing}"),
                    }),
                    ErrContext::Note(note) => Some(werk_util::DiagnosticAnnotation {
                        span: offset.0 as usize..offset.0 as usize,
                        level: Level::Note,
                        message: (*note).to_string(),
                    }),
                    _ => None,
                })
            })
            .into_iter()
            .flatten();

        let hints = self
            .context
            .as_ref()
            .map(|context| {
                context.iter().filter_map(|c| {
                    if let ErrContext::Hint(h) = c.1 {
                        Some(h.to_string())
                    } else {
                        None
                    }
                })
            })
            .into_iter()
            .flatten();

        match self.fail {
            Failure::Unknown => {
                level
                    .diagnostic("P9999")
                    .title("parse error")
                    .snippet(file_id.snippet(context.chain(Some(
                        level.annotation(self.span(), "no error message; this is a parser bug"),
                    ))))
            }
            Failure::Expected(expected) => {
                level
                    .diagnostic("P1001")
                    .title("parse error")
                    .snippet(file_id.snippet(context.chain(Some(
                        level.annotation(self.span(), format_args!("expected {expected}")),
                    ))))
            }
            Failure::ExpectedKeyword(expected) => level
                .diagnostic("P1002")
                .title("parse error")
                .snippet(file_id.snippet(context.chain(Some(
                    level.annotation(self.span(), format_args!("expected keyword `{expected}`")),
                )))),
            Failure::InvalidEscapeChar(ch) => level
                .diagnostic("P1003")
                .title("parse error")
                .snippet(file_id.snippet(context.chain(Some(
                    level.annotation(self.span(), format_args!("invalid escape sequence: '{ch}'")),
                )))),
            Failure::InvalidInterpolationOp => level
                .diagnostic("P1004")
                .title("parse error")
                .snippet(file_id.snippet(context.chain(Some(
                    level.annotation(self.span(), "invalid interpolation operator"),
                )))),
            Failure::ExpectedChar(ch) => {
                level
                    .diagnostic("P1005")
                    .title("parse error")
                    .snippet(file_id.snippet(context.chain(Some(
                        level.annotation(self.span(), format_args!("expected character {ch}")),
                    ))))
            }
            Failure::ValidRegex(ref error) => level
                .diagnostic("P0100")
                .title("parse error")
                .snippet(file_id.snippet(context.chain(Some(
                    level.annotation(self.span(), format_args!("invalid regex pattern: {error}")),
                )))),
            Failure::ParseInt(ref parse_int_error) => level
                .diagnostic("P0101")
                .title("parse error")
                .snippet(file_id.snippet(context.chain(Some(level.annotation(
                    self.span(),
                    format_args!("invalid integer: {parse_int_error}"),
                ))))),
        }
        .footers(hints)
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

#[derive(Debug)]
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
