use std::sync::Arc;

use winnow::{
    error::{AddContext, FromExternalError},
    stream::Stream,
};

use crate::parse_toml::{ExprType, RunExprType};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Toml(#[from] toml_edit::TomlError),
    #[error("unknown root key: {0}")]
    InvalidKey(String),
    #[error("expected table: {0}")]
    ExpectedTable(String),
    #[error("expected string: {0}")]
    ExpectedString(String),
    #[error("expected boolean: {0}")]
    ExpectedBool(String),
    #[error("expected string or table: {0}")]
    ExpectedStringOrTable(String),
    #[error("expected string or array: {0}")]
    ExpectedStringOrArray(String),
    #[error("expected integer: {0}")]
    ExpectedInteger(String),
    #[error("expected key '{1}' in table expression: {0}")]
    ExpectedKey(String, String),
    #[error("expression table contain a root expression, one of: {}", ExprType::all_strs().join(", "))]
    ExpectedMainExpression,
    #[error("expression table can only contain one root expression, found: {0} and {1}")]
    AmbiguousMainExpression(ExprType, ExprType),
    #[error("expression table can only contain one root expression, found: {0} and {1}")]
    AmbiguousRunExpression(RunExprType, RunExprType),
    #[error("unknown chaining expression: {0}")]
    UnknownExpressionChain(String),
    #[error("invalid identifier '{0}': {1}")]
    InvalidIdent(String, ParseError),
    #[error("invalid string expression '{0}': {1}")]
    InvalidStringExpr(String, ParseError),
    #[error("invalid pattern expression '{0}': {1}")]
    InvalidPatternExpr(String, ParseError),
    #[error("duplicate pattern expression in match: {0}")]
    DuplicatePatternExpr(String),
    #[error("unknown config key: {0}")]
    UnknownConfigKey(String),
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
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
    pub error: ContextError,
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

impl<T> From<winnow::error::ParseError<T, ContextError>> for StringExprParseError {
    fn from(value: winnow::error::ParseError<T, ContextError>) -> Self {
        Self {
            offset: value.offset(),
            error: value.into_inner(),
        }
    }
}

impl<T> From<winnow::error::ParseError<T, ContextError>> for ParseError {
    fn from(value: winnow::error::ParseError<T, ContextError>) -> Self {
        Self::InvalidExpr(value.into())
    }
}

#[derive(Debug)]
pub struct ContextError {
    pub stack: Vec<&'static str>,
    pub expected: Expected,
}

impl<I: winnow::stream::Stream> winnow::error::ParserError<I> for ContextError {
    fn from_error_kind(_input: &I, kind: winnow::error::ErrorKind) -> Self {
        Self {
            stack: Vec::new(),
            expected: Expected::Internal(kind),
        }
    }

    fn append(
        self,
        _input: &I,
        _token_start: &I::Checkpoint,
        _kind: winnow::error::ErrorKind,
    ) -> Self {
        self
    }
}

impl<I: winnow::stream::Stream> AddContext<I, Expected> for ContextError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        context: Expected,
    ) -> Self {
        self.expected = context;
        self
    }
}

impl<I: winnow::stream::Stream> AddContext<I, &'static str> for ContextError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        context: &'static str,
    ) -> Self {
        self.stack.push(context);
        self
    }
}

impl<I: winnow::stream::Stream, E: std::error::Error + Send + Sync + 'static>
    FromExternalError<I, E> for ContextError
{
    fn from_external_error(_input: &I, _kind: winnow::error::ErrorKind, e: E) -> Self {
        ContextError {
            expected: Expected::External(Arc::new(e)),
            stack: Vec::new(),
        }
    }
}

impl std::fmt::Display for ContextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expected)?;
        for entry in &self.stack {
            write!(f, "\n    {entry}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Expected {
    #[error("{0}")]
    Internal(winnow::error::ErrorKind),
    #[error("expected {0}")]
    Expected(&'static &'static str),
    #[error("expected character {0}")]
    ExpectedChar(char),
    #[error(transparent)]
    ValidRegex(Arc<regex::Error>),
    #[error(transparent)]
    External(Arc<dyn std::error::Error + Send + Sync>),
}
