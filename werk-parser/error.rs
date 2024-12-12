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
    #[error("invalid expression: {0}")]
    InvalidExpr(winnow::error::ContextError),
}
