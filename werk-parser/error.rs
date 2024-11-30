#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Toml(#[from] toml_edit::TomlError),
    #[error("unknown root key: {0}")]
    InvalidKey(String),
    #[error("expected table: {0}")]
    ExpectedTable(String),
    #[error("expected table containing 'shell', 'glob', or 'string': {0}")]
    InvalidExprTable(String),
    #[error("expected string: {0}")]
    ExpectedString(String),
    #[error("expected string or table: {0}")]
    ExpectedStringOrTable(String),
    #[error("expected string or array: {0}")]
    ExpectedStringOrArray(String),
    #[error("invalid identifier '{0}': {1}")]
    InvalidIdent(String, ParseError),
    #[error("invalid string expression '{0}': {1}")]
    InvalidStringExpr(String, ParseError),
    #[error("invalid pattern expression '{0}': {1}")]
    InvalidPatternExpr(String, ParseError),
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
