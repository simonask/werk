use std::{borrow::Cow, hash::Hash as _};

use crate::{
    hash_is_semantic,
    parser::{Span, Spanned},
    SemanticHash,
};

mod expr;
pub mod keyword;
mod string;
pub mod token;

pub use expr::*;
pub use string::*;
use werk_util::Symbol;

/// Whitespace and comments within statements and expressions (not doc
/// comments).
#[derive(Default, PartialEq, Clone, Copy)]
#[must_use]
pub struct Whitespace(pub Span);

impl std::fmt::Debug for Whitespace {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Trailing whitespace and comments in a block after each statement.
#[derive(Default, Clone, Copy, Debug)]
#[must_use]
pub struct Trailing<T> {
    /// Whitespace before comma or semicolon.
    pub ws: Whitespace,
    /// Comma or semicolon at the end of the item.
    pub token: Option<T>,
}

impl<const CHAR: char> PartialEq for Trailing<token::Token<CHAR>> {
    fn eq(&self, other: &Self) -> bool {
        self.ws == other.ws
            && match (self.token, other.token) {
                (None, None) => true,
                // If the left-hand side does not have a token, but the
                // whitespace is ignored, consider self.token as ignored also.
                (None, Some(rhs)) => self.ws.0.is_ignored() || rhs.0.is_ignored(),
                // If the right-hand side does not have a token, but the
                // whitespace is ignored, consider other.token as ignored also.
                (Some(lhs), None) => other.ws.0.is_ignored() || lhs.0.is_ignored(),
                (Some(lhs), Some(rhs)) => lhs == rhs,
            }
    }
}

#[inline]
pub fn ws(span: std::ops::Range<u32>) -> Whitespace {
    Whitespace(crate::parser::span(span))
}

#[inline]
pub const fn ws_ignore() -> Whitespace {
    Whitespace(Span::ignore())
}

#[must_use]
pub fn kw_ignore<K: keyword::Keyword>() -> K {
    K::ignore()
}

#[inline]
#[must_use]
pub fn token_ignore<const CHAR: char>() -> token::Token<CHAR> {
    token::Token::ignore()
}

#[inline]
pub const fn trailing_ignore<const CHAR: char>() -> Trailing<token::Token<CHAR>> {
    Trailing {
        ws: ws_ignore(),
        token: None,
    }
}

#[derive(Debug, PartialEq, Default, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct Root<'a> {
    pub statements: Vec<BodyStmt<RootStmt<'a>>>,
    /// Comment at the end of the document, not associated with any item.
    #[serde(skip, default)]
    pub ws_trailing: Whitespace,
}

impl Root<'_> {
    #[must_use]
    pub fn find_global(&self, name: &str) -> Option<&LetStmt> {
        self.statements.iter().find_map(|stmt| match stmt {
            BodyStmt {
                statement: RootStmt::Let(stmt),
                ..
            } if stmt.ident.ident == name => Some(stmt),
            _ => None,
        })
    }

    #[must_use]
    pub fn find_command(&self, name: &str) -> Option<&CommandRecipe> {
        self.statements.iter().find_map(|stmt| match stmt {
            BodyStmt {
                statement: RootStmt::Task(stmt),
                ..
            } if stmt.name.ident == name => Some(stmt),
            _ => None,
        })
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum RootStmt<'a> {
    Config(ConfigStmt<'a>),
    Let(LetStmt<'a>),
    Task(CommandRecipe<'a>),
    Build(BuildRecipe<'a>),
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ConfigStmt<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_config: keyword::Config,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub ident: Ident,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_eq: token::Eq,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub value: ConfigValue<'a>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum ConfigValue<'a> {
    String(ConfigString<'a>),
    Bool(ConfigBool),
}

impl Spanned for ConfigValue<'_> {
    fn span(&self) -> Span {
        match self {
            ConfigValue::String(s) => s.0,
            ConfigValue::Bool(b) => b.0,
        }
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ConfigString<'a>(#[serde(skip, default)] pub Span, pub Cow<'a, str>);

#[derive(Debug, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ConfigBool(#[serde(skip, default)] pub Span, pub bool);

#[derive(Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct Ident {
    #[serde(skip, default)]
    pub span: Span,
    pub ident: Symbol,
}

impl Ident {
    pub fn new(span: impl Into<Span>, ident: impl Into<Symbol>) -> Self {
        Self {
            span: span.into(),
            ident: ident.into(),
        }
    }
}

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({:?}, {:?})", self.ident, self.span)
    }
}

impl std::fmt::Display for Ident {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.ident.as_str())
    }
}

impl SemanticHash for Ident {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.as_str().hash(state);
    }
}

impl PartialEq<Symbol> for Ident {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        self.ident == *other
    }
}

impl PartialEq<str> for Ident {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.ident == other
    }
}

impl PartialEq<&str> for Ident {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.ident == *other
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MessageType {
    Info,
    Warning,
}

hash_is_semantic!(MessageType);

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CommandRecipe<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_task: keyword::Task,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub name: Ident,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub body: Body<TaskRecipeStmt<'a>>,
}

impl SemanticHash for CommandRecipe<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.semantic_hash(state);
        self.body.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct BuildRecipe<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_build: keyword::Build,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub pattern: PatternExpr<'a>,
    /// Comment between the pattern and the opening brace.
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub body: Body<BuildRecipeStmt<'a>>,
}

impl SemanticHash for BuildRecipe<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pattern.semantic_hash(state);
        self.body.semantic_hash(state);
    }
}

/// A `{...}` block.
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct Body<T> {
    #[serde(skip, default)]
    pub token_open: token::BraceOpen,
    pub statements: Vec<BodyStmt<T>>,
    /// After the last statement.
    #[serde(skip, default)]
    pub ws_trailing: Whitespace,
    #[serde(skip, default)]
    pub token_close: token::BraceClose,
}

impl<T> Spanned for Body<T> {
    fn span(&self) -> Span {
        self.token_open.span().merge(self.token_close.span())
    }
}

impl<T: SemanticHash> SemanticHash for Body<T> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.statements.as_slice().semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct BodyStmt<T> {
    #[serde(skip, default)]
    pub ws_pre: Whitespace,
    pub statement: T,
    #[serde(skip, default)]
    pub trailing: Trailing<token::Semicolon>,
}

impl<T: SemanticHash> SemanticHash for BodyStmt<T> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.statement.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum BuildRecipeStmt<'a> {
    Let(LetStmt<'a>),
    From(FromStmt<'a>),
    Depfile(DepfileStmt<'a>),
    Run(RunStmt<'a>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    SetCapture(KwExpr<keyword::SetCapture, ConfigBool>),
    SetNoCapture(KwExpr<keyword::SetNoCapture, ConfigBool>),
    Env(EnvStmt<'a>),
    EnvRemove(EnvRemoveStmt<'a>),
}

impl SemanticHash for BuildRecipeStmt<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            BuildRecipeStmt::Let(stmt) => stmt.semantic_hash(state),
            BuildRecipeStmt::From(stmt) => stmt.semantic_hash(state),
            BuildRecipeStmt::Depfile(stmt) => stmt.semantic_hash(state),
            BuildRecipeStmt::Run(stmt) => stmt.semantic_hash(state),
            BuildRecipeStmt::Env(stmt) => stmt.semantic_hash(state),
            BuildRecipeStmt::EnvRemove(stmt) => stmt.semantic_hash(state),
            // Information statements do not contribute to outdatedness.
            BuildRecipeStmt::SetCapture(_)
            | BuildRecipeStmt::SetNoCapture(_)
            | BuildRecipeStmt::Info(_)
            | BuildRecipeStmt::Warn(_) => {}
        }
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum TaskRecipeStmt<'a> {
    Let(LetStmt<'a>),
    Build(BuildStmt<'a>),
    Run(RunStmt<'a>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    SetCapture(KwExpr<keyword::SetCapture, ConfigBool>),
    SetNoCapture(KwExpr<keyword::SetNoCapture, ConfigBool>),
    Env(EnvStmt<'a>),
    EnvRemove(EnvRemoveStmt<'a>),
}

impl SemanticHash for TaskRecipeStmt<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            TaskRecipeStmt::Let(stmt) => stmt.semantic_hash(state),
            TaskRecipeStmt::Build(stmt) => stmt.semantic_hash(state),
            TaskRecipeStmt::Run(stmt) => stmt.semantic_hash(state),
            TaskRecipeStmt::Env(stmt) => stmt.semantic_hash(state),
            TaskRecipeStmt::EnvRemove(stmt) => stmt.semantic_hash(state),
            // Information statements do not contribute to outdatedness.
            TaskRecipeStmt::SetCapture(_)
            | TaskRecipeStmt::SetNoCapture(_)
            | TaskRecipeStmt::Info(_)
            | TaskRecipeStmt::Warn(_) => {}
        }
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct LetStmt<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_let: keyword::Let,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub ident: Ident,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_eq: token::Eq,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    #[serde(flatten)]
    pub value: ExprChain<'a>,
}

impl SemanticHash for LetStmt<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct EnvStmt<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token: keyword::Env,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub key: StringExpr<'a>,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_eq: token::Eq,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub value: StringExpr<'a>,
}

impl SemanticHash for EnvStmt<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

pub type FromStmt<'a> = KwExpr<keyword::From, ExprChain<'a>>;
pub type BuildStmt<'a> = KwExpr<keyword::Build, ExprChain<'a>>;
pub type DepfileStmt<'a> = KwExpr<keyword::Depfile, ExprChain<'a>>;
pub type RunStmt<'a> = KwExpr<keyword::Run, RunExpr<'a>>;
pub type ErrorStmt<'a> = KwExpr<keyword::Error, StringExpr<'a>>;
pub type DeleteExpr<'a> = KwExpr<keyword::Delete, Expr<'a>>;
pub type EnvRemoveStmt<'a> = KwExpr<keyword::RemoveEnv, StringExpr<'a>>;

/// Things that can appear in the `command` part of recipes.
#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum RunExpr<'a> {
    /// Run shell command.
    Shell(ShellExpr<'a>),
    /// Write the result of the expression to the path. The string is an OS path.
    Write(WriteExpr<'a>),
    /// Copy one file to another.
    Copy(CopyExpr<'a>),
    /// Delete a file.
    Delete(DeleteExpr<'a>),
    /// Set an environment variable.
    Env(EnvStmt<'a>),
    /// Remove an environment variable.
    EnvRemove(EnvRemoveStmt<'a>),
    /// Print a message while running the command.
    Info(InfoExpr<'a>),
    /// Print a warning while running the command.
    Warn(WarnExpr<'a>),
    /// List of run expressions.
    List(ListExpr<RunExpr<'a>>),
    /// A `{...}` block.
    Block(Body<RunExpr<'a>>),
}

impl Spanned for RunExpr<'_> {
    fn span(&self) -> Span {
        match self {
            RunExpr::Shell(expr) => expr.span,
            RunExpr::Write(expr) => expr.span,
            RunExpr::Copy(expr) => expr.span,
            RunExpr::Delete(expr) => expr.span,
            RunExpr::Env(expr) => expr.span,
            RunExpr::EnvRemove(expr) => expr.span,
            RunExpr::Info(expr) => expr.span,
            RunExpr::Warn(expr) => expr.span,
            RunExpr::List(list) => list.span,
            RunExpr::Block(block) => block.span(),
        }
    }
}

impl SemanticHash for RunExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            RunExpr::Shell(expr) => expr.semantic_hash(state),
            RunExpr::Write(expr) => expr.semantic_hash(state),
            RunExpr::Copy(expr) => expr.semantic_hash(state),
            RunExpr::Delete(expr) => expr.semantic_hash(state),
            RunExpr::Env(expr) => expr.semantic_hash(state),
            RunExpr::EnvRemove(expr) => expr.semantic_hash(state),
            // Messages don't contribute to outdatedness.
            RunExpr::Info(_) | RunExpr::Warn(_) => (),
            RunExpr::List(expr) => expr.semantic_hash(state),
            RunExpr::Block(block) => block.semantic_hash(state),
        }
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CopyExpr<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_copy: keyword::Copy,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub src: StringExpr<'a>,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_to: keyword::To,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub dest: StringExpr<'a>,
}

impl SemanticHash for CopyExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.src.semantic_hash(state);
        self.dest.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct WriteExpr<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_write: keyword::Write,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub value: Expr<'a>,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_to: keyword::To,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub path: Expr<'a>,
}

impl SemanticHash for WriteExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}
