use std::{borrow::Cow, hash::Hash as _};

use crate::{
    hash_is_semantic,
    parser::{Span, Spanned},
    SemanticHash,
};

mod expr;
mod string;
pub mod token;

pub use expr::*;
pub use string::*;

/// Whitespace and comments within statements and expressions (not doc
/// comments).
#[derive(Default, PartialEq, Clone, Copy, Debug)]
#[must_use]
pub struct Whitespace(pub Span);

#[inline]
pub fn ws(span: std::ops::Range<u32>) -> Whitespace {
    Whitespace(crate::parser::span(span))
}

#[inline]
pub const fn ws_ignore() -> Whitespace {
    Whitespace(Span::ignore())
}

#[must_use]
pub fn kw_ignore<K: token::Keyword>() -> K {
    K::ignore()
}

#[inline]
#[must_use]
pub fn token_ignore<const CHAR: char>() -> token::Token<CHAR> {
    token::Token::ignore()
}

#[derive(Debug, PartialEq, Default)]
pub struct Root<'a> {
    pub statements: Vec<BodyStmt<RootStmt<'a>>>,
    /// Comment at the end of the document, not associated with any item.
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

#[derive(Debug, PartialEq)]
pub enum RootStmt<'a> {
    Config(ConfigStmt<'a>),
    Let(LetStmt<'a>),
    Task(CommandRecipe<'a>),
    Build(BuildRecipe<'a>),
}

#[derive(Debug, PartialEq)]
pub struct ConfigStmt<'a> {
    pub span: Span,
    pub token_config: token::Config,
    pub ws_1: Whitespace,
    pub ident: Ident<'a>,
    pub ws_2: Whitespace,
    pub token_eq: token::Eq,
    pub ws_3: Whitespace,
    pub value: ConfigValue<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConfigValue<'a> {
    String(ConfigString<'a>),
    Bool(ConfigBool),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConfigString<'a>(pub Span, pub Cow<'a, str>);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ConfigBool(pub Span, pub bool);

#[derive(Clone, PartialEq)]
pub struct Ident<'a> {
    pub span: Span,
    pub ident: &'a str,
}

impl<'a> Ident<'a> {
    pub fn new(span: impl Into<Span>, ident: &'a str) -> Self {
        Self {
            span: span.into(),
            ident,
        }
    }
}

impl std::fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({:?}, {:?})", self.ident, self.span)
    }
}

impl std::fmt::Display for Ident<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.ident)
    }
}

impl SemanticHash for Ident<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
    }
}

impl PartialEq<str> for Ident<'_> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.ident == other
    }
}

impl PartialEq<&str> for Ident<'_> {
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

#[derive(Debug, PartialEq)]
pub struct CommandRecipe<'a> {
    pub span: Span,
    pub token_task: token::Task,
    pub ws_1: Whitespace,
    pub name: Ident<'a>,
    pub ws_2: Whitespace,
    pub body: Body<TaskRecipeStmt<'a>>,
}

impl SemanticHash for CommandRecipe<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.semantic_hash(state);
        self.body.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct BuildRecipe<'a> {
    pub span: Span,
    pub token_build: token::Build,
    pub ws_1: Whitespace,
    pub pattern: PatternExpr<'a>,
    /// Comment between the pattern and the opening brace.
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
#[derive(Debug, PartialEq, Clone)]
pub struct Body<T> {
    pub token_open: token::BraceOpen,
    pub statements: Vec<BodyStmt<T>>,
    /// After the last statement.
    pub ws_trailing: Whitespace,
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

#[derive(Debug, PartialEq, Clone)]
pub struct BodyStmt<T> {
    pub ws_pre: Whitespace,
    pub statement: T,
    pub ws_trailing: Option<(Whitespace, token::Semicolon)>,
}

impl<T: SemanticHash> SemanticHash for BodyStmt<T> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.statement.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub enum BuildRecipeStmt<'a> {
    Let(LetStmt<'a>),
    From(FromStmt<'a>),
    Depfile(DepfileStmt<'a>),
    Run(RunStmt<'a>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    SetCapture(KwExpr<token::SetCapture, ConfigBool>),
    SetNoCapture(KwExpr<token::SetNoCapture, ConfigBool>),
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

#[derive(Debug, PartialEq)]
pub enum TaskRecipeStmt<'a> {
    Let(LetStmt<'a>),
    Build(BuildStmt<'a>),
    Run(RunStmt<'a>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    SetCapture(KwExpr<token::SetCapture, ConfigBool>),
    SetNoCapture(KwExpr<token::SetNoCapture, ConfigBool>),
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

#[derive(Debug, PartialEq)]
pub struct LetStmt<'a> {
    pub span: Span,
    pub token_let: token::Let,
    pub ws_1: Whitespace,
    pub ident: Ident<'a>,
    pub ws_2: Whitespace,
    pub token_eq: token::Eq,
    pub ws_3: Whitespace,
    pub value: Expr<'a>,
}

impl SemanticHash for LetStmt<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct EnvStmt<'a> {
    pub span: Span,
    pub token: token::Env,
    pub ws_1: Whitespace,
    pub key: StringExpr<'a>,
    pub ws_2: Whitespace,
    pub token_eq: token::Eq,
    pub ws_3: Whitespace,
    pub value: StringExpr<'a>,
}

impl SemanticHash for EnvStmt<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

pub type FromStmt<'a> = KwExpr<token::From, Expr<'a>>;
pub type BuildStmt<'a> = KwExpr<token::Build, Expr<'a>>;
pub type DepfileStmt<'a> = KwExpr<token::Depfile, Expr<'a>>;
pub type RunStmt<'a> = KwExpr<token::Run, RunExpr<'a>>;
pub type ErrorStmt<'a> = KwExpr<token::Error, StringExpr<'a>>;
pub type DeleteExpr<'a> = KwExpr<token::Delete, Expr<'a>>;
pub type EnvRemoveStmt<'a> = KwExpr<token::RemoveEnv, StringExpr<'a>>;

/// Things that can appear in the `command` part of recipes.
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct CopyExpr<'a> {
    pub span: Span,
    pub token_copy: token::Copy,
    pub ws_1: Whitespace,
    pub src: StringExpr<'a>,
    pub ws_2: Whitespace,
    pub token_to: token::To,
    pub ws_3: Whitespace,
    pub dest: StringExpr<'a>,
}

impl SemanticHash for CopyExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.src.semantic_hash(state);
        self.dest.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct WriteExpr<'a> {
    pub span: Span,
    pub token_write: token::Write,
    pub ws_1: Whitespace,
    pub value: Expr<'a>,
    pub ws_2: Whitespace,
    pub token_to: token::To,
    pub ws_3: Whitespace,
    pub path: Expr<'a>,
}

impl SemanticHash for WriteExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}
