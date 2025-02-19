use std::hash::Hash as _;

use werk_util::{hash_is_semantic, SemanticHash, Span, Spanned};

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
    Whitespace(werk_util::span(span))
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
pub struct Root {
    pub statements: Vec<BodyStmt<RootStmt>>,
    /// Comment at the end of the document, not associated with any item.
    #[serde(skip, default)]
    pub ws_trailing: Whitespace,
}

impl Root {
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
    pub fn find_command(&self, name: &str) -> Option<&TaskRecipe> {
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
pub enum RootStmt {
    Default(DefaultStmt),
    Config(ConfigStmt),
    Let(LetStmt),
    Task(TaskRecipe),
    Build(BuildRecipe),
    Include(IncludeStmt),
}

pub type IncludeStmt = KwExpr<keyword::Include, ExprChain>;

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum DefaultStmt {
    Target(DefaultStmtEntry<keyword::Target, StringExpr>),
    OutDir(DefaultStmtEntry<keyword::OutDir, ConfigString>),
    PrintCommands(DefaultStmtEntry<keyword::PrintCommands, ConfigBool>),
    PrintFresh(DefaultStmtEntry<keyword::PrintFresh, ConfigBool>),
    Quiet(DefaultStmtEntry<keyword::Quiet, ConfigBool>),
    Loud(DefaultStmtEntry<keyword::Loud, ConfigBool>),
    Explain(DefaultStmtEntry<keyword::Explain, ConfigBool>),
    Verbose(DefaultStmtEntry<keyword::Verbose, ConfigBool>),
    WatchDelay(DefaultStmtEntry<keyword::WatchDelay, ConfigInt>),
    Jobs(DefaultStmtEntry<keyword::Jobs, ConfigInt>),
    Edition(DefaultStmtEntry<keyword::Edition, ConfigString>),
}

impl Spanned for DefaultStmt {
    #[inline]
    fn span(&self) -> Span {
        match self {
            DefaultStmt::Target(stmt) => stmt.span,
            DefaultStmt::OutDir(stmt) => stmt.span,
            DefaultStmt::PrintCommands(stmt) => stmt.span,
            DefaultStmt::PrintFresh(stmt) => stmt.span,
            DefaultStmt::Quiet(stmt) => stmt.span,
            DefaultStmt::Loud(stmt) => stmt.span,
            DefaultStmt::Explain(stmt) => stmt.span,
            DefaultStmt::Verbose(stmt) => stmt.span,
            DefaultStmt::WatchDelay(stmt) => stmt.span,
            DefaultStmt::Jobs(stmt) => stmt.span,
            DefaultStmt::Edition(stmt) => stmt.span,
        }
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct DefaultStmtEntry<K, V> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token: keyword::Default,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    #[serde(skip, default)]
    pub key: K,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_eq: token::Eq,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub value: V,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum ConfigValue {
    String(ConfigString),
    Bool(ConfigBool),
}

impl Spanned for ConfigValue {
    fn span(&self) -> Span {
        match self {
            ConfigValue::String(s) => s.0,
            ConfigValue::Bool(b) => b.0,
        }
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ConfigString(#[serde(skip, default)] pub Span, pub String);
impl Spanned for ConfigString {
    #[inline]
    fn span(&self) -> Span {
        self.0
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ConfigInt(#[serde(skip, default)] pub Span, pub i32);
impl Spanned for ConfigInt {
    #[inline]
    fn span(&self) -> Span {
        self.0
    }
}
impl SemanticHash for ConfigInt {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

#[derive(Debug, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ConfigBool(#[serde(skip, default)] pub Span, pub bool);
impl Spanned for ConfigBool {
    #[inline]
    fn span(&self) -> Span {
        self.0
    }
}

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
pub struct TaskRecipe {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_task: keyword::Task,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub name: Ident,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub body: Body<TaskRecipeStmt>,
}

impl SemanticHash for TaskRecipe {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.semantic_hash(state);
        self.body.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct BuildRecipe {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_build: keyword::Build,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub pattern: PatternExpr,
    /// Comment between the pattern and the opening brace.
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub body: Body<BuildRecipeStmt>,
}

impl SemanticHash for BuildRecipe {
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
pub enum BuildRecipeStmt {
    Let(LetStmt),
    From(FromStmt),
    Depfile(DepfileStmt),
    Run(RunStmt),
    Info(InfoExpr),
    Warn(WarnExpr),
    SetCapture(KwExpr<keyword::SetCapture, ConfigBool>),
    SetNoCapture(KwExpr<keyword::SetNoCapture, ConfigBool>),
    Env(EnvStmt),
    EnvRemove(EnvRemoveStmt),
}

impl SemanticHash for BuildRecipeStmt {
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
pub enum TaskRecipeStmt {
    Let(LetStmt),
    Build(BuildStmt),
    Run(RunStmt),
    Info(InfoExpr),
    Warn(WarnExpr),
    SetCapture(KwExpr<keyword::SetCapture, ConfigBool>),
    SetNoCapture(KwExpr<keyword::SetNoCapture, ConfigBool>),
    Env(EnvStmt),
    EnvRemove(EnvRemoveStmt),
}

impl SemanticHash for TaskRecipeStmt {
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
pub struct LetStmt {
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
    pub value: ExprChain,
}

impl SemanticHash for LetStmt {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ConfigStmt {
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
    #[serde(flatten)]
    pub value: ExprChain,
}

impl SemanticHash for ConfigStmt {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct EnvStmt {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token: keyword::Env,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub key: StringExpr,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_eq: token::Eq,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub value: StringExpr,
}

impl SemanticHash for EnvStmt {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}

pub type FromStmt = KwExpr<keyword::From, ExprChain>;
pub type BuildStmt = KwExpr<keyword::Build, ExprChain>;
pub type DepfileStmt = KwExpr<keyword::Depfile, ExprChain>;
pub type RunStmt = KwExpr<keyword::Run, RunExpr>;
pub type ErrorStmt = KwExpr<keyword::Error, StringExpr>;
pub type DeleteExpr = KwExpr<keyword::Delete, Expr>;
pub type TouchExpr = KwExpr<keyword::Touch, Expr>;
pub type EnvRemoveStmt = KwExpr<keyword::RemoveEnv, StringExpr>;

/// Things that can appear in the `command` part of recipes.
#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum RunExpr {
    /// Run shell command.
    Shell(ShellExpr),
    /// Write the result of the expression to the path. The string is an OS path.
    Write(WriteExpr),
    /// Copy one file to another.
    Copy(CopyExpr),
    /// Delete a file.
    Delete(DeleteExpr),
    /// Touch a file.
    Touch(TouchExpr),
    /// Set an environment variable.
    Env(EnvStmt),
    /// Remove an environment variable.
    EnvRemove(EnvRemoveStmt),
    /// Print a message while running the command.
    Info(InfoExpr),
    /// Print a warning while running the command.
    Warn(WarnExpr),
    /// List of run expressions.
    List(ListExpr<RunExpr>),
    /// A `{...}` block.
    Block(Body<RunExpr>),
}

impl Spanned for RunExpr {
    fn span(&self) -> Span {
        match self {
            RunExpr::Shell(expr) => expr.span,
            RunExpr::Write(expr) => expr.span,
            RunExpr::Copy(expr) => expr.span,
            RunExpr::Delete(expr) => expr.span,
            RunExpr::Touch(expr) => expr.span,
            RunExpr::Env(expr) => expr.span,
            RunExpr::EnvRemove(expr) => expr.span,
            RunExpr::Info(expr) => expr.span,
            RunExpr::Warn(expr) => expr.span,
            RunExpr::List(list) => list.span,
            RunExpr::Block(block) => block.span(),
        }
    }
}

impl SemanticHash for RunExpr {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            RunExpr::Shell(expr) => expr.semantic_hash(state),
            RunExpr::Write(expr) => expr.semantic_hash(state),
            RunExpr::Copy(expr) => expr.semantic_hash(state),
            RunExpr::Delete(expr) => expr.semantic_hash(state),
            RunExpr::Touch(expr) => expr.semantic_hash(state),
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
pub struct CopyExpr {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_copy: keyword::Copy,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub src: StringExpr,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_to: keyword::To,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub dest: StringExpr,
}

impl SemanticHash for CopyExpr {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.src.semantic_hash(state);
        self.dest.semantic_hash(state);
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct WriteExpr {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_write: keyword::Write,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub value: Expr,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_to: keyword::To,
    #[serde(skip, default)]
    pub ws_3: Whitespace,
    pub path: Expr,
}

impl SemanticHash for WriteExpr {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.semantic_hash(state);
        self.value.semantic_hash(state);
    }
}
