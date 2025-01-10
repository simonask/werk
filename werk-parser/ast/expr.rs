use std::{borrow::Cow, hash::Hash as _};

use crate::{
    parser::{Span, Spanned},
    SemanticHash,
};

use super::{token, Body, Ident, PatternExpr, StringExpr, Whitespace};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Look up variable in scope.
    Ident(Ident<'a>),
    StringExpr(StringExpr<'a>),
    Shell(ShellExpr<'a>),
    Glob(GlobExpr<'a>),
    Which(WhichExpr<'a>),
    Env(EnvExpr<'a>),
    List(ListExpr<Expr<'a>>),
    Match(MatchExpr<'a>),
    /// Given a list expression, flatten the list and join each element with
    /// separator.
    Join(JoinExpr<'a>),
    Then(Box<ThenExpr<'a>>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    Error(ErrorExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn literal(span: impl Into<Span>, s: impl Into<Cow<'a, str>>) -> Self {
        Self::StringExpr(StringExpr::literal(span, s))
    }
}

impl Spanned for Expr<'_> {
    fn span(&self) -> Span {
        match self {
            Expr::Ident(ident) => ident.span,
            Expr::StringExpr(string_expr) => string_expr.span,
            Expr::Shell(expr) => expr.span,
            Expr::Glob(expr) => expr.span,
            Expr::Which(expr) => expr.span,
            Expr::Env(expr) => expr.span,
            Expr::List(list) => list.span,
            Expr::Match(match_expr) => match_expr.span,
            Expr::Join(join_expr) => join_expr.span,
            Expr::Then(then_expr) => then_expr.span,
            Expr::Info(expr) => expr.span,
            Expr::Warn(expr) => expr.span,
            Expr::Error(expr) => expr.span,
        }
    }
}

impl SemanticHash for Expr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Expr::Ident(ident) => ident.semantic_hash(state),
            Expr::StringExpr(s) => s.semantic_hash(state),
            Expr::Shell(s) => s.semantic_hash(state),
            Expr::Glob(s) => s.semantic_hash(state),
            Expr::Which(s) => s.semantic_hash(state),
            Expr::Env(s) => s.semantic_hash(state),
            Expr::List(list) => list.semantic_hash(state),
            Expr::Match(expr) => expr.semantic_hash(state),
            Expr::Join(expr) => expr.semantic_hash(state),
            Expr::Then(expr) => expr.semantic_hash(state),
            // Messages don't contribute to outdatedness.
            Expr::Info(_) | Expr::Warn(_) | Expr::Error(_) => (),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ListExpr<E> {
    pub span: Span,
    pub token_open: token::BracketOpen,
    pub items: Vec<ListItem<E>>,
    pub ws_trailing: Whitespace,
    pub token_close: token::BracketClose,
}

impl<E: SemanticHash> SemanticHash for ListExpr<E> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.items.as_slice().semantic_hash(state);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ListItem<E> {
    pub ws_pre: Whitespace,
    pub item: E,
    pub ws_trailing: Option<(Whitespace, token::Comma)>,
}

impl<'a, E: SemanticHash> SemanticHash for ListItem<E> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.semantic_hash(state);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr<'a> {
    pub span: Span,
    pub token_match: token::Match,
    pub ws_1: Whitespace,
    pub body: Body<MatchArm<'a>>,
}

impl SemanticHash for MatchExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.body.semantic_hash(state);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm<'a> {
    pub span: Span,
    pub pattern: PatternExpr<'a>,
    /// Whitespace between the pattern and the fat arrow.
    pub ws_1: Whitespace,
    pub token_fat_arrow: token::FatArrow,
    /// Whitespace between the fat arrow and the expression.
    pub ws_2: Whitespace,
    pub expr: Expr<'a>,
}

impl SemanticHash for MatchArm<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pattern.semantic_hash(state);
        self.expr.semantic_hash(state);
    }
}

/// Pipe the result of one expression into another (`=>`).
///
/// Some expressions require this, like `match`.
#[derive(Clone, Debug, PartialEq)]
pub struct ThenExpr<'a> {
    pub span: Span,
    pub expr: Expr<'a>,
    pub ws_1: Whitespace,
    pub token_pipe: token::Pipe,
    pub ws_2: Whitespace,
    pub then: Expr<'a>,
}

impl SemanticHash for ThenExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.expr.semantic_hash(state);
        self.then.semantic_hash(state);
    }
}

pub type JoinExpr<'a> = KwExpr<token::Join, StringExpr<'a>>;
pub type GlobExpr<'a> = KwExpr<token::Glob, StringExpr<'a>>;
pub type WhichExpr<'a> = KwExpr<token::Which, StringExpr<'a>>;
pub type EnvExpr<'a> = KwExpr<token::Env, StringExpr<'a>>;
pub type ShellExpr<'a> = KwExpr<token::Shell, StringExpr<'a>>;
pub type InfoExpr<'a> = KwExpr<token::Info, StringExpr<'a>>;
pub type WarnExpr<'a> = KwExpr<token::Warn, StringExpr<'a>>;
pub type ErrorExpr<'a> = KwExpr<token::Error, StringExpr<'a>>;

/// Expression that is a pair of a token and a parameter, such as `<keyword>
/// <expr>`. Example: `join ","`
#[derive(Clone, Debug, PartialEq)]
pub struct KwExpr<Token, Param> {
    pub span: Span,
    pub token: Token,
    pub ws_1: Whitespace,
    pub param: Param,
}

impl<T, P: SemanticHash> SemanticHash for KwExpr<T, P> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.param.semantic_hash(state);
    }
}
