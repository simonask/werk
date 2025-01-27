use std::{borrow::Cow, hash::Hash as _};

use crate::{
    parser::{Span, Spanned},
    SemanticHash,
};

use super::{token, Body, BodyStmt, Ident, PatternExpr, StringExpr, Whitespace};

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Expr<'a> {
    // Look up variable in scope.
    Ident(Ident<'a>),
    StringExpr(StringExpr<'a>),
    Shell(ShellExpr<'a>),
    Read(ReadExpr<'a>),
    Glob(GlobExpr<'a>),
    Which(WhichExpr<'a>),
    Env(EnvExpr<'a>),
    List(ListExpr<Expr<'a>>),
    /// `<expr> | ...`
    Chain(ChainExpr<'a>),

    Error(ErrorExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn literal(span: impl Into<Span>, s: impl Into<Cow<'a, str>>) -> Self {
        Self::StringExpr(StringExpr::literal(span, s))
    }
}

impl Spanned for Expr<'_> {
    #[inline]
    fn span(&self) -> Span {
        match self {
            Expr::Ident(ident) => ident.span,
            Expr::StringExpr(string_expr) => string_expr.span,
            Expr::Shell(expr) => expr.span,
            Expr::Read(expr) => expr.span,
            Expr::Glob(expr) => expr.span,
            Expr::Which(expr) => expr.span,
            Expr::Env(expr) => expr.span,
            Expr::List(list) => list.span,
            Expr::Chain(chain) => chain.span,
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
            Expr::Read(s) => s.semantic_hash(state),
            Expr::Glob(s) => s.semantic_hash(state),
            Expr::Which(s) => s.semantic_hash(state),
            Expr::Env(s) => s.semantic_hash(state),
            Expr::List(list) => list.semantic_hash(state),
            Expr::Chain(expr) => expr.semantic_hash(state),
            // The error message does not contribute to outdatedness.
            Expr::Error(_) => (),
        }
    }
}

/// An operation within an expression chain (`... | <op>`).
///
/// These are expressions that take an input (left-hand side of the pipe symbol)
/// and produce an output, which will be passed to any subsequent operations, or
/// returned as the value.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ExprOp<'a> {
    Match(MatchExpr<'a>),
    Map(MapExpr<'a>),
    Flatten(#[serde(skip, default)] FlattenExpr<'a>),
    Filter(FilterExpr<'a>),
    FilterMatch(FilterMatchExpr<'a>),
    Discard(DiscardExpr<'a>),
    Join(JoinExpr<'a>),
    Split(SplitExpr<'a>),
    Lines(#[serde(skip, default)] LinesExpr<'a>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    Error(ErrorExpr<'a>),
    AssertEq(AssertEqExpr<'a>),
    AssertMatch(AssertMatchExpr<'a>),
}

impl Spanned for ExprOp<'_> {
    #[inline]
    fn span(&self) -> Span {
        match self {
            ExprOp::Match(expr) => expr.span,
            ExprOp::Map(expr) => expr.span,
            ExprOp::Flatten(expr) => expr.span(),
            ExprOp::Filter(expr) => expr.span,
            ExprOp::FilterMatch(expr) => expr.span,
            ExprOp::Discard(expr) => expr.span,
            ExprOp::Join(expr) => expr.span,
            ExprOp::Split(expr) => expr.span,
            ExprOp::Lines(expr) => expr.span(),
            ExprOp::Info(expr) => expr.span,
            ExprOp::Warn(expr) => expr.span,
            ExprOp::Error(expr) => expr.span,
            ExprOp::AssertEq(expr) => expr.span,
            ExprOp::AssertMatch(expr) => expr.span,
        }
    }
}

impl SemanticHash for ExprOp<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            ExprOp::Match(expr) => expr.semantic_hash(state),
            ExprOp::Map(expr) => expr.semantic_hash(state),
            ExprOp::Filter(expr) => expr.semantic_hash(state),
            ExprOp::FilterMatch(expr) => expr.semantic_hash(state),
            ExprOp::Discard(expr) => expr.semantic_hash(state),
            ExprOp::Join(expr) => expr.semantic_hash(state),
            ExprOp::Split(expr) => expr.semantic_hash(state),
            // Contents of messages do not contribute to outdatedness.
            ExprOp::Info(_)
            | ExprOp::Warn(_)
            | ExprOp::Error(_)
            | ExprOp::AssertEq(_)
            | ExprOp::AssertMatch(_)
            // `flatten` and `lines` are caught by the discriminant
            | ExprOp::Flatten(_) | ExprOp::Lines(_)
            => (),
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ListExpr<E> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_open: token::BracketOpen,
    pub items: Vec<ListItem<E>>,
    #[serde(skip, default)]
    pub ws_trailing: Whitespace,
    #[serde(skip, default)]
    pub token_close: token::BracketClose,
}

impl<E: SemanticHash> SemanticHash for ListExpr<E> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.items.as_slice().semantic_hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ListItem<E> {
    #[serde(skip, default)]
    pub ws_pre: Whitespace,
    pub item: E,
    #[serde(skip, default)]
    pub ws_trailing: Option<(Whitespace, token::Comma)>,
}

impl<E: SemanticHash> SemanticHash for ListItem<E> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.semantic_hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum MatchBody<'a> {
    Single(Box<MatchArm<'a>>),
    Braced(Body<MatchArm<'a>>),
}

impl<'a> MatchBody<'a> {
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        match self {
            MatchBody::Single(_) => 1,
            MatchBody::Braced(body) => body.statements.len(),
        }
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        match self {
            MatchBody::Single(_) => false,
            MatchBody::Braced(body) => body.statements.is_empty(),
        }
    }

    #[must_use]
    pub fn iter<'b>(&'b self) -> MatchBodyIter<'a, 'b> {
        match self {
            MatchBody::Single(match_arm) => MatchBodyIter::Single(Some(match_arm)),
            MatchBody::Braced(body) => MatchBodyIter::Braced(body.statements.iter()),
        }
    }
}

impl<'a, 'b> IntoIterator for &'b MatchBody<'a> {
    type IntoIter = MatchBodyIter<'a, 'b>;
    type Item = &'b MatchArm<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub enum MatchBodyIter<'a, 'b> {
    Single(Option<&'b MatchArm<'a>>),
    Braced(std::slice::Iter<'b, BodyStmt<MatchArm<'a>>>),
}

impl<'a, 'b> Iterator for MatchBodyIter<'a, 'b> {
    type Item = &'b MatchArm<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MatchBodyIter::Single(match_arm) => match_arm.take(),
            MatchBodyIter::Braced(iter) => iter.next().map(|stmt| &stmt.statement),
        }
    }
}

impl SemanticHash for MatchBody<'_> {
    #[inline]
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            MatchBody::Single(match_arm) => {
                // Hash the single arm as a slice such that a body `{ ... }`
                // with a single arm is not different from an unbraced single
                // match arm.
                [match_arm].semantic_hash(state);
            }
            MatchBody::Braced(body) => body.statements.as_slice().semantic_hash(state),
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MatchArm<'a> {
    #[serde(skip, default)]
    pub span: Span,
    pub pattern: PatternExpr<'a>,
    /// Whitespace between the pattern and the fat arrow.
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    #[serde(skip, default)]
    pub token_fat_arrow: token::FatArrow,
    /// Whitespace between the fat arrow and the expression.
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub expr: Expr<'a>,
}

impl SemanticHash for MatchArm<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pattern.semantic_hash(state);
        self.expr.semantic_hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ChainExpr<'a> {
    #[serde(skip, default)]
    pub span: Span,
    /// The initial expression of the chain.
    pub head: Box<Expr<'a>>,
    /// All subsequent links, i.e. each `| expr` part.
    pub tail: Vec<ChainSubExpr<'a>>,
}

impl SemanticHash for ChainExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.head.semantic_hash(state);
        self.tail.as_slice().semantic_hash(state);
    }
}

/// Entry in an expression chain `| expr`.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ChainSubExpr<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    #[serde(skip, default)]
    pub token_pipe: token::Pipe,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub expr: ExprOp<'a>,
}

impl SemanticHash for ChainSubExpr<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.expr.semantic_hash(state);
    }
}

pub type JoinExpr<'a> = KwExpr<token::Join, StringExpr<'a>>;
pub type MapExpr<'a> = KwExpr<token::Map, StringExpr<'a>>;
pub type GlobExpr<'a> = KwExpr<token::Glob, StringExpr<'a>>;
pub type WhichExpr<'a> = KwExpr<token::Which, StringExpr<'a>>;
pub type EnvExpr<'a> = KwExpr<token::Env, StringExpr<'a>>;
pub type ShellExpr<'a> = KwExpr<token::Shell, StringExpr<'a>>;
pub type ReadExpr<'a> = KwExpr<token::Read, StringExpr<'a>>;
pub type InfoExpr<'a> = KwExpr<token::Info, StringExpr<'a>>;
pub type WarnExpr<'a> = KwExpr<token::Warn, StringExpr<'a>>;
pub type ErrorExpr<'a> = KwExpr<token::Error, StringExpr<'a>>;
pub type AssertEqExpr<'a> = KwExpr<token::AssertEq, Box<Expr<'a>>>;
pub type AssertMatchExpr<'a> = KwExpr<token::AssertEq, Box<PatternExpr<'a>>>;
pub type FlattenExpr<'a> = token::Flatten;
pub type SplitExpr<'a> = KwExpr<token::Split, PatternExpr<'a>>;
pub type LinesExpr<'a> = token::Lines;
pub type FilterExpr<'a> = KwExpr<token::Filter, PatternExpr<'a>>;
pub type FilterMatchExpr<'a> = KwExpr<token::FilterMatch, MatchBody<'a>>;
pub type MatchExpr<'a> = KwExpr<token::Match, MatchBody<'a>>;
pub type DiscardExpr<'a> = KwExpr<token::Discard, PatternExpr<'a>>;

/// Expression that is a pair of a token and a parameter, such as `<keyword>
/// <expr>`. Example: `join ","`
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct KwExpr<Token, Param> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token: Token,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub param: Param,
}

impl<T, P: SemanticHash> SemanticHash for KwExpr<T, P> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.param.semantic_hash(state);
    }
}
