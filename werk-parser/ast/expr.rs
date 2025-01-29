use std::{borrow::Cow, hash::Hash as _};

use crate::{
    parser::{Span, Spanned},
    SemanticHash,
};

use super::{keyword, token, Body, BodyStmt, Ident, PatternExpr, StringExpr, Trailing, Whitespace};

/// "Atomic" expression (no pipe chaining).
///
/// Pipe chains always start with an atomic expression, optionally followed by a
/// chain of `| op | ...`. A chained expression becomes "atomic" when it is in
/// parentheses.
///
/// Most operations take an atomic expression as a parameter - in other words,
/// passing the output of a pipe expression as a parameter to an operation requires that it is parenthesized.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum Expr<'a> {
    // Look up variable in scope.
    Ident(Ident<'a>),
    StringExpr(StringExpr<'a>),
    Shell(ShellExpr<'a>),
    Read(ReadExpr<'a>),
    Glob(GlobExpr<'a>),
    Which(WhichExpr<'a>),
    Env(EnvExpr<'a>),
    List(ListExpr<ExprChain<'a>>),
    /// `(<expr>)`
    SubExpr(SubExpr<'a>),
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
            Expr::SubExpr(expr) => expr.span,
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
            Expr::SubExpr(expr) => expr.expr.semantic_hash(state),
            // The error message does not contribute to outdatedness.
            Expr::Error(_) => (),
        }
    }
}

/// Parenthesized sub-expression.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct SubExpr<'a> {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_open: token::ParenOpen,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub expr: Box<ExprChain<'a>>,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    #[serde(skip, default)]
    pub token_close: token::ParenClose,
}

/// An operation within an expression chain (`... | <op>`).
///
/// These are expressions that take an input (left-hand side of the pipe symbol)
/// and produce an output, which will be passed to any subsequent operations, or
/// returned as the value.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ExprOp<'a> {
    SubExpr(SubExpr<'a>),
    Match(MatchExpr<'a>),
    Map(MapExpr<'a>),
    Flatten(FlattenExpr<'a>),
    Filter(FilterExpr<'a>),
    FilterMatch(FilterMatchExpr<'a>),
    Discard(DiscardExpr<'a>),
    Join(JoinExpr<'a>),
    Split(SplitExpr<'a>),
    Lines(LinesExpr<'a>),
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
            ExprOp::SubExpr(expr) => expr.span,
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
            ExprOp::SubExpr(expr) => expr.expr.semantic_hash(state),
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
    pub trailing: Trailing<token::Comma>,
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
    pub token_fat_arrow: keyword::FatArrow,
    /// Whitespace between the fat arrow and the expression.
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub expr: ExprChain<'a>,
}

impl SemanticHash for MatchArm<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pattern.semantic_hash(state);
        self.expr.semantic_hash(state);
    }
}

/// Expression with optional chain of operations. This is valid after `let =`,
/// inside parentheses, as list elements, or the right-hand side of braced match arms.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ExprChain<'a> {
    #[serde(skip, default)]
    pub span: Span,
    /// The initial expression of the chain.
    pub expr: Expr<'a>,
    /// All subsequent links, i.e. each `| expr` part.
    pub ops: Vec<ChainSubExpr<'a>>,
}

impl<'a> From<Expr<'a>> for ExprChain<'a> {
    fn from(expr: Expr<'a>) -> Self {
        Self {
            span: expr.span(),
            expr,
            ops: Vec::new(),
        }
    }
}

impl<'a> From<StringExpr<'a>> for ExprChain<'a> {
    fn from(atom: StringExpr<'a>) -> Self {
        Self {
            span: atom.span,
            expr: Expr::StringExpr(atom),
            ops: Vec::new(),
        }
    }
}

impl SemanticHash for ExprChain<'_> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.expr.semantic_hash(state);
        self.ops.as_slice().semantic_hash(state);
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

pub type JoinExpr<'a> = KwExpr<keyword::Join, StringExpr<'a>>;
pub type MapExpr<'a> = KwExpr<keyword::Map, Expr<'a>>;
pub type GlobExpr<'a> = KwExpr<keyword::Glob, StringExpr<'a>>;
pub type WhichExpr<'a> = KwExpr<keyword::Which, StringExpr<'a>>;
pub type EnvExpr<'a> = KwExpr<keyword::Env, StringExpr<'a>>;
pub type ShellExpr<'a> = KwExpr<keyword::Shell, StringExpr<'a>>;
pub type ReadExpr<'a> = KwExpr<keyword::Read, StringExpr<'a>>;
pub type InfoExpr<'a> = KwExpr<keyword::Info, StringExpr<'a>>;
pub type WarnExpr<'a> = KwExpr<keyword::Warn, StringExpr<'a>>;
pub type ErrorExpr<'a> = KwExpr<keyword::Error, StringExpr<'a>>;
pub type AssertEqExpr<'a> = KwExpr<keyword::AssertEq, Box<Expr<'a>>>;
pub type AssertMatchExpr<'a> = KwExpr<keyword::AssertEq, Box<PatternExpr<'a>>>;
pub type FlattenExpr<'a> = keyword::Flatten;
pub type SplitExpr<'a> = KwExpr<keyword::Split, PatternExpr<'a>>;
pub type LinesExpr<'a> = keyword::Lines;
pub type FilterExpr<'a> = KwExpr<keyword::Filter, PatternExpr<'a>>;
pub type FilterMatchExpr<'a> = KwExpr<keyword::FilterMatch, MatchBody<'a>>;
pub type MatchExpr<'a> = KwExpr<keyword::Match, MatchBody<'a>>;
pub type DiscardExpr<'a> = KwExpr<keyword::Discard, PatternExpr<'a>>;

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
