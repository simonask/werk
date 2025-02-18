use std::hash::Hash as _;

use super::{keyword, token, Body, BodyStmt, Ident, PatternExpr, StringExpr, Trailing, Whitespace};
use werk_util::{SemanticHash, Span, Spanned};

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
pub enum Expr {
    // Look up variable in scope.
    Ident(Ident),
    StringExpr(StringExpr),
    Shell(ShellExpr),
    Read(ReadExpr),
    Glob(GlobExpr),
    Which(WhichExpr),
    Env(EnvExpr),
    List(ListExpr<ExprChain>),
    /// `(<expr>)`
    SubExpr(SubExpr),
    Error(ErrorExpr),
}

impl Expr {
    pub fn literal(span: impl Into<Span>, s: impl Into<String>) -> Self {
        Self::StringExpr(StringExpr::literal(span, s))
    }
}

impl Spanned for Expr {
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

impl SemanticHash for Expr {
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
pub struct SubExpr {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub token_open: token::ParenOpen,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    pub expr: Box<ExprChain>,
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
pub enum ExprOp {
    SubExpr(SubExpr),
    StringExpr(StringExpr),
    Match(MatchExpr),
    Map(MapExpr),
    Flatten(FlattenExpr),
    Filter(FilterExpr),
    FilterMatch(FilterMatchExpr),
    Discard(DiscardExpr),
    Join(JoinExpr),
    Split(SplitExpr),
    Lines(LinesExpr),
    Dedup(DedupExpr),
    Info(InfoExpr),
    Warn(WarnExpr),
    Error(ErrorExpr),
    AssertEq(AssertEqExpr),
    AssertMatch(AssertMatchExpr),
}

impl Spanned for ExprOp {
    #[inline]
    fn span(&self) -> Span {
        match self {
            ExprOp::SubExpr(expr) => expr.span,
            ExprOp::StringExpr(expr) => expr.span,
            ExprOp::Match(expr) => expr.span,
            ExprOp::Map(expr) => expr.span,
            ExprOp::Flatten(expr) => expr.span(),
            ExprOp::Filter(expr) => expr.span,
            ExprOp::FilterMatch(expr) => expr.span,
            ExprOp::Discard(expr) => expr.span,
            ExprOp::Join(expr) => expr.span,
            ExprOp::Split(expr) => expr.span,
            ExprOp::Dedup(expr) => expr.span(),
            ExprOp::Lines(expr) => expr.span(),
            ExprOp::Info(expr) => expr.span,
            ExprOp::Warn(expr) => expr.span,
            ExprOp::Error(expr) => expr.span,
            ExprOp::AssertEq(expr) => expr.span,
            ExprOp::AssertMatch(expr) => expr.span,
        }
    }
}

impl SemanticHash for ExprOp {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            ExprOp::SubExpr(expr) => expr.expr.semantic_hash(state),
            ExprOp::StringExpr(expr) => expr.semantic_hash(state),
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
            // Covered by the discriminant:
            | ExprOp::Dedup(_) | ExprOp::Flatten(_) | ExprOp::Lines(_)
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
pub enum MatchBody {
    Single(Box<MatchArm>),
    Braced(Body<MatchArm>),
}

impl MatchBody {
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
    pub fn iter(&self) -> MatchBodyIter<'_> {
        match self {
            MatchBody::Single(match_arm) => MatchBodyIter::Single(Some(match_arm)),
            MatchBody::Braced(body) => MatchBodyIter::Braced(body.statements.iter()),
        }
    }
}

impl<'a> IntoIterator for &'a MatchBody {
    type IntoIter = MatchBodyIter<'a>;
    type Item = &'a MatchArm;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub enum MatchBodyIter<'a> {
    Single(Option<&'a MatchArm>),
    Braced(std::slice::Iter<'a, BodyStmt<MatchArm>>),
}

impl<'a> Iterator for MatchBodyIter<'a> {
    type Item = &'a MatchArm;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MatchBodyIter::Single(match_arm) => match_arm.take(),
            MatchBodyIter::Braced(iter) => iter.next().map(|stmt| &stmt.statement),
        }
    }
}

impl SemanticHash for MatchBody {
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
pub struct MatchArm {
    #[serde(skip, default)]
    pub span: Span,
    pub pattern: PatternExpr,
    /// Whitespace between the pattern and the fat arrow.
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    #[serde(skip, default)]
    pub token_fat_arrow: keyword::FatArrow,
    /// Whitespace between the fat arrow and the expression.
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub expr: ExprChain,
}

impl SemanticHash for MatchArm {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pattern.semantic_hash(state);
        self.expr.semantic_hash(state);
    }
}

/// Expression with optional chain of operations. This is valid after `let =`,
/// inside parentheses, as list elements, or the right-hand side of braced match arms.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ExprChain {
    #[serde(skip, default)]
    pub span: Span,
    /// The initial expression of the chain.
    pub expr: Expr,
    /// All subsequent links, i.e. each `| expr` part.
    pub ops: Vec<ChainSubExpr>,
}

impl From<Expr> for ExprChain {
    fn from(expr: Expr) -> Self {
        Self {
            span: expr.span(),
            expr,
            ops: Vec::new(),
        }
    }
}

impl From<StringExpr> for ExprChain {
    fn from(atom: StringExpr) -> Self {
        Self {
            span: atom.span,
            expr: Expr::StringExpr(atom),
            ops: Vec::new(),
        }
    }
}

impl SemanticHash for ExprChain {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.expr.semantic_hash(state);
        self.ops.as_slice().semantic_hash(state);
    }
}

/// Entry in an expression chain `| expr`.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct ChainSubExpr {
    #[serde(skip, default)]
    pub span: Span,
    #[serde(skip, default)]
    pub ws_1: Whitespace,
    #[serde(skip, default)]
    pub token_pipe: token::Pipe,
    #[serde(skip, default)]
    pub ws_2: Whitespace,
    pub expr: ExprOp,
}

impl SemanticHash for ChainSubExpr {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.expr.semantic_hash(state);
    }
}

pub type JoinExpr = KwExpr<keyword::Join, StringExpr>;
pub type MapExpr = KwExpr<keyword::Map, Expr>;
pub type GlobExpr = KwExpr<keyword::Glob, StringExpr>;
pub type WhichExpr = KwExpr<keyword::Which, StringExpr>;
pub type EnvExpr = KwExpr<keyword::Env, StringExpr>;
pub type ShellExpr = KwExpr<keyword::Shell, StringExpr>;
pub type ReadExpr = KwExpr<keyword::Read, StringExpr>;
pub type InfoExpr = KwExpr<keyword::Info, StringExpr>;
pub type WarnExpr = KwExpr<keyword::Warn, StringExpr>;
pub type ErrorExpr = KwExpr<keyword::Error, StringExpr>;
pub type AssertEqExpr = KwExpr<keyword::AssertEq, Box<Expr>>;
pub type AssertMatchExpr = KwExpr<keyword::AssertEq, Box<PatternExpr>>;
pub type FlattenExpr = keyword::Flatten;
pub type SplitExpr = KwExpr<keyword::Split, PatternExpr>;
pub type DedupExpr = keyword::Dedup;
pub type LinesExpr = keyword::Lines;
pub type FilterExpr = KwExpr<keyword::Filter, PatternExpr>;
pub type FilterMatchExpr = KwExpr<keyword::FilterMatch, MatchBody>;
pub type MatchExpr = KwExpr<keyword::Match, MatchBody>;
pub type DiscardExpr = KwExpr<keyword::Discard, PatternExpr>;

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
