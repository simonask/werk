use std::{borrow::Cow, hash::Hash as _};

use crate::{
    parser::{Span, Spanned},
    SemanticHash,
};

use super::{token, Body, BodyStmt, Ident, PatternExpr, StringExpr, Whitespace};

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
    Map(MapExpr<'a>),
    Flatten(FlattenExpr<'a>),
    Filter(FilterExpr<'a>),
    FilterMatch(FilterMatchExpr<'a>),
    Discard(DiscardExpr<'a>),
    Split(SplitExpr<'a>),
    Lines(LinesExpr<'a>),
    Chain(ChainExpr<'a>),
    Info(InfoExpr<'a>),
    Warn(WarnExpr<'a>),
    Error(ErrorExpr<'a>),

    AssertEq(AssertEqExpr<'a>),
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
            Expr::Map(map_expr) => map_expr.span,
            Expr::Flatten(flatten_expr) => flatten_expr.span(),
            Expr::Chain(chain) => chain.span,
            Expr::Info(expr) => expr.span,
            Expr::Warn(expr) => expr.span,
            Expr::Error(expr) => expr.span,
            Expr::AssertEq(expr) => expr.span,
            Expr::Filter(expr) => expr.span,
            Expr::FilterMatch(expr) => expr.span,
            Expr::Discard(expr) => expr.span,
            Expr::Split(expr) => expr.span,
            Expr::Lines(expr) => expr.span(),
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
            Expr::Map(expr) => expr.semantic_hash(state),
            Expr::Flatten(_) => (),
            Expr::Filter(expr) => expr.semantic_hash(state),
            Expr::FilterMatch(expr) => expr.semantic_hash(state),
            Expr::Discard(expr) => expr.semantic_hash(state),
            Expr::Split(expr) => expr.semantic_hash(state),
            Expr::Lines(_) => (),
            Expr::Chain(expr) => expr.semantic_hash(state),
            // Messages don't contribute to outdatedness.
            Expr::Info(_) | Expr::Warn(_) | Expr::Error(_) => (),
            // Debug assertions don't contribute to outdatedness.
            Expr::AssertEq(_) => (),
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
pub enum MatchBody<'a> {
    Single(Box<MatchArm<'a>>),
    Braced(Body<MatchArm<'a>>),
}

impl<'a> MatchBody<'a> {
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            MatchBody::Single(_) => 1,
            MatchBody::Braced(body) => body.statements.len(),
        }
    }

    pub fn iter<'b>(&'b self) -> MatchBodyIter<'a, 'b> {
        match self {
            MatchBody::Single(match_arm) => MatchBodyIter::Single(Some(match_arm)),
            MatchBody::Braced(body) => MatchBodyIter::Braced(body.statements.iter()),
        }
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
                (&[match_arm]).semantic_hash(state);
            }
            MatchBody::Braced(body) => body.statements.as_slice().semantic_hash(state),
        }
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

#[derive(Clone, Debug, PartialEq)]
pub struct ChainExpr<'a> {
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
#[derive(Clone, Debug, PartialEq)]
pub struct ChainSubExpr<'a> {
    pub span: Span,
    pub ws_1: Whitespace,
    pub token_pipe: token::Pipe,
    pub ws_2: Whitespace,
    pub expr: Expr<'a>,
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
pub type InfoExpr<'a> = KwExpr<token::Info, StringExpr<'a>>;
pub type WarnExpr<'a> = KwExpr<token::Warn, StringExpr<'a>>;
pub type ErrorExpr<'a> = KwExpr<token::Error, StringExpr<'a>>;
pub type AssertEqExpr<'a> = KwExpr<token::AssertEq, Box<Expr<'a>>>;
pub type FlattenExpr<'a> = token::Flatten;
pub type SplitExpr<'a> = KwExpr<token::Split, PatternExpr<'a>>;
pub type LinesExpr<'a> = token::Lines;
pub type FilterExpr<'a> = KwExpr<token::Filter, PatternExpr<'a>>;
pub type FilterMatchExpr<'a> = KwExpr<token::FilterMatch, MatchBody<'a>>;
pub type MatchExpr<'a> = KwExpr<token::Match, MatchBody<'a>>;
pub type DiscardExpr<'a> = KwExpr<token::Discard, PatternExpr<'a>>;

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
