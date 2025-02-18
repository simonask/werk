use winnow::Parser as _;

use werk_util::{Offset, Span, Spanned};

use crate::{
    parser::{Input, PResult, Parse, Parser as _},
    Failure,
};

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Token<const CHAR: char>(pub Offset);
impl<const CHAR: char> Token<CHAR> {
    #[inline]
    #[must_use]
    pub const fn with_span(span: Span) -> Self {
        Self(span.start)
    }

    #[must_use]
    pub const fn ignore() -> Self {
        Self(Offset::ignore())
    }
}

impl<const CHAR: char> std::fmt::Debug for Token<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{CHAR}' {:?}", self.0)
    }
}

impl<const CHAR: char> Spanned for Token<CHAR> {
    #[inline]
    fn span(&self) -> Span {
        Span {
            start: self.0,
            end: Offset(self.0 .0 + 1),
        }
    }
}

impl<const CHAR: char> Parse for Token<CHAR> {
    fn parse(input: &mut Input) -> PResult<Self> {
        CHAR.or_fail(Failure::ExpectedChar(CHAR))
            .token_span()
            .map(Token::with_span)
            .parse_next(input)
    }
}

macro_rules! def_token {
    ($t:ident, $s:literal) => {
        #[doc = concat!("`", $s, "`")]
        pub type $t = Token<$s>;
    };
}

def_token!(Colon, ':');
def_token!(Eq, '=');
def_token!(Comma, ',');
def_token!(Semicolon, ';');
def_token!(BraceOpen, '{');
def_token!(BraceClose, '}');
def_token!(ParenOpen, '(');
def_token!(ParenClose, ')');
def_token!(BracketOpen, '[');
def_token!(BracketClose, ']');
def_token!(LessThan, '<');
def_token!(GreaterThan, '>');
def_token!(DoubleQuote, '"');
def_token!(Percent, '%');
def_token!(Pipe, '|');
