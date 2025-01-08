use crate::parser::{Offset, Span, Spanned};

pub trait Keyword: Spanned {
    const TOKEN: &'static str;
    fn with_span(span: Span) -> Self;

    fn ignore() -> Self
    where
        Self: Sized,
    {
        Self::with_span(Span::ignore())
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Hash)]
pub struct Token<const CHAR: char>(pub Offset);
impl<const CHAR: char> Token<CHAR> {
    #[inline]
    pub fn with_span(span: Span) -> Self {
        Self(span.start)
    }

    pub const fn ignore() -> Self {
        Self(Offset::ignore())
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

macro_rules! def_token {
    ($t:ident, $s:literal) => {
        #[doc = concat!("`", $s, "`")]
        #[derive(Clone, Copy, Debug, Default, PartialEq, Hash)]
        pub struct $t(pub Offset);
        impl Keyword for $t {
            const TOKEN: &'static str = $s;
            fn with_span(span: Span) -> Self {
                Self(span.start)
            }
        }
        impl crate::parser::Spanned for $t {
            #[inline]
            fn span(&self) -> Span {
                Span {
                    start: self.0,
                    end: Offset(self.0 .0 + $s.len() as u32),
                }
            }
        }
    };
}

macro_rules! def_token_char {
    ($t:ident, $s:literal) => {
        #[doc = concat!("`", $s, "`")]
        pub type $t = Token<$s>;
    };
}

def_token!(Let, "let");
def_token!(Config, "config");
def_token!(Build, "build");
def_token!(Task, "task");
def_token!(Shell, "shell");
def_token!(Glob, "glob");
def_token!(Which, "which");
def_token!(Env, "env");
def_token!(Join, "join");
def_token!(Then, "then");
def_token!(Info, "info");
def_token!(Warn, "warn");
def_token!(Error, "error");
def_token!(Match, "match");
def_token!(Write, "write");
def_token!(Read, "read");
def_token!(Run, "run");
def_token!(Copy, "copy");
def_token!(Delete, "delete");
def_token!(FatArrow, "=>");
def_token!(From, "from");
def_token!(Depfile, "depfile");
def_token!(False, "false");
def_token!(True, "true");

def_token_char!(Colon, ':');
def_token_char!(Eq, '=');
def_token_char!(Comma, ',');
def_token_char!(Semicolon, ';');
def_token_char!(BraceOpen, '{');
def_token_char!(BraceClose, '}');
def_token_char!(ParenOpen, '(');
def_token_char!(ParenClose, ')');
def_token_char!(BracketOpen, '[');
def_token_char!(BracketClose, ']');
def_token_char!(DoubleQuote, '"');
def_token_char!(Percent, '%');
