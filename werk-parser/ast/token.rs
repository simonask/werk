use crate::parser::{Offset, Span, Spanned};

pub trait Keyword: Spanned {
    const TOKEN: &'static str;
    fn with_span(span: Span) -> Self;

    #[inline]
    #[must_use]
    fn ignore() -> Self
    where
        Self: Sized,
    {
        Self::with_span(Span::ignore())
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
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

impl<const CHAR: char> Spanned for Token<CHAR> {
    #[inline]
    fn span(&self) -> Span {
        Span {
            start: self.0,
            end: Offset(self.0 .0 + 1),
        }
    }
}

macro_rules! def_keyword {
    ($t:ident, $s:literal) => {
        #[doc = concat!("`", $s, "`")]
        #[derive(Clone, Copy, Debug, Default, PartialEq)]
        pub struct $t(pub Offset);
        impl Keyword for $t {
            const TOKEN: &'static str = $s;
            #[inline]
            fn with_span(span: Span) -> Self {
                Self(span.start)
            }
        }
        impl crate::parser::Spanned for $t {
            #[inline]
            #[allow(clippy::cast_possible_truncation)]
            fn span(&self) -> Span {
                Span {
                    start: self.0,
                    end: Offset(self.0 .0 + $s.len() as u32),
                }
            }
        }
        impl serde::Serialize for $t {
            fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
                ser.serialize_unit()
            }
        }
        impl<'de> serde::Deserialize<'de> for $t {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct Visitor;
                impl<'de> serde::de::Visitor<'de> for Visitor {
                    type Value = $t;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        write!(formatter, "unit")
                    }

                    fn visit_unit<E>(self) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        Ok($t::default())
                    }
                }
                deserializer.deserialize_unit(Visitor)
            }
        }
    };
}

macro_rules! def_token {
    ($t:ident, $s:literal) => {
        #[doc = concat!("`", $s, "`")]
        pub type $t = Token<$s>;
    };
}

def_keyword!(Let, "let");
def_keyword!(Config, "config");
def_keyword!(Build, "build");
def_keyword!(Task, "task");
def_keyword!(Shell, "shell");
def_keyword!(Glob, "glob");
def_keyword!(Which, "which");
def_keyword!(Env, "env");
def_keyword!(Join, "join");
def_keyword!(Then, "then");
def_keyword!(Info, "info");
def_keyword!(Warn, "warn");
def_keyword!(Error, "error");
def_keyword!(Match, "match");
def_keyword!(Write, "write");
def_keyword!(Read, "read");
def_keyword!(Run, "run");
def_keyword!(Copy, "copy");
def_keyword!(Delete, "delete");
def_keyword!(FatArrow, "=>");
def_keyword!(From, "from");
def_keyword!(Depfile, "depfile");
def_keyword!(False, "false");
def_keyword!(True, "true");
def_keyword!(To, "to");
def_keyword!(Map, "map");
def_keyword!(Flatten, "flatten");
def_keyword!(Filter, "filter");
def_keyword!(FilterMatch, "filter-match");
def_keyword!(Discard, "discard");
def_keyword!(Split, "split");
def_keyword!(Lines, "lines");

def_keyword!(AssertEq, "assert-eq");
def_keyword!(SetCapture, "capture");
def_keyword!(SetNoCapture, "no-capture");
def_keyword!(SetEnv, "env");
def_keyword!(RemoveEnv, "env-remove");

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
def_token!(DoubleQuote, '"');
def_token!(Percent, '%');
def_token!(Pipe, '|');
