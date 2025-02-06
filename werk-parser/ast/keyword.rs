use winnow::Parser as _;

use crate::{
    parser::{Input, Offset, PResult, Parse, Parser as _, Span, Spanned},
    Failure,
};

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

#[inline]
fn end_of_keyword(input: &mut Input) -> PResult<()> {
    winnow::combinator::alt((
        winnow::token::any
            .verify(|c: &char| !c.is_alphanumeric() && *c != '-' && *c != '_')
            .value(()),
        winnow::combinator::eof.value(()),
    ))
    .parse_next(input)
}

macro_rules! def_keyword {
    ($t:ident, $s:literal) => {
        #[doc = concat!("`", $s, "`")]
        #[derive(Clone, Copy, Default, PartialEq)]
        pub struct $t(pub Offset);
        impl Keyword for $t {
            const TOKEN: &'static str = $s;
            #[inline]
            fn with_span(span: Span) -> Self {
                Self(span.start)
            }
        }
        impl std::fmt::Debug for $t {
            #[inline]
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
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
        impl<'a> Parse<'a> for $t {
            fn parse(input: &mut Input<'a>) -> PResult<Self> {
                winnow::combinator::terminated($s, winnow::combinator::peek(end_of_keyword))
                    .or_fail(Failure::ExpectedKeyword(&$s))
                    .token_span()
                    .map(Self::with_span)
                    .parse_next(input)
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
def_keyword!(Dedup, "dedup");
def_keyword!(Lines, "lines");

def_keyword!(AssertEq, "assert-eq");
def_keyword!(SetCapture, "capture");
def_keyword!(SetNoCapture, "no-capture");
def_keyword!(SetEnv, "env");
def_keyword!(RemoveEnv, "env-remove");
