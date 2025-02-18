use std::{fmt::Write, sync::Arc};

use crate::{
    ast::{self, token},
    fatal,
    parser::{Input, Parser as _},
    Error, Failure, ModalErr,
};
use werk_util::{Offset, Symbol};
use winnow::{
    ascii::{dec_int, digit1, multispace1},
    combinator::{
        alt, cut_err, delimited, empty, opt, peek, preceded, repeat, separated, separated_pair,
        terminated,
    },
    stream::Location,
    token::{any, one_of, take_till, take_while},
    Parser,
};

use super::{parse, PResult, Parse, Spanned};

impl Parse for ast::StringExpr {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut expr, span) = delimited(
            parse::<token::DoubleQuote>.expect(&"string literal"),
            string_expr_inside_quotes,
            cut_err(parse::<token::DoubleQuote>),
        )
        .with_token_span()
        .while_parsing("string literal")
        .parse_next(input)?;
        expr.span = span;
        Ok(expr)
    }
}

impl Spanned for ast::StringExpr {
    #[inline]
    fn span(&self) -> super::Span {
        self.span
    }
}

impl Parse for ast::PatternExpr {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut expr, span) = delimited(
            parse::<token::DoubleQuote>.expect(&"pattern literal"),
            pattern_expr_inside_quotes,
            cut_err(parse::<token::DoubleQuote>),
        )
        .with_token_span()
        .while_parsing("pattern literal")
        .parse_next(input)?;
        expr.span = span;
        Ok(expr)
    }
}

pub fn string_expr_inside_quotes(input: &mut Input) -> PResult<ast::StringExpr> {
    let (mut expr, span) = repeat(0.., string_fragment)
        .fold(ast::StringExpr::default, |mut expr, fragment| {
            push_string_fragment(&mut expr, fragment);
            expr
        })
        .with_token_span()
        .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

pub fn pattern_expr_inside_quotes(input: &mut Input) -> PResult<ast::PatternExpr> {
    let (mut expr, span) = repeat(0.., pattern_fragment)
        .fold(ast::PatternExpr::default, |mut expr, fragment| {
            push_pattern_fragment(&mut expr, fragment);
            expr
        })
        .with_token_span()
        .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

/// Parse the inside of a pattern expr (i.e. without surrounding quotes). This
/// is used for deserialization and testing.
pub fn parse_pattern_expr_unquoted(pattern: &str) -> Result<ast::PatternExpr, Error> {
    pattern_expr_inside_quotes
        .parse(Input::new(pattern))
        .map_err(winnow::error::ParseError::into_inner)
}

/// Parse the inside of a string expr (i.e. without surrounding quotes). This
/// is used for deserialization and testing.
pub fn parse_string_expr_unquoted(string: &str) -> Result<ast::StringExpr, Error> {
    string_expr_inside_quotes
        .parse(Input::new(string))
        .map_err(winnow::error::ParseError::into_inner)
}

fn string_fragment<'a>(input: &mut Input<'a>) -> PResult<StringFragment<'a>> {
    // TODO: Consider escape sequences etc.
    alt((
        '%'.value(StringFragment::PatternStem),
        string_literal_fragment::<false>.map(StringFragment::Literal),
        escaped_char.map(StringFragment::EscapedChar),
        escaped_whitespace.value(StringFragment::EscapedWhitespace),
        string_interpolation.map(StringFragment::Interpolation),
        path_interpolation.map(StringFragment::Interpolation),
    ))
    .parse_next(input)
}

fn pattern_fragment<'a>(input: &mut Input<'a>) -> PResult<StringFragment<'a>> {
    // TODO: Consider escape sequences etc.
    alt((
        '%'.value(StringFragment::PatternStem),
        pattern_one_of.map(StringFragment::OneOf),
        string_literal_fragment::<true>.map(StringFragment::Literal),
        escaped_char.map(StringFragment::EscapedChar),
        escaped_whitespace.value(StringFragment::EscapedWhitespace),
        string_interpolation.map(StringFragment::Interpolation),
        path_interpolation.map(StringFragment::Interpolation),
    ))
    .parse_next(input)
}

#[inline]
#[must_use]
pub const fn needs_string_escape(ch: char) -> bool {
    // TODO: Add `%` to this list.
    matches!(ch, '\\' | '{' | '}' | '<' | '>' | '%' | '"')
}

#[inline]
#[must_use]
pub const fn needs_pattern_escape(ch: char) -> bool {
    matches!(ch, '\\' | '{' | '}' | '<' | '>' | '%' | '(' | ')' | '"')
}

#[inline]
#[must_use]
const fn needs_escape<const IS_PATTERN: bool>(ch: char) -> bool {
    if IS_PATTERN {
        needs_pattern_escape(ch)
    } else {
        needs_string_escape(ch)
    }
}

#[inline]
#[must_use]
pub fn escape_special_char(ch: char) -> Option<char> {
    match ch {
        '\n' => Some('n'),
        '\r' => Some('r'),
        '\t' => Some('t'),
        '\0' => Some('0'),
        _ => None,
    }
}

#[derive(Copy, Clone, Debug)]
struct Escape<'a, const IS_PATTERN: bool>(pub &'a str);
impl<const IS_PATTERN: bool> std::fmt::Display for Escape<'_, IS_PATTERN> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for ch in self.0.chars() {
            if needs_escape::<IS_PATTERN>(ch) {
                f.write_char('\\')?;
                f.write_char(ch)?;
            } else if let Some(escape_char) = escape_special_char(ch) {
                f.write_char('\\')?;
                f.write_char(escape_char)?;
            } else {
                f.write_char(ch)?;
            }
        }
        Ok(())
    }
}

#[inline]
#[must_use]
pub fn escape_string_literal(literal: &str) -> impl std::fmt::Display + '_ {
    Escape::<false>(literal)
}

#[inline]
#[must_use]
pub fn escape_pattern_literal(literal: &str) -> impl std::fmt::Display + '_ {
    Escape::<true>(literal)
}

fn string_literal_fragment<'a, const IS_PATTERN: bool>(input: &mut Input<'a>) -> PResult<&'a str> {
    take_till(1.., needs_escape::<IS_PATTERN>)
        .verify(|s: &str| !s.is_empty())
        .parse_next(input)
}

fn escaped_char(input: &mut Input) -> Result<char, ModalErr> {
    let escape_seq_char = winnow::combinator::dispatch! {
        any::<Input, ModalErr>;
        '\\' => empty.value('\\'),
        '{' => empty.value('{'),
        '}' => empty.value('}'),
        '<' => empty.value('<'),
        '>' => empty.value('>'),
        '%' => empty.value('%'),
        '(' => empty.value('('),
        ')' => empty.value(')'),
        '"' => empty.value('"'),
        'n' => empty.value('\n'),
        'r' => empty.value('\r'),
        't' => empty.value('\t'),
        '0' => empty.value('\0'),
        otherwise => fatal(Failure::InvalidEscapeChar(otherwise)),
    };

    preceded('\\', escape_seq_char).parse_next(input)
}

fn escaped_whitespace<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    preceded('\\', multispace1).parse_next(input)
}

fn is_identifier_start(ch: char) -> bool {
    unicode_ident::is_xid_start(ch)
}

fn is_identifier_continue(ch: char) -> bool {
    // Allow kebab-case identifiers
    ch == '-' || unicode_ident::is_xid_continue(ch)
}

fn ident_str<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    (
        take_while(1, is_identifier_start),
        take_while(0.., is_identifier_continue),
    )
        .expect(&"identifier")
        .take()
        .parse_next(input)
}

fn ident(input: &mut Input) -> PResult<Symbol> {
    ident_str.map(Symbol::new).parse_next(input)
}

#[derive(Debug, Clone)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWhitespace,
    Interpolation(ast::Interpolation),
    PatternStem,
    OneOf(Vec<String>),
}

fn push_string_fragment(expr: &mut ast::StringExpr, frag: StringFragment) {
    match frag {
        StringFragment::Literal(lit) => {
            if let Some(ast::StringFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.push_str(lit);
            } else {
                expr.fragments
                    .push(ast::StringFragment::Literal(lit.to_owned()));
            }
        }
        StringFragment::EscapedChar(ch) => {
            if let Some(ast::StringFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.push(ch);
            } else {
                expr.fragments
                    .push(ast::StringFragment::Literal(ch.to_string()));
            }
        }
        StringFragment::EscapedWhitespace => {}
        StringFragment::Interpolation(string_interpolation) => {
            expr.fragments
                .push(ast::StringFragment::Interpolation(string_interpolation));
        }
        StringFragment::PatternStem => expr.fragments.push(ast::StringFragment::PatternStem),
        StringFragment::OneOf(_) => panic!("captue group in string expr must be escaped"),
    }
}

fn push_pattern_fragment(expr: &mut ast::PatternExpr, frag: StringFragment) {
    match frag {
        StringFragment::Literal(lit) => {
            if let Some(ast::PatternFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.push_str(lit);
            } else {
                expr.fragments
                    .push(ast::PatternFragment::Literal(lit.to_owned()));
            }
        }
        StringFragment::EscapedChar(ch) => {
            if let Some(ast::PatternFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.push(ch);
            } else {
                expr.fragments
                    .push(ast::PatternFragment::Literal(ch.to_string()));
            }
        }
        StringFragment::EscapedWhitespace => {}
        StringFragment::Interpolation(string_interpolation) => {
            expr.fragments
                .push(ast::PatternFragment::Interpolation(string_interpolation));
        }
        StringFragment::PatternStem => expr.fragments.push(ast::PatternFragment::PatternStem),
        StringFragment::OneOf(one_of) => expr.fragments.push(ast::PatternFragment::OneOf(one_of)),
    }
}

fn pattern_one_of(input: &mut Input) -> PResult<Vec<String>> {
    delimited(
        parse::<token::ParenOpen>.expect(&"start of pattern one-of group"),
        // TODO: Allow more than just identifiers here.
        separated(1.., cut_err(ident).map(|s| s.as_str().to_owned()), '|'),
        cut_err(parse::<token::ParenClose>),
    )
    .while_parsing("pattern capture group")
    .parse_next(input)
}

fn string_interpolation(input: &mut Input) -> PResult<ast::Interpolation> {
    delimited(
        parse::<token::BraceOpen>.expect(&"string interpolation block"),
        cut_err(interpolation_inner::<'}'>),
        cut_err(parse::<token::BraceClose>),
    )
    .while_parsing("string interpolation block")
    .parse_next(input)
}

fn path_interpolation(input: &mut Input) -> PResult<ast::Interpolation> {
    let mut interp = delimited(
        parse::<token::LessThan>.expect(&"path interpolation block"),
        cut_err(interpolation_inner::<'>'>),
        cut_err(parse::<token::GreaterThan>),
    )
    .while_parsing("path interpolation block")
    .parse_next(input)?;

    interp
        .options
        .get_or_insert_default()
        .ops
        .push(ast::InterpolationOp::ResolveOsPath);

    Ok(interp)
}

fn interpolation_inner<const TERMINATE: char>(input: &mut Input) -> PResult<ast::Interpolation> {
    let stem = opt(interpolation_stem)
        .parse_next(input)?
        .unwrap_or(ast::InterpolationStem::Implied);
    let index = opt(interpolation_index).parse_next(input)?;
    let join = opt(interpolation_join).parse_next(input)?;
    let ops = opt(interpolation_ops).parse_next(input)?;

    cut_err(peek(TERMINATE))
        .or_fail(Failure::ExpectedChar(TERMINATE))
        .parse_next(input)?;

    Ok(ast::Interpolation {
        stem,
        index,
        options: match (ops, join) {
            (Some(ops), None) if ops.is_empty() => None,
            (Some(ops), join) => Some(Box::new(ast::InterpolationOptions { ops, join })),
            (None, Some(join)) => Some(Box::new(ast::InterpolationOptions {
                ops: vec![],
                join: Some(join),
            })),
            (None, None) => None,
        },
    })
}

fn interpolation_index(input: &mut Input) -> PResult<ast::InterpolationIndex> {
    delimited(
        '[',
        cut_err(alt((
            dec_int.map(ast::InterpolationIndex::Const),
            ident.map(ast::InterpolationIndex::Ident),
            fatal(Failure::Expected(&"identifier or integer constant")),
        ))),
        cut_err(']').or_fail(Failure::ExpectedChar(']')),
    )
    .parse_next(input)
}

fn interpolation_stem(input: &mut Input) -> PResult<ast::InterpolationStem> {
    alt((
        '%'.value(ast::InterpolationStem::PatternCapture),
        digit1
            .try_map(str::parse)
            .map(ast::InterpolationStem::CaptureGroup),
        ident.map(ast::InterpolationStem::Ident),
    ))
    .expect(&"one of %, a capture group number, or an identifier")
    .parse_next(input)
}

fn interpolation_join(input: &mut Input) -> PResult<String> {
    const VALID_JOIN_SEPARATORS: &[char] = &['+', ',', '.', '|', '/', '\\', ':', ';', ' '];
    let sep: String = terminated(repeat(0.., one_of(VALID_JOIN_SEPARATORS)), '*')
        .expect(&"join separator")
        .parse_next(input)?;
    if sep.is_empty() {
        return Ok(String::from(" "));
    }
    Ok(sep)
}

// At least one interpolation option
fn interpolation_ops(input: &mut Input) -> PResult<Vec<ast::InterpolationOp>> {
    preceded(
        ':'.expect(&"interpolation options"),
        separated(0.., interpolation_op, ','),
    )
    .parse_next(input)
}

fn interpolation_op(input: &mut Input) -> PResult<ast::InterpolationOp> {
    alt((
        interpolation_op_replace_ext.map(|(from, to)| ast::InterpolationOp::ReplaceExtension {
            from: from.to_owned(),
            to: to.to_owned(),
        }),
        interpolation_op_regex_replace.map(ast::InterpolationOp::RegexReplace),
        interpolation_op_kw,
    ))
    .parse_next(input)
}

fn interpolation_op_kw(input: &mut Input) -> PResult<ast::InterpolationOp> {
    let location = input.current_token_start();
    let ident = ident_str.parse_next(input)?;
    match ident {
        "dedup" => Ok(ast::InterpolationOp::Dedup),
        "filename" => Ok(ast::InterpolationOp::Filename),
        "dir" => Ok(ast::InterpolationOp::Dirname),
        "ext" => Ok(ast::InterpolationOp::Ext),
        "out-dir" => Ok(ast::InterpolationOp::ResolveOutDir),
        "workspace" => Ok(ast::InterpolationOp::ResolveWorkspace),
        _ => Err(ModalErr::Error(Error::new(
            Offset(location as u32),
            Failure::InvalidInterpolationOp,
        ))),
    }
}

fn interpolation_op_replace_ext<'a>(input: &mut Input<'a>) -> PResult<(&'a str, &'a str)> {
    separated_pair(file_extension, '=', file_extension)
        .expect(&"replace extension operation in the form of '.ext1=.ext2' (periods required)")
        .parse_next(input)
}

fn interpolation_op_regex_replace(input: &mut Input) -> PResult<ast::RegexInterpolationOp> {
    winnow::combinator::seq! { ast::RegexInterpolationOp {
        _: "s/",
        regex: cut_err(regex_replace_pattern),
        _: '/',
        replacer: take_till(0.., ['/']).map(String::from),
        _: '/'
    }}
    .parse_next(input)
}

fn regex_replace_pattern(input: &mut Input) -> PResult<regex::Regex> {
    // TODO: Properly handle regex patterns containing slashes. Need to skip
    // ahead in the stream, skipping escaped slashes.
    let location = input.current_token_start();
    let regex_pattern = take_till(1.., ['/']).parse_next(input)?;

    regex::Regex::new(regex_pattern).map_err(|err| {
        ModalErr::Backtrack(Offset(location as u32), Failure::ValidRegex(Arc::new(err)))
    })
}

fn file_extension<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    preceded(
        '.',
        take_while(1.., |ch: char| {
            ch.is_alphanumeric() || ch == '.' || ch == '-' || ch.is_whitespace() || ch == '_'
        }),
    )
    .expect(&"file extension")
    .take()
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use werk_util::{span, Span};

    use super::*;

    #[test]
    fn string_escape() {
        let input = "*.\\{c,cpp\\}";
        let expected = ast::StringExpr {
            span: Span::from(0..input.len()),
            fragments: vec![ast::StringFragment::Literal("*.{c,cpp}".into())],
        };
        let result = super::string_expr_inside_quotes
            .parse(Input::new(input))
            .unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn string_expr() {
        let input = Input::new(r#""hello, world""#);
        assert_eq!(
            parse::<ast::StringExpr>.parse(input).unwrap(),
            ast::StringExpr {
                span: span(0..14),
                fragments: vec![ast::StringFragment::Literal("hello, world".into())]
            }
        );

        let input = Input::new(r#""hello, \"world\"""#);
        assert_eq!(
            parse::<ast::StringExpr>.parse(input).unwrap(),
            ast::StringExpr {
                span: span(0..18),
                fragments: vec![ast::StringFragment::Literal(r#"hello, "world""#.into())]
            }
        );

        let input = Input::new(r#""hello, \\\"world\\\"""#);
        assert_eq!(
            parse::<ast::StringExpr>.parse(input).unwrap(),
            ast::StringExpr {
                span: span(0..22),
                fragments: vec![ast::StringFragment::Literal(r#"hello, \"world\""#.into())]
            }
        );

        let input = "hello %world% {name} <1:.ext1=.ext2>";
        let expected = ast::StringExpr {
            span: Span::from(0..input.len()),
            fragments: vec![
                ast::StringFragment::Literal("hello ".into()),
                ast::StringFragment::PatternStem,
                ast::StringFragment::Literal("world".into()),
                ast::StringFragment::PatternStem,
                ast::StringFragment::Literal(" ".into()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::Ident("name".into()),
                    index: None,
                    options: None,
                }),
                ast::StringFragment::Literal(" ".into()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::CaptureGroup(1),
                    index: None,
                    options: Some(Box::new(ast::InterpolationOptions {
                        ops: vec![
                            ast::InterpolationOp::ReplaceExtension {
                                from: ".ext1".into(),
                                to: ".ext2".into(),
                            },
                            ast::InterpolationOp::ResolveOsPath,
                        ],
                        join: None,
                    })),
                }),
            ],
        };

        let result = super::string_expr_inside_quotes
            .parse(Input::new(input))
            .unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_join() {
        let plain_expansion = "*";
        assert_eq!(
            interpolation_join
                .parse(Input::new(plain_expansion))
                .unwrap(),
            " "
        );

        let explicit_space = " *";
        assert_eq!(
            interpolation_join
                .parse(Input::new(explicit_space))
                .unwrap(),
            " "
        );

        let comma = ",*";
        assert_eq!(interpolation_join.parse(Input::new(comma)).unwrap(), ",");

        let colon = ":*";
        assert_eq!(interpolation_join.parse(Input::new(colon)).unwrap(), ":");
    }

    #[test]
    fn test_stem() {
        let stem_ident = "name";
        assert_eq!(
            interpolation_stem.parse(Input::new(stem_ident)).unwrap(),
            ast::InterpolationStem::Ident("name".into())
        );

        let stem_pattern_stem = "%";
        assert_eq!(
            interpolation_stem
                .parse(Input::new(stem_pattern_stem))
                .unwrap(),
            ast::InterpolationStem::PatternCapture
        );

        let stem_pattern_stem = "123";
        assert_eq!(
            interpolation_stem
                .parse(Input::new(stem_pattern_stem))
                .unwrap(),
            ast::InterpolationStem::CaptureGroup(123)
        );
    }

    #[test]
    fn simple_interpolation() {
        let input = "{name}";
        assert_eq!(
            string_interpolation.parse(Input::new(input)).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: None
            }
        );
    }

    #[test]
    #[allow(clippy::literal_string_with_formatting_args)]
    fn test_interpolation_options_implicit() {
        let empty_options = "{:}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(empty_options))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: None
            }
        );

        let replace_ext_option = "{:.ext1=.ext2}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(replace_ext_option))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::ReplaceExtension {
                        from: ".ext1".into(),
                        to: ".ext2".into()
                    }],
                    join: None,
                })),
            }
        );

        let regex_replace_option = "{:s/regex/replacement/}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(regex_replace_option))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::RegexReplace(
                        ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: "replacement".into(),
                        }
                    )],
                    join: None,
                })),
            }
        );

        let both = "{:.ext1=.ext2,s/regex/replacement/}";
        assert_eq!(
            string_interpolation.parse(Input::new(both)).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![
                        ast::InterpolationOp::ReplaceExtension {
                            from: ".ext1".into(),
                            to: ".ext2".into()
                        },
                        ast::InterpolationOp::RegexReplace(ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: "replacement".into(),
                        }),
                    ],
                    join: None,
                })),
            }
        );
    }

    #[test]
    fn test_interpolation_options_var() {
        let empty_options = "{name:}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(empty_options))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: None
            }
        );

        let replace_ext_option = "{name:.ext1=.ext2}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(replace_ext_option))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::ReplaceExtension {
                        from: ".ext1".into(),
                        to: ".ext2".into()
                    }],
                    join: None,
                })),
            }
        );

        let regex_replace_option = "{name:s/regex/replacement/}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(regex_replace_option))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::RegexReplace(
                        ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: "replacement".into(),
                        }
                    )],
                    join: None,
                })),
            }
        );

        let both = "{name:.ext1=.ext2,s/regex/replacement/}";
        assert_eq!(
            string_interpolation.parse(Input::new(both)).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![
                        ast::InterpolationOp::ReplaceExtension {
                            from: ".ext1".into(),
                            to: ".ext2".into()
                        },
                        ast::InterpolationOp::RegexReplace(ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: "replacement".into(),
                        }),
                    ],
                    join: None,
                })),
            }
        );
    }

    #[test]
    fn test_expansion_implicit() {
        let implicit_expansion = "{*}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(implicit_expansion))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                })),
            }
        );

        let implicit_expansion_with_space = "{ *}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(implicit_expansion_with_space))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                })),
            }
        );

        let implicit_expansion_comma = "{,*}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(implicit_expansion_comma))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(",")),
                })),
            }
        );
    }

    #[test]
    fn test_expansion_var() {
        let implicit_expansion = "{name*}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(implicit_expansion))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                })),
            }
        );

        let implicit_expansion_with_space = "{name *}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(implicit_expansion_with_space))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                })),
            }
        );

        let implicit_expansion_comma = "{name,*}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(implicit_expansion_comma))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident("name".into()),
                index: None,
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(",")),
                })),
            }
        );
    }
}
