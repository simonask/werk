use std::{borrow::Cow, fmt::Write, sync::Arc};

use crate::{
    ast::{self, token},
    fatal,
    parser::{Input, Offset, Parser as _},
    Error, Failure, ModalErr,
};
use werk_util::Symbol;
use winnow::{
    ascii::{digit1, multispace1, space0},
    combinator::{
        alt, cut_err, delimited, empty, opt, peek, preceded, repeat, separated, separated_pair,
        terminated,
    },
    stream::Location,
    token::{any, one_of, take_till, take_while},
    Parser,
};

use super::{parse, PResult, Parse};

impl<'a> Parse<'a> for ast::StringExpr<'a> {
    fn parse(input: &mut Input<'a>) -> PResult<Self> {
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

impl<'a> Parse<'a> for ast::PatternExpr<'a> {
    fn parse(input: &mut Input<'a>) -> PResult<Self> {
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

fn string_expr_inside_quotes<'a>(input: &mut Input<'a>) -> PResult<ast::StringExpr<'a>> {
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

fn pattern_expr_inside_quotes<'a>(input: &mut Input<'a>) -> PResult<ast::PatternExpr<'a>> {
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
pub fn parse_pattern_expr_unquoted(pattern: &str) -> Result<ast::PatternExpr<'_>, Error> {
    pattern_expr_inside_quotes
        .parse(Input::new(pattern))
        .map_err(winnow::error::ParseError::into_inner)
}

/// Parse the inside of a string expr (i.e. without surrounding quotes). This
/// is used for deserialization and testing.
pub fn parse_string_expr_unquoted(string: &str) -> Result<ast::StringExpr<'_>, Error> {
    string_expr_inside_quotes
        .parse(Input::new(string))
        .map_err(winnow::error::ParseError::into_inner)
}

fn string_fragment<'a>(input: &mut Input<'a>) -> PResult<StringFragment<'a>> {
    // TODO: Consider escape sequences etc.
    alt((
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
    matches!(ch, '\\' | '{' | '}' | '<' | '>' | '"')
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
pub struct Escape<'a, const IS_PATTERN: bool>(pub &'a str);
impl<const IS_PATTERN: bool> std::fmt::Display for Escape<'_, IS_PATTERN> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
pub fn escape_string_literal(literal: &str) -> String {
    format!("{}", Escape::<false>(literal))
}

#[inline]
#[must_use]
pub fn escape_pattern_literal(literal: &str) -> String {
    format!("{}", Escape::<true>(literal))
}

fn string_literal_fragment<'a, const IS_PATTERN: bool>(input: &mut Input<'a>) -> PResult<&'a str> {
    take_till(1.., needs_escape::<IS_PATTERN>)
        .verify(|s: &str| !s.is_empty())
        .parse_next(input)
}

fn escaped_char(input: &mut Input) -> Result<char, ModalErr> {
    let escape_seq_char = winnow::combinator::dispatch! {
        any::<Input<'_>, ModalErr>;
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

fn ident(input: &mut Input<'_>) -> PResult<Symbol> {
    (
        take_while(1, is_identifier_start),
        take_while(0.., is_identifier_continue),
    )
        .expect(&"identifier")
        .take()
        .map(Symbol::new)
        .parse_next(input)
}

#[derive(Debug, Clone)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWhitespace,
    Interpolation(ast::Interpolation<'a>),
    PatternStem,
    OneOf(Vec<Cow<'a, str>>),
}

fn push_string_fragment<'a>(expr: &mut ast::StringExpr<'a>, frag: StringFragment<'a>) {
    match frag {
        StringFragment::Literal(lit) => {
            if let Some(ast::StringFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.to_mut().push_str(lit);
            } else {
                expr.fragments
                    .push(ast::StringFragment::Literal(Cow::Borrowed(lit)));
            }
        }
        StringFragment::EscapedChar(ch) => {
            if let Some(ast::StringFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.to_mut().push(ch);
            } else {
                expr.fragments
                    .push(ast::StringFragment::Literal(Cow::Owned(ch.to_string())));
            }
        }
        StringFragment::EscapedWhitespace => {}
        StringFragment::Interpolation(string_interpolation) => {
            expr.fragments
                .push(ast::StringFragment::Interpolation(string_interpolation));
        }
        StringFragment::PatternStem => panic!("pattern stem in string expr must be escaped"),
        StringFragment::OneOf(_) => panic!("captue group in string expr must be escaped"),
    }
}

fn push_pattern_fragment<'a>(expr: &mut ast::PatternExpr<'a>, frag: StringFragment<'a>) {
    match frag {
        StringFragment::Literal(lit) => {
            if let Some(ast::PatternFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.to_mut().push_str(lit);
            } else {
                expr.fragments
                    .push(ast::PatternFragment::Literal(Cow::Borrowed(lit)));
            }
        }
        StringFragment::EscapedChar(ch) => {
            if let Some(ast::PatternFragment::Literal(ref mut last)) = expr.fragments.last_mut() {
                last.to_mut().push(ch);
            } else {
                expr.fragments
                    .push(ast::PatternFragment::Literal(Cow::Owned(ch.to_string())));
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

fn pattern_one_of<'a>(input: &mut Input<'a>) -> PResult<Vec<Cow<'a, str>>> {
    delimited(
        parse::<token::ParenOpen>.expect(&"start of pattern one-of group"),
        // TODO: Allow more than just identifiers here.
        separated(1.., cut_err(ident).map(|s| Cow::Borrowed(s.as_str())), '|'),
        cut_err(parse::<token::ParenClose>),
    )
    .while_parsing("pattern capture group")
    .parse_next(input)
}

fn string_interpolation<'a>(input: &mut Input<'a>) -> PResult<ast::Interpolation<'a>> {
    delimited(
        parse::<token::BraceOpen>.expect(&"string interpolation block"),
        cut_err(interpolation_inner::<'}'>),
        cut_err(parse::<token::BraceClose>),
    )
    .while_parsing("string interpolation block")
    .parse_next(input)
}

fn path_interpolation<'a>(input: &mut Input<'a>) -> PResult<ast::Interpolation<'a>> {
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

fn interpolation_inner<'a, const TERMINATE: char>(
    input: &mut Input<'a>,
) -> PResult<ast::Interpolation<'a>> {
    alt((
        // Interpolation with stem
        interpolation_inner_with_stem::<TERMINATE>,
        // Implied value with options: {join:options}
        interpolation_options.map(|options| ast::Interpolation {
            stem: ast::InterpolationStem::Implied,
            options: options.map(Box::new),
        }),
        // Just the implied value with no options `{}`.
        preceded(space0, peek(TERMINATE)).map(|_| ast::Interpolation {
            stem: ast::InterpolationStem::Implied,
            options: None,
        }),
    ))
    .parse_next(input)
}

fn interpolation_inner_with_stem<'a, const TERMINATE: char>(
    input: &mut Input<'a>,
) -> PResult<ast::Interpolation<'a>> {
    (
        interpolation_stem.expect(&"interpolation stem"),
        alt((
            // {stem*}, {stem:...}, or {stem*:...}
            interpolation_options.map(|options| options.map(Box::new)),
            // No options
            preceded(space0, peek(TERMINATE)).value(None),
        ))
        .expect(&"interpolation options or end of interpolation"),
    )
        .map(|(stem, options)| ast::Interpolation { stem, options })
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

fn interpolation_options<'a>(
    input: &mut Input<'a>,
) -> PResult<Option<ast::InterpolationOptions<'a>>> {
    let join = opt(interpolation_join).parse_next(input)?;
    let ops = if join.is_some() {
        opt(interpolation_ops).parse_next(input)?
    } else {
        // If there is no join operator, `interpolation_options` should only
        // succeed if there are other options.
        Some(interpolation_ops.parse_next(input)?)
    };
    Ok(match (join, ops) {
        (None, None) => None,
        (Some(join), None) => Some(ast::InterpolationOptions {
            join: Some(join),
            ops: Vec::new(),
        }),
        (None, Some(ops)) if ops.is_empty() => None,
        (join, Some(ops)) => Some(ast::InterpolationOptions { ops, join }),
    })
}

fn interpolation_join<'a>(input: &mut Input<'a>) -> PResult<Cow<'a, str>> {
    const VALID_JOIN_SEPARATORS: &[char] = &['+', ',', '.', '|', '/', '\\', ':', ';', ' '];
    let sep: String = terminated(repeat(0.., one_of(VALID_JOIN_SEPARATORS)), '*')
        .expect(&"join separator")
        .parse_next(input)?;
    if sep.is_empty() {
        return Ok(Cow::Borrowed(" "));
    }
    Ok(Cow::Owned(sep))
}

// At least one interpolation option
fn interpolation_ops<'a>(input: &mut Input<'a>) -> PResult<Vec<ast::InterpolationOp<'a>>> {
    preceded(':', separated(0.., interpolation_op, ','))
        .expect(&"interpolation options")
        .parse_next(input)
}

fn interpolation_op<'a>(input: &mut Input<'a>) -> PResult<ast::InterpolationOp<'a>> {
    alt((
        interpolation_op_replace_ext.map(|(from, to)| ast::InterpolationOp::ReplaceExtension {
            from: Cow::from(from),
            to: Cow::from(to),
        }),
        interpolation_op_regex_replace.map(ast::InterpolationOp::RegexReplace),
    ))
    .parse_next(input)
}

fn interpolation_op_replace_ext<'a>(input: &mut Input<'a>) -> PResult<(&'a str, &'a str)> {
    separated_pair(file_extension, '=', file_extension)
        .expect(&"replace extension operation in the form of '.ext1=.ext2' (periods required)")
        .parse_next(input)
}

fn interpolation_op_regex_replace<'a>(
    input: &mut Input<'a>,
) -> PResult<ast::RegexInterpolationOp<'a>> {
    winnow::combinator::seq! { ast::RegexInterpolationOp {
        _: "s/",
        regex: cut_err(regex_replace_pattern),
        _: '/',
        replacer: take_till(0.., ['/']).map(Cow::Borrowed),
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
    use crate::parser::{parse, span, Span};

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
                ast::StringFragment::Literal("hello %world% ".into()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::Ident("name".into()),
                    options: None,
                }),
                ast::StringFragment::Literal(" ".into()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::CaptureGroup(1),
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
                options: None
            }
        );
    }

    #[test]
    fn test_interpolation_options_implicit() {
        let empty_options = "{:}";
        assert_eq!(
            string_interpolation
                .parse(Input::new(empty_options))
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
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
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(Cow::from(" ")),
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
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(Cow::from(" ")),
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
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(Cow::from(",")),
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
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(Cow::from(" ")),
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
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(Cow::from(" ")),
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
                options: Some(Box::new(ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(Cow::from(",")),
                })),
            }
        );
    }
}
