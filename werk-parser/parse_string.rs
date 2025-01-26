use std::{borrow::Cow, sync::Arc};

use crate::{
    ast,
    parser::{Input, Offset, TokenParserExt as _},
    Failure, ParseError, TomlParseError,
};
use winnow::{
    ascii::{digit1, multispace1, space0},
    combinator::{
        alt, cut_err, delimited, opt, peek, preceded, repeat, separated, separated_pair, terminated,
    },
    stream::Location as _,
    token::{one_of, take_till, take_while},
    Parser,
};

pub type PResult<T> = winnow::PResult<T, ParseError>;

fn is_identifier_start(ch: char) -> bool {
    unicode_ident::is_xid_start(ch)
}

fn is_identifier_continue(ch: char) -> bool {
    // Allow kebab-case identifiers
    ch == '-' || unicode_ident::is_xid_continue(ch)
}

pub fn parse_ident(input: &str) -> Result<&str, TomlParseError> {
    let mut chars = input.chars();
    let Some(first) = chars.next() else {
        return Err(TomlParseError::EmptyIdentifier);
    };

    if !is_identifier_start(first) {
        return Err(TomlParseError::InvalidIdentifier(first));
    }

    for ch in chars {
        if !is_identifier_continue(ch) {
            return Err(TomlParseError::InvalidIdentifier(ch));
        }
    }

    Ok(input)
}

pub fn parse_string_expr(input: &str) -> Result<ast::StringExpr<'_>, TomlParseError> {
    string_expr_inside_quotes
        .parse(Input::new(input))
        .map_err(Into::into)
}

pub fn parse_pattern_expr(input: &str) -> Result<ast::PatternExpr, TomlParseError> {
    pattern_expr_inside_quotes
        .parse(Input::new(input))
        .map_err(Into::into)
}

fn ident<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    (
        take_while(1, is_identifier_start),
        take_while(0.., is_identifier_continue),
    )
        .expect(&"identifier")
        .take()
        .parse_next(input)
}

#[derive(Debug, Clone)]
pub(crate) enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWhitespace,
    Interpolation(ast::Interpolation<'a>),
    PatternStem,
    OneOf(Vec<Cow<'a, str>>),
}

pub(crate) fn push_string_fragment<'a>(expr: &mut ast::StringExpr<'a>, frag: StringFragment<'a>) {
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

pub(crate) fn push_pattern_fragment<'a>(expr: &mut ast::PatternExpr<'a>, frag: StringFragment<'a>) {
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

fn string_fragment<'a>(input: &mut Input<'a>) -> PResult<StringFragment<'a>> {
    // TODO: Consider escape sequences etc.
    alt((
        string_literal::<false>.map(StringFragment::Literal),
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
        string_literal::<true>.map(StringFragment::Literal),
        escaped_char.map(StringFragment::EscapedChar),
        escaped_whitespace.value(StringFragment::EscapedWhitespace),
        string_interpolation.map(StringFragment::Interpolation),
        path_interpolation.map(StringFragment::Interpolation),
    ))
    .parse_next(input)
}

fn string_literal<'a, const IS_PATTERN: bool>(input: &mut Input<'a>) -> PResult<&'a str> {
    let until = if IS_PATTERN {
        &['\\', '{', '}', '<', '>', '(', ')', '%'] as &[char]
    } else {
        &['\\', '{', '}', '<', '>'] as &[char]
    };

    take_till(1.., until)
        .verify(|s: &str| !s.is_empty())
        .parse_next(input)
}

fn escaped_char(input: &mut Input) -> PResult<char> {
    preceded(
        '\\',
        alt((
            '\\'.value('\\'),
            '{'.value('{'),
            '}'.value('}'),
            '<'.value('<'),
            '>'.value('>'),
            '('.value('('),
            ')'.value(')'),
            '%'.value('%'),
        )),
    )
    .parse_next(input)
}

fn escaped_whitespace<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    preceded('\\', multispace1).parse_next(input)
}

pub(crate) fn pattern_one_of<'a>(input: &mut Input<'a>) -> PResult<Vec<Cow<'a, str>>> {
    delimited(
        '('.expect(&"start of pattern one-of group"),
        // TODO: Allow more than just identifiers here.
        separated(1.., cut_err(ident).map(Cow::Borrowed), '|'),
        ')'.or_cut(Failure::ExpectedChar(')')),
    )
    .while_parsing("pattern capture group")
    .parse_next(input)
}

pub(crate) fn string_interpolation<'a>(input: &mut Input<'a>) -> PResult<ast::Interpolation<'a>> {
    delimited(
        '{'.expect(&"string interpolation block"),
        cut_err(interpolation_inner::<'}'>),
        '}'.or_cut(Failure::ExpectedChar('}')),
    )
    .while_parsing("string interpolation block")
    .parse_next(input)
}

pub(crate) fn path_interpolation<'a>(input: &mut Input<'a>) -> PResult<ast::Interpolation<'a>> {
    let mut interp = delimited(
        '<'.expect(&"path interpolation block"),
        cut_err(interpolation_inner::<'>'>),
        '>'.or_cut(Failure::ExpectedChar('>')),
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

fn interpolation_stem<'a>(input: &mut Input<'a>) -> PResult<ast::InterpolationStem<'a>> {
    alt((
        '%'.value(ast::InterpolationStem::PatternCapture),
        digit1
            .try_map(str::parse)
            .map(ast::InterpolationStem::CaptureGroup),
        ident.map(Cow::Borrowed).map(ast::InterpolationStem::Ident),
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
        interpolation_op_replace_ext.map(|(from, to)| {
            ast::InterpolationOp::ReplaceExtension(Cow::from(from), Cow::from(to))
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
    let location = input.location();
    let regex_pattern = take_till(1.., ['/']).parse_next(input)?;

    regex::Regex::new(regex_pattern).map_err(|err| {
        winnow::error::ErrMode::Backtrack(ParseError {
            context: None,
            fail: Failure::ValidRegex(Arc::new(err)),
            offset: Offset(location as u32),
        })
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
    use crate::parser::Span;

    use super::*;

    #[test]
    fn test_string_expr() {
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
                            ast::InterpolationOp::ReplaceExtension(".ext1".into(), ".ext2".into()),
                            ast::InterpolationOp::ResolveOsPath,
                        ],
                        join: None,
                    })),
                }),
            ],
        };

        let result = string_expr_inside_quotes.parse(Input::new(input)).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_string_escape() {
        let input = "*.\\{c,cpp\\}";
        let expected = ast::StringExpr {
            span: Span::from(0..input.len()),
            fragments: vec![ast::StringFragment::Literal("*.{c,cpp}".into())],
        };
        let result = string_expr_inside_quotes.parse(Input::new(input)).unwrap();
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
                    ops: vec![ast::InterpolationOp::ReplaceExtension(
                        ".ext1".into(),
                        ".ext2".into()
                    )],
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
                        ast::InterpolationOp::ReplaceExtension(".ext1".into(), ".ext2".into()),
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
                    ops: vec![ast::InterpolationOp::ReplaceExtension(
                        ".ext1".into(),
                        ".ext2".into()
                    )],
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
                        ast::InterpolationOp::ReplaceExtension(".ext1".into(), ".ext2".into()),
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
