use std::{borrow::Cow, sync::Arc};

use crate::{
    ast,
    parser::{Input, TokenParserExt as _},
    ContextError, Expected, ParseError,
};
use winnow::{
    ascii::{digit1, multispace1, space0},
    combinator::{
        alt, cut_err, delimited, opt, peek, preceded, repeat, separated, separated_pair, terminated,
    },
    token::{one_of, take_till, take_while},
    Parser,
};

pub type PResult<T> = winnow::PResult<T, ContextError>;

fn is_identifier_start(ch: char) -> bool {
    unicode_ident::is_xid_start(ch)
}

fn is_identifier_continue(ch: char) -> bool {
    // Allow kebab-case identifiers
    ch == '-' || unicode_ident::is_xid_continue(ch)
}

pub fn parse_ident(input: &str) -> Result<&str, ParseError> {
    let mut chars = input.chars();
    let Some(first) = chars.next() else {
        return Err(ParseError::EmptyIdentifier);
    };

    if !is_identifier_start(first) {
        return Err(ParseError::InvalidIdentifier(first));
    }

    while let Some(ch) = chars.next() {
        if !is_identifier_continue(ch) {
            return Err(ParseError::InvalidIdentifier(ch));
        }
    }

    Ok(input)
}

pub fn parse_string_expr(input: &str) -> Result<ast::StringExpr<'_>, ParseError> {
    string_expr_inside_quotes
        .parse(Input::new(input))
        .map_err(Into::into)
}

pub fn parse_pattern_expr(input: &str) -> Result<ast::PatternExpr, ParseError> {
    pattern_expr_inside_quotes
        .parse(Input::new(input))
        .map_err(Into::into)
}

fn ident<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    (
        take_while(1, |ch| is_identifier_start(ch)),
        take_while(0.., |ch| is_identifier_continue(ch)),
    )
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
        StringFragment::PatternStem => panic!("pattern in string expr"),
        StringFragment::OneOf(_) => panic!("pattern in string expr"),
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
    .context("string fragment")
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
    .context("pattern fragment")
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
        '(',
        // TODO: Allow more than just identifiers here.
        separated(1.., cut_err(ident).map(Cow::Borrowed), '|'),
        cut_err(')').context(Expected::ExpectedChar(')')),
    )
    .parse_next(input)
}

pub(crate) fn string_interpolation<'a>(input: &mut Input<'a>) -> PResult<ast::Interpolation<'a>> {
    delimited(
        '{',
        cut_err(interpolation_inner::<'}'>).context("interpolation contents"),
        cut_err('}').context(Expected::ExpectedChar('}')),
    )
    .context("{...} interpolation")
    .parse_next(input)
}

pub(crate) fn path_interpolation<'a>(input: &mut Input<'a>) -> PResult<ast::Interpolation<'a>> {
    let mut interp = delimited(
        '<',
        cut_err(interpolation_inner::<'>'>),
        cut_err('>').context(Expected::ExpectedChar('>')),
    )
    .context("<...> interpolation")
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
    .context(Expected::Expected(&"interpolation expression"))
    .parse_next(input)
}

fn interpolation_inner_with_stem<'a, const TERMINATE: char>(
    input: &mut Input<'a>,
) -> PResult<ast::Interpolation<'a>> {
    (
        interpolation_stem.context(Expected::Expected(&"interpolation stem")),
        alt((
            // {stem*}, {stem:...}, or {stem*:...}
            interpolation_options.map(|options| options.map(Box::new)),
            // No options
            preceded(space0, peek(TERMINATE)).value(None),
        ))
        .context(Expected::Expected(
            &"interpolation options or end of interpolation",
        )),
    )
        .context("interpolation with stem and options")
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
    .context(Expected::Expected(
        &"one of %, a capture group number, or an identifier",
    ))
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
        (join, Some(ops)) => Some(ast::InterpolationOptions { join, ops }),
    })
}

fn interpolation_join<'a>(input: &mut Input<'a>) -> PResult<Cow<'a, str>> {
    const VALID_JOIN_SEPARATORS: &[char] = &['+', ',', '.', '|', '/', '\\', ':', ';', ' '];
    let sep: String = terminated(repeat(0.., one_of(VALID_JOIN_SEPARATORS)), '*')
        .context("join separator")
        .parse_next(input)?;
    if sep.is_empty() {
        return Ok(Cow::Borrowed(" "));
    }
    Ok(Cow::Owned(sep))
}

// At least one interpolation option
fn interpolation_ops<'a>(input: &mut Input<'a>) -> PResult<Vec<ast::InterpolationOp<'a>>> {
    preceded(':', separated(0.., interpolation_op, ','))
        .context("interpolation options")
        .parse_next(input)
}

fn interpolation_op<'a>(input: &mut Input<'a>) -> PResult<ast::InterpolationOp<'a>> {
    alt((
        interpolation_op_replace_ext.map(|(from, to)| {
            ast::InterpolationOp::ReplaceExtension(Cow::from(from), Cow::from(to))
        }),
        interpolation_op_regex_replace
            .map(|regex_interpolation| ast::InterpolationOp::RegexReplace(regex_interpolation)),
    ))
    .parse_next(input)
}

fn interpolation_op_replace_ext<'a>(input: &mut Input<'a>) -> PResult<(&'a str, &'a str)> {
    separated_pair(file_extension, '=', file_extension)
        .context(Expected::Expected(
            &"replace extension operation in the form of '.ext1=.ext2' (periods required)",
        ))
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
    let regex_pattern = take_till(1.., ['/']).parse_next(input)?;

    regex::Regex::new(&regex_pattern).map_err(|err| {
        winnow::error::ErrMode::Backtrack(ContextError {
            stack: Vec::new(),
            expected: Expected::ValidRegex(Arc::new(err)),
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
    .context("file extension")
    .take()
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::span;

    use super::*;

    #[test]
    fn test_string_expr() {
        let input = "hello %world% {name} <1:.ext1=.ext2>";
        let expected = ast::StringExpr {
            span: span(0..input.len()),
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
            span: span(0..input.len()),
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
