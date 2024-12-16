use std::sync::Arc;

use crate::{ast, ContextError, Expected, ParseError};
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

pub fn parse_ident(input: &str) -> Result<String, ParseError> {
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

    Ok(input.to_owned())
}

pub fn parse_string_expr(input: &str) -> Result<ast::StringExpr, ParseError> {
    string_expr.parse(input).map_err(Into::into)
}

pub fn parse_pattern_expr(input: &str) -> Result<ast::PatternExpr, ParseError> {
    pattern_expr.parse(input).map_err(Into::into)
}

fn ident(input: &mut &str) -> PResult<String> {
    (
        take_while(1, |ch| is_identifier_start(ch)),
        take_while(0.., |ch| is_identifier_continue(ch)),
    )
        .take()
        .parse_next(input)
        .map(ToOwned::to_owned)
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
        StringFragment::PatternStem => panic!("pattern in string expr"),
        StringFragment::OneOf(_) => panic!("pattern in string expr"),
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

fn string_expr(input: &mut &str) -> PResult<ast::StringExpr> {
    let mut build_string =
        repeat(0.., string_fragment).fold(ast::StringExpr::default, |mut expr, fragment| {
            push_string_fragment(&mut expr, fragment);
            expr
        });
    build_string.parse_next(input)
}

fn pattern_expr(input: &mut &str) -> PResult<ast::PatternExpr> {
    let mut build_pattern =
        repeat(0.., pattern_fragment).fold(ast::PatternExpr::default, |mut expr, fragment| {
            push_pattern_fragment(&mut expr, fragment);
            expr
        });
    build_pattern.parse_next(input)
}

fn string_fragment<'a>(input: &mut &'a str) -> PResult<StringFragment<'a>> {
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

fn pattern_fragment<'a>(input: &mut &'a str) -> PResult<StringFragment<'a>> {
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

fn string_literal<'a, const IS_PATTERN: bool>(input: &mut &'a str) -> PResult<&'a str> {
    let until = if IS_PATTERN {
        &['\\', '{', '}', '<', '>', '(', ')', '%'] as &[char]
    } else {
        &['\\', '{', '}', '<', '>'] as &[char]
    };

    take_till(1.., until)
        .verify(|s: &str| !s.is_empty())
        .parse_next(input)
}

fn escaped_char(input: &mut &str) -> PResult<char> {
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

fn escaped_whitespace<'a>(input: &mut &'a str) -> PResult<&'a str> {
    preceded('\\', multispace1).parse_next(input)
}

fn pattern_one_of(input: &mut &str) -> PResult<Vec<String>> {
    delimited(
        '(',
        // TODO: Allow more than just identifiers here.
        separated(1.., cut_err(ident), '|'),
        cut_err(')').context(Expected::ExpectedChar(')')),
    )
    .parse_next(input)
}

fn string_interpolation(input: &mut &str) -> PResult<ast::Interpolation> {
    delimited(
        '{',
        cut_err(interpolation_inner::<'}'>).context("interpolation contents"),
        cut_err('}').context(Expected::ExpectedChar('}')),
    )
    .context("{...} interpolation")
    .parse_next(input)
}

fn path_interpolation(input: &mut &str) -> PResult<ast::Interpolation> {
    let mut interp = delimited(
        '<',
        cut_err(interpolation_inner::<'>'>),
        cut_err('>').context(Expected::ExpectedChar('>')),
    )
    .context("<...> interpolation")
    .parse_next(input)?;

    interp.options.ops.push(ast::InterpolationOp::ResolveOsPath);

    Ok(interp)
}

fn interpolation_inner<const TERMINATE: char>(input: &mut &str) -> PResult<ast::Interpolation> {
    alt((
        // Interpolation with stem
        interpolation_inner_with_stem::<TERMINATE>,
        // Implied value with options: {join:options}
        interpolation_options.map(|options| ast::Interpolation {
            stem: ast::InterpolationStem::Implied,
            options,
        }),
        // Just the implied value with no options `{}`.
        preceded(space0, peek(TERMINATE)).map(|_| ast::Interpolation {
            stem: ast::InterpolationStem::Implied,
            options: ast::InterpolationOptions::default(),
        }),
    ))
    .context(Expected::Expected(&"interpolation expression"))
    .parse_next(input)
}

fn interpolation_inner_with_stem<const TERMINATE: char>(
    input: &mut &str,
) -> PResult<ast::Interpolation> {
    (
        interpolation_stem.context(Expected::Expected(&"interpolation stem")),
        alt((
            // {stem*}, {stem:...}, or {stem*:...}
            interpolation_options,
            // No options
            preceded(space0, peek(TERMINATE)).value(ast::InterpolationOptions::default()),
        ))
        .context(Expected::Expected(
            &"interpolation options or end of interpolation",
        )),
    )
        .context("interpolation with stem and options")
        .map(|(stem, options)| ast::Interpolation { stem, options })
        .parse_next(input)
}

fn interpolation_stem(input: &mut &str) -> PResult<ast::InterpolationStem> {
    alt((
        '%'.map(|_| ast::InterpolationStem::PatternCapture),
        digit1
            .try_map(str::parse)
            .map(ast::InterpolationStem::CaptureGroup),
        ident.map(ast::InterpolationStem::Ident),
    ))
    .context(Expected::Expected(
        &"one of %, a capture group number, or an identifier",
    ))
    .parse_next(input)
}

fn interpolation_options(input: &mut &str) -> PResult<ast::InterpolationOptions> {
    let join = opt(interpolation_join).parse_next(input)?;
    let ops = if join.is_some() {
        opt(interpolation_ops).parse_next(input)?
    } else {
        // If there is no join operator, `interpolation_options` should only
        // succeed if there are no other options.
        Some(interpolation_ops.parse_next(input)?)
    };
    Ok(ast::InterpolationOptions {
        join,
        ops: ops.unwrap_or_default(),
    })
}

fn interpolation_join(input: &mut &str) -> PResult<String> {
    const VALID_JOIN_SEPARATORS: &[char] = &['+', ',', '.', '|', '/', '\\', ':', ';', ' '];
    let mut sep: String = terminated(repeat(0.., one_of(VALID_JOIN_SEPARATORS)), '*')
        .context("join separator")
        .parse_next(input)?;
    if sep.is_empty() {
        sep = String::from(" ");
    }
    Ok(sep)
}

// At least one interpolation option
fn interpolation_ops(input: &mut &str) -> PResult<Vec<ast::InterpolationOp>> {
    preceded(':', separated(0.., interpolation_op, ','))
        .context("interpolation options")
        .parse_next(input)
}

fn interpolation_op(input: &mut &str) -> PResult<ast::InterpolationOp> {
    alt((
        interpolation_op_replace_ext.map(|(from, to)| {
            ast::InterpolationOp::ReplaceExtension(from.to_string(), to.to_string())
        }),
        interpolation_op_regex_replace
            .map(|regex_interpolation| ast::InterpolationOp::RegexReplace(regex_interpolation)),
    ))
    .parse_next(input)
}

fn interpolation_op_replace_ext<'a>(input: &mut &'a str) -> PResult<(&'a str, &'a str)> {
    separated_pair(file_extension, '=', file_extension)
        .context(Expected::Expected(
            &"replace extension operation in the form of '.ext1=.ext2' (periods required)",
        ))
        .parse_next(input)
}

fn interpolation_op_regex_replace(input: &mut &str) -> PResult<ast::RegexInterpolationOp> {
    winnow::combinator::seq! { ast::RegexInterpolationOp {
        _: "s/",
        regex: cut_err(regex_replace_pattern),
        _: '/',
        replacer: take_till(0.., ['/']).map(|s: &str| s.to_owned()),
        _: '/'
    }}
    .parse_next(input)
}

fn regex_replace_pattern(input: &mut &str) -> PResult<regex::Regex> {
    let regex_pattern = take_till(1.., ['/']).parse_next(input)?;

    regex::Regex::new(&regex_pattern).map_err(|err| {
        winnow::error::ErrMode::Backtrack(ContextError {
            stack: Vec::new(),
            expected: Expected::ValidRegex(Arc::new(err)),
        })
    })
}

fn file_extension<'a>(input: &mut &'a str) -> PResult<&'a str> {
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
    use super::*;

    #[test]
    fn test_string_expr() {
        let input = "hello %world% {name} <1:.ext1=.ext2>";
        let expected = ast::StringExpr {
            fragments: vec![
                ast::StringFragment::Literal("hello %world% ".to_owned()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::Ident("name".to_owned()),
                    options: ast::InterpolationOptions::default(),
                }),
                ast::StringFragment::Literal(" ".to_owned()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::CaptureGroup(1),
                    options: ast::InterpolationOptions {
                        ops: vec![
                            ast::InterpolationOp::ReplaceExtension(
                                ".ext1".to_owned(),
                                ".ext2".to_owned(),
                            ),
                            ast::InterpolationOp::ResolveOsPath,
                        ],
                        join: None,
                    },
                }),
            ],
        };

        let result = string_expr.parse(input).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_string_escape() {
        let input = "*.\\{c,cpp\\}";
        let expected = ast::StringExpr {
            fragments: vec![ast::StringFragment::Literal(String::from("*.{c,cpp}"))],
        };
        let result = string_expr.parse(input).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_join() {
        let plain_expansion = "*";
        assert_eq!(interpolation_join.parse(plain_expansion).unwrap(), " ");

        let explicit_space = " *";
        assert_eq!(interpolation_join.parse(explicit_space).unwrap(), " ");

        let comma = ",*";
        assert_eq!(interpolation_join.parse(comma).unwrap(), ",");

        let colon = ":*";
        assert_eq!(interpolation_join.parse(colon).unwrap(), ":");
    }

    #[test]
    fn test_stem() {
        let stem_ident = "name";
        assert_eq!(
            interpolation_stem.parse(stem_ident).unwrap(),
            ast::InterpolationStem::Ident(String::from("name"))
        );

        let stem_pattern_stem = "%";
        assert_eq!(
            interpolation_stem.parse(stem_pattern_stem).unwrap(),
            ast::InterpolationStem::PatternCapture
        );

        let stem_pattern_stem = "123";
        assert_eq!(
            interpolation_stem.parse(stem_pattern_stem).unwrap(),
            ast::InterpolationStem::CaptureGroup(123)
        );
    }

    #[test]
    fn simple_interpolation() {
        let input = "{name}";
        assert_eq!(
            string_interpolation.parse(input).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions::default(),
            }
        );
    }

    #[test]
    fn test_interpolation_options_implicit() {
        let empty_options = "{:}";
        assert_eq!(
            string_interpolation.parse(empty_options).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions::default(),
            }
        );

        let replace_ext_option = "{:.ext1=.ext2}";
        assert_eq!(
            string_interpolation.parse(replace_ext_option).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::ReplaceExtension(
                        ".ext1".to_owned(),
                        ".ext2".to_owned()
                    )],
                    join: None,
                },
            }
        );

        let regex_replace_option = "{:s/regex/replacement/}";
        assert_eq!(
            string_interpolation.parse(regex_replace_option).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::RegexReplace(
                        ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: String::from("replacement"),
                        }
                    )],
                    join: None,
                },
            }
        );

        let both = "{:.ext1=.ext2,s/regex/replacement/}";
        assert_eq!(
            string_interpolation.parse(both).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions {
                    ops: vec![
                        ast::InterpolationOp::ReplaceExtension(
                            ".ext1".to_owned(),
                            ".ext2".to_owned()
                        ),
                        ast::InterpolationOp::RegexReplace(ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: String::from("replacement"),
                        }),
                    ],
                    join: None,
                },
            }
        );
    }

    #[test]
    fn test_interpolation_options_var() {
        let empty_options = "{name:}";
        assert_eq!(
            string_interpolation.parse(empty_options).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions::default(),
            }
        );

        let replace_ext_option = "{name:.ext1=.ext2}";
        assert_eq!(
            string_interpolation.parse(replace_ext_option).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::ReplaceExtension(
                        ".ext1".to_owned(),
                        ".ext2".to_owned()
                    )],
                    join: None,
                },
            }
        );

        let regex_replace_option = "{name:s/regex/replacement/}";
        assert_eq!(
            string_interpolation.parse(regex_replace_option).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions {
                    ops: vec![ast::InterpolationOp::RegexReplace(
                        ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: String::from("replacement"),
                        }
                    )],
                    join: None,
                },
            }
        );

        let both = "{name:.ext1=.ext2,s/regex/replacement/}";
        assert_eq!(
            string_interpolation.parse(both).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions {
                    ops: vec![
                        ast::InterpolationOp::ReplaceExtension(
                            ".ext1".to_owned(),
                            ".ext2".to_owned()
                        ),
                        ast::InterpolationOp::RegexReplace(ast::RegexInterpolationOp {
                            regex: regex::Regex::new("regex").unwrap(),
                            replacer: String::from("replacement"),
                        }),
                    ],
                    join: None,
                },
            }
        );
    }

    #[test]
    fn test_expansion_implicit() {
        let implicit_expansion = "{*}";
        assert_eq!(
            string_interpolation.parse(implicit_expansion).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                },
            }
        );

        let implicit_expansion_with_space = "{ *}";
        assert_eq!(
            string_interpolation
                .parse(implicit_expansion_with_space)
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                },
            }
        );

        let implicit_expansion_comma = "{,*}";
        assert_eq!(
            string_interpolation
                .parse(implicit_expansion_comma)
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Implied,
                options: ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(",")),
                },
            }
        );
    }

    #[test]
    fn test_expansion_var() {
        let implicit_expansion = "{name*}";
        assert_eq!(
            string_interpolation.parse(implicit_expansion).unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                },
            }
        );

        let implicit_expansion_with_space = "{name *}";
        assert_eq!(
            string_interpolation
                .parse(implicit_expansion_with_space)
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(" ")),
                },
            }
        );

        let implicit_expansion_comma = "{name,*}";
        assert_eq!(
            string_interpolation
                .parse(implicit_expansion_comma)
                .unwrap(),
            ast::Interpolation {
                stem: ast::InterpolationStem::Ident(String::from("name")),
                options: ast::InterpolationOptions {
                    ops: vec![],
                    join: Some(String::from(",")),
                },
            }
        );
    }
}
