use crate::{ast, ParseError};
use winnow::{
    ascii::digit1,
    combinator::{alt, cut_err, delimited, opt, peek, preceded, repeat, separated, separated_pair},
    error::{
        StrContext::{self, Expected},
        StrContextValue::Description,
    },
    token::take_while,
    PResult, Parser,
};

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
    string_expr
        .parse(input)
        .map_err(|e| ParseError::InvalidExpr(e.into_inner().into()))
}

pub fn parse_pattern_expr(input: &str) -> Result<ast::PatternExpr, ParseError> {
    pattern_expr
        .parse(input)
        .map_err(|e| ParseError::InvalidExpr(e.into_inner().into()))
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

fn string_expr(input: &mut &str) -> PResult<ast::StringExpr> {
    let fragments = repeat(0.., string_fragment)
        .context(StrContext::Label("string expression"))
        .parse_next(input)?;
    Ok(ast::StringExpr { fragments })
}

fn pattern_expr(input: &mut &str) -> PResult<ast::PatternExpr> {
    let fragments = repeat(0.., pattern_fragment)
        .context(StrContext::Label("pattern expression"))
        .parse_next(input)?;
    Ok(ast::PatternExpr { fragments })
}

fn string_fragment(input: &mut &str) -> PResult<ast::StringFragment> {
    // TODO: Consider escape sequences etc.
    alt((
        "\\{".map(|_| ast::StringFragment::Literal(String::from("{{"))),
        "\\}".map(|_| ast::StringFragment::Literal(String::from("}}"))),
        "\\<".map(|_| ast::StringFragment::Literal(String::from("<"))),
        "\\>".map(|_| ast::StringFragment::Literal(String::from(">"))),
        string_interpolation.map(ast::StringFragment::Interpolation),
        path_interpolation.map(ast::StringFragment::Interpolation),
        string_literal_fragment.map(ast::StringFragment::Literal),
    ))
    .context(StrContext::Label("string fragment"))
    .parse_next(input)
}

fn pattern_fragment(input: &mut &str) -> PResult<ast::PatternFragment> {
    alt((
        "\\{".map(|_| ast::PatternFragment::Literal(String::from("{"))),
        "\\}".map(|_| ast::PatternFragment::Literal(String::from("}"))),
        "\\<".map(|_| ast::PatternFragment::Literal(String::from("<"))),
        "\\>".map(|_| ast::PatternFragment::Literal(String::from(">"))),
        "\\(".map(|_| ast::PatternFragment::Literal(String::from("("))),
        "\\)".map(|_| ast::PatternFragment::Literal(String::from(")"))),
        "\\%".map(|_| ast::PatternFragment::Literal(String::from("%"))),
        '%'.value(ast::PatternFragment::PatternStem),
        string_interpolation.map(ast::PatternFragment::Interpolation),
        path_interpolation.map(ast::PatternFragment::Interpolation),
        pattern_one_of.map(ast::PatternFragment::OneOf),
        pattern_literal_fragment.map(ast::PatternFragment::Literal),
    ))
    .context(StrContext::Label("pattern fragment"))
    .parse_next(input)
}

pub const SPECIAL_STRING_CHARS: &str = r#"{}<>"#;
pub const SPECIAL_PATTERN_CHARS: &str = r#"{}<>()%"#;

fn string_literal_fragment(input: &mut &str) -> PResult<String> {
    take_while(1.., |ch| !SPECIAL_STRING_CHARS.contains(ch))
        .take()
        .map(ToOwned::to_owned)
        .parse_next(input)
}

fn pattern_literal_fragment(input: &mut &str) -> PResult<String> {
    take_while(1.., |ch| !SPECIAL_PATTERN_CHARS.contains(ch))
        .take()
        .map(ToOwned::to_owned)
        .parse_next(input)
}

fn pattern_one_of(input: &mut &str) -> PResult<Vec<String>> {
    delimited('(', separated(1.., cut_err(ident), '|'), ')').parse_next(input)
}

fn string_interpolation(input: &mut &str) -> PResult<ast::StringInterpolation> {
    let (stem, join, operation) = delimited('{', cut_err(interpolation_inner::<'}'>), '}')
        .context(StrContext::Label("{...} interpolation"))
        .parse_next(input)?;
    Ok(ast::StringInterpolation {
        stem,
        operation,
        interpolate_as_resolved_path: false,
        join,
    })
}

fn path_interpolation(input: &mut &str) -> PResult<ast::StringInterpolation> {
    let (stem, join, operation) = delimited('<', cut_err(interpolation_inner::<'>'>), '>')
        .context(StrContext::Label("<...> interpolation"))
        .parse_next(input)?;
    Ok(ast::StringInterpolation {
        stem,
        operation,
        interpolate_as_resolved_path: true,
        join,
    })
}

fn interpolation_inner<const TERMINATE: char>(
    input: &mut &str,
) -> PResult<(
    ast::StringInterpolationStem,
    Option<char>,
    Option<ast::StringInterpolationOperation>,
)> {
    alt((
        // `{}`
        peek(TERMINATE).value((ast::StringInterpolationStem::Implied, None, None)),
        // `{:...}`
        (
            ':'.value(ast::StringInterpolationStem::Implied),
            cut_err(string_interpolation_op),
        )
            .map(|(stem, op)| (stem, None, Some(op))),
        // `{stem*:...}`
        (
            alt((
                '*'.value((ast::StringInterpolationStem::Implied, Some(' '))),
                (cut_err(string_interpolation_stem), opt('*'.value(' '))),
            )),
            opt(preceded(':', cut_err(string_interpolation_op))),
        )
            .map(|((stem, join), operation)| (stem, join, operation)),
        // `{*}`
        '*'.value((ast::StringInterpolationStem::Implied, Some(' '), None)),
    ))
    .parse_next(input)
}

fn string_interpolation_stem(input: &mut &str) -> PResult<ast::StringInterpolationStem> {
    alt((
        "%".map(|_| ast::StringInterpolationStem::PatternCapture),
        digit1
            .try_map(str::parse)
            .map(ast::StringInterpolationStem::CaptureGroup),
        ident.map(ast::StringInterpolationStem::Ident),
    ))
    .context(Expected(Description(
        "one of %, a capture group number, or an identifier",
    )))
    .parse_next(input)
}

fn string_interpolation_op(input: &mut &str) -> PResult<ast::StringInterpolationOperation> {
    alt((
        separated_pair(file_extension, '=', file_extension).context(Expected(Description(
            "replace extension operation in the form of '.ext1=.ext2' (periods required)",
        ))),
    ))
    .map(|(a, b)| ast::StringInterpolationOperation::ReplaceExtension(a.to_owned(), b.to_owned()))
    .parse_next(input)
}

fn file_extension<'a>(input: &mut &'a str) -> PResult<&'a str> {
    preceded(
        '.',
        take_while(1.., |ch: char| {
            ch.is_alphanumeric() || ch == '.' || ch == '-' || ch.is_whitespace() || ch == '_'
        }),
    )
    .context(StrContext::Label("file extension"))
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
                ast::StringFragment::Interpolation(ast::StringInterpolation {
                    stem: ast::StringInterpolationStem::Ident("name".to_owned()),
                    operation: None,
                    interpolate_as_resolved_path: false,
                    join: None,
                }),
                ast::StringFragment::Literal(" ".to_owned()),
                ast::StringFragment::Interpolation(ast::StringInterpolation {
                    stem: ast::StringInterpolationStem::CaptureGroup(1),
                    operation: Some(ast::StringInterpolationOperation::ReplaceExtension(
                        ".ext1".to_owned(),
                        ".ext2".to_owned(),
                    )),
                    interpolate_as_resolved_path: true,
                    join: None,
                }),
            ],
        };

        let result = string_expr.parse(input).unwrap();
        assert_eq!(result, expected);
    }
}
