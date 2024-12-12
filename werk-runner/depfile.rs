use std::{path::PathBuf, sync::Arc};

use winnow::{
    ascii::{line_ending, multispace0, space0, space1},
    combinator::{
        alt, cut_err, delimited, eof, not, peek, preceded, repeat, separated_pair, terminated,
    },
    error::{StrContext, StrContextValue},
    token::{none_of, take_till},
    PResult, Parser as _,
};

#[derive(Default)]
pub struct Depfile {
    pub target: PathBuf,
    pub deps: Vec<PathBuf>,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum DepfileError {
    #[error("depfile is empty")]
    Empty,
    #[error("error parsing depfile at offset {0}: {2:?}\n{1}")]
    ParseError(usize, String, winnow::error::ContextError),
    #[error("depfile is not valid UTF-8")]
    InvalidUtf8(#[from] std::str::Utf8Error),
    #[error("error absolutizing path '{0}': {1}")]
    Absolutize(String, Arc<std::io::Error>),
}

impl Depfile {
    /// Parse a depfile generated by a Make-compatible tool, like `clang -MM`
    /// etc.
    ///
    /// This parser is somewhat more lax than POSIX Make, and cannot be used to
    /// determine whether the input is a valid Makefile. For example, it allows
    /// non-tab indentation, and both the rule name and the prerequisites can
    /// contain escaped spaces (though not escaped newlines, which aren't
    /// allowed in paths anyway).
    ///
    /// At the same time, it does not support the full Makefile syntax: Rule
    /// bodies are not supported (and aren't generated by Make-compatible
    /// tools).
    pub fn parse(depfile_contents: &[u8]) -> Result<Depfile, DepfileError> {
        let depfile_contents = std::str::from_utf8(depfile_contents)?;

        let depfile_contents = depfile_contents.trim();
        if depfile_contents.is_empty() {
            return Err(DepfileError::Empty);
        }

        let (target, deps) = parse_depfile.parse(depfile_contents).map_err(|err| {
            DepfileError::ParseError(err.offset(), (*err.input()).to_owned(), err.into_inner())
        })?;
        // TODO: Go through `Io` to absolutize!
        let target = std::path::absolute(&target)
            .map_err(|err| DepfileError::Absolutize(target, Arc::new(err)))?;
        let deps = deps
            .into_iter()
            .map(|p| {
                std::path::absolute(&p).map_err(|err| DepfileError::Absolutize(p, Arc::new(err)))
            })
            .collect::<Result<_, _>>()?;
        Ok(Depfile { target, deps })
    }
}

fn parse_depfile(input: &mut &str) -> PResult<(String, Vec<String>)> {
    delimited(
        // Ignore initial whitespace.
        multispace0,
        separated_pair(
            cut_err(parse_rule_name),
            (':').context(StrContext::Expected(StrContextValue::Description(
                "rule name must be followed by ':'",
            ))),
            prerequisites,
        ),
        // Ignore whitespace before EOF.
        (
            multispace0,
            eof.context(StrContext::Expected(StrContextValue::Description(
                "end-of-file; perhaps there is an unescaped newline?",
            ))),
        ),
    )
    .context(StrContext::Label("depfile"))
    .parse_next(input)
}

fn prerequisites(input: &mut &str) -> PResult<Vec<String>> {
    repeat(0.., preceded(separator, parse_prerequisite))
        .context(StrContext::Label("prerequisites"))
        .parse_next(input)
}

fn separator<'a>(input: &mut &'a str) -> PResult<&'a str> {
    // Note: The escaped newline is more lax than Makefiles, which requires tab
    // indentation.
    alt((
        delimited(
            (space0, '\\', space0),
            line_ending.context(StrContext::Expected(StrContextValue::Description(
                "line ending ('\\n' or CRLF)",
            ))).take(),
            space0,
        ),
        space1.context(StrContext::Label("space separator")),
    ))
    .context(StrContext::Expected(StrContextValue::Description(
        "prerequisites must be separated by unescaped spaces or an escaped newline followed by whitespace",
    )))
    .parse_next(input)
}

/// Allow whitespace in the initial rule name. This just parses all characters
/// literally until a colon followed by whitespace. Note that path components
/// cannot start with whitespace on Windows.
fn parse_rule_name(input: &mut &str) -> PResult<String> {
    repeat(1.., rule_name_fragment)
        .fold(String::new, |mut string, fragment| {
            match fragment {
                PathFragment::Literal(lit) => string.push_str(lit),
                PathFragment::EscapedChar(ch) => string.push(ch),
            }
            string
        })
        .context(StrContext::Label("rule name"))
        .parse_next(input)
}

fn parse_prerequisite(input: &mut &str) -> PResult<String> {
    repeat(1.., prerequisite_fragment)
        .fold(String::new, |mut string, fragment| {
            match fragment {
                PathFragment::Literal(lit) => string.push_str(lit),
                PathFragment::EscapedChar(ch) => string.push(ch),
            }
            string
        })
        .context(StrContext::Label("Makefile prerequisites"))
        .parse_next(input)
}

#[derive(Clone, Copy, Debug)]
enum PathFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
}

fn rule_name_fragment<'a>(input: &mut &'a str) -> PResult<PathFragment<'a>> {
    alt((
        // Colon followed by whitespace is what terminates the initial rule
        // name. Note that path components cannot start or end with whitespace
        // on Windows.
        terminated(':', peek(none_of([' ', '\t', '\r', '\n'])))
            .value(PathFragment::EscapedChar(':')),
        rule_name_literal.map(PathFragment::Literal),
    ))
    .context(StrContext::Label("rule name fragment"))
    .parse_next(input)
}

fn rule_name_literal<'a>(input: &mut &'a str) -> PResult<&'a str> {
    // Note: Backspaces are allowed as-is. The rule name does not support escaping.
    take_till(1.., ':')
        .context(StrContext::Label("rule name literal"))
        .parse_next(input)
}

fn prerequisite_fragment<'a>(input: &mut &'a str) -> PResult<PathFragment<'a>> {
    alt((
        // Backslash followed by non-whitespace is allowed (it's a Windows path
        // separator).
        terminated('\\', peek(none_of([' ', '\t', '\r', '\n'])))
            .value(PathFragment::EscapedChar('\\')),
        prerequisite_literal.map(PathFragment::Literal),
        escaped_space.map(PathFragment::Literal),
    ))
    .context(StrContext::Label("path fragment"))
    .parse_next(input)
}

fn escaped_space<'a>(input: &mut &'a str) -> PResult<&'a str> {
    // Note: Newlines are *NOT* escapable in prerequisites, because they can be
    // used to separate prerequisites, but normal spaces and tabs can be
    // escaped.
    preceded('\\', space1)
        .context(StrContext::Label("escaped whitespace"))
        .parse_next(input)
}

fn prerequisite_literal<'a>(input: &mut &'a str) -> PResult<&'a str> {
    take_till(1.., [' ', '\t', '\r', '\n', '\\'])
        .context(StrContext::Label("path literal fragment"))
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let input = "target: dep1 dep2 dep3";
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "target");
        assert_eq!(deps, vec!["dep1", "dep2", "dep3"]);
    }

    #[test]
    fn test_prerequisite_separator() {
        let one_space = " ";
        assert_eq!(separator.parse(one_space).unwrap(), " ");
        let two_spaces = "  ";
        assert_eq!(separator.parse(two_spaces).unwrap(), "  ");
        let newline = "\\\n ";
        assert_eq!(separator.parse(newline).unwrap(), "\n");
        let space_and_newline = " \\\n ";
        assert_eq!(separator.parse(space_and_newline).unwrap(), "\n");
    }

    #[test]
    fn windows_paths_inline() {
        let input = r#"E:\my-project\test.c: dep1 e:\my-project\test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "E:\\my-project\\test.c");
        assert_eq!(deps, vec!["dep1", "e:\\my-project\\test.h"]);
    }

    #[test]
    fn windows_paths_newlines() {
        let input = r#"E:\my-project\test.c: dep1 \
            e:\my-project\test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "E:\\my-project\\test.c");
        assert_eq!(deps, vec!["dep1", "e:\\my-project\\test.h"]);
    }

    #[test]
    fn windows_paths_crlf() {
        let input = "E:\\my-project\\test.c: dep1 \\\r\n  e:\\my-project\\test.h";
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "E:\\my-project\\test.c");
        assert_eq!(deps, vec!["dep1", "e:\\my-project\\test.h"]);
    }

    #[test]
    fn windows_paths_with_spaces() {
        let input = r#"E:\my project\test.c: dep1 \
            e:\my\ project\test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "E:\\my project\\test.c");
        assert_eq!(deps, vec!["dep1", "e:\\my project\\test.h"]);
    }

    #[test]
    fn windows_paths_example_c() {
        let input = r#"E:\werk\examples\c\main.c: E:\werk\examples\c\main.c \
  E:\werk\examples\c\foo.h
  "#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "E:\\werk\\examples\\c\\main.c");
        assert_eq!(
            deps,
            vec![
                "E:\\werk\\examples\\c\\main.c",
                "E:\\werk\\examples\\c\\foo.h",
            ]
        );
    }

    #[test]
    fn unix_paths_inline() {
        let input = r#"/my-project/test.c: dep1 /my-project/test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "/my-project/test.c");
        assert_eq!(deps, vec!["dep1", "/my-project/test.h"]);
    }

    #[test]
    fn unix_paths_newlines() {
        let input = r#"/my-project/test.c: dep1 \
            /my-project/test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "/my-project/test.c");
        assert_eq!(deps, vec!["dep1", "/my-project/test.h"]);
    }

    #[test]
    fn unix_paths_with_spaces() {
        let input = r#"/my project/test.c: dep1 \
            /my\ project/test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "/my project/test.c");
        assert_eq!(deps, vec!["dep1", "/my project/test.h"]);
    }

    #[test]
    fn relative_paths_inline() {
        let input = r#"E:\my-project\test.c: dep1 e:\my-project\..\test.h"#;
        let (target, deps) = parse_depfile.parse(input).unwrap();
        assert_eq!(target, "E:\\my-project\\test.c");
        assert_eq!(deps, vec!["dep1", "e:\\my-project\\..\\test.h"]);
    }
}
