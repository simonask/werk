use std::borrow::Cow;

use werk_parser::parser::Span;

#[derive(Debug, Clone)]
pub struct Pattern<'a> {
    /// The original string representation of the pattern.
    pub string: String,
    /// The source span for the pattern.
    pub span: Span,
    /// Parsed fragments of the pattern.
    pub fragments: Box<[PatternFragment<'a>]>,
    /// The regular expression used to match this pattern.
    pub regex: Box<regex::Regex>,
    /// The index of the regex capture group that represents the pattern stem.
    pub stem_capture_index: Option<usize>,
    /// The number of "one-of" capture groups in the pattern.
    pub num_capture_groups: usize,
}

impl PartialEq for Pattern<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for Pattern<'_> {}

impl std::hash::Hash for Pattern<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

#[derive(Debug, Clone, Default)]
pub struct PatternBuilder<'a> {
    span: Span,
    string: String,
    fragments: Vec<PatternFragment<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternFragment<'a> {
    Literal(Cow<'a, str>),
    PatternStem,
    OneOf(Vec<Cow<'a, str>>),
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct PatternMatchData {
    /// The matched stem, if the pattern has a stem.
    pub stem: Option<Box<str>>,
    /// One entry for each `OneOf` capture group `(a|b|...)` in the pattern.
    pub captures: Box<[Box<str>]>,
}

impl PatternMatchData {
    pub fn new(
        stem: Option<impl Into<Box<str>>>,
        captures: impl IntoIterator<Item: Into<Box<str>>>,
    ) -> Self {
        Self {
            stem: stem.map(Into::into),
            captures: captures.into_iter().map(Into::into).collect(),
        }
    }
}

impl<'a> PatternBuilder<'a> {
    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    pub fn push_str(&mut self, s: &str) {
        self.string.push_str(s);
        if let Some(PatternFragment::Literal(ref mut tail)) = self.fragments.last_mut() {
            tail.to_mut().push_str(s);
        } else {
            self.fragments
                .push(PatternFragment::Literal(s.to_owned().into()));
        }
    }

    pub fn push_one_of(&mut self, one_of: Vec<Cow<'a, str>>) {
        let string = &mut self.string;
        string.push('(');
        for (i, capture) in one_of.iter().enumerate() {
            if i != 0 {
                string.push('|');
            }
            string.push_str(capture);
        }
        string.push(')');

        self.fragments.push(PatternFragment::OneOf(one_of));
    }

    pub fn push_pattern_stem(&mut self) {
        self.string.push('%');
        self.fragments.push(PatternFragment::PatternStem);
    }

    /// Ensure that the pattern starts with `/`. This should be used in build
    /// recipe patterns, where output paths are always "absolutized" before
    /// matching.
    pub fn ensure_absolute_path(&mut self) {
        if let Some(PatternFragment::Literal(first)) = self.fragments.first_mut() {
            if !first.starts_with('/') {
                self.string.insert(0, '/');
                first.to_mut().insert(0, '/');
            }
        } else {
            self.string.insert(0, '/');
            self.fragments
                .insert(0, PatternFragment::Literal("/".into()));
        }
    }

    pub fn build(self) -> Pattern<'a> {
        let mut regex_pattern = String::from("^");
        let mut capture_count = 0;
        let mut stem_capture_index = None;
        let mut num_capture_groups = 0;
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(lit) => regex_pattern.push_str(&regex::escape(lit)),
                PatternFragment::PatternStem => {
                    regex_pattern.push_str(r"(.*)");
                    stem_capture_index = Some(capture_count);
                    capture_count += 1;
                }
                PatternFragment::OneOf(vec) => {
                    regex_pattern.push('(');
                    for (i, capture) in vec.iter().enumerate() {
                        if i != 0 {
                            regex_pattern.push('|');
                        }
                        regex_pattern.push_str(&regex::escape(capture));
                    }
                    regex_pattern.push(')');
                    capture_count += 1;
                    num_capture_groups += 1;
                }
            }
        }
        regex_pattern.push('$');

        let regex = regex::RegexBuilder::new(&regex_pattern)
            .unicode(true)
            .build()
            .unwrap();

        Pattern {
            span: self.span,
            string: self.string,
            fragments: self.fragments.into(),
            regex: Box::new(regex),
            stem_capture_index,
            num_capture_groups,
        }
    }
}

impl<'a> Pattern<'a> {
    pub fn parse(pattern: &'a str) -> Result<Self, werk_parser::ParseError> {
        let parsed = werk_parser::parse_string::parse_pattern_expr(pattern)?;
        let mut builder = PatternBuilder::default();
        for fragment in parsed.fragments {
            match fragment {
                werk_parser::ast::PatternFragment::Literal(lit) => builder.push_str(&lit),
                werk_parser::ast::PatternFragment::PatternStem => builder.push_pattern_stem(),
                werk_parser::ast::PatternFragment::OneOf(one_of) => builder.push_one_of(one_of),
                werk_parser::ast::PatternFragment::Interpolation(_) => panic!(
                    "Pattern::parse cannot handle interpolations; use `eval_pattern` instead"
                ),
            }
        }
        Ok(builder.build())
    }

    pub fn match_string(&self, string: &str) -> Option<PatternMatchData> {
        let m = self.regex.captures(string)?;
        let mut capture_groups = Vec::with_capacity(self.num_capture_groups);
        let mut stem = None;

        let mut group_matches = m.iter();
        // Skip the implicit whole-string match group.
        group_matches.next().unwrap();

        for (index, group) in group_matches.enumerate() {
            let group_str = group.unwrap().as_str();
            if self.stem_capture_index == Some(index) {
                stem = Some(group_str);
            } else {
                capture_groups.push(group_str);
            }
        }

        Some(PatternMatchData::new(stem, capture_groups))
    }

    pub fn match_path(&self, path: &werk_fs::Path) -> Option<PatternMatchData> {
        tracing::trace!("Matching '{path}' against {:?}", self.regex);
        self.match_string(path.as_str())
    }
}

impl std::fmt::Display for Pattern<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.string)
    }
}

impl PatternMatchData {
    /// True if the pattern did not contain a stem.
    #[inline]
    pub fn is_verbatim(&self) -> bool {
        self.stem.is_none()
    }

    #[inline]
    pub fn stem(&self) -> Option<&str> {
        self.stem.as_deref()
    }

    #[inline]
    pub fn captures(&self) -> &[Box<str>] {
        &self.captures
    }

    #[inline]
    pub fn capture_group(&self, group: usize) -> Option<&str> {
        self.captures.get(group).map(|s| &**s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn match_stem() {
        let pattern_expr = werk_parser::parse_string::parse_pattern_expr("%.c").unwrap();
        let mut builder = PatternBuilder::default();
        for fragment in pattern_expr.fragments {
            match fragment {
                werk_parser::ast::PatternFragment::Literal(lit) => builder.push_str(&lit),
                werk_parser::ast::PatternFragment::PatternStem => builder.push_pattern_stem(),
                werk_parser::ast::PatternFragment::OneOf(vec) => builder.push_one_of(vec),
                werk_parser::ast::PatternFragment::Interpolation(_) => {
                    panic!("unexpected interpolation")
                }
            }
        }
        let pattern = builder.build();

        assert_eq!(
            &*pattern.fragments,
            &[
                PatternFragment::PatternStem,
                PatternFragment::Literal(".c".into())
            ] as &[_]
        );

        let pattern_match = pattern.match_string("/main.c").unwrap();
        assert_eq!(pattern_match.stem(), Some("/main"));
        assert!(pattern_match.captures.is_empty());
    }
}
