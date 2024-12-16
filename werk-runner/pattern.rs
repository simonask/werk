use crate::AmbiguousPatternError;

#[derive(Debug, Clone)]
pub struct Pattern {
    /// The original string representation of the pattern.
    pub string: String,
    pub fragments: Box<[PatternFragment]>,
    /// The regular expression used to match this pattern.
    pub regex: Box<regex::Regex>,
    /// The index of the regex capture group that represents the pattern stem.
    pub stem_capture_index: Option<usize>,
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.fragments == other.fragments
    }
}

impl Eq for Pattern {}

impl std::hash::Hash for Pattern {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fragments.hash(state);
    }
}

#[derive(Debug, Clone, Default)]
pub struct PatternBuilder {
    string: String,
    fragments: Vec<PatternFragment>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternFragment {
    Literal(String),
    PatternStem,
    OneOf(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternMatch<'a> {
    pub pattern: &'a Pattern,
    pub data: PatternMatchData,
}

impl PartialEq<PatternMatchData> for PatternMatch<'_> {
    #[inline]
    fn eq(&self, other: &PatternMatchData) -> bool {
        self.data == *other
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct PatternMatchData {
    /// The matched stem, if the pattern has a stem.
    pub stem: Option<Box<str>>,
    /// One entry for each `OneOf` capture group `(a|b|...)` in the pattern.
    pub captures: Box<[String]>,
}

impl PatternMatchData {
    pub fn new(
        stem: Option<impl Into<String>>,
        captures: impl IntoIterator<Item = String>,
    ) -> Self {
        Self {
            stem: stem.map(Into::into).map(String::into_boxed_str),
            captures: captures.into_iter().map(Into::into).collect(),
        }
    }
}

impl PatternBuilder {
    pub fn push_str(&mut self, s: &str) {
        self.string.push_str(s);
        if let Some(PatternFragment::Literal(ref mut tail)) = self.fragments.last_mut() {
            tail.push_str(s);
        } else {
            self.fragments.push(PatternFragment::Literal(s.to_owned()));
        }
    }

    pub fn push_one_of(&mut self, one_of: Vec<String>) {
        self.string.push('(');
        for (i, capture) in one_of.iter().enumerate() {
            if i != 0 {
                self.string.push('|');
            }
            self.string.push_str(capture);
        }
        self.string.push(')');

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
                first.insert(0, '/');
            }
        } else {
            self.string.insert(0, '/');
            self.fragments
                .insert(0, PatternFragment::Literal(String::from("/")));
        }
    }

    pub fn build(self) -> Pattern {
        let mut regex_pattern = String::from("^");
        let mut capture_count = 0;
        let mut stem_capture_index = None;
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
                }
            }
        }
        regex_pattern.push('$');

        let regex = regex::RegexBuilder::new(&regex_pattern)
            .unicode(true)
            .build()
            .unwrap();

        Pattern {
            string: self.string,
            fragments: self.fragments.into(),
            regex: Box::new(regex),
            stem_capture_index,
        }
    }
}

impl Pattern {
    pub fn parse(pattern: &str) -> Result<Pattern, werk_parser::ParseError> {
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
        let mut capture_groups = Vec::new();
        let mut stem = None;

        let mut group_matches = m.iter();
        // Skip the implicit whole-string match group.
        group_matches.next().unwrap();

        for (index, group) in group_matches.enumerate() {
            let group_str = group.unwrap().as_str();
            if self.stem_capture_index == Some(index) {
                stem = Some(group_str.to_owned());
            } else {
                capture_groups.push(group_str.to_owned());
            }
        }

        Some(PatternMatchData {
            stem: stem.map(Into::into),
            captures: capture_groups.into(),
        })
    }
    pub fn match_path(&self, path: &werk_fs::Path) -> Option<PatternMatchData> {
        tracing::trace!("Matching '{path}' against {:?}", self.regex);
        self.match_string(path.as_str())
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.string)
    }
}

impl std::fmt::Display for PatternMatch<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut captures = self.data.captures.iter();
        for fragment in &self.pattern.fragments {
            match fragment {
                PatternFragment::Literal(s) => f.write_str(s)?,
                PatternFragment::PatternStem => {
                    write!(f, "{}", self.data.stem.as_deref().unwrap_or(""))?
                }
                PatternFragment::OneOf(_) => {
                    let capture: &str = captures.next().map(|s| &**s).unwrap_or("");
                    f.write_str(capture)?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> PatternMatch<'a> {
    pub fn from_pattern_and_data(pattern: &'a Pattern, data: PatternMatchData) -> Self {
        Self { pattern, data }
    }

    pub fn to_path_buf(&self) -> werk_fs::PathBuf {
        let mut path = String::new();
        let mut captures = self.data.captures.iter();
        for fragment in &self.pattern.fragments {
            match fragment {
                PatternFragment::Literal(s) => path.push_str(s),
                PatternFragment::PatternStem => {
                    path.push_str(
                        self.data
                            .stem
                            .as_deref()
                            .expect("invalid pattern match for pattern; no stem"),
                    );
                }
                PatternFragment::OneOf(_) => {
                    let capture: &str = captures
                        .next()
                        .map(|s| &**s)
                        .expect("invalid pattern match for pattern; not enough captures");
                    path.push_str(capture);
                }
            }
        }

        werk_fs::PathBuf::new_unchecked(path)
    }

    /// True if the pattern did not contain a stem.
    #[inline]
    pub fn is_verbatim(&self) -> bool {
        self.data.is_verbatim()
    }

    #[inline]
    pub fn stem(&self) -> Option<&str> {
        self.data.stem()
    }

    #[inline]
    pub fn captures(&self) -> &[String] {
        self.data.captures()
    }

    #[inline]
    pub fn capture_group(&self, group: usize) -> Option<&str> {
        self.data.capture_group(group)
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
    pub fn captures(&self) -> &[String] {
        &self.captures
    }

    #[inline]
    pub fn capture_group(&self, group: usize) -> Option<&str> {
        self.captures.get(group).map(|s| &**s)
    }
}

#[derive(Clone, Debug, Default)]
pub struct PatternSet {
    // Could use `regex::RegexSet`, but the main benefit of that is that it only
    // needs to traverse the haystack once. But our "haystack" (paths and match
    // arms) are almost always very short, and `RegexSet` requires that
    // individual patterns are re-matched to get capture groups, so in total it
    // is unlikely that `RegexSet` would be faster in total.
    patterns: Vec<Pattern>,
}

impl PatternSet {
    pub fn new(patterns: impl IntoIterator<Item = Pattern>) -> Self {
        Self {
            patterns: patterns.into_iter().collect(),
        }
    }

    #[inline]
    pub fn push(&mut self, pattern: Pattern) {
        self.patterns.push(pattern);
    }

    pub fn matches<'a: 'b, 'b>(
        &'a self,
        haystack: &'b str,
    ) -> impl Iterator<Item = (usize, PatternMatch<'a>)> + 'b {
        self.patterns
            .iter()
            .enumerate()
            .filter_map(move |(index, pattern)| {
                pattern
                    .match_string(haystack)
                    .map(|data| (index, PatternMatch::from_pattern_and_data(pattern, data)))
            })
    }

    pub fn best_match_string<'a>(
        &'a self,
        haystack: &str,
    ) -> Result<Option<(usize, PatternMatch<'a>)>, AmbiguousPatternError> {
        let mut best_match = None;

        for (index, pattern_match) in self.matches(haystack) {
            match best_match {
                None => {
                    // No match yet, pick this candidate.
                    best_match = Some((index, pattern_match));
                }
                Some((_, ref best)) => match (best.stem(), pattern_match.stem()) {
                    (None, Some(_)) => {
                        // Best match is exact, do nothing.
                    }
                    (Some(_), None) => {
                        // Candidate is exact, do nothing.
                        best_match = Some((index, pattern_match))
                    }
                    (Some(best_stem), Some(candidate_stem))
                        if candidate_stem.len() < best_stem.len() =>
                    {
                        // Candidate has a shorter stem, so it's better.
                        best_match = Some((index, pattern_match));
                    }
                    (Some(best_stem), Some(candidate_stem))
                        if candidate_stem.len() > best_stem.len() =>
                    {
                        // Candidate has a longer stem, so it's worse; do nothing.
                    }
                    _ => {
                        // Candidate and best have the same length, or both are
                        // exact.
                        return Err(AmbiguousPatternError {
                            pattern1: best.pattern.to_string(),
                            pattern2: pattern_match.pattern.to_string(),
                            path: haystack.to_string(),
                        });
                    }
                },
            }
        }

        Ok(best_match)
    }

    #[inline]
    pub fn best_match_path<'a>(
        &'a self,
        path: &werk_fs::Path,
    ) -> Result<Option<(usize, PatternMatch<'a>)>, AmbiguousPatternError> {
        self.best_match_string(path.as_str())
    }

    pub fn iter(&self) -> impl Iterator<Item = &Pattern> {
        self.patterns.iter()
    }
}

impl std::ops::Index<usize> for PatternSet {
    type Output = Pattern;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.patterns[index]
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
                PatternFragment::Literal(String::from(".c"))
            ] as &[_]
        );

        let pattern_match = pattern.match_string("/main.c").unwrap();
        assert_eq!(pattern_match.stem(), Some("/main"));
        assert!(pattern_match.captures.is_empty());
    }
}
