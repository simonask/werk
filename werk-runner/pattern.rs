use werk_util::DiagnosticSpan;

#[derive(Debug, Clone)]
pub struct Pattern {
    /// The source span for the pattern.
    pub span: DiagnosticSpan,
    /// The canonical string representation of the pattern in Werk syntax.
    pub string: String,
    pub(crate) matcher: PatternMatcher,
}

#[derive(Debug, Clone)]
pub(crate) enum PatternMatcher {
    Literal(String),
    Regex(PatternRegex),
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct PatternRegexCaptures {
    pub stem_capture_index: Option<usize>,
    pub num_normal_capture_groups: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct PatternRegex {
    /// The regular expression used to match this pattern.
    pub regex: regex::Regex,
    /// Information about the capture groups in the regex.
    pub captures: PatternRegexCaptures,
}

impl PartialEq for Pattern {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (&self.matcher, &other.matcher) {
            (PatternMatcher::Literal(ref lhs), PatternMatcher::Literal(ref rhs)) => *lhs == *rhs,
            (PatternMatcher::Regex(ref lhs), PatternMatcher::Regex(ref rhs)) => {
                lhs.regex.as_str() == rhs.regex.as_str()
            }
            _ => false,
        }
    }
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

impl Pattern {
    #[must_use]
    pub fn match_whole_string(&self, string: &str) -> Option<PatternMatchData> {
        match self.matcher {
            PatternMatcher::Literal(ref needle) => {
                if string == needle {
                    Some(PatternMatchData {
                        stem: None,
                        captures: Box::default(),
                    })
                } else {
                    None
                }
            }
            PatternMatcher::Regex(ref regex) => {
                let m = regex.regex.captures(string)?;
                let mut capture_groups =
                    Vec::with_capacity(regex.captures.num_normal_capture_groups);
                let mut stem = None;

                let mut group_matches = m.iter();
                // Skip the implicit whole-string match group.
                group_matches.next().unwrap();

                for (index, group) in group_matches.enumerate() {
                    let group_str = group.unwrap().as_str();
                    if regex.captures.stem_capture_index == Some(index) {
                        stem = Some(group_str);
                    } else {
                        capture_groups.push(group_str);
                    }
                }

                Some(PatternMatchData::new(stem, capture_groups))
            }
        }
    }

    #[must_use]
    pub fn match_whole_path(&self, path: &werk_fs::Path) -> Option<PatternMatchData> {
        tracing::trace!("Matching '{path}' against {:?}", self.matcher);
        self.match_whole_string(path.as_str())
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.string)
    }
}

impl PatternMatchData {
    /// True if the pattern did not contain a stem.
    #[inline]
    #[must_use]
    pub fn is_verbatim(&self) -> bool {
        self.stem.is_none()
    }

    #[inline]
    #[must_use]
    pub fn stem(&self) -> Option<&str> {
        self.stem.as_deref()
    }

    #[inline]
    #[must_use]
    pub fn captures(&self) -> &[Box<str>] {
        &self.captures
    }

    #[inline]
    #[must_use]
    pub fn capture_group(&self, group: usize) -> Option<&str> {
        self.captures.get(group).map(|s| &**s)
    }
}
