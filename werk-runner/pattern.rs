use std::fmt::Write as _;

#[derive(Debug, Clone)]
pub struct Pattern {
    fragments: Box<[PatternFragment]>,
    regex: Box<regex::Regex>,
    stem_capture_index: usize,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternMatchData {
    /// The matched stem, if the pattern has a stem.
    pub stem: Option<Box<str>>,
    /// One entry for each `OneOf` capture group `(a|b|...)` in the pattern.
    pub captures: Box<[String]>,
}

impl PatternBuilder {
    pub fn push_str(&mut self, s: &str) {
        if let Some(PatternFragment::Literal(ref mut tail)) = self.fragments.last_mut() {
            tail.push_str(s);
        } else {
            self.fragments.push(PatternFragment::Literal(s.to_owned()));
        }
    }

    pub fn push_one_of(&mut self, one_of: Vec<String>) {
        self.fragments.push(PatternFragment::OneOf(one_of));
    }

    pub fn push_pattern_stem(&mut self) {
        self.fragments.push(PatternFragment::PatternStem);
    }

    fn ensure_absolute(&mut self) {
        if let Some(PatternFragment::Literal(s)) = self.fragments.first() {
            if !s.starts_with('/') {
                self.fragments
                    .insert(0, PatternFragment::Literal("/".to_owned()));
            }
        } else {
            self.fragments
                .insert(0, PatternFragment::Literal("/".to_owned()));
        }
    }

    pub fn build(mut self) -> Pattern {
        self.ensure_absolute();

        let mut regex_pattern = String::from("^");
        let mut capture_count = 0;
        let mut stem_capture_index = 0;
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(lit) => regex_pattern.push_str(&regex::escape(lit)),
                PatternFragment::PatternStem => {
                    regex_pattern.push_str(r"(.+?)");
                    stem_capture_index = capture_count;
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
            fragments: self.fragments.into(),
            regex: Box::new(regex),
            stem_capture_index,
        }
    }
}

impl Pattern {
    pub fn match_path(&self, path: &werk_fs::Path) -> Option<PatternMatchData> {
        tracing::trace!("Matching '{path}' against {:?}", self.regex);
        let m = self.regex.captures_iter(path.as_str()).next()?;
        let mut capture_groups = Vec::new();
        let mut stem = None;

        let mut group_matches = m.iter();
        // Skip the implicit whole-string match group.
        group_matches.next();

        for (index, group) in group_matches.enumerate() {
            let group_str = group.unwrap().as_str();
            if index == self.stem_capture_index {
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
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(s) => write!(f, "{}", s)?,
                PatternFragment::PatternStem => write!(f, "%")?,
                PatternFragment::OneOf(one_of) => {
                    f.write_char('(')?;
                    for (i, s) in one_of.iter().enumerate() {
                        if i > 0 {
                            f.write_char('|')?;
                        }
                        write!(f, "{}", s)?;
                    }
                    f.write_char(')')?;
                }
            }
        }
        Ok(())
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
