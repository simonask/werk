use std::{
    collections::BTreeSet,
    ops::{BitOr, BitOrAssign},
};

use crate::TaskId;

/// A reason why a variable or recipe is "outdated".
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reason {
    /// The output file of a recipe does not exist.
    Missing(werk_fs::PathBuf),
    /// A source file was newer than its output.
    Modified(werk_fs::PathBuf, std::time::SystemTime),
    /// The result of a glob operation changed between runs.
    Glob(String),
    /// The value of a used environment variable changed between runs.
    Env(String),
    /// The resolved path of a binary executable changed between runs.
    Which(String),
    /// `werk.toml` is newer than `.werk-cache`.
    RecipesModified(std::time::SystemTime),
    /// The recipe has a dependency that was rebuilt.
    Rebuilt(TaskId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Outdatedness {
    pub reasons: BTreeSet<Reason>,
}

impl Outdatedness {
    pub fn new<I: IntoIterator<Item = Reason>>(reasons: I) -> Self {
        Self::from_iter(reasons)
    }

    pub fn unchanged() -> Self {
        Self {
            reasons: BTreeSet::new(),
        }
    }

    #[inline]
    pub fn outdated(reason: Reason) -> Self {
        let mut reasons = BTreeSet::new();
        reasons.insert(reason);
        Self { reasons }
    }

    #[inline]
    pub fn is_unchanged(&self) -> bool {
        self.reasons.is_empty()
    }

    #[inline]
    pub fn is_outdated(&self) -> bool {
        !self.reasons.is_empty()
    }

    #[inline]
    pub fn insert(&mut self, reason: Reason) {
        self.reasons.insert(reason);
    }
}

impl FromIterator<Reason> for Outdatedness {
    fn from_iter<T: IntoIterator<Item = Reason>>(iter: T) -> Self {
        Outdatedness {
            reasons: BTreeSet::from_iter(iter),
        }
    }
}

impl BitOrAssign for Outdatedness {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        if self.reasons.is_empty() {
            self.reasons = rhs.reasons;
            return;
        }

        self.reasons.extend(rhs.reasons);
    }
}

impl BitOrAssign<&Outdatedness> for Outdatedness {
    #[inline]
    fn bitor_assign(&mut self, rhs: &Self) {
        self.reasons.extend(rhs.reasons.iter().cloned());
    }
}

impl BitOr for Outdatedness {
    type Output = Self;

    #[inline]
    fn bitor(mut self, rhs: Self) -> Self {
        self |= rhs;
        self
    }
}

impl BitOr<&Outdatedness> for Outdatedness {
    type Output = Self;

    #[inline]
    fn bitor(mut self, rhs: &Self) -> Self {
        self |= rhs;
        self
    }
}

impl std::fmt::Display for Reason {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reason::Missing(path_buf) => write!(f, "`{path_buf}` does not exist"),
            Reason::Modified(path_buf, _) => write!(f, "`{path_buf}` was modified"),
            Reason::Glob(pattern) => write!(f, "glob result '{pattern}' changed"),
            Reason::Env(env) => write!(f, "environment variable `{env}` changed"),
            Reason::Which(program) => write!(f, "resolved path of `{program}` changed"),
            Reason::RecipesModified(_) => write!(f, "recipe file was changed"),
            Reason::Rebuilt(TaskId::Command(name)) => {
                write!(f, "`{name}` is a command recipe")
            }
            Reason::Rebuilt(TaskId::Build(path)) => {
                write!(f, "dependency `{path}` was rebuilt")
            }
        }
    }
}
