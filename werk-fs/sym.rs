use stringleton::Symbol;

use crate::{Absolute, Path, PathBuf};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymPath(Symbol);

impl SymPath {
    #[must_use]
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self(Symbol::new(path.as_ref().as_str()))
    }

    #[inline]
    #[must_use]
    pub fn as_path(&self) -> &'static Path {
        Path::new_unchecked(self.0.as_str())
    }

    #[inline]
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        self.0.as_str()
    }
}

impl AsRef<Path> for SymPath {
    #[inline]
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl std::fmt::Debug for SymPath {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_path(), f)
    }
}

impl std::fmt::Display for SymPath {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_path(), f)
    }
}

impl Absolute<SymPath> {
    #[must_use]
    pub fn symbolicate(path: impl AsRef<Absolute<Path>>) -> Self {
        Absolute::new_unchecked(SymPath::new(&**path.as_ref()))
    }

    #[inline]
    #[must_use]
    pub fn as_path(&self) -> &'static Absolute<crate::Path> {
        Absolute::new_ref_unchecked(self.as_inner().as_path())
    }
}

impl From<&Absolute<Path>> for Absolute<SymPath> {
    #[inline]
    fn from(path: &Absolute<Path>) -> Self {
        Self::symbolicate(path)
    }
}

impl From<Absolute<PathBuf>> for Absolute<SymPath> {
    #[inline]
    fn from(path: Absolute<PathBuf>) -> Self {
        Self::symbolicate(path)
    }
}
