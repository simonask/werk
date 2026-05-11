#[derive(Clone, Debug)]
#[allow(clippy::struct_excessive_bools)]
pub struct GlobSettings {
    /// Read the workspace directory's `.gitignore`. Enabled by default.
    pub git_ignore: bool,
    /// Read a global gitignore file, specified in the `core.excludeFiles`
    /// config option. Enabled by default.
    pub git_ignore_global: bool,
    /// Read `.git/info/exclude`. Enabled by default.
    pub git_ignore_exclude: bool,
    /// Read `.gitignore` from parent directoryes. Enabled by default.
    pub git_ignore_from_parents: bool,
    /// Enables reading `.ignore` files, supported by `ripgrep` and The Silver Searcher. Enabled by default.
    pub dot_ignore: bool,
    /// Explicit file name patterns to ignore in addition to gitignore and .ignore files.
    pub ignore_explicitly: globset::GlobSet,
}

impl Default for GlobSettings {
    #[inline]
    fn default() -> Self {
        Self {
            git_ignore: true,
            git_ignore_global: true,
            git_ignore_exclude: true,
            git_ignore_from_parents: true,
            dot_ignore: true,
            ignore_explicitly: globset::GlobSet::empty(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum GlobError {
    #[error(transparent)]
    Glob(#[from] globset::Error),
}
