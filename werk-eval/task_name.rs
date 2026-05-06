use stringleton::Symbol;
use werk_fs::{Absolute, SymPath};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TaskName {
    Task(Symbol),
    // TODO: When recipes can build multiple files, this needs to change to some
    // ID that encapsulates the "recipe instance" rather than the path of a
    // single target.
    Build(Absolute<SymPath>),
}

impl TaskName {
    pub fn command(s: impl Into<Symbol>) -> Self {
        let name = s.into();
        debug_assert!(!name.as_str().starts_with('/'));
        TaskName::Task(name)
    }

    pub fn build(p: impl AsRef<Absolute<werk_fs::Path>>) -> Self {
        TaskName::Build(Absolute::symbolicate(p))
    }

    pub fn try_build<P>(p: P) -> Result<Self, P::Error>
    where
        P: TryInto<Absolute<werk_fs::PathBuf>>,
    {
        let path = p.try_into()?;
        Ok(TaskName::build(path))
    }

    #[inline]
    #[must_use]
    pub fn is_command(&self) -> bool {
        matches!(self, TaskName::Task(_))
    }

    #[inline]
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            TaskName::Task(task) => task.as_str(),
            TaskName::Build(build) => build.as_inner().as_str(),
        }
    }

    #[inline]
    #[must_use]
    pub fn as_path(&self) -> Option<&Absolute<werk_fs::Path>> {
        if let TaskName::Build(build) = self {
            Some(build.as_path())
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn short_name(&self) -> &'static str {
        match self {
            TaskName::Task(task) => task.as_str(),
            TaskName::Build(path) => {
                let Some((_prefix, filename)) = path
                    .as_inner()
                    .as_str()
                    .rsplit_once(werk_fs::Path::SEPARATOR)
                else {
                    // The path is absolute.
                    unreachable!()
                };
                filename
            }
        }
    }
}

impl std::fmt::Display for TaskName {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
