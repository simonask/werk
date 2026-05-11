use werk_fs::Absolute;

#[derive(Clone, PartialEq)]
pub struct ShellCommandLine {
    /// The name of the program to run. Should be an absolute path, either from
    /// a `which` expression or an `<var>` interpolation when running an
    /// executable produced by another recipe.
    pub program: Absolute<std::path::PathBuf>,
    pub arguments: Vec<String>,
}

impl std::fmt::Display for ShellCommandLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.program.display())?;
        for arg in &self.arguments {
            write!(f, " {}", werk_util::DisplayArg(arg))?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for ShellCommandLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.program)?;
        for arg in &self.arguments {
            write!(f, " {}", werk_util::DisplayArgQuoted(arg))?;
        }
        Ok(())
    }
}
