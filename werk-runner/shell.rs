use std::{
    collections::{BTreeMap, BTreeSet},
    ffi::{OsStr, OsString},
};

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

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Env {
    pub env: BTreeMap<OsString, OsString>,
    pub env_remove: BTreeSet<OsString>,
}

impl Env {
    pub fn merge_from(&mut self, other: &Self) {
        for k in &other.env_remove {
            self.env_remove(k);
        }
        for (k, v) in &other.env {
            self.env(k, v);
        }
    }

    pub fn get(&self, key: impl AsRef<OsStr>) -> Option<&OsString> {
        self.env.get(key.as_ref())
    }

    pub fn envs<I, K, V>(&mut self, envs: I) -> &mut Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.env.extend(
            envs.into_iter()
                .map(|(k, v)| (k.as_ref().to_os_string(), v.as_ref().to_os_string())),
        );
        self
    }

    /// Set an environment variable in the child process.
    pub fn env(&mut self, key: impl AsRef<OsStr>, value: impl AsRef<OsStr>) -> &mut Self {
        let key = key.as_ref();
        self.env
            .insert(key.to_os_string(), value.as_ref().to_os_string());
        self.env_remove.remove(key);
        self
    }

    /// Remove an environment variable from the child process, i.e. make sure
    /// that it does not inherit it from the parent process.
    pub fn env_remove(&mut self, key: impl AsRef<OsStr>) -> &mut Self {
        let key = key.as_ref();
        self.env_remove.insert(key.to_os_string());
        self.env.remove(key);
        self
    }

    /// Set the `CLICOLOR_FORCE` and `FORCE_COLOR` environment variable for this
    /// command. Also clears the `NO_COLOR` environment variable.
    pub fn set_force_color(&mut self) -> &mut Self {
        // Remove `NO_COLOR` if previously set.
        self.env.remove(OsStr::new("NO_COLOR"));

        // Prevent the inherited environment from setting `NO_COLOR`.
        self.env_remove
            .insert(OsStr::new("NO_COLOR").to_os_string());

        // Remove earlier disablement of `FORCE_COLOR`.
        self.env_remove.remove(OsStr::new("FORCE_COLOR"));

        self.env("FORCE_COLOR", "1");
        self.env("CLICOLOR", "1");
        self.env("CLICOLOR_FORCE", "1");
        self
    }

    /// Set the `NO_COLOR` environment variable for this command. Also clears
    /// the `CLICOLOR_FORCE` and `CLICOLOR` environment variables.
    pub fn set_no_color(&mut self) -> &mut Self {
        // Remove enablement from this command if previously set.
        self.env.remove(OsStr::new("FORCE_COLOR"));
        self.env.remove(OsStr::new("CLICOLOR"));
        self.env.remove(OsStr::new("CLICOLOR_FORCE"));

        // Prevent the inherited environment from setting `FORCE_COLOR`.
        self.env_remove
            .insert(OsStr::new("FORCE_COLOR").to_os_string());
        self.env_remove
            .insert(OsStr::new("CLICOLOR").to_os_string());
        self.env_remove
            .insert(OsStr::new("CLICOLOR_FORCE").to_os_string());

        // Remove earlier disablement of `NO_COLOR`.
        self.env_remove.remove(OsStr::new("NO_COLOR"));

        self.env("NO_COLOR", "1");
        self
    }
}
