use std::{
    collections::{BTreeMap, BTreeSet},
    ffi::{OsStr, OsString},
};

use werk_fs::Absolute;
use werk_parser::parser::Span;
use werk_util::Symbol;

use crate::{eval::UsedVariable, EvalError, Value, Workspace};

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
            // TODO: Don't escape Unicode sequences, just newlines, control chars, and quotes.
            if arg.contains(char::is_whitespace) {
                write!(f, " \"{arg}\"")?;
            } else {
                write!(f, " {arg}")?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for ShellCommandLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.program)?;
        for arg in &self.arguments {
            write!(f, " \"{}\"", arg.escape_debug())?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ShellCommandLineBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut parts = self.parts.iter();
        if let Some(program) = parts.next() {
            write!(f, "{program}")?;

            for arg in parts {
                if arg.contains(char::is_whitespace) {
                    write!(f, " \"{arg}\"")?;
                } else {
                    write!(f, " {arg}")?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct ShellCommandLineBuilder {
    in_quotes: Option<InQuotes>,
    escape: bool,
    parts: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
enum InQuotes {
    Single,
    Double,
}

impl ShellCommandLineBuilder {
    fn push_char(&mut self, ch: char) {
        if let Some(last) = self.parts.last_mut() {
            last.push(ch);
        } else {
            self.parts.push(ch.to_string());
        }
    }

    /// Push literal string. Any double-quote char enters/exits a quoted
    /// argument. When not inside quotes, whitespace separates arguments.
    pub fn push_lit(&mut self, s: &str) -> &mut Self {
        for ch in s.chars() {
            if self.escape {
                self.escape = false;
                self.push_char(ch);
            } else if ch == '\\' {
                self.escape = true;
            } else if ch == '"' {
                match self.in_quotes {
                    Some(InQuotes::Single) => self.push_char('"'),
                    Some(InQuotes::Double) => self.in_quotes = None,
                    None => self.in_quotes = Some(InQuotes::Double),
                }
            } else if ch == '\'' {
                match self.in_quotes {
                    Some(InQuotes::Single) => self.in_quotes = None,
                    Some(InQuotes::Double) => self.push_char('\''),
                    None => self.in_quotes = Some(InQuotes::Single),
                }
            } else if ch.is_whitespace() && self.in_quotes.is_none() {
                if !self.parts.last().is_some_and(std::string::String::is_empty) {
                    self.parts.push(String::new());
                }
            } else {
                self.push_char(ch);
            }
        }
        self
    }

    /// Append string verbatim to the last argument.
    pub fn push_str(&mut self, s: &str) -> &mut Self {
        if let Some(last) = self.parts.last_mut() {
            last.push_str(s);
        } else if !s.is_empty() {
            self.parts.push(s.to_owned());
        }
        self
    }

    /// Append a string representing arguments.
    ///
    /// 1. If currently inside quotes, the string is appended to the last
    ///    argument verbatim (including whichspace and quotes, which will not
    ///    terminate the current quotation).
    /// 2. Otherwise, split the string by whitespace and pass each part as a
    ///    separate argument.
    pub fn push_arg(&mut self, s: &str) -> &mut Self {
        if self.in_quotes.is_some() {
            self.push_str(s);
        } else {
            let trimmed = s.trim();
            if !trimmed.is_empty() {
                if let Some(last) = self.parts.last_mut() {
                    if last.is_empty() {
                        last.push_str(trimmed);
                        return self;
                    }
                }

                self.parts.push(trimmed.to_owned());
            }
        }
        self
    }

    /// Append values recursively. If currently inside of quotes, the values are
    /// passed as a single argument. Otherwise, each value is passed as a
    /// separate argument.
    pub fn push_all(&mut self, value: &Value) -> &mut Self {
        if self.in_quotes.is_some() {
            let mut first = true;
            value.for_each_string_recursive(|s| {
                let s = s.trim();
                if !s.is_empty() {
                    if first {
                        first = false;
                    } else {
                        self.push_char(' ');
                    }
                    self.push_str(s);
                }
            });
        } else {
            value.for_each_string_recursive(|s| {
                self.push_arg(s);
            });
        }
        self
    }

    pub fn build(
        &mut self,
        span: Span,
        workspace: &Workspace,
    ) -> Result<(ShellCommandLine, Option<UsedVariable>), EvalError> {
        if self.in_quotes.is_some() {
            Err(EvalError::UnterminatedQuote(span))
        } else {
            let mut parts = self.parts.drain(..);
            let Some(program) = parts.next() else {
                return Err(EvalError::EmptyCommand(span));
            };

            let (program_path, hash) = workspace
                .which(&program)
                .map_err(|err| EvalError::CommandNotFound(span, program.clone(), err))?;
            let used = hash.map(|hash| UsedVariable::Which(Symbol::new(&program), hash));

            Ok((
                ShellCommandLine {
                    program: program_path.into_owned(),
                    arguments: parts.collect(),
                },
                used,
            ))
        }
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
