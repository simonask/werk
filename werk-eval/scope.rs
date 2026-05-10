use stringleton::Symbol;
use werk_fs::{Absolute, PathError};
use werk_util::{DiagnosticSpan, IoError};

use crate::{DirEntry, Eval, GlobError, Io, Messenger, PatternMatchData, Used, Value, Warning};

pub type LocalVariables = indexmap::IndexMap<Symbol, Eval<Value>>;

pub struct ConfigVar {
    /// Snapshot of the value when the `config` statement was evaluated
    /// (unshadowed).
    pub value: Value,
    /// Doc comment
    pub comment: String,
    /// The span of the `config` statement.
    pub span: DiagnosticSpan,
}

pub trait Scope: Send + Sync {
    fn get(&self, name: Lookup) -> Option<LookupValue<'_>>;
    fn io(&self) -> &dyn Io;
    fn messenger(&self) -> &dyn Messenger;
    fn message(&self, message: &str);
    fn warning(&self, warning: &Warning);
    fn which(&self, program_name: &str)
    -> Result<Eval<Absolute<std::path::PathBuf>>, which::Error>;
    fn env(&self, variable_name: &str) -> Eval<Option<String>>;
    fn resolve_path(&self, path: &Absolute<werk_fs::Path>) -> Absolute<std::path::PathBuf>;
    fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError>;

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, GlobError>;
    fn current_working_directory(&self) -> &Absolute<std::path::Path>;

    fn stat_file_if_exists(
        &self,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Option<DirEntry>, IoError> {
        match self.stat_file(path) {
            Ok(metadata) => Ok(Some(metadata)),
            Err(err) if err.error.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(err) => Err(err),
        }
    }

    fn stat_file(&self, path: &Absolute<werk_fs::Path>) -> Result<DirEntry, IoError> {
        let os_path = self.resolve_path(path);
        self.io().metadata(&os_path).map(|metadata| DirEntry {
            path: os_path,
            metadata: crate::Metadata {
                mtime: metadata.mtime,
                is_file: metadata.is_file,
                is_symlink: metadata.is_symlink,
            },
        })
    }
}

pub trait ScopeMut: Scope {
    fn set_local(&mut self, span: DiagnosticSpan, name: Symbol, value: Eval<Value>);
}

pub trait BuildTaskScope: ScopeMut {
    fn push_input_files(&mut self, files: Vec<String>);
}

pub struct SubexprScope<'a> {
    parent: &'a dyn Scope,
    /// The value in the current scope that will be used in stemless `{}` string
    /// interpolations.
    pub implied_value: &'a Eval<Value>,
}

pub struct MatchScope<'a> {
    parent: &'a dyn Scope,
    pattern_match: &'a PatternMatchData,
    /// The matched string.
    pub implied_value: &'a Eval<Value>,
}

/// Look up a variable in a scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lookup {
    /// The scope's implied value (forwarded from another expression). An empty
    /// stem in a string interpolation.
    Implied,
    /// The stem of the current pattern. `{%}` in string interpolation.
    PatternStem,
    /// The captured value of a one-of pattern in the current pattern. `{1}` in
    /// string interpolation.
    CaptureGroup(u32),
    /// Lookup by identifier. `{ident}` in string interpolation.
    Ident(Symbol),
    /// The `^` special variable in build recipes. Cannot be shadowed.
    InputFile,
    /// The `@` special variable in build recipes. Cannot be shadowed.
    OutputFile,
}

#[derive(Debug, Clone)]
pub enum LookupValue<'a> {
    Owned(Eval<Value>),
    EvalRef(&'a Eval<Value>),
    ValueRef(Eval<&'a Value>),
    Ref(&'a Value, &'a Used),
}

impl LookupValue<'_> {
    #[inline]
    #[must_use]
    pub fn used(&self) -> &Used {
        match self {
            LookupValue::Owned(value) => &value.used,
            LookupValue::EvalRef(value) => &value.used,
            LookupValue::ValueRef(value) => &value.used,
            LookupValue::Ref(_, used) => used,
        }
    }

    #[must_use]
    pub fn into_value(self) -> Value {
        match self {
            LookupValue::Owned(eval) => eval.value,
            LookupValue::EvalRef(eval) => eval.value.clone(),
            LookupValue::ValueRef(eval) => eval.value.clone(),
            LookupValue::Ref(value, _) => value.clone(),
        }
    }

    #[must_use]
    pub fn into_owned(self) -> Eval<Value> {
        match self {
            LookupValue::Owned(eval) => eval,
            LookupValue::EvalRef(eval) => eval.clone(),
            LookupValue::ValueRef(eval) => Eval {
                value: eval.value.clone(),
                used: eval.used,
            },
            LookupValue::Ref(value, used) => Eval {
                value: value.clone(),
                used: used.clone(),
            },
        }
    }
}

impl std::ops::Deref for LookupValue<'_> {
    type Target = Value;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self {
            LookupValue::Owned(value) => value,
            LookupValue::EvalRef(value) => value,
            LookupValue::ValueRef(value) => value,
            LookupValue::Ref(value, _) => value,
        }
    }
}

impl<'a> SubexprScope<'a> {
    #[inline]
    pub fn new(parent: &'a dyn Scope, implied_value: &'a Eval<Value>) -> Self {
        SubexprScope {
            parent,
            implied_value,
        }
    }
}

impl<'a> MatchScope<'a> {
    #[inline]
    pub fn new(
        parent: &'a dyn Scope,
        pattern_match: &'a PatternMatchData,
        matched_string: &'a Eval<Value>,
    ) -> Self {
        MatchScope {
            parent,
            pattern_match,
            implied_value: matched_string,
        }
    }
}

impl Scope for SubexprScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
        match lookup {
            Lookup::Implied => Some(LookupValue::EvalRef(self.implied_value)),
            _ => self.parent.get(lookup),
        }
    }

    fn io(&self) -> &dyn Io {
        self.parent.io()
    }

    fn messenger(&self) -> &dyn Messenger {
        self.parent.messenger()
    }

    fn message(&self, message: &str) {
        self.parent.message(message);
    }

    fn warning(&self, warning: &Warning) {
        self.parent.warning(warning);
    }

    fn which(
        &self,
        program_name: &str,
    ) -> Result<Eval<Absolute<std::path::PathBuf>>, which::Error> {
        self.parent.which(program_name)
    }

    fn env(&self, variable_name: &str) -> Eval<Option<String>> {
        self.parent.env(variable_name)
    }

    fn resolve_path(&self, path: &Absolute<werk_fs::Path>) -> Absolute<std::path::PathBuf> {
        self.parent.resolve_path(path)
    }

    fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        self.parent.unresolve_path(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, GlobError> {
        self.parent.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.parent.current_working_directory()
    }
}

impl Scope for MatchScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
        match lookup {
            Lookup::PatternStem => {
                let stem = self.pattern_match.stem()?;
                Some(LookupValue::Owned(Eval::inherent(Value::from(stem))))
            }
            Lookup::CaptureGroup(index) => {
                let group = self.pattern_match.capture_group(index as usize)?;
                Some(LookupValue::Owned(Eval::inherent(Value::from(group))))
            }
            Lookup::Implied => Some(LookupValue::EvalRef(self.implied_value)),
            _ => self.parent.get(lookup),
        }
    }

    fn io(&self) -> &dyn Io {
        self.parent.io()
    }

    fn messenger(&self) -> &dyn Messenger {
        self.parent.messenger()
    }

    fn message(&self, message: &str) {
        self.parent.message(message);
    }

    fn warning(&self, warning: &Warning) {
        self.parent.warning(warning);
    }

    fn which(
        &self,
        program_name: &str,
    ) -> Result<Eval<Absolute<std::path::PathBuf>>, which::Error> {
        self.parent.which(program_name)
    }

    fn env(&self, variable_name: &str) -> Eval<Option<String>> {
        self.parent.env(variable_name)
    }

    fn resolve_path(&self, path: &Absolute<werk_fs::Path>) -> Absolute<std::path::PathBuf> {
        self.parent.resolve_path(path)
    }

    fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        self.parent.unresolve_path(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, GlobError> {
        self.parent.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.parent.current_working_directory()
    }
}
