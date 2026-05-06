use stringleton::{Symbol, sym};
use werk_fs::Absolute;
use werk_util::DiagnosticSpan;

use crate::{
    BuildRecipeMatch, DirEntry, Eval, EvalError, Io, Messenger, PatternMatchData, ResolvePathError,
    ResolvePathMode, TaskId, Used, Value, Warning, default_global_constants,
};

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
    fn resolve_path(
        &self,
        path: &Absolute<werk_fs::Path>,
        mode: ResolvePathMode,
    ) -> Result<Absolute<std::path::PathBuf>, ResolvePathError>;

    fn get_output_file_path(
        &self,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Absolute<std::path::PathBuf>, ResolvePathError> {
        self.resolve_path(path, ResolvePathMode::OutDir)
    }

    fn get_input_file(&self, path: &Absolute<werk_fs::Path>) -> Option<DirEntry>;
    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, globset::Error>;
    fn current_working_directory(&self) -> &Absolute<std::path::Path>;
}

pub struct TaskRecipeScope<'a> {
    global_scope: &'a dyn Scope,
    vars: LocalVariables,
    task_id: TaskId,
}

pub struct BuildRecipeScope<'a> {
    global_scope: &'a dyn Scope,
    vars: LocalVariables,
    task_id: TaskId,
    recipe_match: &'a BuildRecipeMatch<'a>,
    input_files: Value,
    output_file: Value,
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

impl<'a> TaskRecipeScope<'a> {
    #[inline]
    #[must_use]
    pub fn new(global_scope: &'a dyn Scope, task_id: TaskId) -> Self {
        Self {
            global_scope,
            vars: LocalVariables::new(),
            task_id,
        }
    }

    pub fn set(&mut self, name: Symbol, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            // TODO: Convert to real warning.
            tracing::warn!("Shadowing built-in constant `{}`", name);
        }
        self.vars.insert(name, value);
    }
}

impl<'a> BuildRecipeScope<'a> {
    #[inline]
    #[must_use]
    pub fn new(
        global_scope: &'a dyn Scope,
        task_id: TaskId,
        recipe_match: &'a BuildRecipeMatch<'a>,
    ) -> Self {
        Self {
            global_scope,
            vars: LocalVariables::new(),
            task_id,
            recipe_match,
            input_files: Value::List(Vec::new()),
            output_file: recipe_match.target_file.to_string().into(),
        }
    }

    pub fn set(&mut self, name: Symbol, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            tracing::warn!("Shadowing built-in constant `{}`", name);
        }
        self.vars.insert(name, value);
    }

    pub fn push_input_file(&mut self, name: String) {
        let Value::List(ref mut input_files) = self.input_files else {
            unreachable!()
        };
        input_files.push(name.into());
    }

    pub fn push_input_files(&mut self, names: impl IntoIterator<Item = String>) {
        let Value::List(ref mut input_files) = self.input_files else {
            unreachable!()
        };
        input_files.extend(names.into_iter().map(Into::into));
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

impl Scope for TaskRecipeScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
        let Lookup::Ident(name) = lookup else {
            return None;
        };

        let Some(local) = self.vars.get(&name) else {
            return self.global_scope.get(lookup);
        };

        Some(LookupValue::Ref(&local.value, &local.used))
    }

    fn io(&self) -> &dyn Io {
        self.global_scope.io()
    }

    fn messenger(&self) -> &dyn Messenger {
        self.global_scope.messenger()
    }

    fn message(&self, message: &str) {
        self.messenger().message(Some(self.task_id), message);
    }

    fn warning(&self, warning: &Warning) {
        self.messenger().warning(Some(self.task_id), warning)
    }

    fn which(
        &self,
        program_name: &str,
    ) -> Result<Eval<Absolute<std::path::PathBuf>>, which::Error> {
        self.global_scope.which(program_name)
    }

    fn env(&self, variable_name: &str) -> Eval<Option<String>> {
        self.global_scope.env(variable_name)
    }

    fn resolve_path(
        &self,
        path: &Absolute<werk_fs::Path>,
        mode: ResolvePathMode,
    ) -> Result<Absolute<std::path::PathBuf>, ResolvePathError> {
        self.global_scope.resolve_path(path, mode)
    }

    fn get_input_file(&self, path: &Absolute<werk_fs::Path>) -> Option<DirEntry> {
        self.global_scope.get_input_file(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, globset::Error> {
        self.global_scope.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.global_scope.current_working_directory()
    }
}

impl Scope for BuildRecipeScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
        match lookup {
            Lookup::Implied => None,
            Lookup::PatternStem => {
                let stem = self.recipe_match.match_data.stem()?;
                Some(LookupValue::Owned(Eval::inherent(Value::from(stem))))
            }
            Lookup::CaptureGroup(index) => {
                let group = self.recipe_match.match_data.capture_group(index as usize)?;
                Some(LookupValue::Owned(Eval::inherent(Value::from(group))))
            }
            Lookup::InputFile => Some(LookupValue::ValueRef(Eval::inherent(&self.input_files))),
            Lookup::OutputFile => Some(LookupValue::ValueRef(Eval::inherent(&self.output_file))),
            Lookup::Ident(name) => {
                if name == sym!(in) {
                    return Some(LookupValue::ValueRef(Eval::inherent(&self.input_files)));
                } else if name == sym!(out) {
                    return Some(LookupValue::ValueRef(Eval::inherent(&self.output_file)));
                }

                let Some(local) = self.vars.get(&name) else {
                    return self.global_scope.get(lookup);
                };
                Some(LookupValue::EvalRef(local))
            }
        }
    }

    fn io(&self) -> &dyn Io {
        self.global_scope.io()
    }

    fn messenger(&self) -> &dyn Messenger {
        self.global_scope.messenger()
    }

    fn message(&self, message: &str) {
        self.messenger().message(Some(self.task_id), message);
    }

    fn warning(&self, warning: &Warning) {
        self.messenger().warning(Some(self.task_id), warning)
    }

    fn which(
        &self,
        program_name: &str,
    ) -> Result<Eval<Absolute<std::path::PathBuf>>, which::Error> {
        self.global_scope.which(program_name)
    }

    fn env(&self, variable_name: &str) -> Eval<Option<String>> {
        self.global_scope.env(variable_name)
    }

    fn resolve_path(
        &self,
        path: &Absolute<werk_fs::Path>,
        mode: ResolvePathMode,
    ) -> Result<Absolute<std::path::PathBuf>, ResolvePathError> {
        self.global_scope.resolve_path(path, mode)
    }

    fn get_input_file(&self, path: &Absolute<werk_fs::Path>) -> Option<DirEntry> {
        self.global_scope.get_input_file(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, globset::Error> {
        self.global_scope.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.global_scope.current_working_directory()
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
        self.parent.message(message)
    }

    fn warning(&self, warning: &Warning) {
        self.parent.warning(warning)
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

    fn resolve_path(
        &self,
        path: &Absolute<werk_fs::Path>,
        mode: ResolvePathMode,
    ) -> Result<Absolute<std::path::PathBuf>, ResolvePathError> {
        self.parent.resolve_path(path, mode)
    }

    fn get_input_file(&self, path: &Absolute<werk_fs::Path>) -> Option<DirEntry> {
        self.parent.get_input_file(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, globset::Error> {
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
        self.parent.message(message)
    }

    fn warning(&self, warning: &Warning) {
        self.parent.warning(warning)
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

    fn resolve_path(
        &self,
        path: &Absolute<werk_fs::Path>,
        mode: ResolvePathMode,
    ) -> Result<Absolute<std::path::PathBuf>, ResolvePathError> {
        self.parent.resolve_path(path, mode)
    }

    fn get_input_file(&self, path: &Absolute<werk_fs::Path>) -> Option<DirEntry> {
        self.parent.get_input_file(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, globset::Error> {
        self.parent.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.parent.current_working_directory()
    }
}
