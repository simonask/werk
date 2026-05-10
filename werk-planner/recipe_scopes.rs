use stringleton::{Symbol, sym};
use werk_eval::{
    Eval, GlobError, Io, LocalVariables, Lookup, LookupValue, Messenger, Scope, ScopeMut, TaskName,
    Value, Warning, default_global_constants,
};
use werk_fs::{Absolute, PathError};

use crate::BuildRecipeMatch;

pub struct BuildRecipeScope<'a> {
    global_scope: &'a dyn Scope,
    vars: LocalVariables,
    task_id: TaskName,
    recipe_match: &'a BuildRecipeMatch<'a>,
    input_files: Value,
    output_file: Value,
}

impl<'a> BuildRecipeScope<'a> {
    #[inline]
    #[must_use]
    pub fn new(
        global_scope: &'a dyn Scope,
        task_id: TaskName,
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

    pub fn set(&mut self, local_name: Symbol, value: Eval<Value>) {
        self.vars.insert(local_name, value);
    }

    pub fn push_input_file(&mut self, name: String) {
        let Value::List(ref mut input_files) = self.input_files else {
            unreachable!()
        };
        input_files.push(name.into());
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
        self.messenger().warning(Some(self.task_id), warning);
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

    fn resolve_path(&self, path: &Absolute<werk_fs::Path>) -> Absolute<std::path::PathBuf> {
        self.global_scope.resolve_path(path)
    }

    fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        self.global_scope.unresolve_path(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, GlobError> {
        self.global_scope.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.global_scope.current_working_directory()
    }
}

impl ScopeMut for BuildRecipeScope<'_> {
    fn set_local(&mut self, span: werk_util::DiagnosticSpan, name: Symbol, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            self.warning(&Warning::ShadowingGlobalConstant(span, name));
        }
        self.vars.insert(name, value);
    }
}

impl werk_eval::BuildTaskScope for BuildRecipeScope<'_> {
    fn push_input_files(&mut self, files: Vec<String>) {
        let Value::List(ref mut input_files) = self.input_files else {
            unreachable!()
        };
        input_files.extend(files.into_iter().map(Into::into));
    }
}

pub struct TaskRecipeScope<'a> {
    global_scope: &'a dyn Scope,
    vars: LocalVariables,
    task_id: TaskName,
}

impl<'a> TaskRecipeScope<'a> {
    #[inline]
    #[must_use]
    pub fn new(global_scope: &'a dyn Scope, task_id: TaskName) -> Self {
        Self {
            global_scope,
            vars: LocalVariables::new(),
            task_id,
        }
    }

    pub fn set(&mut self, local_name: Symbol, value: Eval<Value>) {
        self.vars.insert(local_name, value);
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
        self.messenger().warning(Some(self.task_id), warning);
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

    fn resolve_path(&self, path: &Absolute<werk_fs::Path>) -> Absolute<std::path::PathBuf> {
        self.global_scope.resolve_path(path)
    }

    fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        self.global_scope.unresolve_path(path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, GlobError> {
        self.global_scope.glob_workspace_files(pattern_string)
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.global_scope.current_working_directory()
    }
}

impl ScopeMut for TaskRecipeScope<'_> {
    fn set_local(&mut self, span: werk_util::DiagnosticSpan, name: Symbol, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            self.warning(&Warning::ShadowingGlobalConstant(span, name));
        }
        self.vars.insert(name, value);
    }
}
