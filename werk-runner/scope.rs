use ahash::HashMap;

use crate::{
    eval::{Eval, Used},
    ir, Io, PatternMatchData, Render, TaskId, Value, Workspace,
};

pub type LocalVariables = indexmap::IndexMap<String, Eval<Value>>;
pub type GlobalVariables = indexmap::IndexMap<String, GlobalVar>;

pub struct GlobalVar {
    pub value: Eval<Value>,
    /// Doc comment.
    pub comment: String,
}

pub struct RootScope<'a> {
    pub workspace: &'a Workspace<'a>,
}

pub struct TaskRecipeScope<'a> {
    parent: &'a RootScope<'a>,
    vars: LocalVariables,
    task_id: &'a TaskId,
}

pub struct BuildRecipeScope<'a> {
    parent: &'a RootScope<'a>,
    vars: LocalVariables,
    task_id: &'a TaskId,
    recipe_match: &'a ir::BuildRecipeMatch<'a>,
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
pub enum Lookup<'a> {
    /// The scope's implied value (forwarded from another expression). An empty
    /// stem in a string interpolation.
    Implied,
    /// The stem of the current pattern. `{%}` in string interpolation.
    PatternStem,
    /// The captured value of a one-of pattern in the current pattern. `{1}` in
    /// string interpolation.
    CaptureGroup(u32),
    /// Lookup by identifier. `{ident}` in string interpolation.
    Ident(&'a str),
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

pub trait Scope: Send + Sync {
    fn get<'b>(&'b self, name: Lookup<'_>) -> Option<LookupValue<'b>>;
    fn workspace(&self) -> &Workspace;

    fn task_id(&self) -> Option<&TaskId>;
    fn render(&self) -> &dyn Render;

    fn io(&self) -> &dyn Io {
        self.workspace().io()
    }
}

impl<'a> RootScope<'a> {
    #[inline]
    pub fn new(workspace: &'a Workspace) -> Self {
        Self { workspace }
    }
}

impl<'a> TaskRecipeScope<'a> {
    #[inline]
    #[must_use]
    pub fn new(root: &'a RootScope<'a>, task_id: &'a TaskId) -> Self {
        Self {
            parent: root,
            vars: LocalVariables::new(),
            task_id,
        }
    }

    pub fn set(&mut self, name: String, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            tracing::warn!("Shadowing built-in constant `{}`", name);
        }
        self.vars.insert(name, value);
    }
}

impl<'a> BuildRecipeScope<'a> {
    #[inline]
    #[must_use]
    pub fn new(
        root: &'a RootScope<'a>,
        task_id: &'a TaskId,
        recipe_match: &'a ir::BuildRecipeMatch<'a>,
    ) -> Self {
        Self {
            parent: root,
            vars: LocalVariables::new(),
            task_id,
            recipe_match,
            input_files: Value::List(Vec::new()),
            output_file: Value::String(recipe_match.target_file.to_string()),
        }
    }

    pub fn set(&mut self, name: String, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            tracing::warn!("Shadowing built-in constant `{}`", name);
        }
        self.vars.insert(name, value);
    }

    pub fn push_input_file(&mut self, name: String) {
        let Value::List(ref mut input_files) = self.input_files else {
            unreachable!()
        };
        input_files.push(Value::String(name));
    }

    pub fn push_input_files(&mut self, names: &[String]) {
        let Value::List(ref mut input_files) = self.input_files else {
            unreachable!()
        };
        for name in names {
            input_files.push(Value::String(name.clone()));
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

pub fn default_global_constants() -> &'static HashMap<String, Value> {
    static GLOBAL_CONSTANTS: std::sync::OnceLock<HashMap<String, Value>> =
        std::sync::OnceLock::new();
    GLOBAL_CONSTANTS.get_or_init(|| {
        let mut map = HashMap::default();
        map.insert(
            "EXE_SUFFIX".to_owned(),
            Value::String(if cfg!(windows) { ".exe" } else { "" }.to_owned()),
        );
        map.insert("EMPTY".to_owned(), Value::String(String::new()));
        map
    })
}

impl Scope for RootScope<'_> {
    #[inline]
    fn get<'b>(&'b self, name: Lookup<'_>) -> Option<LookupValue<'b>> {
        let Lookup::Ident(name) = name else {
            return None;
        };

        let Some(global) = self.workspace.manifest.globals.get(name) else {
            return default_global_constants()
                .get(name)
                .map(Eval::inherent)
                .map(LookupValue::ValueRef);
        };

        Some(LookupValue::Ref(&global.value.value, &global.value.used))
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.workspace
    }

    #[inline]
    fn task_id(&self) -> Option<&TaskId> {
        None
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.workspace.render
    }
}

impl Scope for TaskRecipeScope<'_> {
    #[inline]
    fn get<'b>(&'b self, lookup: Lookup<'_>) -> Option<LookupValue<'b>> {
        let Lookup::Ident(name) = lookup else {
            return None;
        };

        let Some(local) = self.vars.get(name) else {
            return self.parent.get(lookup);
        };

        Some(LookupValue::Ref(&local.value, &local.used))
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.parent.workspace
    }

    #[inline]
    fn task_id(&self) -> Option<&TaskId> {
        Some(self.task_id)
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.workspace.render
    }
}

impl Scope for BuildRecipeScope<'_> {
    #[inline]
    fn get<'b>(&'b self, lookup: Lookup<'_>) -> Option<LookupValue<'b>> {
        match lookup {
            Lookup::Implied => None,
            Lookup::PatternStem => {
                let stem = self.recipe_match.match_data.stem()?;
                Some(LookupValue::Owned(Eval::inherent(Value::String(
                    stem.to_owned(),
                ))))
            }
            Lookup::CaptureGroup(index) => {
                let group = self.recipe_match.match_data.capture_group(index as usize)?;
                Some(LookupValue::Owned(Eval::inherent(Value::String(
                    group.to_owned(),
                ))))
            }
            Lookup::InputFile | Lookup::Ident("in") => {
                Some(LookupValue::ValueRef(Eval::inherent(&self.input_files)))
            }
            Lookup::OutputFile | Lookup::Ident("out") => {
                Some(LookupValue::ValueRef(Eval::inherent(&self.output_file)))
            }
            Lookup::Ident(name) => {
                let Some(local) = self.vars.get(name) else {
                    return self.parent.get(lookup);
                };
                Some(LookupValue::EvalRef(local))
            }
        }
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.parent.workspace
    }

    #[inline]
    fn task_id(&self) -> Option<&TaskId> {
        Some(self.task_id)
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.workspace.render
    }
}

impl Scope for SubexprScope<'_> {
    #[inline]
    fn get<'b>(&'b self, lookup: Lookup<'_>) -> Option<LookupValue<'b>> {
        match lookup {
            Lookup::Implied => Some(LookupValue::EvalRef(self.implied_value)),
            _ => self.parent.get(lookup),
        }
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.parent.workspace()
    }

    #[inline]
    fn task_id(&self) -> Option<&TaskId> {
        self.parent.task_id()
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.render()
    }
}

impl Scope for MatchScope<'_> {
    #[inline]
    fn get<'b>(&'b self, lookup: Lookup<'_>) -> Option<LookupValue<'b>> {
        match lookup {
            Lookup::PatternStem => {
                let stem = self.pattern_match.stem()?;
                Some(LookupValue::Owned(Eval::inherent(Value::String(
                    stem.to_owned(),
                ))))
            }
            Lookup::CaptureGroup(index) => {
                let group = self.pattern_match.capture_group(index as usize)?;
                Some(LookupValue::Owned(Eval::inherent(Value::String(
                    group.to_owned(),
                ))))
            }
            Lookup::Implied => Some(LookupValue::EvalRef(self.implied_value)),
            _ => self.parent.get(lookup),
        }
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.parent.workspace()
    }

    #[inline]
    fn task_id(&self) -> Option<&TaskId> {
        self.parent.task_id()
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.render()
    }
}
