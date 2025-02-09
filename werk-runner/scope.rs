use ahash::HashMap;
use werk_parser::parser::Span;
use werk_util::{Symbol, SymbolRegistryLock};

use crate::{
    eval::{Eval, Used},
    ir, EvalError, Io, PatternMatchData, Render, TaskId, Value, Workspace,
};

pub type LocalVariables = indexmap::IndexMap<Symbol, Eval<Value>>;

#[derive(Default)]
pub struct GlobalVariables {
    /// `config` variables and their values at the point when they were
    /// evaluated.
    pub configs: indexmap::IndexMap<Symbol, ConfigVar>,
    /// All variables (both `let` and `config`) with their values at the current
    /// point in their scope.
    pub variables: ahash::HashMap<Symbol, Eval<Value>>,
}

pub struct ConfigVar {
    /// Snapshot of the value when the `config` statement was evaluated
    /// (unshadowed).
    pub value: Value,
    /// Doc comment
    pub comment: String,
    /// The span of the `config` statement.
    pub span: Span,
}

impl GlobalVariables {
    #[inline]
    pub fn set(&mut self, name: Symbol, value: Eval<Value>) {
        self.variables.insert(name, value);
    }

    #[inline]
    #[must_use]
    pub fn get(&self, name: Symbol) -> Option<&Eval<Value>> {
        self.variables.get(&name)
    }

    /// Set the value of an evaluated `config` statement.
    ///
    /// This fails if a previous config statement has already defined a value
    /// here, and captures the value at this point to prevent shadowing `let`
    /// statements from polluting the output of `--list`.
    pub fn set_config(
        &mut self,
        name: Symbol,
        value: Eval<Value>,
        span: Span,
        comment: String,
    ) -> Result<(), EvalError> {
        if let Some(previous_value) = self.configs.insert(
            name,
            ConfigVar {
                value: value.value.clone(),
                span,
                comment,
            },
        ) {
            return Err(EvalError::DuplicateConfigStatement(
                span,
                previous_value.span,
            ));
        }
        self.variables.insert(name, value);
        Ok(())
    }
}

pub struct RootScope<'a> {
    pub workspace: &'a Workspace<'a>,
}

pub struct TaskRecipeScope<'a> {
    parent: &'a RootScope<'a>,
    vars: LocalVariables,
    task_id: TaskId,
}

pub struct BuildRecipeScope<'a> {
    parent: &'a RootScope<'a>,
    vars: LocalVariables,
    task_id: TaskId,
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

pub trait Scope: Send + Sync {
    fn get(&self, name: Lookup) -> Option<LookupValue<'_>>;
    fn workspace(&self) -> &Workspace;

    fn task_id(&self) -> Option<TaskId>;
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
    pub fn new(root: &'a RootScope<'a>, task_id: TaskId) -> Self {
        Self {
            parent: root,
            vars: LocalVariables::new(),
            task_id,
        }
    }

    pub fn set(&mut self, name: Symbol, value: Eval<Value>) {
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
        task_id: TaskId,
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

#[must_use]
pub const fn current_os() -> &'static str {
    if cfg!(target_os = "windows") {
        "windows"
    } else if cfg!(target_os = "macos") {
        "macos"
    } else if cfg!(target_os = "ios") {
        "ios"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else if cfg!(target_os = "android") {
        "android"
    } else if cfg!(target_os = "freebsd") {
        "freebsd"
    } else if cfg!(target_os = "dragonfly") {
        "dragonfly"
    } else if cfg!(target_os = "openbsd") {
        "openbsd"
    } else if cfg!(target_os = "netbsd") {
        "netbsd"
    } else if cfg!(target_family = "wasm") {
        "wasm-wasi"
    } else {
        "none"
    }
}

#[must_use]
pub const fn current_os_family() -> &'static str {
    if cfg!(target_family = "unix") {
        "unix"
    } else if cfg!(target_family = "windows") {
        "windows"
    } else if cfg!(target_family = "wasm") {
        "wasm"
    } else {
        "none"
    }
}

#[must_use]
pub const fn current_arch() -> &'static str {
    if cfg!(target_arch = "x86") {
        "x86"
    } else if cfg!(target_arch = "x86_64") {
        "x86_64"
    } else if cfg!(target_arch = "mips") {
        "mips"
    } else if cfg!(target_arch = "powerpc") {
        "powerpc"
    } else if cfg!(target_arch = "powerpc64") {
        "powerpc64"
    } else if cfg!(target_arch = "arm") {
        "arm"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else if cfg!(target_family = "wasm") {
        "wasm"
    } else {
        "none"
    }
}

#[must_use]
pub const fn current_arch_family() -> &'static str {
    if cfg!(any(target_arch = "x86", target_arch = "x86_64")) {
        "x86"
    } else if cfg!(target_arch = "mips") {
        "mips"
    } else if cfg!(any(target_arch = "powerpc", target_arch = "powerpc64")) {
        "powerpc"
    } else if cfg!(any(target_arch = "arm", target_arch = "aarch64")) {
        "arm"
    } else if cfg!(target_family = "wasm") {
        "wasm"
    } else {
        "none"
    }
}

#[must_use]
pub const fn exe_suffix() -> &'static str {
    if cfg!(windows) {
        ".exe"
    } else {
        ""
    }
}

#[must_use]
pub const fn dylib_prefix() -> &'static str {
    if cfg!(windows) {
        ""
    } else {
        "lib"
    }
}

#[must_use]
pub const fn dylib_suffix() -> &'static str {
    if cfg!(windows) {
        ".dll"
    } else if cfg!(any(target_os = "macos", target_os = "ios")) {
        ".dylib"
    } else {
        ".so"
    }
}

#[must_use]
pub const fn staticlib_prefix() -> &'static str {
    if cfg!(windows) {
        ""
    } else {
        "lib"
    }
}

#[must_use]
pub const fn staticlib_suffix() -> &'static str {
    if cfg!(windows) {
        ".lib"
    } else {
        ".a"
    }
}

pub fn default_global_constants() -> &'static HashMap<Symbol, Value> {
    static GLOBAL_CONSTANTS: std::sync::OnceLock<HashMap<Symbol, Value>> =
        std::sync::OnceLock::new();
    GLOBAL_CONSTANTS.get_or_init(|| {
        let mut map = HashMap::default();
        let mut sym = SymbolRegistryLock::lock();
        map.extend([
            (sym.insert("EMPTY"), Value::String(String::new())),
            (
                sym.insert("EXE_SUFFIX"),
                Value::String(exe_suffix().to_owned()),
            ),
            (
                sym.insert("DYLIB_PREFIX"),
                Value::String(dylib_prefix().to_owned()),
            ),
            (
                sym.insert("DYLIB_SUFFIX"),
                Value::String(dylib_suffix().to_owned()),
            ),
            (
                sym.insert("STATICLIB_PREFIX"),
                Value::String(staticlib_prefix().to_owned()),
            ),
            (
                sym.insert("STATICLIB_SUFFIX"),
                Value::String(staticlib_suffix().to_owned()),
            ),
            (sym.insert("OS"), Value::String(current_os().to_owned())),
            (
                sym.insert("OS_FAMILY"),
                Value::String(current_os_family().to_owned()),
            ),
            (sym.insert("ARCH"), Value::String(current_arch().to_owned())),
            (
                sym.insert("ARCH_FAMILY"),
                Value::String(current_arch_family().to_owned()),
            ),
        ]);
        map
    })
}

pub struct SymCache {
    pub symbol_in: Symbol,
    pub symbol_out: Symbol,
    pub symbol_color: Symbol,
}

impl SymCache {
    pub fn get() -> &'static SymCache {
        static CACHE: std::sync::OnceLock<SymCache> = std::sync::OnceLock::new();
        CACHE.get_or_init(|| {
            let mut sym = SymbolRegistryLock::lock();
            SymCache {
                symbol_in: sym.insert("in"),
                symbol_out: sym.insert("out"),
                symbol_color: sym.insert("COLOR"),
            }
        })
    }
}

impl Scope for RootScope<'_> {
    #[inline]
    fn get(&self, name: Lookup) -> Option<LookupValue<'_>> {
        let Lookup::Ident(name) = name else {
            return None;
        };

        let Some(global) = self.workspace.manifest.globals.get(name) else {
            // Global build-time constants.
            if let Some(global_constant) = default_global_constants()
                .get(&name)
                .map(Eval::inherent)
                .map(LookupValue::ValueRef)
            {
                return Some(global_constant);
            }

            // Runtime constants.
            let cache = SymCache::get();
            if name == cache.symbol_color {
                return Some(LookupValue::Owned(Eval::inherent(Value::String(
                    if self.workspace.force_color { "1" } else { "0" }.to_owned(),
                ))));
            }

            return None;
        };

        Some(LookupValue::Ref(&global.value, &global.used))
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.workspace
    }

    #[inline]
    fn task_id(&self) -> Option<TaskId> {
        None
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.workspace.render
    }
}

impl Scope for TaskRecipeScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
        let Lookup::Ident(name) = lookup else {
            return None;
        };

        let Some(local) = self.vars.get(&name) else {
            return self.parent.get(lookup);
        };

        Some(LookupValue::Ref(&local.value, &local.used))
    }

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.parent.workspace
    }

    #[inline]
    fn task_id(&self) -> Option<TaskId> {
        Some(self.task_id)
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.workspace.render
    }
}

impl Scope for BuildRecipeScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
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
            Lookup::InputFile => Some(LookupValue::ValueRef(Eval::inherent(&self.input_files))),
            Lookup::OutputFile => Some(LookupValue::ValueRef(Eval::inherent(&self.output_file))),
            Lookup::Ident(name) => {
                let sym_cache = SymCache::get();
                if name == sym_cache.symbol_in {
                    return Some(LookupValue::ValueRef(Eval::inherent(&self.input_files)));
                } else if name == sym_cache.symbol_out {
                    return Some(LookupValue::ValueRef(Eval::inherent(&self.output_file)));
                }

                let Some(local) = self.vars.get(&name) else {
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
    fn task_id(&self) -> Option<TaskId> {
        Some(self.task_id)
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.workspace.render
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

    #[inline]
    fn workspace(&self) -> &Workspace {
        self.parent.workspace()
    }

    #[inline]
    fn task_id(&self) -> Option<TaskId> {
        self.parent.task_id()
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.render()
    }
}

impl Scope for MatchScope<'_> {
    #[inline]
    fn get(&self, lookup: Lookup) -> Option<LookupValue<'_>> {
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
    fn task_id(&self) -> Option<TaskId> {
        self.parent.task_id()
    }

    #[inline]
    fn render(&self) -> &dyn Render {
        self.parent.render()
    }
}
