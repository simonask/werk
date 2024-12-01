use ahash::HashMap;

use crate::{Eval, PatternMatch, TaskId, Value, Watcher, Workspace};

pub type LocalVariables = indexmap::IndexMap<String, Eval<Value>>;

pub struct RootScope<'a> {
    globals: &'a LocalVariables,
    pub workspace: &'a Workspace,
    pub watcher: &'a dyn Watcher,
}

pub struct RecipeScope<'a> {
    parent: &'a RootScope<'a>,
    vars: LocalVariables,
    pattern_match: Option<&'a PatternMatch<'a>>,
    task_id: &'a TaskId,
}

pub struct SubexprScope<'a> {
    parent: &'a dyn Scope,
    pub implied_value: Option<Eval<Value>>,
}

pub trait Scope: Send + Sync {
    fn implied_value(&self) -> Option<&Eval<Value>>;
    fn pattern_match(&self) -> Option<&PatternMatch<'_>>;
    fn get<'b>(&'b self, name: &str) -> Option<Eval<&'b Value>>;
    fn workspace(&self) -> &Workspace;

    fn pattern_stem(&self) -> Option<&str> {
        self.pattern_match().and_then(|pm| pm.stem())
    }

    fn capture_group(&self, group: usize) -> Option<&str> {
        self.pattern_match().and_then(|pm| pm.capture_group(group))
    }

    fn task_id(&self) -> Option<&TaskId>;
    fn watcher(&self) -> &dyn Watcher;
}

impl dyn Scope + '_ {
    pub fn subexpr(&self, implied_value: Option<Eval<Value>>) -> SubexprScope<'_> {
        SubexprScope {
            parent: self,
            implied_value,
        }
    }
}

impl<'a> RootScope<'a> {
    #[inline]
    pub fn new(
        globals: &'a LocalVariables,
        workspace: &'a Workspace,
        watcher: &'a dyn Watcher,
    ) -> Self {
        Self {
            globals,
            workspace,
            watcher,
        }
    }
}

impl<'a> RecipeScope<'a> {
    #[inline]
    pub fn new(
        root: &'a RootScope<'a>,
        task_id: &'a TaskId,
        pattern_match: Option<&'a PatternMatch<'a>>,
    ) -> Self {
        RecipeScope {
            parent: root,
            vars: LocalVariables::new(),
            task_id,
            pattern_match,
        }
    }

    pub fn set(&mut self, name: String, value: Eval<Value>) {
        if default_global_constants().contains_key(&name) {
            tracing::warn!("Shadowing built-in constant `{}`", name);
        }
        self.vars.insert(name.to_owned(), value);
    }
}

impl<'a> SubexprScope<'a> {
    #[inline]
    pub fn new(parent: &'a dyn Scope, implied_value: Option<Eval<Value>>) -> Self {
        SubexprScope {
            parent,
            implied_value,
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
        map
    })
}

impl Scope for RootScope<'_> {
    #[inline]
    fn implied_value(&self) -> Option<&Eval<Value>> {
        None
    }

    #[inline]
    fn pattern_match(&self) -> Option<&PatternMatch<'_>> {
        None
    }

    #[inline]
    fn get<'b>(&'b self, name: &str) -> Option<Eval<&'b Value>> {
        self.globals
            .get(name)
            .map(Eval::as_ref)
            .or_else(|| default_global_constants().get(name).map(Eval::unchanged))
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
    fn watcher(&self) -> &dyn Watcher {
        self.watcher
    }
}

impl Scope for RecipeScope<'_> {
    #[inline]
    fn implied_value(&self) -> Option<&Eval<Value>> {
        None
    }

    #[inline]
    fn pattern_match(&self) -> Option<&PatternMatch<'_>> {
        self.pattern_match
    }

    #[inline]
    fn get<'b>(&'b self, name: &str) -> Option<Eval<&'b Value>> {
        self.vars
            .get(name)
            .map(Eval::as_ref)
            .or_else(|| self.parent.get(name))
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
    fn watcher(&self) -> &dyn Watcher {
        self.parent.watcher
    }
}

impl Scope for SubexprScope<'_> {
    #[inline]
    fn implied_value(&self) -> Option<&Eval<Value>> {
        self.implied_value
            .as_ref()
            .or_else(|| self.parent.implied_value())
    }

    #[inline]
    fn pattern_match(&self) -> Option<&PatternMatch<'_>> {
        self.parent.pattern_match()
    }

    #[inline]
    fn get<'b>(&'b self, name: &str) -> Option<Eval<&'b Value>> {
        self.parent.get(name)
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
    fn watcher(&self) -> &dyn Watcher {
        self.parent.watcher()
    }
}
