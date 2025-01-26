use std::{
    collections::{BTreeMap, BTreeSet},
    ops::{BitOr, BitOrAssign},
};

use werk_fs::Absolute;

use crate::{
    cache::TargetOutdatednessCache,
    eval::{Used, UsedVariable},
    ir, TaskId, Workspace,
};

/// A reason why a variable or recipe is "outdated".
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reason {
    /// The output file of a recipe does not exist.
    Missing(Absolute<werk_fs::PathBuf>),
    /// A source file was newer than its output.
    Modified(Absolute<werk_fs::PathBuf>, std::time::SystemTime),
    /// The result of a glob operation changed between runs.
    Glob(String),
    /// The value of a used environment variable changed between runs.
    Env(String),
    /// The resolved path of a binary executable changed between runs.
    Which(String),
    /// The constant value of a global variable changed between runs.
    GlobalChanged(String),
    /// Recipe changed between runs.
    RecipeChanged,
    /// Manual define changed.
    Define(String),
    /// The recipe has a dependency that was rebuilt.
    Rebuilt(TaskId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Outdatedness {
    pub reasons: BTreeSet<Reason>,
}

impl Outdatedness {
    pub fn new<I: IntoIterator<Item = Reason>>(reasons: I) -> Self {
        Self::from_iter(reasons)
    }

    #[must_use]
    pub fn unchanged() -> Self {
        Self {
            reasons: BTreeSet::new(),
        }
    }

    #[inline]
    #[must_use]
    pub fn outdated(reason: Reason) -> Self {
        let mut reasons = BTreeSet::new();
        reasons.insert(reason);
        Self { reasons }
    }

    #[inline]
    #[must_use]
    pub fn is_unchanged(&self) -> bool {
        self.reasons.is_empty()
    }

    #[inline]
    #[must_use]
    pub fn is_outdated(&self) -> bool {
        !self.reasons.is_empty()
    }

    #[inline]
    pub fn insert(&mut self, reason: Reason) {
        self.reasons.insert(reason);
    }
}

impl FromIterator<Reason> for Outdatedness {
    fn from_iter<T: IntoIterator<Item = Reason>>(iter: T) -> Self {
        Outdatedness {
            reasons: BTreeSet::from_iter(iter),
        }
    }
}

impl BitOrAssign for Outdatedness {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        if self.reasons.is_empty() {
            self.reasons = rhs.reasons;
            return;
        }

        self.reasons.extend(rhs.reasons);
    }
}

impl BitOrAssign<&Outdatedness> for Outdatedness {
    #[inline]
    fn bitor_assign(&mut self, rhs: &Self) {
        self.reasons.extend(rhs.reasons.iter().cloned());
    }
}

impl BitOr for Outdatedness {
    type Output = Self;

    #[inline]
    fn bitor(mut self, rhs: Self) -> Self {
        self |= rhs;
        self
    }
}

impl BitOr<&Outdatedness> for Outdatedness {
    type Output = Self;

    #[inline]
    fn bitor(mut self, rhs: &Self) -> Self {
        self |= rhs;
        self
    }
}

impl std::fmt::Display for Reason {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reason::Missing(path_buf) => write!(f, "`{path_buf}` does not exist"),
            Reason::Modified(path_buf, _) => write!(f, "`{path_buf}` was modified"),
            Reason::Glob(pattern) => write!(f, "glob result '{pattern}' changed"),
            Reason::Env(env) => write!(f, "environment variable `{env}` changed"),
            Reason::Which(program) => write!(f, "resolved path of `{program}` changed"),
            Reason::RecipeChanged => f.write_str("recipe changed"),
            Reason::GlobalChanged(variable) => write!(f, "global variable `{variable}` changed"),
            Reason::Define(define) => write!(f, "variable `{define}` was manually overridden"),
            Reason::Rebuilt(task_id) => {
                if task_id.is_command() {
                    write!(f, "`{task_id}` is a command recipe")
                } else {
                    write!(f, "dependency `{task_id}` was rebuilt")
                }
            }
        }
    }
}

pub struct OutdatednessTracker<'a> {
    outdatedness: Outdatedness,
    cache: Option<&'a TargetOutdatednessCache>,
    new_cache: TargetOutdatednessCache,
    target_mtime: Option<std::time::SystemTime>,
}

impl<'a> OutdatednessTracker<'a> {
    pub fn new(
        workspace: &'a Workspace,
        cache: Option<&'a TargetOutdatednessCache>,
        recipe: &ir::BuildRecipe,
        target_mtime: Option<std::time::SystemTime>,
    ) -> Self {
        let mut outdatedness = Outdatedness::unchanged();
        let recipe_hash = workspace.register_used_recipe_hash(recipe);
        if let Some(cache) = cache {
            if recipe_hash != cache.recipe_hash {
                outdatedness.insert(Reason::RecipeChanged);
            }
        }
        let new_cache = TargetOutdatednessCache {
            recipe_hash,
            glob: BTreeMap::default(),
            which: BTreeMap::default(),
            env: BTreeMap::default(),
            define: BTreeMap::default(),
            global: BTreeMap::default(),
        };

        Self {
            outdatedness,
            cache,
            new_cache,
            target_mtime,
        }
    }

    pub fn did_use(&mut self, used: Used) {
        for var in used.vars {
            match var {
                UsedVariable::Glob(glob, hash) => {
                    if self
                        .cache
                        .is_some_and(|cache| cache.is_glob_outdated(&glob, hash))
                    {
                        self.outdatedness.insert(Reason::Glob(glob.clone()));
                    }
                    self.new_cache.glob.insert(glob, hash);
                }
                UsedVariable::Which(which, hash) => {
                    if self
                        .cache
                        .is_some_and(|cache| cache.is_which_outdated(&which, hash))
                    {
                        self.outdatedness.insert(Reason::Which(which.clone()));
                    }
                    self.new_cache.which.insert(which.clone(), hash);
                }
                UsedVariable::Env(env, hash) => {
                    if self
                        .cache
                        .is_some_and(|cache| cache.is_env_outdated(&env, hash))
                    {
                        self.outdatedness.insert(Reason::Env(env.clone()));
                    }
                    self.new_cache.env.insert(env.clone(), hash);
                }
                UsedVariable::Define(def, hash) => {
                    if self
                        .cache
                        .is_some_and(|cache| cache.is_define_outdated(&def, hash))
                    {
                        self.outdatedness.insert(Reason::Define(def.clone()));
                    }
                    self.new_cache.define.insert(def.clone(), hash);
                }
                UsedVariable::Global(var, hash) => {
                    if self
                        .cache
                        .is_some_and(|cache| cache.is_global_outdated(&var, hash))
                    {
                        self.outdatedness.insert(Reason::GlobalChanged(var.clone()));
                    }
                    self.new_cache.global.insert(var.clone(), hash);
                }
                UsedVariable::WorkspaceFile(path, mtime) => {
                    if let Some(target_mtime) = self.target_mtime {
                        if mtime > target_mtime {
                            self.outdatedness.insert(Reason::Modified(path, mtime));
                        }
                    }
                }
            }
        }
    }

    pub fn target_does_not_exist(&mut self, target: Absolute<werk_fs::PathBuf>) {
        self.outdatedness.insert(Reason::Missing(target));
    }

    pub fn add_reason(&mut self, reason: Reason) {
        self.outdatedness.insert(reason);
    }

    pub fn add_reasons(&mut self, reasons: impl IntoIterator<Item = Reason>) {
        self.outdatedness.reasons.extend(reasons);
    }

    #[must_use]
    pub fn finish(mut self) -> (Outdatedness, TargetOutdatednessCache) {
        // Any manual defines that were previously used, but were not used this
        // time, should also contribute to outdatedness.
        if let Some(cache) = self.cache {
            for key in cache.define.keys() {
                if !self.new_cache.define.contains_key(key) {
                    self.outdatedness.insert(Reason::Define(key.clone()));
                }
            }
        }

        (self.outdatedness, self.new_cache)
    }
}
