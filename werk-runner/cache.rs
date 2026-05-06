use std::collections::BTreeMap;

use stringleton::Symbol;
use werk_fs::Absolute;
use werk_util::hash128::Hash128;

/// The contents of `.werk-cache`.
#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct WerkCache {
    /// Per-build-target caches.
    #[serde(default)]
    pub build: BTreeMap<Absolute<werk_fs::PathBuf>, TargetOutdatednessCache>,
}

/// Per-target cache of used outdatedness information.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TargetOutdatednessCache {
    /// Hash of the recipe AST.
    pub recipe_hash: Hash128,
    /// Hash of used glob patterns.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub glob: BTreeMap<Symbol, Hash128>,
    /// Hash of resolved binary paths.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub which: BTreeMap<Symbol, Hash128>,
    /// Hash of environment variables.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub env: BTreeMap<Symbol, Hash128>,
    /// Hash of the definitions (AST expressions) of global variables used.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub global: BTreeMap<Symbol, Hash128>,
    /// Hash of `define` variables.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub define: BTreeMap<Symbol, Hash128>,
}

impl TargetOutdatednessCache {
    #[inline]
    pub fn is_recipe_outdated(&self, new_hash: Hash128) -> bool {
        self.recipe_hash != new_hash
    }

    #[inline]
    pub fn is_glob_outdated(&self, glob: Symbol, new_hash: Hash128) -> bool {
        self.glob
            .get(&glob)
            .is_some_and(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_which_outdated(&self, which: Symbol, new_hash: Hash128) -> bool {
        self.which
            .get(&which)
            .is_some_and(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_env_outdated(&self, env: Symbol, new_hash: Hash128) -> bool {
        self.env
            .get(&env)
            .is_some_and(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_define_outdated(&self, define: Symbol, new_hash: Hash128) -> bool {
        self.define
            .get(&define)
            .is_none_or(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_global_outdated(&self, var: Symbol, new_hash: Hash128) -> bool {
        self.global
            .get(&var)
            .is_none_or(|old_hash| *old_hash != new_hash)
    }
}
