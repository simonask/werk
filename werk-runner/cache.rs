use std::collections::BTreeMap;

/// The contents of `.werk-cache`.
#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct WerkCache {
    /// Per-build-target caches.
    pub build: BTreeMap<werk_fs::PathBuf, TargetOutdatednessCache>,
}

/// Per-target cache of used outdatedness information.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TargetOutdatednessCache {
    /// Hash of the recipe AST.
    pub recipe_hash: Hash128,
    /// Hash of used glob patterns.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub glob: BTreeMap<String, Hash128>,
    /// Hash of resolved binary paths.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub which: BTreeMap<String, Hash128>,
    /// Hash of environment variables.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub env: BTreeMap<String, Hash128>,
    /// Hash of the definitions (AST expressions) of global variables used.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub global: BTreeMap<String, Hash128>,
    /// Hash of `define` variables.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub define: BTreeMap<String, Hash128>,
}

impl TargetOutdatednessCache {
    #[inline]
    pub fn is_recipe_outdated(&self, new_hash: Hash128) -> bool {
        self.recipe_hash != new_hash
    }

    #[inline]
    pub fn is_glob_outdated(&self, glob: &str, new_hash: Hash128) -> bool {
        self.glob
            .get(glob)
            .is_some_and(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_which_outdated(&self, which: &str, new_hash: Hash128) -> bool {
        self.which
            .get(which)
            .is_some_and(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_env_outdated(&self, env: &str, new_hash: Hash128) -> bool {
        self.env
            .get(env)
            .is_some_and(|old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_define_outdated(&self, define: &str, new_hash: Hash128) -> bool {
        self.define
            .get(define)
            .map_or(true, |old_hash| *old_hash != new_hash)
    }

    #[inline]
    pub fn is_global_outdated(&self, var: &str, new_hash: Hash128) -> bool {
        self.global
            .get(var)
            .map_or(true, |old_hash| *old_hash != new_hash)
    }
}

impl rustc_stable_hash::FromStableHash for Hash128 {
    type Hash = rustc_stable_hash::SipHasher128Hash;

    fn from(hash: Self::Hash) -> Self {
        let hi = u128::from(hash.0[0]) << 64;
        let lo = u128::from(hash.0[1]);
        Hash128(hi | lo)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Hash128(pub u128);
impl From<u128> for Hash128 {
    #[inline]
    fn from(n: u128) -> Self {
        Hash128(n)
    }
}

impl serde::Serialize for Hash128 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize as hex string. Also, TOML doesn't support 64-bit integers.
        serializer.serialize_str(&format!("{:016x}", self.0))
    }
}

impl<'de> serde::Deserialize<'de> for Hash128 {
    fn deserialize<D>(deserializer: D) -> Result<Hash128, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let n = u128::from_str_radix(&s, 16).map_err(serde::de::Error::custom)?;
        Ok(Hash128(n))
    }
}
