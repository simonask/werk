use ahash::HashMap;
use indexmap::IndexMap;
use parking_lot::Mutex;
use std::collections::hash_map;
use werk_fs::PathError;

use crate::{BuildStatus, DirEntry, Error, Io, Outdatedness, Reason};

#[derive(Clone)]
pub struct WorkspaceSettings {
    pub git_ignore: bool,
    pub git_ignore_global: bool,
    pub git_ignore_exclude: bool,
    pub git_ignore_from_parents: bool,
    pub ignore_hidden: bool,
    pub ignore: Vec<std::path::PathBuf>,
    pub defines: HashMap<String, String>,
    /// When true, the [`Runner`](crate::Runner) sets the `FORCE_COLOR` and
    /// `CLICOLOR_FORCE` environment variables to "1" when executing recipe
    /// commands (not when capturing their output in variables).
    pub force_color: bool,
}

impl Default for WorkspaceSettings {
    fn default() -> Self {
        Self {
            git_ignore: true,
            git_ignore_global: true,
            git_ignore_exclude: true,
            git_ignore_from_parents: false,
            ignore_hidden: false,
            ignore: Vec::new(),
            defines: HashMap::default(),
            force_color: false,
        }
    }
}

impl WorkspaceSettings {
    /// Override a global variable in the root scope.
    pub fn define(&mut self, key: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.defines.insert(key.into(), value.into());
        self
    }
}

pub struct Workspace {
    // Project root - note that the workspace only accesses this directory
    // through the `Io` trait, and never directly.
    project_root: std::path::PathBuf,
    // Project root - note that the workspace only accesses this directory
    // through the `Io` trait, and never directly.
    output_directory: std::path::PathBuf,
    // Using IndexMap to ensure that the ordering of glob results is well-defined.
    workspace_files: IndexMap<werk_fs::PathBuf, DirEntry, ahash::RandomState>,
    /// The contents of `<out-dir>/.werk-cache.toml`.
    werk_cache: PersistedCache,
    /// Caches of expensive runtime values (glob, which, env).
    runtime_caches: Mutex<Caches>,
    /// Overridden global variables from the command line.
    pub defines: HashMap<String, String>,
    pub force_color: bool,
}

#[derive(Default)]
struct Caches {
    glob_cache: HashMap<String, (Vec<werk_fs::PathBuf>, Outdatedness, Hash128)>,
    which_cache: HashMap<String, Result<(String, Outdatedness), which::Error>>,
    env_cache: HashMap<String, (String, Outdatedness, Hash128)>,
}

/// Persisted workspace cache, i.e. the contents of `<out-dir>/.werk-cache`.
#[derive(Default, serde::Serialize, serde::Deserialize)]
struct PersistedCache {
    /// Glob strings and the hash of their results. This is used to determine if
    /// a glob changed between runs.
    pub glob: IndexMap<String, Hash128>,
    /// Which cache, mapping program names to their resolved paths. When a
    /// `which` expression results in a different path, the result is considered
    /// outdated.
    pub which: IndexMap<String, String>,
    /// Env cache, mapping used environment variables to their last known hash.
    /// Using a hash here just avoid potentially leaking secrets.
    pub env: IndexMap<String, Hash128>,
}

pub const WERK_CACHE_FILENAME: &str = ".werk-cache";

impl Workspace {
    pub async fn new(
        io: &dyn Io,
        project_root: std::path::PathBuf,
        output_directory: std::path::PathBuf,
        settings: WorkspaceSettings,
    ) -> Result<Self, Error> {
        let werk_cache = read_workspace_cache(io, &output_directory).await;

        let mut workspace_files =
            IndexMap::with_capacity_and_hasher(1024, ahash::RandomState::default());

        for entry in io.walk_directory(&project_root, &settings, &[&output_directory])? {
            let entry = entry?;

            if entry.path.file_name() == Some(WERK_CACHE_FILENAME.as_ref()) {
                return Err(Error::ClobberedWorkspace(entry.path));
            }

            let path_in_project = match werk_fs::Path::unresolve(&entry.path, &project_root) {
                Ok(path_in_project) => path_in_project,
                // This should not be possible.
                Err(err @ PathError::UnresolveBeyondRoot) => return Err(err.into()),
                Err(_) => continue,
            };
            tracing::trace!("Workspace file: {path_in_project}");
            workspace_files.insert(path_in_project, entry);
        }

        // Deterministic workspace order to preserve the ordering of glob
        // results.
        workspace_files.sort_unstable_keys();

        Ok(Self {
            project_root,
            output_directory,
            workspace_files,
            werk_cache,
            runtime_caches: Mutex::new(Caches {
                glob_cache: HashMap::default(),
                which_cache: HashMap::default(),
                env_cache: HashMap::default(),
            }),
            defines: settings.defines,
            force_color: settings.force_color,
        })
    }

    /// Write outdatedness cache (`which` and `glob`)  to "<out-dir>/.werk-cache".
    pub async fn finalize(&self, io: &dyn Io) {
        let caches = self.runtime_caches.lock();
        let new_cache = PersistedCache {
            glob: caches
                .glob_cache
                .iter()
                .map(|(k, (_, _, hash))| (k.clone(), *hash))
                .collect(),
            which: caches
                .which_cache
                .iter()
                .filter_map(|(k, v)| {
                    if let Ok(v) = v {
                        Some((k.clone(), v.0.clone()))
                    } else {
                        None
                    }
                })
                .collect(),
            env: caches
                .env_cache
                .iter()
                .map(|(k, (_, _, hash))| (k.clone(), *hash))
                .collect(),
        };

        write_workspace_cache(io, &self.output_directory, &new_cache).await;
    }

    #[inline]
    pub fn project_root(&self) -> &std::path::Path {
        &self.project_root
    }

    #[inline]
    pub fn output_directory(&self) -> &std::path::Path {
        &self.output_directory
    }

    pub fn get_project_file(&self, path: &werk_fs::Path) -> Option<&DirEntry> {
        self.workspace_files.get(path)
    }

    pub fn get_existing_project_or_output_file(
        &self,
        io: &dyn Io,
        path: &werk_fs::Path,
    ) -> Result<Option<DirEntry>, Error> {
        if let Some(dir_entry) = self.get_project_file(path) {
            return Ok(Some(dir_entry.clone()));
        }

        self.get_existing_output_file(io, path)
    }

    pub fn get_existing_output_file(
        &self,
        io: &dyn Io,
        path: &werk_fs::Path,
    ) -> Result<Option<DirEntry>, Error> {
        let fs_path = path.resolve(&self.output_directory)?;
        match io.metadata(&fs_path) {
            Ok(metadata) => Ok(Some(DirEntry {
                path: fs_path,
                metadata,
            })),
            Err(Error::Io(err)) => {
                if err.kind() == std::io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(err.into())
                }
            }
            Err(err) => Err(err),
        }
    }

    pub async fn create_output_parent_dirs(
        &self,
        io: &dyn Io,
        path: &werk_fs::Path,
    ) -> Result<(), Error> {
        let fs_path = path.resolve(&self.output_directory)?;
        io.create_parent_dirs(&fs_path).await
    }

    /// Resolve abstract path. If the path exists in the workspace, return the
    /// input file path. Otherwise, return the file path in the output
    /// directory, regardless of whether it exists.
    pub fn resolve_path(&self, path: &werk_fs::Path) -> Result<std::path::PathBuf, PathError> {
        let abs_path;
        let path = if !path.is_absolute() {
            abs_path = path.absolutize(werk_fs::Path::ROOT)?;
            &abs_path
        } else {
            path
        };

        if self.workspace_files.contains_key(path) {
            path.resolve(&self.project_root)
        } else {
            path.resolve(&self.output_directory)
        }
    }

    pub fn unresolve_path(&self, path: &std::path::Path) -> Result<werk_fs::PathBuf, PathError> {
        match werk_fs::Path::unresolve(path, &self.output_directory) {
            Ok(path) => return Ok(path),
            // The path is not in the output directory, try the project root.
            Err(werk_fs::PathError::UnresolveBeyondRoot) => {}
            Err(err) => return Err(err),
        }

        werk_fs::Path::unresolve(path, &self.project_root)
    }

    pub fn glob_workspace_files(
        &self,
        pattern: &str,
    ) -> Result<(Vec<werk_fs::PathBuf>, Outdatedness), globset::Error> {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.glob_cache.entry(pattern.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                let (paths, status, _) = entry.get();
                Ok((paths.clone(), status.clone()))
            }
            hash_map::Entry::Vacant(entry) => {
                let glob = globset::Glob::new(pattern)?;
                let matcher = glob.compile_matcher();

                // Note: Workspace files are already sorted.
                let matches = self
                    .workspace_files
                    .iter()
                    .filter_map(|(path, entry)| {
                        if entry.metadata.is_file && matcher.is_match(path.as_os_path()) {
                            Some(path.clone())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                let hash = compute_glob_hash(&matches);

                // Determine the outdatedness of the glob.
                let outdated = if self
                    .werk_cache
                    .glob
                    .get(pattern)
                    .is_some_and(|existing_hash| *existing_hash == hash)
                {
                    Outdatedness::unchanged()
                } else {
                    Outdatedness::outdated(Reason::Glob(pattern.to_owned()))
                };

                entry.insert((matches.clone(), outdated.clone(), hash));
                Ok((matches, outdated))
            }
        }
    }

    pub fn which(
        &self,
        io: &dyn Io,
        command: &str,
    ) -> Result<(String, Outdatedness), which::Error> {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.which_cache.entry(command.to_owned()) {
            hash_map::Entry::Occupied(entry) => entry.get().clone(),
            hash_map::Entry::Vacant(entry) => {
                let result = io.which(command);
                let outdated = match result {
                    Ok(ref path) => {
                        if self
                            .werk_cache
                            .which
                            .get(command)
                            .is_some_and(|existing_path| {
                                std::path::Path::new(&*existing_path) == path
                            })
                        {
                            Outdatedness::unchanged()
                        } else {
                            Outdatedness::outdated(Reason::Which(command.to_owned()))
                        }
                    }
                    Err(_) => Outdatedness::outdated(Reason::Which(command.to_owned())),
                };

                let result = result.map(|value| (value.to_string_lossy().into_owned(), outdated));

                entry.insert(result.clone());
                result
            }
        }
    }

    pub fn env(&self, io: &dyn Io, name: &str) -> (String, Outdatedness) {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.env_cache.entry(name.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                let (value, outdated, _hash) = entry.get();
                (value.clone(), outdated.clone())
            }
            hash_map::Entry::Vacant(entry) => {
                let result = io.read_env(name).unwrap_or_default();
                let hash = compute_stable_hash(&result);
                let outdated = if self
                    .werk_cache
                    .env
                    .get(name)
                    .is_some_and(|existing_value| *existing_value == hash)
                {
                    Outdatedness::unchanged()
                } else {
                    Outdatedness::outdated(Reason::Env(name.to_owned()))
                };

                entry.insert((result.clone(), outdated.clone(), hash));
                (result, outdated)
            }
        }
    }
}

fn compute_stable_hash<T: std::hash::Hash + ?Sized>(value: &T) -> Hash128 {
    let mut hasher = rustc_stable_hash::StableSipHasher128::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn compute_glob_hash(files: &[werk_fs::PathBuf]) -> Hash128 {
    compute_stable_hash(files)
}

async fn read_workspace_cache(io: &dyn Io, output_dir: &std::path::Path) -> PersistedCache {
    let data = match io.read_file(&output_dir.join(WERK_CACHE_FILENAME)).await {
        Ok(data) => data,
        Err(err) => {
            if err.kind() != std::io::ErrorKind::NotFound {
                tracing::error!("Failed to read workspace cache, even though it exists: {err}");
            }
            return PersistedCache::default();
        }
    };

    if data.is_empty() {
        return PersistedCache::default();
    }

    match toml_edit::de::from_slice(&data) {
        Ok(cache) => cache,
        Err(err) => {
            tracing::error!("Failed to parse workspace cache: {err}");
            PersistedCache::default()
        }
    }
}

async fn write_workspace_cache(io: &dyn Io, output_dir: &std::path::Path, cache: &PersistedCache) {
    let data = match toml_edit::ser::to_vec(cache) {
        Ok(data) => data,
        Err(err) => {
            tracing::error!("Serialization error writing .werk-cache: {err}");
            return;
        }
    };

    let path = output_dir.join(WERK_CACHE_FILENAME);

    if let Err(err) = io.create_parent_dirs(&path).await {
        tracing::error!(
            "Error creating parent directory for .werk-cache '{}': {err}",
            output_dir.display()
        );
        return;
    }

    match io.write_file(&path, &data).await {
        Ok(()) => {}
        Err(err) => {
            tracing::error!("Error writing .werk-cache: {err}");
        }
    }
}

impl rustc_stable_hash::FromStableHash for Hash128 {
    type Hash = rustc_stable_hash::SipHasher128Hash;

    fn from(hash: Self::Hash) -> Self {
        let hi = (hash.0[0] as u128) << 64;
        let lo = hash.0[1] as u128;
        Hash128(hi | lo)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
struct Hash128(pub u128);
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
