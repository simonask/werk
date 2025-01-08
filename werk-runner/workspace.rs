use ahash::HashMap;
use indexmap::IndexMap;
use parking_lot::Mutex;
use std::collections::hash_map;
use werk_fs::{Absolute, PathError};

use crate::{
    cache::{Hash128, TargetOutdatednessCache, WerkCache},
    ir, DirEntry, Error, Io, UsedVariable,
};

#[derive(Clone)]
pub struct WorkspaceSettings {
    pub output_directory: Absolute<std::path::PathBuf>,
    /// Settings for globbing the workspace directory. Note that the
    /// `output_directory` is not automatically ignored, and must either be
    /// present in `.gitignore` or explicitly ignored here.
    pub glob: GlobSettings,
    /// Command-line `--define` or `-D` arguments, overriding global variables.
    pub defines: HashMap<String, String>,
    /// When true, the [`Runner`](crate::Runner) sets the `FORCE_COLOR` and
    /// `CLICOLOR_FORCE` environment variables to "1" when executing recipe
    /// commands (not when capturing their output in variables).
    pub force_color: bool,
}

impl WorkspaceSettings {
    pub fn new(output_dir: Absolute<std::path::PathBuf>) -> Self {
        WorkspaceSettings {
            output_directory: output_dir,
            glob: GlobSettings::default(),
            defines: HashMap::default(),
            force_color: false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct GlobSettings {
    /// Read the workspace directory's `.gitignore`. Enabled by default.
    pub git_ignore: bool,
    /// Read a global gitignore file, specified in the `core.excludeFiles`
    /// config option. Enabled by default.
    pub git_ignore_global: bool,
    /// Read `.git/info/exclude`. Enabled by default.
    pub git_ignore_exclude: bool,
    /// Read `.gitignore` from parent directoryes. Enabled by default.
    pub git_ignore_from_parents: bool,
    /// Enables reading `.ignore` files, supported by `ripgrep` and The Silver Searcher. Enabled by default.
    pub dot_ignore: bool,
    /// Explicit file name patterns to ignore in addition to gitignore and .ignore files.
    pub ignore_explicitly: globset::GlobSet,
}

impl Default for GlobSettings {
    #[inline]
    fn default() -> Self {
        Self {
            git_ignore: true,
            git_ignore_global: true,
            git_ignore_exclude: true,
            git_ignore_from_parents: true,
            dot_ignore: true,
            ignore_explicitly: globset::GlobSet::empty(),
        }
    }
}

impl WorkspaceSettings {
    /// Override a global variable in the root scope.
    pub fn define(&mut self, key: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.defines.insert(key.into(), value.into());
        self
    }

    pub fn ignore_explicitly(&mut self, globset: globset::GlobSet) -> &mut Self {
        self.glob.ignore_explicitly = globset;
        self
    }
}

pub struct Workspace {
    // Project root - note that the workspace only accesses this directory
    // through the `Io` trait, and never directly.
    project_root: Absolute<std::path::PathBuf>,
    // Project root - note that the workspace only accesses this directory
    // through the `Io` trait, and never directly.
    output_directory: Absolute<std::path::PathBuf>,
    // Using IndexMap to ensure that the ordering of glob results is well-defined.
    workspace_files: IndexMap<Absolute<werk_fs::PathBuf>, DirEntry, ahash::RandomState>,
    /// The contents of `<out-dir>/.werk-cache.toml`.
    werk_cache: Mutex<WerkCache>,
    /// Caches of expensive runtime values (glob, which, env).
    runtime_caches: Mutex<Caches>,
    /// Overridden global variables from the command line.
    pub defines: HashMap<String, String>,
    pub force_color: bool,
}

#[derive(Default)]
struct Caches {
    glob_cache: HashMap<String, (Vec<Absolute<werk_fs::PathBuf>>, Hash128)>,
    which_cache: HashMap<String, Result<(Absolute<std::path::PathBuf>, Hash128), which::Error>>,
    env_cache: HashMap<String, (String, Hash128)>,
    build_recipe_hashes: HashMap<String, Hash128>,
}

pub const WERK_CACHE_FILENAME: &str = ".werk-cache";

impl Workspace {
    pub async fn new(
        io: &dyn Io,
        project_root: Absolute<std::path::PathBuf>,
        settings: &WorkspaceSettings,
    ) -> Result<Self, Error> {
        let werk_cache = read_workspace_cache(io, settings.output_directory.as_deref()).await;

        let mut workspace_files =
            IndexMap::with_capacity_and_hasher(1024, ahash::RandomState::default());

        for entry in io
            .glob_workspace(project_root.as_deref(), &settings.glob)
            .await?
        {
            if entry.path.file_name() == Some(WERK_CACHE_FILENAME.as_ref()) {
                return Err(Error::ClobberedWorkspace(entry.path.into_inner()));
            }

            let path_in_project =
                match werk_fs::Path::unresolve(&entry.path, project_root.as_path()) {
                    Ok(path_in_project) => path_in_project,
                    // This should not be possible.
                    Err(err @ PathError::UnresolveBeyondRoot) => {
                        return Err(Error::InvalidTargetPath(
                            entry.path.display().to_string(),
                            err,
                        ))
                    }
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
            output_directory: settings.output_directory.clone(),
            workspace_files,
            werk_cache: Mutex::new(werk_cache),
            runtime_caches: Mutex::new(Caches {
                glob_cache: HashMap::default(),
                which_cache: HashMap::default(),
                env_cache: HashMap::default(),
                build_recipe_hashes: HashMap::default(),
            }),
            defines: settings.defines.clone(),
            force_color: settings.force_color,
        })
    }

    /// Write outdatedness cache (`which` and `glob`)  to "<out-dir>/.werk-cache".
    pub async fn finalize(&self, io: &dyn Io) -> std::io::Result<()> {
        let cache = self.werk_cache.lock();
        write_workspace_cache(io, self.output_directory.as_deref(), &*cache).await
    }

    #[inline]
    pub fn project_root(&self) -> &Absolute<std::path::Path> {
        self.project_root.as_deref()
    }

    #[inline]
    pub fn output_directory(&self) -> &std::path::Path {
        &self.output_directory
    }

    pub fn get_project_file(&self, path: &Absolute<werk_fs::Path>) -> Option<&DirEntry> {
        self.workspace_files.get(path)
    }

    pub fn get_existing_project_or_output_file(
        &self,
        io: &dyn Io,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Option<DirEntry>, Error> {
        if let Some(dir_entry) = self.get_project_file(path) {
            return Ok(Some(dir_entry.clone()));
        }

        self.get_existing_output_file(io, path)
    }

    pub fn get_existing_output_file(
        &self,
        io: &dyn Io,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Option<DirEntry>, Error> {
        let fs_path = path
            .resolve(werk_fs::Path::ROOT, self.output_directory.as_deref())
            .expect("out dir resolve error");
        match io.metadata(fs_path.as_deref()) {
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

    pub fn get_output_file_path(
        &self,
        path: &werk_fs::Path,
    ) -> Result<Absolute<std::path::PathBuf>, PathError> {
        path.resolve(werk_fs::Path::ROOT, self.output_directory.as_deref())
    }

    pub async fn create_output_parent_dirs(
        &self,
        io: &dyn Io,
        path: &werk_fs::Path,
    ) -> Result<(), Error> {
        let fs_path = path
            .resolve(werk_fs::Path::ROOT, self.output_directory.as_deref())
            .expect("out dir resolve error");
        io.create_parent_dirs(fs_path.as_deref())
            .await
            .map_err(Into::into)
    }

    /// Resolve abstract path. If the path exists in the workspace, return the
    /// input file path. Otherwise, return the file path in the output
    /// directory, regardless of whether it exists.
    pub fn resolve_path(
        &self,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Absolute<std::path::PathBuf>, PathError> {
        if self.workspace_files.contains_key(path) {
            path.resolve(werk_fs::Path::ROOT, self.project_root.as_deref())
        } else {
            path.resolve(werk_fs::Path::ROOT, self.output_directory.as_deref())
        }
    }

    pub fn unresolve_path(
        &self,
        path: &std::path::Path,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        match werk_fs::Path::unresolve(path, self.output_directory.as_deref()) {
            Ok(path) => return Ok(path),
            // The path is not in the output directory, try the project root.
            Err(werk_fs::PathError::UnresolveBeyondRoot) => {}
            Err(err) => return Err(err),
        }

        werk_fs::Path::unresolve(path, self.project_root.as_deref())
    }

    pub fn glob_workspace_files(
        &self,
        pattern: &str,
    ) -> Result<(Vec<Absolute<werk_fs::PathBuf>>, Hash128), globset::Error> {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.glob_cache.entry(pattern.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                let (paths, hash) = entry.get();
                Ok((paths.clone(), *hash))
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

                entry.insert((matches.clone(), hash));
                Ok((matches, hash))
            }
        }
    }

    pub fn which(
        &self,
        io: &dyn Io,
        command: &str,
    ) -> Result<(Absolute<std::path::PathBuf>, Option<Hash128>), which::Error> {
        let path = std::path::Path::new(command);
        if path.is_absolute() {
            return Ok((Absolute::new_unchecked(path.to_path_buf()), None));
        }

        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.which_cache.entry(command.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                entry.get().clone().map(|(path, hash)| (path, Some(hash)))
            }
            hash_map::Entry::Vacant(entry) => {
                let result = io.which(command).map(|path| {
                    let hash = compute_stable_hash(&path);
                    (path, hash)
                });

                entry.insert(result.clone());
                result.map(|(path, hash)| (path, Some(hash)))
            }
        }
    }

    pub fn env(&self, io: &dyn Io, name: &str) -> (String, Hash128) {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.env_cache.entry(name.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                let (value, hash) = entry.get();
                (value.clone(), *hash)
            }
            hash_map::Entry::Vacant(entry) => {
                let result = io.read_env(name).unwrap_or_default();
                let hash = compute_stable_hash(&result);
                entry.insert((result.clone(), hash));
                (result, hash)
            }
        }
    }

    pub fn register_used_recipe_hash(&self, recipe: &ir::BuildRecipe) -> Hash128 {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state
            .build_recipe_hashes
            .entry(recipe.pattern.string.clone())
        {
            hash_map::Entry::Occupied(entry) => *entry.get(),
            hash_map::Entry::Vacant(entry) => {
                let hash = recipe.hash;
                entry.insert(hash);
                hash
            }
        }
    }

    pub(crate) fn take_build_target_cache(
        &self,
        path: &werk_fs::Path,
    ) -> Option<TargetOutdatednessCache> {
        self.werk_cache.lock().build.remove(path)
    }

    pub(crate) fn store_build_target_cache(
        &self,
        path: werk_fs::PathBuf,
        cache: TargetOutdatednessCache,
    ) {
        self.werk_cache.lock().build.insert(path, cache);
    }
}

pub(crate) fn compute_stable_hash<T: std::hash::Hash + ?Sized>(value: &T) -> Hash128 {
    let mut hasher = rustc_stable_hash::StableSipHasher128::new();
    value.hash(&mut hasher);
    hasher.finish()
}

pub(crate) fn compute_stable_semantic_hash<T: werk_parser::SemanticHash + ?Sized>(
    value: &T,
) -> Hash128 {
    let mut hasher = rustc_stable_hash::StableSipHasher128::new();
    value.semantic_hash(&mut hasher);
    hasher.finish()
}

fn compute_glob_hash(files: &[Absolute<werk_fs::PathBuf>]) -> Hash128 {
    compute_stable_hash(files)
}

async fn read_workspace_cache(io: &dyn Io, output_dir: &Absolute<std::path::Path>) -> WerkCache {
    let werk_cache_path = Absolute::new_unchecked(output_dir.join(WERK_CACHE_FILENAME));
    tracing::debug!("trying to read .werk-cache: {}", werk_cache_path.display());
    let data = match io.read_file(werk_cache_path.as_deref()).await {
        Ok(data) => data,
        Err(err) => {
            if err.kind() != std::io::ErrorKind::NotFound {
                tracing::error!("Failed to read workspace cache, even though it exists: {err}");
            }
            tracing::debug!(".werk-cache does not exist");
            return WerkCache::default();
        }
    };

    if data.is_empty() {
        tracing::debug!(".werk-cache is empty");
        return WerkCache::default();
    }

    match toml_edit::de::from_slice(&data) {
        Ok(cache) => {
            tracing::trace!(".werk-cache contents: {cache:#?}");
            cache
        }
        Err(err) => {
            tracing::error!("Failed to parse workspace cache: {err}");
            WerkCache::default()
        }
    }
}

async fn write_workspace_cache(
    io: &dyn Io,
    output_dir: &Absolute<std::path::Path>,
    cache: &WerkCache,
) -> std::io::Result<()> {
    let mut doc = match toml_edit::ser::to_document(cache) {
        Ok(data) => data,
        Err(err) => {
            tracing::error!("Serialization error writing .werk-cache: {err}");
            panic!("Serialization error writing .werk-cache: {err}");
        }
    };

    fn make_table(item: &mut toml_edit::Item) -> Option<&mut toml_edit::Table> {
        match std::mem::take(item).into_table() {
            Ok(table) => {
                *item = toml_edit::Item::Table(table);
                if let toml_edit::Item::Table(table) = item {
                    Some(table)
                } else {
                    unreachable!()
                }
            }
            Err(not_table) => {
                *item = not_table;
                None
            }
        }
    }

    if let Some(build) = doc.get_mut("build") {
        let build = make_table(build).expect("build is not a table");
        build.set_implicit(true);
        for (_target, target_info) in build.iter_mut() {
            make_table(target_info);
        }
    }

    let toml = format!("# Generated by werk. It can be safely deleted.\n\n{}", doc);

    let path = Absolute::new_unchecked(output_dir.join(WERK_CACHE_FILENAME));
    tracing::debug!("writing .werk-cache to {}", path.display());

    if let Err(err) = io.create_parent_dirs(path.as_deref()).await {
        tracing::error!(
            "Error creating parent directory for .werk-cache '{}': {err}",
            output_dir.display()
        );
        return Err(err);
    }

    match io.write_file(path.as_deref(), toml.as_bytes()).await {
        Ok(()) => Ok(()),
        Err(err) => {
            tracing::error!("Error writing .werk-cache: {err}");
            Err(err)
        }
    }
}
