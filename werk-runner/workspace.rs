use ahash::{HashMap, HashSet};
use parking_lot::Mutex;
use std::{borrow::Cow, collections::hash_map, sync::Arc};
use stringleton::{Symbol, sym};
use werk_eval::{
    Eval, EvalError, GlobError, GlobSettings, Io, Lookup, LookupValue, Scope, UsedVariable, Value,
    Warning,
};
use werk_fs::{Absolute, Normalize as _, PathError, SymPath};
use werk_parser::ast;
use werk_planner::{BuildRecipe, Manifest, TaskRecipe};
use werk_util::{DiagnosticFileId, DiagnosticSpan, IoError, hash128::Hash128};

use crate::{
    Error, Render,
    cache::{TargetOutdatednessCache, WERK_CACHE_FILENAME, WerkCache},
};

#[derive(Clone)]
pub struct WorkspaceSettings {
    pub cache_dir: Absolute<std::path::PathBuf>,
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
    /// Number of jobs to execute in parallel. Default is 1. If below 1, this
    /// will automatically be clamped to 1.
    pub jobs: usize,

    /// Insert artificial delay between executed commands. Useful for testing.
    pub artificial_delay: Option<std::time::Duration>,
}

impl WorkspaceSettings {
    #[must_use]
    pub fn new(cache_dir: Absolute<std::path::PathBuf>) -> Self {
        WorkspaceSettings {
            cache_dir,
            glob: GlobSettings::default(),
            defines: HashMap::default(),
            force_color: false,
            jobs: 1,
            artificial_delay: None,
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
    pub manifest: Manifest,
    protected_paths: HashSet<Absolute<werk_fs::PathBuf>>,
    // Project root - note that the workspace only accesses this directory
    // through the `Io` trait, and never directly.
    project_root: Absolute<std::path::PathBuf>,
    // Project root - note that the workspace only accesses this directory
    // through the `Io` trait, and never directly.
    cache_dir: Absolute<std::path::PathBuf>,
    /// The contents of `<out-dir>/.werk-cache.toml`.
    werk_cache: Mutex<WerkCache>,
    /// Caches of expensive runtime values (glob, which, env).
    runtime_caches: Mutex<Caches>,
    /// Overridden global variables from the command line.
    pub defines: HashMap<Symbol, String>,
    pub default_target: Option<String>,
    pub force_color: bool,
    pub io: Arc<dyn Io>,
    pub render: Arc<dyn Render>,
    pub(crate) artificial_delay: Option<std::time::Duration>,
    pub max_concurrent_jobs: usize,
    glob_settings: GlobSettings,
}

#[derive(Default)]
struct Caches {
    glob_cache: HashMap<String, (Vec<Absolute<werk_fs::PathBuf>>, Hash128)>,
    which_cache: HashMap<String, Result<(Absolute<std::path::PathBuf>, Hash128), which::Error>>,
    env_cache: HashMap<String, (String, Hash128)>,
    build_recipe_hashes: HashMap<String, Hash128>,
}

impl Workspace {
    pub fn new(
        io: Arc<dyn Io>,
        render: Arc<dyn Render>,
        project_root: Absolute<std::path::PathBuf>,
        settings: &WorkspaceSettings,
    ) -> Result<Self, Error> {
        let werk_cache = WerkCache::read(&*io, &settings.cache_dir);

        let manifest = Manifest::default();

        let workspace = Self {
            manifest,
            protected_paths: HashSet::from_iter(
                settings
                    .cache_dir
                    .join(WERK_CACHE_FILENAME)
                    .ok()
                    .and_then(|p| p.unresolve(&project_root).ok()),
            ),
            project_root,
            cache_dir: settings.cache_dir.clone(),
            werk_cache: Mutex::new(werk_cache),
            runtime_caches: Mutex::new(Caches {
                glob_cache: HashMap::default(),
                which_cache: HashMap::default(),
                env_cache: HashMap::default(),
                build_recipe_hashes: HashMap::default(),
            }),
            defines: settings
                .defines
                .iter()
                .map(|(k, v)| (Symbol::new(k), v.clone()))
                .collect(),
            default_target: None,
            force_color: settings.force_color,
            io,
            render,
            artificial_delay: settings.artificial_delay,
            max_concurrent_jobs: settings.jobs,
            glob_settings: settings.glob.clone(),
        };

        Ok(workspace)
    }

    /// Add arbitrary workspace paths to the protected set.
    pub fn protect_paths(&mut self, paths: impl IntoIterator<Item = Absolute<werk_fs::PathBuf>>) {
        self.protected_paths.extend(paths);
    }

    /// Returns whether the given path is protected from being modified by recipes.
    pub fn is_path_protected(&self, path: &Absolute<werk_fs::Path>) -> bool {
        self.protected_paths.contains(path)
    }

    fn register_werkfile_source(
        &mut self,
        path: &Absolute<werk_fs::Path>,
        source: &str,
        included_from: Option<DiagnosticSpan>,
    ) -> Result<DiagnosticFileId, EvalError> {
        let path = path.to_owned();
        let id = match self.manifest.source_map.insert_check_duplicate(
            path.clone().into_inner().into(),
            source.to_owned(),
            included_from,
        ) {
            Ok(id) => id,
            Err(previously_included_from) => {
                return Err(EvalError::IncludeDuplicate(
                    included_from.unwrap_or(DiagnosticFileId(0).span(
                        werk_util::Span::from_offset_and_len(werk_util::Offset(0), 0),
                    )),
                    previously_included_from,
                    path.clone(),
                ));
            }
        };

        self.render.add_source_file(id, path.as_str(), source);
        self.protected_paths.insert(path);
        Ok(id)
    }

    /// Parse a werkfile source document and add it to the workspace, evaluating
    /// all statements in global scope.
    pub fn add_werkfile_source(
        &mut self,
        path: &Absolute<werk_fs::Path>,
        source: &str,
    ) -> Result<DiagnosticFileId, EvalError> {
        let file = self.register_werkfile_source(path, source, None)?;
        let ast = werk_parser::parse_werk(source)
            .map_err(|err| EvalError::Parse(werk_parser::ErrorInFile { file, error: err }))?;
        self.eval_werkfile(file, ast, false)
    }

    /// Add a parsed werkfile document to the workspace, evaluating all
    /// statements in the global scope. This is usually what you want for the
    /// main `Werkfile`, because `default` statements must be parsed before
    /// creating the workspace.
    pub fn add_werkfile_parsed(
        &mut self,
        path: &Absolute<werk_fs::Path>,
        source: &str,
        ast: ast::Root,
    ) -> Result<DiagnosticFileId, EvalError> {
        let file = self.register_werkfile_source(path, source, None)?;
        self.eval_werkfile(file, ast, false)
    }

    fn eval_werkfile(
        &mut self,
        file: DiagnosticFileId,
        ast: ast::Root,
        is_include: bool,
    ) -> Result<DiagnosticFileId, EvalError> {
        for stmt in ast.statements {
            match stmt.statement {
                ast::RootStmt::Include(ref stmt) => {
                    self.eval_include(file, stmt)?;
                }
                ast::RootStmt::Default(ref stmt) => {
                    self.eval_default(file, stmt, is_include)?;
                }
                ast::RootStmt::Config(ref config_stmt) => {
                    let doc_comment =
                        werk_parser::extract_doc_comment(&self.manifest, file, stmt.ws_pre)
                            .to_string();
                    self.eval_config(file, doc_comment, config_stmt)?;
                }
                ast::RootStmt::Let(ref let_stmt) => {
                    self.eval_let(file, let_stmt)?;
                }
                ast::RootStmt::Task(task_recipe) => {
                    let doc_comment =
                        werk_parser::extract_doc_comment(&self.manifest, file, stmt.ws_pre)
                            .to_string();
                    self.eval_task_recipe(file, doc_comment, task_recipe);
                }
                ast::RootStmt::Build(build_recipe) => {
                    let doc_comment =
                        werk_parser::extract_doc_comment(&self.manifest, file, stmt.ws_pre)
                            .to_string();
                    self.eval_build_recipe(file, doc_comment, build_recipe)?;
                }
            }
        }

        // Warn about defines set on the command-line that have no effect.
        for key in self.defines.keys() {
            if !self.manifest.config_variables.contains_key(key) {
                self.render
                    .warning(None, &Warning::UnusedDefine(key.as_str().to_owned()));
            }
        }

        Ok(file)
    }

    fn eval_include(
        &mut self,
        file: DiagnosticFileId,
        stmt: &ast::IncludeStmt,
    ) -> Result<(), EvalError> {
        let value = werk_eval::eval_chain(self, &stmt.param, file)?;
        let mut files = vec![];
        value.value.try_visit(|include_file| {
            let path = werk_fs::PathBuf::new(include_file.string)
                .and_then(|path| path.absolutize(werk_fs::Path::ROOT).map(Cow::into_owned))
                .map_err(|err| EvalError::Path(file.span(stmt.span), err))?;
            files.push(path.to_path_buf());
            Ok::<_, EvalError>(())
        })?;
        for included_file in files {
            let file_entry = self.stat_file(&included_file).map_err(|err| {
                EvalError::IncludeIoError(file.span(stmt.span), included_file.to_string(), err)
            })?;

            // Note: The included file's mtime is not included in outdatedness
            // calculations, because variables and recipes are tracked using
            // fine-grained hashing.

            let source = self.io.read_file(&file_entry.path).map_err(|err| {
                EvalError::IncludeIoError(file.span(stmt.span), included_file.to_string(), err)
            })?;

            let source = String::from_utf8(source).map_err(|_| {
                EvalError::NonUtf8Read(file.span(stmt.span), file_entry.path.clone().into_inner())
            })?;

            let id =
                self.register_werkfile_source(&included_file, &source, Some(file.span(stmt.span)))?;
            werk_parser::parse_werk(&source)
                .map_err(|err| {
                    EvalError::Parse(werk_parser::ErrorInFile {
                        file: id,
                        error: err,
                    })
                })
                .and_then(|ast| self.eval_werkfile(id, ast, true))
                .map_err(|err| match err {
                    EvalError::IncludeDuplicate(..) => err,
                    _ => EvalError::IncludeError(file.span(stmt.span), Box::new(err)),
                })?;
        }

        Ok(())
    }

    fn eval_default(
        &mut self,
        file: DiagnosticFileId,
        stmt: &ast::DefaultStmt,
        is_include: bool,
    ) -> Result<(), EvalError> {
        use werk_util::Spanned as _;

        if is_include {
            return Err(EvalError::DefaultInInclude(file.span(stmt.span())));
        }

        // Note: Other types of `default` statements are handled upstream while
        // parsing `Defaults`, which happens prior to creating the workspace.
        if let ast::DefaultStmt::Target(stmt) = stmt {
            let value = werk_eval::eval_string_expr(self, &stmt.value, file)?;
            self.default_target = Some(value.value.string);
        }

        Ok(())
    }

    fn eval_config(
        &mut self,
        file: DiagnosticFileId,
        doc_comment: String,
        stmt: &ast::ConfigStmt,
    ) -> Result<(), EvalError> {
        let hash = compute_stable_semantic_hash(&stmt.value);
        if let Some(config_override) = self.defines.get(&stmt.ident.ident) {
            tracing::trace!(
                "Overriding config variable `{}` with `{}`",
                stmt.ident.ident,
                config_override
            );
            let evaluated = Eval::<Value>::using_vars(
                config_override.clone().into(),
                [
                    UsedVariable::Global(stmt.ident.ident, hash),
                    UsedVariable::Define(stmt.ident.ident, compute_stable_hash(config_override)),
                ],
            );
            self.manifest.set_config(
                stmt.ident.ident,
                evaluated.value.clone(),
                file.span(stmt.span),
                doc_comment,
            )?;
            self.manifest
                .global_variables
                .insert(stmt.ident.ident, evaluated);
        } else {
            let mut value = werk_eval::eval_chain(self, &stmt.value, file)?;
            value
                .used
                .insert(UsedVariable::Global(stmt.ident.ident, hash));
            tracing::trace!("(global) config `{}` = {:?}", stmt.ident, value);
            self.manifest.set_config(
                stmt.ident.ident,
                value.value.clone(),
                file.span(stmt.span),
                doc_comment,
            )?;
            self.manifest
                .global_variables
                .insert(stmt.ident.ident, value);
        }

        Ok(())
    }

    fn eval_let(&mut self, file: DiagnosticFileId, stmt: &ast::LetStmt) -> Result<(), EvalError> {
        let hash = compute_stable_semantic_hash(&stmt.value);
        let mut value = werk_eval::eval_chain(self, &stmt.value, file)?;
        value
            .used
            .insert(UsedVariable::Global(stmt.ident.ident, hash));
        tracing::trace!("(global) let `{}` = {:?}", stmt.ident, value);
        self.manifest
            .global_variables
            .insert(stmt.ident.ident, value);
        Ok(())
    }

    fn eval_task_recipe(
        &mut self,
        file: DiagnosticFileId,
        doc_comment: String,
        task_recipe: ast::TaskRecipe,
    ) {
        let hash = compute_stable_semantic_hash(&task_recipe);
        self.manifest.task_recipes.insert(
            task_recipe.name.ident.as_str(),
            TaskRecipe {
                span: file.span(task_recipe.span),
                name: task_recipe.name.ident,
                doc_comment,
                ast: task_recipe,
                hash,
            },
        );
    }

    fn eval_build_recipe(
        &mut self,
        file: DiagnosticFileId,
        doc_comment: String,
        build_recipe: ast::BuildRecipe,
    ) -> Result<(), EvalError> {
        let hash = compute_stable_semantic_hash(&build_recipe);
        let mut pattern_builder =
            werk_eval::eval_pattern_builder(self, &build_recipe.pattern, file)?;

        // TODO: Consider if it isn't better to do this while matching recipes.
        pattern_builder.ensure_absolute_path();

        self.manifest.build_recipes.push(BuildRecipe {
            span: file.span(build_recipe.span),
            pattern: pattern_builder.build().value,
            doc_comment,
            ast: build_recipe,
            hash,
        });
        Ok(())
    }

    #[inline]
    pub fn io(&self) -> &dyn Io {
        &*self.io
    }

    /// Write outdatedness cache (`which` and `glob`)  to "<out-dir>/.werk-cache".
    #[expect(clippy::unused_async)] // Preserving `async` for future-proofing.
    pub async fn finalize(&self) -> Result<(), IoError> {
        let cache = self.werk_cache.lock();
        cache.write(&*self.io, &self.cache_dir)
    }

    #[inline]
    pub fn project_root(&self) -> &Absolute<std::path::Path> {
        &self.project_root
    }

    #[inline]
    pub fn cache_dir(&self) -> &Absolute<std::path::Path> {
        &self.cache_dir
    }

    pub fn create_parent_dirs(&self, path: &Absolute<werk_fs::Path>) -> Result<(), Error> {
        let fs_path = path.resolve(self.project_root());
        self.io.create_parent_dirs(&fs_path).map_err(Into::into)
    }

    pub fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        path.unresolve(&self.project_root)
    }

    pub fn glob_workspace_files(
        &self,
        pattern: &str,
    ) -> Result<(Vec<Absolute<werk_fs::PathBuf>>, Hash128), GlobError> {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.glob_cache.entry(pattern.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                let (paths, hash) = entry.get();
                Ok((paths.clone(), *hash))
            }
            hash_map::Entry::Vacant(entry) => {
                let glob = globset::Glob::new(pattern)?;
                let matches = Mutex::new(vec![]);
                let matcher = glob.compile_matcher();
                self.io.walk_directory(
                    &self.project_root,
                    self.glob_settings.clone(),
                    &|path| {
                        if let Some(unresolved_path) = self.unresolve_path(path).ok()
                            && let Ok(metadata) = self.io.metadata(path)
                            && metadata.is_file
                            && matcher.is_match(unresolved_path.as_os_path())
                        {
                            matches.lock().push(unresolved_path);
                        }
                    },
                )?;

                let mut matches = matches.into_inner();
                // Sorting the matches for consistent hashing.
                matches.sort();
                let hash = compute_glob_hash(&matches);

                entry.insert((matches.clone(), hash));
                Ok((matches, hash))
            }
        }
    }

    pub fn which<'p>(
        &self,
        command: &'p str,
    ) -> Result<(Cow<'p, Absolute<std::path::Path>>, Option<Hash128>), which::Error> {
        let path = std::path::Path::new(command);
        if path.is_absolute() {
            // The program has already been which'ed, just use it directly.
            return Ok((
                path.normalize().expect("failed to normalize program path"),
                None,
            ));
        }

        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.which_cache.entry(command.to_owned()) {
            hash_map::Entry::Occupied(entry) => entry
                .get()
                .clone()
                .map(|(path, hash)| (Cow::Owned(path), Some(hash))),
            hash_map::Entry::Vacant(entry) => {
                let result = self.io.which(command).map(|path| {
                    let hash = compute_stable_hash(&path);
                    (path, hash)
                });

                entry.insert(result.clone());
                result.map(|(path, hash)| (Cow::Owned(path), Some(hash)))
            }
        }
    }

    pub fn env(&self, name: &str) -> (String, Hash128) {
        let mut state = self.runtime_caches.lock();
        let state = &mut *state;
        match state.env_cache.entry(name.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                let (value, hash) = entry.get();
                (value.clone(), *hash)
            }
            hash_map::Entry::Vacant(entry) => {
                let result = self.io.read_env(name).unwrap_or_default();
                let hash = compute_stable_hash(&result);
                entry.insert((result.clone(), hash));
                (result, hash)
            }
        }
    }

    pub fn register_used_recipe_hash(&self, recipe: &BuildRecipe) -> Hash128 {
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
        path: Absolute<SymPath>,
    ) -> Option<TargetOutdatednessCache> {
        self.werk_cache.lock().build.remove(path.as_path())
    }

    pub(crate) fn store_build_target_cache(
        &self,
        path: Absolute<SymPath>,
        cache: TargetOutdatednessCache,
    ) {
        self.werk_cache
            .lock()
            .build
            .insert(path.as_path().to_path_buf(), cache);
    }
}

pub(crate) fn compute_stable_hash<T: std::hash::Hash + ?Sized>(value: &T) -> Hash128 {
    let mut hasher = rustc_stable_hash::StableSipHasher128::new();
    value.hash(&mut hasher);
    hasher.finish()
}

pub(crate) fn compute_stable_semantic_hash<T: werk_util::SemanticHash + ?Sized>(
    value: &T,
) -> Hash128 {
    let mut hasher = rustc_stable_hash::StableSipHasher128::new();
    value.semantic_hash(&mut hasher);
    hasher.finish()
}

fn compute_glob_hash(files: &[Absolute<werk_fs::PathBuf>]) -> Hash128 {
    compute_stable_hash(files)
}

impl werk_util::DiagnosticSourceMap for &Workspace {
    #[inline]
    fn get_source(
        &self,
        id: werk_util::DiagnosticFileId,
    ) -> Option<werk_util::DiagnosticSource<'_>> {
        self.manifest.get_source(id)
    }
}

impl werk_eval::Scope for Workspace {
    fn get(&self, name: Lookup) -> Option<LookupValue<'_>> {
        let Lookup::Ident(name) = name else {
            return None;
        };

        if let Some(var) = self
            .manifest
            .global_variables
            .get(&name)
            .map(LookupValue::EvalRef)
        {
            return Some(var);
        }

        // Global build-time constants.
        if let Some(global_constant) = werk_eval::default_global_constants()
            .get(&name)
            .map(Eval::inherent)
            .map(LookupValue::ValueRef)
        {
            return Some(global_constant);
        }

        // Runtime constants.
        if name == sym!(COLOR) {
            return Some(LookupValue::Owned(Eval::inherent(Value::from(
                if self.force_color { "1" } else { "0" }.to_owned(),
            ))));
        }

        None
    }

    fn io(&self) -> &dyn Io {
        &*self.io
    }

    fn messenger(&self) -> &dyn werk_eval::Messenger {
        &*self.render
    }

    fn message(&self, message: &str) {
        self.render.message(None, message);
    }

    fn warning(&self, warning: &werk_eval::Warning) {
        self.render.warning(None, warning);
    }

    fn which(
        &self,
        program_name: &str,
    ) -> Result<Eval<Absolute<std::path::PathBuf>>, which::Error> {
        let (path, hash) = Workspace::which(self, program_name)?;
        let path = path.into_owned();
        Ok(Eval::using_vars(
            path,
            hash.map(|hash| UsedVariable::Which(Symbol::new(program_name), hash)),
        ))
    }

    fn env(&self, variable_name: &str) -> Eval<Option<String>> {
        let (value, hash) = Workspace::env(self, variable_name);
        Eval::using_var(
            Some(value),
            UsedVariable::Env(Symbol::new(variable_name), hash),
        )
    }

    fn resolve_path(&self, path: &Absolute<werk_fs::Path>) -> Absolute<std::path::PathBuf> {
        path.resolve(self.current_working_directory())
    }

    fn unresolve_path(
        &self,
        path: &Absolute<std::path::Path>,
    ) -> Result<Absolute<werk_fs::PathBuf>, PathError> {
        Workspace::unresolve_path(self, path)
    }

    fn glob_workspace_files(
        &self,
        pattern_string: &str,
    ) -> Result<Eval<Vec<Absolute<werk_fs::PathBuf>>>, GlobError> {
        let (results, hash) = Workspace::glob_workspace_files(self, pattern_string)?;
        Ok(Eval::using_var(
            results,
            UsedVariable::Glob(Symbol::new(pattern_string), hash),
        ))
    }

    fn current_working_directory(&self) -> &Absolute<std::path::Path> {
        self.project_root()
    }
}
