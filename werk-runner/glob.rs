use std::collections::hash_map;

use ahash::{HashMap, HashSet};
use werk_fs::PathError;

use crate::Error;

#[derive(Clone)]
pub struct GlobSettings {
    pub git_ignore: bool,
    pub git_ignore_global: bool,
    pub git_ignore_exclude: bool,
    pub git_ignore_from_parents: bool,
    pub ignore_hidden: bool,
    pub ignore: Vec<std::path::PathBuf>,
}

impl Default for GlobSettings {
    fn default() -> Self {
        Self {
            git_ignore: true,
            git_ignore_global: true,
            git_ignore_exclude: true,
            git_ignore_from_parents: false,
            ignore_hidden: false,
            ignore: Vec::new(),
        }
    }
}

#[derive(Default)]
pub struct Globber {
    cache: HashMap<String, Vec<werk_fs::PathBuf>>,
}

impl Globber {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn glob(
        &mut self,
        glob_str: &str,
        workspace: &Workspace,
    ) -> Result<&[werk_fs::PathBuf], Error> {
        match self.cache.entry(glob_str.to_owned()) {
            hash_map::Entry::Occupied(occupied_entry) => Ok(occupied_entry.into_mut()),
            hash_map::Entry::Vacant(vacant_entry) => {
                let glob = globset::Glob::new(glob_str)?;
                let matcher = glob.compile_matcher();
                let matches = workspace
                    .workspace
                    .iter()
                    .filter(|path| matcher.is_match(path.as_os_path()))
                    .cloned()
                    .collect::<Vec<_>>();
                if matches.is_empty() {
                    tracing::warn!("glob `{glob_str}` matched zero files");
                }
                Ok(vacant_entry.insert(matches))
            }
        }
    }
}

pub struct Workspace {
    settings: GlobSettings,
    pub project_root: std::path::PathBuf,
    pub out_dir: std::path::PathBuf,
    workspace: HashSet<werk_fs::PathBuf>,
}

impl Workspace {
    pub fn new(
        project_root: std::path::PathBuf,
        out_dir: std::path::PathBuf,
        settings: GlobSettings,
    ) -> Result<Self, Error> {
        // Walk the entire workspace according to the global rules.
        let mut workspace = HashSet::default();
        let mut walker = ignore::WalkBuilder::new(&project_root);
        walker
            .git_ignore(settings.git_ignore)
            .git_global(settings.git_ignore_global)
            .git_exclude(settings.git_ignore_exclude)
            .parents(settings.git_ignore_from_parents)
            .hidden(!settings.ignore_hidden);

        // Ignore the out_dir in globs.
        walker.add_ignore(&out_dir);
        for ignore in &settings.ignore {
            walker.add_ignore(ignore);
        }

        let mut walker = walker.build();

        while let Some(entry) = walker.next() {
            let entry = entry?;
            let path_in_project = match werk_fs::Path::unresolve(entry.path(), &project_root) {
                Ok(path_in_project) => path_in_project,
                // This should not be possible.
                Err(err @ PathError::UnresolveBeyondRoot) => return Err(err.into()),
                Err(_) => continue,
            };
            tracing::trace!("Workspace file: {path_in_project}");
            workspace.insert(path_in_project);
        }

        Ok(Workspace {
            settings,
            project_root,
            out_dir,
            workspace,
        })
    }

    pub fn contains(&self, path: &werk_fs::Path) -> Result<bool, Error> {
        let abs_path;
        let path = if path.is_absolute() {
            path
        } else {
            abs_path = path.absolutize(werk_fs::Path::ROOT)?;
            &abs_path
        };

        Ok(self.workspace.contains(path))
    }
}
