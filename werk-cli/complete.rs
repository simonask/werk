use std::sync::Arc;

use clap::{CommandFactory, FromArgMatches};
use clap_complete::CompletionCandidate;
use werk_fs::Normalize;
use werk_runner::Workspace;
use werk_util::DiagnosticFileId;

use crate::dry_run::DryRun;
use crate::render::null::NullRender;
use crate::render::ColorOutputKind;
use crate::Args;
use crate::{find_werkfile, get_workspace_dir, get_workspace_settings};

fn with_werk<T: Default>(f: impl FnOnce(Workspace) -> Result<T, anyhow::Error> + 'static) -> T {
    let result = (|| -> Result<T, anyhow::Error> {
        let args = std::env::args().skip(2);
        let arg_matches = Args::command()
            .disable_version_flag(true)
            .disable_help_flag(true)
            .ignore_errors(true)
            .try_get_matches_from(args)?;
        let args = Args::from_arg_matches(&arg_matches)?;

        let werkfile = match &args.file {
            Some(file) => file.clone().normalize()?,
            _ => find_werkfile()?,
        };

        let source_code = std::fs::read_to_string(&werkfile)?;
        let ast = werk_parser::parse_werk(&source_code)?;
        let config = werk_runner::ir::Defaults::new(&ast, DiagnosticFileId(0))?;

        let io = Arc::new(DryRun::new());
        let renderer = Arc::new(NullRender);

        let workspace_dir = get_workspace_dir(&args, &werkfile)?;
        let settings =
            get_workspace_settings(&config, &args, &workspace_dir, ColorOutputKind::Never)?;

        let mut workspace = Workspace::new(io, renderer, workspace_dir.into_owned(), &settings)?;
        let werkfile_path = workspace.unresolve_path(&werkfile)?;
        workspace.add_werkfile_parsed(&werkfile_path, &source_code, ast)?;

        let result = f(workspace)?;

        Ok(result)
    })();

    result.unwrap_or_default()
}

pub fn targets() -> Vec<CompletionCandidate> {
    with_werk(|workspace| {
        let tasks = workspace
            .manifest
            .task_recipes
            .into_iter()
            .map(|(name, recipe)| {
                CompletionCandidate::new(name).help(Some(recipe.doc_comment.into()))
            });
        let builds = workspace
            .manifest
            .build_recipes
            .into_iter()
            .map(|build_recipe| {
                CompletionCandidate::new(build_recipe.pattern.to_string())
                    .help(Some(build_recipe.doc_comment.into()))
            });

        Ok(tasks.chain(builds).collect())
    })
}

pub fn defines() -> Vec<CompletionCandidate> {
    with_werk(|workspace| {
        let defines = workspace
            .manifest
            .config_variables
            .iter()
            .map(|(symbol, global)| {
                let help = global.value.to_string();
                CompletionCandidate::new(format!("{symbol}=")).help(Some(help.into()))
            });

        Ok(defines.collect())
    })
}
