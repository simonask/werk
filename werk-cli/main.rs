mod watcher;

use std::sync::Arc;

use anyhow::Result;
use clap::Parser;
use werk_runner::{Project, Recipes, Runner, Settings};

#[derive(Debug, clap::Parser)]
pub struct Args {
    /// The target to build.
    pub target: Option<String>,
    #[clap(short, long)]
    /// The path to the werk file. Defaults to searching for `werk.toml` in the
    /// working dir and its parents.
    pub file: Option<std::path::PathBuf>,
    /// List the available targets.
    #[clap(short, long)]
    pub list: bool,
    /// Dry run; do not execute any recipe commands. Note: Shell commands used
    /// in global variables are still executed! This also implies
    /// `--print-commands`.
    #[clap(long)]
    pub dry_run: bool,
    /// Print recipe shell commands as they are executed.
    #[clap(long)]
    pub print_commands: bool,
    #[clap(long, default_value = "auto")]
    pub color: ColorChoice,
    #[clap(long, default_value = "auto")]
    pub output_format: OutputChoice,
    /// Number of tasks to execute in parallel. Defaults to the number of CPU cores.
    #[clap(long, short)]
    pub jobs: Option<usize>,
}

/// Color mode.
#[derive(Clone, Copy, Default, Debug, clap::ValueEnum)]
pub enum ColorChoice {
    /// Probe the current terminal and environment variables for color support.
    /// If the command is not running in a terminal, color is disabled. If the
    /// command is running in a terminal, color is enabled if the `FORCE_COLOR`
    /// environment variable is set, or disabled if the `NO_COLOR` environment
    /// variable is set.
    #[default]
    Auto,
    /// Force color output, even if the command is not running in a terminal.
    /// Equivalent to settings the `FORCE_COLOR` environment variable. Note:
    /// Setting this does not implicitly set `FORCE_COLOR` for executed shell
    /// commands.
    Always,
    /// Do not use color output, regardless of whether the command is running in
    /// a terminal. Equivalent to setting the `NO_COLOR` environment variable.
    /// Note: Setting this does not implicitly set `NO_COLOR` for executed shell
    /// commands.
    Never,
}

/// Terminal output mode.
#[derive(Clone, Copy, Default, Debug, clap::ValueEnum)]
pub enum OutputChoice {
    /// Choose the best output format for the current terminal.
    #[default]
    Auto,
    /// Emit the progress as log statements (assuming `WERK_LOG` is set to a value).
    Log,
    /// Report progress as JSON to stdout. This also disables color output.
    Json,
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    let args = Args::parse();
    let mut builder = tokio::runtime::Builder::new_multi_thread();
    if let Some(jobs) = args.jobs {
        builder.worker_threads(jobs.max(1));
    }
    builder.enable_all();
    let runtime = builder.build().unwrap();
    let result = runtime.block_on(try_main(args));
    result
}

async fn try_main(args: Args) -> Result<()> {
    let werkfile_path = if let Some(file) = args.file {
        file
    } else {
        find_werkfile()?
    };
    tracing::debug!("Using werkfile: {}", werkfile_path.display());

    let werkfile_contents = std::fs::read_to_string(&werkfile_path)?;
    let ast = werk_parser::parse_toml(&werkfile_contents)?;

    let project_dir = werkfile_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("{} has no parent directory", werkfile_path.display()))?;
    let out_dir = project_dir.join("target");
    tracing::debug!("Project directory: {}", project_dir.display());
    tracing::debug!("Output directory: {}", out_dir.display());

    let project = Project::new(
        project_dir.to_owned(),
        out_dir,
        Settings {
            glob: Default::default(),
            dry_run: args.dry_run,
        },
    )?;

    let recipes = Recipes::new(ast, &project).await?;

    if args.list {
        for (name, _) in &recipes.ast.commands {
            println!("{name}");
        }
        for pattern in &recipes.build_recipe_patterns {
            println!("{}", pattern);
        }
        return Ok(());
    }

    let Some(target) = args.target else {
        anyhow::bail!("No target specified");
    };

    let tracker: Arc<dyn werk_runner::Watcher> =
        Arc::new(watcher::StdoutWatcher::new(args.color, args.print_commands));

    let mut runner = Runner::new(&project, recipes, tracker, args.dry_run);
    runner.build_or_run(&target).await?;

    Ok(())
}

fn find_werkfile() -> Result<std::path::PathBuf> {
    let mut current = std::env::current_dir()?;
    loop {
        let candidate = current.join("werk.toml");
        if candidate.is_file() {
            return Ok(candidate);
        }
        if let Some(parent) = current.parent() {
            current = parent.to_owned();
        } else {
            anyhow::bail!("werk.toml not found in the current directory or any parent directory");
        }
    }
}
