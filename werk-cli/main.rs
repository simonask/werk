pub mod dry_run;
mod watcher;

use std::{io::Write as _, sync::Arc};

use anyhow::Result;
use clap::Parser;
use owo_colors::OwoColorize as _;
use werk_runner::{Runner, Workspace, WorkspaceSettings};

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
    /// in global variables are still executed!
    #[clap(long)]
    pub dry_run: bool,

    /// Print recipe shell commands as they are executed. Implied by
    /// `--verbose`.
    #[clap(long)]
    pub print_commands: bool,

    /// Print build targets that were not rebuilt because they were up-to-date.
    /// Implied by `--verbose`.
    #[clap(long)]
    pub print_fresh: bool,

    /// Forward the stdout of all executed commands to the terminal, even when
    /// successful. Implied by `--verbose`.
    #[clap(long)]
    pub no_capture: bool,

    /// For each outdated recipe, explain why it was outdated. Implied by
    /// `--verbose`.
    #[clap(long)]
    pub explain: bool,

    /// Shorthand for `--explain --print-commands --print-fresh --no-capture`.
    #[clap(long, short)]
    pub verbose: bool,

    #[clap(long, default_value = "auto")]
    pub color: ColorChoice,

    #[clap(long, default_value = "auto")]
    pub output_format: OutputChoice,

    /// Number of tasks to execute in parallel. Defaults to the number of CPU cores.
    #[clap(long, short)]
    pub jobs: Option<usize>,

    /// Override the workspace directory. Defaults to the directory containing
    /// werk.toml.
    #[clap(long)]
    pub workspace_dir: Option<std::path::PathBuf>,

    /// Use the output directory instead of the default. In unspecified, uses
    /// the `out-dir` configuration variable from werk.toml, or if that is
    /// unspecified, uses`target` next to the root werk.toml file.
    #[clap(long)]
    pub output_dir: Option<std::path::PathBuf>,

    /// Override variable in the werk.toml's `[global]` section. This takes the
    /// form `name=value`.
    #[clap(long, short = 'D')]
    pub define: Vec<String>,

    /// Enable debug logging to stdout. The value is a logging directive (same
    /// syntax as the conventional `RUST_LOG` environment variable), so it can
    /// be a log level like "info" or "trace", or a module path like
    /// "werk_runner=debug". If passed without a directive string, this enables
    /// logging at the "info" level for only the `werk` runner.
    #[clap(long)]
    pub log: Option<Option<String>>,
}

/// Color mode.
#[derive(Clone, Copy, Default, Debug, clap::ValueEnum)]
pub enum ColorChoice {
    /// Probe the current terminal and environment variables for color support.
    /// If the command is not running in a terminal, color is disabled. If the
    /// command is running in a terminal color is enabled for `werk` and all
    /// subcommands or disabled if the `NO_COLOR` environment variable is set.
    #[default]
    Auto,
    /// Force color output, even if the command is not running in a terminal.
    /// Equivalent to settings the `FORCE_COLOR` environment variable. Note:
    /// Setting this also sets `FORCE_COLOR` and `CLICOLOR_FORCE` for executed
    /// shell commands.
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
    let args = Args::parse();
    match args.log {
        Some(Some(ref directive)) => tracing_subscriber::fmt::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::new(directive))
            .init(),
        Some(None) => tracing_subscriber::fmt::fmt()
            .with_env_filter("werk=info,werk_runner=info")
            .init(),
        _ => (),
    }

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
    let werkfile_path = std::path::absolute(werkfile_path)?;
    tracing::info!("Using werkfile: {}", werkfile_path.display());

    let werkfile_contents = std::fs::read_to_string(&werkfile_path)?;
    let ast = werk_parser::parse_toml(&werkfile_contents)?;

    let workspace_dir = if let Some(ref workspace_dir) = args.workspace_dir {
        if !workspace_dir.is_dir() {
            anyhow::bail!(
                "Workspace dir is not a directory: {}",
                workspace_dir.display()
            );
        } else {
            &*workspace_dir
        }
    } else {
        werkfile_path
            .parent()
            .ok_or_else(|| anyhow::anyhow!("{} has no parent directory", werkfile_path.display()))?
    };
    let workspace_dir = std::path::absolute(workspace_dir)?;

    let out_dir = args
        .output_dir
        .or_else(|| {
            ast.config
                .output_directory
                .as_ref()
                .map(|s| workspace_dir.join(s))
        })
        .unwrap_or_else(|| workspace_dir.join("target"));
    let out_dir = std::path::absolute(out_dir)?;
    tracing::info!("Project directory: {}", workspace_dir.display());
    tracing::info!("Output directory: {}", out_dir.display());

    let watcher = Arc::new(watcher::StdoutWatcher::new(watcher::OutputSettings {
        logging_enabled: args.log.is_some(),
        color: args.color,
        print_recipe_commands: args.print_commands | args.verbose,
        print_fresh: args.print_fresh | args.verbose,
        dry_run: args.dry_run,
        no_capture: args.no_capture | args.verbose,
        explain: args.explain | args.verbose,
    }));

    let mut settings = WorkspaceSettings::default();
    settings.output_directory = out_dir;
    for def in &args.define {
        let Some((key, value)) = def.split_once('=') else {
            return Err(anyhow::anyhow!(
                "Invalid variable definition (must take the form 'key=value'): {}",
                def
            ));
        };
        settings.define(key, value);
    }
    settings.force_color = watcher.enable_color();

    let io: Arc<dyn werk_runner::Io>;
    if args.dry_run || args.list {
        io = Arc::new(dry_run::DryRun::new());
    } else {
        io = Arc::new(werk_runner::RealSystem::new());
    }

    let workspace = Workspace::new(&*io, workspace_dir.to_owned(), &settings).await?;

    let target = args.target.or_else(|| ast.config.default.clone());

    let mut runner = Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;
    if args.list {
        let recipes = runner.recipes();

        let globals = runner
            .globals()
            .iter()
            .map(|(k, v)| (k, format!("{}", v.value.display_friendly(80)), &v.comment))
            .collect::<Vec<_>>();
        let max_global_name_len = globals
            .iter()
            .map(|(name, _, _)| name.len())
            .max()
            .unwrap_or(0);
        let max_global_value_len = globals
            .iter()
            .map(|(_, value, comment)| if !comment.is_empty() { value.len() } else { 0 })
            .max()
            .unwrap_or(0);

        let max_command_len = recipes
            .ast
            .commands
            .iter()
            .map(|(name, _)| name.len())
            .max()
            .unwrap_or(0);
        let max_pattern_len = recipes
            .build_recipes()
            .map(|(pattern, _)| pattern.string.len())
            .max()
            .unwrap_or(0);

        let mut out_lock = watcher.lock();
        let out = &mut out_lock.stdout;

        if max_global_name_len != 0 {
            _ = writeln!(out, "{}", "Global variables:".purple());

            for (name, value, comment) in globals {
                if comment.is_empty() {
                    _ = writeln!(
                        out,
                        "  {} = {}",
                        format_args!("{: <w$}", name, w = max_global_name_len).bright_yellow(),
                        value,
                    );
                } else {
                    _ = writeln!(
                        out,
                        "  {} = {} {}",
                        format_args!("{: <w$}", name, w = max_global_name_len).bright_yellow(),
                        format_args!("{: <w$}", value, w = max_global_value_len),
                        comment,
                    );
                }
            }

            if max_command_len != 0 || max_pattern_len != 0 {
                _ = writeln!(out);
            }
        }

        if max_command_len != 0 {
            _ = writeln!(out, "{}", "Available commands:".purple());
            for (name, command) in &recipes.ast.commands {
                if command.comment.is_empty() {
                    _ = writeln!(out, "  {}", name.cyan());
                } else {
                    _ = writeln!(
                        out,
                        "  {} {}",
                        format_args!("{: <w$}", name.bright_cyan(), w = max_command_len),
                        command.comment,
                    );
                }
            }
            if max_pattern_len != 0 {
                _ = writeln!(out);
            }
        }

        if max_pattern_len != 0 {
            _ = writeln!(out, "{}", "Available recipes:".purple());
            for (pattern, recipe) in recipes.build_recipes() {
                if recipe.comment.is_empty() {
                    _ = writeln!(out, "  {}", pattern.bright_yellow());
                } else {
                    _ = writeln!(
                        out,
                        "  {} {}",
                        format_args!(
                            "{: <w$}",
                            pattern.string.bright_yellow(),
                            w = max_pattern_len
                        ),
                        recipe.comment,
                    );
                }
            }
        }
        return Ok(());
    }

    let Some(target) = target else {
        anyhow::bail!("No target specified. Pass a target name on the command-line, or set the `config.default` variable. Use `--list` to get a list of available targets.");
    };

    // Hide cursor and disable line wrapping while running.
    watcher.lock().start_advanced_rendering();

    let result = runner.build_or_run(&target).await;

    // Show the cursor again and re-enable line wrapping.
    watcher.lock().finish_advanced_rendering();

    let write_cache = match result {
        Ok(_) => true,
        Err(ref err) => err.should_still_write_werk_cache(),
    };

    if write_cache {
        if let Err(err) = runner.workspace().finalize(&*io).await {
            eprintln!("Error writing `.werk-cache`: {err}")
        }
    }

    result.map(|_| ()).map_err(Into::into)
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
