pub mod dry_run;
mod render;

use std::sync::Arc;

use ahash::HashSet;
use clap::Parser;
use futures::future::Either;
use notify_debouncer_full::notify;
use owo_colors::OwoColorize as _;
use render::AutoStream;
use werk_fs::{Absolute, Normalize as _, PathError};
use werk_runner::{Runner, Workspace, WorkspaceSettings};
use werk_util::{Diagnostic, DiagnosticError, DiagnosticFileRepository, DiagnosticSource};

shadow_rs::shadow!(build);

fn version_string() -> String {
    format!(
        "{} ({} {})",
        build::PKG_VERSION,
        &build::COMMIT_HASH[0..8],
        &build::BUILD_TIME[0..10]
    )
}

#[derive(clap::Args, Debug)]
#[command(next_help_heading = "Output options")]
pub struct OutputArgs {
    /// Print recipe commands as they are executed. Implied by `--verbose`.
    #[clap(long, next_help_heading = "print")]
    pub print_commands: bool,

    /// Print recipes that were up-to-date.
    /// Implied by `--verbose`.
    #[clap(long, help_heading = "print")]
    pub print_fresh: bool,

    /// Silence informational output from executed commands, only printing to
    /// the terminal when a recipe fails.
    #[clap(long)]
    pub quiet: bool,

    /// Print all informational output from executed commands to the terminal,
    /// even for quiet recipes. Implied by `--verbose`.
    #[clap(long)]
    pub loud: bool,

    /// For each outdated target, explain why it was outdated. Implied by
    /// `--verbose`.
    #[clap(long)]
    pub explain: bool,

    /// Shorthand for `--explain --print-commands --print-fresh --no-capture --loud`.
    #[clap(long, short)]
    pub verbose: bool,

    #[clap(long, default_value = "auto")]
    pub color: ColorChoice,

    #[clap(long, default_value = "ansi")]
    pub output_format: OutputChoice,

    /// Enable debug logging to stdout.
    ///
    /// This takes a logging directive like `RUST_LOG`.
    #[clap(long)]
    pub log: Option<Option<String>>,
}

#[derive(Debug, clap::Parser)]
#[command(version = version_string(),)]
pub struct Args {
    /// The target to build.
    pub target: Option<String>,

    /// The path to the Werkfile. Defaults to searching for `Werkfile` in the
    /// current working directory and its parents.
    #[clap(short, long)]
    pub file: Option<std::path::PathBuf>,

    /// List the available recipes.
    #[clap(short, long)]
    pub list: bool,

    /// Dry run; do not execute any recipe commands. Note: Shell commands used
    /// in global variables are still executed!
    #[clap(long)]
    pub dry_run: bool,

    /// Build the target, then keep rebuilding it when the workspace changes.
    #[clap(long, short)]
    pub watch: bool,

    /// Number of milliseconds to wait after a filesystem change before
    /// rebuilding. Implies `--watch`.
    #[clap(long, default_value = "250")]
    pub watch_delay: u64,

    /// Number of tasks to execute in parallel. Defaults to the number of CPU cores.
    #[clap(long, short)]
    pub jobs: Option<usize>,

    /// Override the workspace directory. Defaults to the directory containing
    /// Werkfile.
    #[clap(long)]
    pub workspace_dir: Option<std::path::PathBuf>,

    /// Use the output directory instead of the default.
    #[clap(long)]
    pub output_dir: Option<std::path::PathBuf>,

    /// Override global variable. This takes the form `name=value`.
    #[clap(long, short = 'D')]
    pub define: Vec<String>,

    #[command(flatten)]
    pub output: OutputArgs,
}

/// Color mode.
#[derive(Clone, Copy, Default, Debug, clap::ValueEnum)]
pub enum ColorChoice {
    /// Probe the current terminal and environment variables for color support.
    #[default]
    Auto,
    /// Force color output, even if the command is not running in a terminal.
    Always,
    /// Do not use color output.
    Never,
}

/// Terminal output mode.
#[derive(Clone, Copy, Default, Debug, clap::ValueEnum)]
pub enum OutputChoice {
    /// Provide friendly user feedback assuming an ANSI terminal.
    #[default]
    Ansi,
    /// Emit the progress as log statements (assuming `WERK_LOG` is set to a value).
    Log,
    /// Report progress as JSON to stdout. This also disables color output.
    Json,
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Parsing error")]
    Parse,
    #[error("Evaluation error")]
    Eval,
    #[error("Runner error")]
    Runner,
    #[error("Invalid workspace directory '{0}': {1}")]
    WorkspaceDirectory(String, std::io::Error),
    #[error("Invalid output directory '{0}': {1}")]
    OutputDirectory(String, PathError),
    #[error("Werkfile not found in this directory or any parent directory")]
    NoWerkfile,
    #[error("Invalid define (must take the form `key=value`): {0}")]
    InvalidDefineArg(String),
    #[error("No target specified. Pass a target name on the command-line, or set the `config.default` variable. Use `--list` to get a list of available targets.")]
    NoTarget,
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Notify(#[from] notify::Error),
}

fn main() -> Result<(), Error> {
    let args = Args::parse();
    match args.output.log {
        Some(Some(ref directive)) => tracing_subscriber::fmt::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::new(directive))
            .init(),
        Some(_) => tracing_subscriber::fmt::fmt()
            .with_env_filter("werk=info,werk_runner=info")
            .init(),
        None => tracing_subscriber::fmt::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_env("WERK_LOG"))
            .init(),
    }

    smol::block_on(try_main(args))
}

async fn try_main(args: Args) -> Result<(), Error> {
    anstyle_query::windows::enable_ansi_colors();

    let color_stdout = render::ColorOutputKind::initialize(&std::io::stdout(), args.output.color);
    let color_stderr = render::ColorOutputKind::initialize(&std::io::stderr(), args.output.color);

    let werkfile = if let Some(file) = args.file {
        file.normalize()?
    } else {
        find_werkfile()?
    };
    tracing::info!("Using werkfile: {}", werkfile.display());

    // Determine the workspace directory.
    let workspace_dir_abs;
    let workspace_dir = if let Some(ref workspace_dir) = args.workspace_dir {
        workspace_dir_abs = workspace_dir.as_path().normalize()?;
        if !workspace_dir_abs.is_dir() {
            return Err(Error::WorkspaceDirectory(
                workspace_dir.display().to_string(),
                std::io::Error::new(std::io::ErrorKind::NotADirectory, "not a directory"),
            ));
        } else {
            &workspace_dir_abs
        }
    } else {
        werkfile
            .parent()
            .expect("normalized Werkfile path has no parent directory")
    };

    // Parse the werk manifest!
    let source_code = std::fs::read_to_string(&werkfile)?;

    let ast = werk_parser::parse_werk(&werkfile, &source_code).map_err(|err| {
        print_parse_error(err.into_diagnostic_error(DiagnosticSource::new(&werkfile, &source_code)))
    })?;

    // Read the configuration statements from the AST.
    let config = werk_runner::ir::Config::new(&ast).map_err(|err| {
        print_eval_error(err.into_diagnostic_error(DiagnosticSource::new(&werkfile, &source_code)))
    })?;

    let out_dir = find_output_directory(
        workspace_dir,
        args.output_dir.as_deref(),
        config.output_directory.as_deref(),
    )?;

    tracing::info!("Project directory: {}", workspace_dir.display());
    tracing::info!("Output directory: {}", out_dir.display());

    let mut settings = WorkspaceSettings::new(workspace_dir.to_owned());
    settings.jobs = args.jobs.unwrap_or_else(num_cpus::get);
    settings.output_directory = out_dir;
    for def in &args.define {
        let Some((key, value)) = def.split_once('=') else {
            return Err(Error::InvalidDefineArg(def.clone()));
        };
        settings.define(key, value);
    }
    settings.force_color = color_stdout.supports_color();

    settings.artificial_delay = std::env::var("_WERK_ARTIFICIAL_DELAY")
        .ok()
        .and_then(|s| s.parse().ok())
        .map(std::time::Duration::from_millis);

    let io: Arc<dyn werk_runner::Io> = if args.dry_run || args.list {
        Arc::new(dry_run::DryRun::new())
    } else {
        Arc::new(werk_runner::RealSystem::new())
    };

    let renderer = render::make_renderer(render::OutputSettings {
        logging_enabled: args.output.log.is_some() || args.list,
        color: color_stderr,
        output: if args.output.log.is_some() {
            OutputChoice::Log
        } else {
            args.output.output_format
        },
        print_recipe_commands: args.output.print_commands | args.output.verbose,
        print_fresh: args.output.print_fresh | args.output.verbose,
        dry_run: args.dry_run,
        quiet: args.output.quiet && !args.output.verbose && !args.output.loud,
        loud: args.output.loud | args.output.verbose,
        explain: args.output.explain | args.output.verbose,
    });

    let workspace = Workspace::new_with_diagnostics(
        &ast,
        &*io,
        &*renderer,
        workspace_dir.to_owned(),
        &settings,
    )
    .map_err(print_error)?;

    if args.list {
        let mut output = AutoStream::new(std::io::stdout(), color_stdout);
        print_list(&workspace.manifest, &mut output);
        return Ok(());
    }

    let target = args
        .target
        .clone()
        .or_else(|| config.default_target.clone());
    let Some(target) = target else {
        return Err(Error::NoTarget);
    };

    let runner = Runner::new(&workspace);
    let result = runner.build_or_run(&target).await;

    let write_cache = match result {
        Ok(_) => true,
        Err(ref err) => err.error.should_still_write_werk_cache(),
    };

    if write_cache {
        if let Err(err) = workspace.finalize().await {
            eprintln!("Error writing `.werk-cache`: {err}")
        }
    }

    std::mem::drop(runner);

    if args.watch {
        autowatch_loop(
            std::time::Duration::from_millis(args.watch_delay),
            workspace,
            werkfile.clone(),
            args.target,
            args.output_dir.as_deref(),
            &settings,
        )
        .await?;
        Ok(())
    } else {
        result.map(|_| ()).map_err(print_error)
    }
}

async fn autowatch_loop(
    timeout: std::time::Duration,
    // The initial workspace built by main(). Must be finalize()d.
    workspace: Workspace<'_>,
    werkfile: Absolute<std::path::PathBuf>,
    // Target to keep building
    target_from_args: Option<String>,
    output_directory_from_args: Option<&std::path::Path>,
    settings: &WorkspaceSettings,
) -> Result<(), notify::Error> {
    let (notification_sender, notification_receiver) = smol::channel::bounded(1);

    let (ctrlc_sender, ctrlc_receiver) = smol::channel::bounded(1);
    _ = ctrlc::set_handler(move || {
        _ = ctrlc_sender.try_send(());
    });

    let (io, render) = (workspace.io, workspace.render);

    let watch_manifest = HashSet::from_iter([werkfile.clone()]);
    let mut watch_set = watch_manifest.clone();
    watch_set.extend(workspace.workspace_files().filter_map(|(_, entry)| {
        if entry.metadata.is_file {
            Some(entry.path.clone())
        } else {
            None
        }
    }));
    let workspace_dir = workspace.project_root().to_path_buf();
    std::mem::drop(workspace);

    let mut settings = settings.clone();

    loop {
        if watch_set == watch_manifest {
            render.runner_message("Watching manifest for changes, press Ctrl-C to stop");
        } else {
            render.runner_message(&format!(
                "Watching {} files for changes, press Ctrl-C to stop",
                watch_set.len(),
            ));
        }

        // Start the notifier.
        let notifier = make_notifier_for_files(&watch_set, notification_sender.clone(), timeout)?;
        let notification_recv = notification_receiver.recv();
        let ctrlc_recv = ctrlc_receiver.recv();
        smol::pin!(notification_recv);
        smol::pin!(ctrlc_recv);

        match futures::future::select(notification_recv, ctrlc_recv).await {
            Either::Left((result, _)) => result.expect("notifier channel error"),
            Either::Right((result, _)) => {
                if result.is_ok() {
                    render.runner_message("Stopping...");
                    return Ok(());
                }
            }
        }

        // Stop the notifier again immediately. TODO: Consider if it makes sense to reuse it.
        notifier.stop();

        // Reset any progress indicators between runs.
        render.reset();

        // Re-read the manifest.
        let source_code = match std::fs::read_to_string(&werkfile) {
            Ok(source_code) => source_code,
            Err(err) => {
                render.warning(None, &format!("Error reading manifest: {err}"));
                watch_set = watch_manifest.clone();
                continue;
            }
        };

        let ast = werk_parser::parse_werk_with_diagnostics(&werkfile, &source_code);

        let ast = match ast {
            Ok(ast) => ast,
            Err(err) => {
                print_parse_error(err);
                watch_set = watch_manifest.clone();
                continue;
            }
        };

        // Reload config.
        let config = match werk_runner::ir::Config::new_with_diagnostics(&ast) {
            Ok(config) => config,
            Err(err) => {
                print_eval_error(err);
                watch_set = watch_manifest.clone();
                continue;
            }
        };

        let out_dir = match find_output_directory(
            &workspace_dir,
            output_directory_from_args,
            config.output_directory.as_deref(),
        ) {
            Ok(out_dir) => out_dir,
            Err(err) => {
                render.warning(None, &format!("Error finding output directory: {err}"));
                watch_set = watch_manifest.clone();
                continue;
            }
        };

        if out_dir != settings.output_directory {
            render.warning(
                None,
                &format!(
                    "Output directory changed: `{}` => `{}`",
                    settings.output_directory.display(),
                    out_dir.display()
                ),
            );
            settings.output_directory = out_dir;
        }

        let target = target_from_args
            .clone()
            .or_else(|| config.default_target.clone());
        let Some(target) = target else {
            render.warning(None, "No configured default target");
            watch_set = watch_manifest.clone();
            continue;
        };

        let workspace = match Workspace::new_with_diagnostics(
            &ast,
            io,
            render,
            workspace_dir.clone(),
            &settings,
        ) {
            Ok(workspace) => workspace,
            Err(err) => {
                print_error(err);
                // Workspace evaluation may depend on other files, so just keep
                // the current watchset.
                continue;
            }
        };

        // Update the watchset.
        watch_set.clear();
        watch_set.extend(watch_manifest.iter().cloned());
        watch_set.extend(workspace.workspace_files().filter_map(|(_, entry)| {
            if entry.metadata.is_file {
                Some(entry.path.clone())
            } else {
                None
            }
        }));

        // Finally, rebuild the target!
        let runner = Runner::new(&workspace);
        let write_cache = match runner.build_or_run(&target).await {
            Ok(_) => true,
            Err(err) => {
                let write_cache = err.error.should_still_write_werk_cache();
                print_error(err);
                write_cache
            }
        };

        if write_cache {
            if let Err(err) = workspace.finalize().await {
                eprintln!("Error writing `.werk-cache`: {err}");
                return Err(err.into());
            }
        }
    }
}

fn make_notifier_for_files(
    watch_set: &HashSet<Absolute<std::path::PathBuf>>,
    notification_sender: smol::channel::Sender<()>,
    timeout: std::time::Duration,
) -> Result<
    notify_debouncer_full::Debouncer<
        notify::RecommendedWatcher,
        notify_debouncer_full::RecommendedCache,
    >,
    notify::Error,
> {
    let mut notifier =
        notify_debouncer_full::new_debouncer(timeout, Some(timeout), move |_event| {
            _ = notification_sender.force_send(());
        })?;

    for path in watch_set {
        _ = notifier.watch(path, notify::RecursiveMode::NonRecursive);
    }

    Ok(notifier)
}

pub fn print_list(doc: &werk_runner::ir::Manifest, out: &mut dyn std::io::Write) {
    let globals = doc
        .globals
        .iter()
        .map(|(k, v)| (k, format!("{}", v.value.display_friendly(80)), &v.comment))
        .collect::<Vec<_>>();
    let max_global_name_len = globals
        .iter()
        .map(|(name, _, _)| name.as_str().len())
        .max()
        .unwrap_or(0);
    let max_global_value_len = globals
        .iter()
        .map(|(_, value, comment)| if !comment.is_empty() { value.len() } else { 0 })
        .max()
        .unwrap_or(0);

    let max_command_len = doc
        .task_recipes
        .iter()
        .map(|(name, _)| name.len())
        .max()
        .unwrap_or(0);
    let max_pattern_len = doc
        .build_recipes
        .iter()
        .map(|recipe| recipe.pattern.string.len())
        .max()
        .unwrap_or(0);

    if max_global_name_len != 0 {
        _ = writeln!(out, "{}", "Global variables:".bright_purple());

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
                    comment.dimmed(),
                );
            }
        }

        if max_command_len != 0 || max_pattern_len != 0 {
            _ = writeln!(out);
        }
    }

    if max_command_len != 0 {
        _ = writeln!(out, "{}", "Available commands:".bright_purple());
        for (name, recipe) in &doc.task_recipes {
            if recipe.doc_comment.is_empty() {
                _ = writeln!(out, "  {}", name.bright_cyan());
            } else {
                _ = writeln!(
                    out,
                    "  {} {}",
                    format_args!("{: <w$}", name.bright_cyan(), w = max_command_len),
                    recipe.doc_comment.dimmed(),
                );
            }
        }
        if max_pattern_len != 0 {
            _ = writeln!(out);
        }
    }

    if max_pattern_len != 0 {
        _ = writeln!(out, "{}", "Available recipes:".bright_purple());
        for recipe in &doc.build_recipes {
            if recipe.doc_comment.is_empty() {
                _ = writeln!(out, "  {}", recipe.pattern.string.bright_yellow());
            } else {
                _ = writeln!(
                    out,
                    "  {} {}",
                    format_args!(
                        "{: <w$}",
                        recipe.pattern.string.bright_yellow(),
                        w = max_pattern_len
                    ),
                    recipe.doc_comment.dimmed(),
                );
            }
        }
    }
}

fn find_werkfile() -> Result<Absolute<std::path::PathBuf>, Error> {
    const WERKFILE_NAMES: &[&str] = &["Werkfile", "werkfile", "build.werk"];

    let mut current = Absolute::current_dir()?;

    loop {
        for name in WERKFILE_NAMES {
            let candidate = current.join(name).unwrap();
            if candidate.is_file() {
                return Ok(candidate);
            }
        }

        if let Some(parent) = current.parent() {
            current = parent.to_owned();
        } else {
            return Err(Error::NoWerkfile);
        }
    }
}

fn find_output_directory(
    workspace_dir: &Absolute<std::path::Path>,
    from_args: Option<&std::path::Path>,
    from_config: Option<&str>,
) -> Result<Absolute<std::path::PathBuf>, Error> {
    if let Some(from_args) = from_args {
        workspace_dir
            .join(from_args)
            .map_err(|err| Error::OutputDirectory(from_args.display().to_string(), err.into()))
    } else if let Some(from_config) = from_config {
        workspace_dir
            .join(from_config)
            .map_err(|err| Error::OutputDirectory(from_config.to_owned(), err.into()))
    } else {
        Ok(workspace_dir.join("target").unwrap())
    }
}

fn print_error<E: Diagnostic, R: DiagnosticFileRepository>(err: DiagnosticError<E, R>) -> Error {
    print_diagnostic(err);
    Error::Runner
}

fn print_eval_error<E: Diagnostic, R: DiagnosticFileRepository>(
    err: DiagnosticError<E, R>,
) -> Error {
    print_diagnostic(err);
    Error::Eval
}

fn print_parse_error<E: Diagnostic, R: DiagnosticFileRepository>(
    err: DiagnosticError<E, R>,
) -> Error {
    print_diagnostic(err);
    Error::Parse
}

fn print_diagnostic<E: Diagnostic, R: DiagnosticFileRepository>(err: DiagnosticError<E, R>) {
    use annotate_snippets::renderer::DEFAULT_TERM_WIDTH;
    let renderer = annotate_snippets::Renderer::styled().term_width(
        render::stderr_width()
            .diagnostic_terminal_width()
            .unwrap_or(DEFAULT_TERM_WIDTH),
    );
    anstream::eprintln!("{}", err.with_renderer(&renderer));
}
