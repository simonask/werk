pub mod dry_run;
mod watcher;

use std::sync::Arc;

use clap::Parser;
use owo_colors::OwoColorize as _;
use watcher::AutoStream;
use werk_fs::Absolute;
use werk_parser::parser::Spanned as _;
use werk_runner::{Runner, Workspace, WorkspaceSettings};

shadow_rs::shadow!(build);

fn version_string() -> String {
    format!(
        "{} ({} {})",
        build::PKG_VERSION,
        &build::COMMIT_HASH[0..8],
        &build::BUILD_TIME[0..10]
    )
}

#[derive(Debug, clap::Parser)]
#[command(version = version_string())]
pub struct Args {
    /// The target to build.
    pub target: Option<String>,

    #[clap(short, long)]
    /// The path to the Werkfile. Defaults to searching for `Werkfile` in the
    /// current working directory and its parents.
    pub file: Option<std::path::PathBuf>,

    /// List the available recipes.
    #[clap(short, long)]
    pub list: bool,

    /// Dry run; do not execute any recipe commands. Note: Shell commands used
    /// in global variables are still executed!
    #[clap(long)]
    pub dry_run: bool,

    /// Print recipe commands as they are executed. Implied by `--verbose`.
    #[clap(long)]
    pub print_commands: bool,

    /// Print recipes that were up-to-date.
    /// Implied by `--verbose`.
    #[clap(long)]
    pub print_fresh: bool,

    /// Silence informational output from executed commands, only printing to
    /// the terminal when a recipe fails.
    #[clap(long)]
    pub quiet: bool,

    /// For each outdated target, explain why it was outdated. Implied by
    /// `--verbose`.
    #[clap(long)]
    pub explain: bool,

    /// Shorthand for `--explain --print-commands --print-fresh --no-capture`.
    #[clap(long, short)]
    pub verbose: bool,

    #[clap(long, default_value = "auto")]
    pub color: ColorChoice,

    #[clap(long, default_value = "ansi")]
    pub output_format: OutputChoice,

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

    /// Enable debug logging to stdout.
    ///
    /// This takes a logging directive like `RUST_LOG`.
    #[clap(long)]
    pub log: Option<Option<String>>,
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
    #[error("Invalid workspace directory: {}", .0.display())]
    Workspace(std::path::PathBuf),
    #[error("werk.toml or Werkfile not found in this directory or any parent directory")]
    NoWerkfile,
    #[error("Invalid define (must take the form `key=value`): {0}")]
    InvalidDefineArg(String),
    #[error("No target specified. Pass a target name on the command-line, or set the `config.default` variable. Use `--list` to get a list of available targets.")]
    NoTarget,
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

fn main() -> Result<(), Error> {
    let args = Args::parse();
    match args.log {
        Some(Some(ref directive)) => tracing_subscriber::fmt::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::new(directive))
            .init(),
        Some(_) => tracing_subscriber::fmt::fmt()
            .with_env_filter("werk=info,werk_runner=info")
            .init(),
        _ => (),
    }

    smol::block_on(try_main(args))
}

async fn try_main(args: Args) -> Result<(), Error> {
    anstyle_query::windows::enable_ansi_colors();

    let color_stdout = watcher::ColorOutputKind::initialize(&std::io::stdout(), args.color);
    let color_stderr = watcher::ColorOutputKind::initialize(&std::io::stderr(), args.color);

    let werkfile = if let Some(file) = args.file {
        let file = Absolute::new_unchecked(std::path::absolute(file)?);
        if file.extension() == Some("toml".as_ref()) {
            Werkfile::Toml(file)
        } else {
            Werkfile::Werk(file)
        }
    } else {
        find_werkfile()?
    };
    tracing::info!("Using werkfile: {}", werkfile.as_ref().display());

    // Determine the workspace directory.
    let workspace_dir_abs;
    let workspace_dir = if let Some(ref workspace_dir) = args.workspace_dir {
        workspace_dir_abs = std::path::absolute(workspace_dir)?;
        if !workspace_dir_abs.is_dir() {
            return Err(Error::Workspace(workspace_dir.clone()));
        } else {
            Absolute::new_ref_unchecked(&*workspace_dir_abs)
        }
    } else {
        Absolute::new_ref_unchecked(
            werkfile
                .as_ref()
                .parent()
                .ok_or_else(|| Error::Workspace(werkfile.as_ref().to_path_buf()))?,
        )
    };

    // Parse the werk manifest!
    let source_code = std::fs::read_to_string(&werkfile)?;
    let display_error = |err: werk_runner::Error| print_error(werkfile.as_ref(), &source_code, err);
    let display_parse_error =
        |err: werk_parser::Error| print_parse_error(werkfile.as_ref(), &source_code, err);
    let display_root_eval_error =
        |err: werk_runner::EvalError| print_eval_error(werkfile.as_ref(), &source_code, err);

    let toml_document;
    let ast = match werkfile {
        Werkfile::Werk(_) => werk_parser::parse_werk(&source_code).map_err(display_parse_error)?,
        Werkfile::Toml(_) => {
            toml_document = toml_edit::ImDocument::parse(&*source_code)
                .map_err(Into::into)
                .map_err(display_parse_error)?;
            werk_parser::parse_toml_document(&toml_document).map_err(display_parse_error)?
        }
    };

    // Read the configuration statements from the AST.
    let config = werk_runner::ir::Config::new(&ast).map_err(display_root_eval_error)?;

    let out_dir = args
        .output_dir
        .or_else(|| {
            config
                .output_directory
                .as_ref()
                .map(|s| workspace_dir.join(&**s))
        })
        .unwrap_or_else(|| workspace_dir.join("target"));
    let out_dir = Absolute::new_unchecked(std::path::absolute(out_dir)?);
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

    let io: Arc<dyn werk_runner::Io> = if args.dry_run || args.list {
        Arc::new(dry_run::DryRun::new())
    } else {
        Arc::new(werk_runner::RealSystem::new())
    };

    let watcher = watcher::make_watcher(watcher::OutputSettings {
        logging_enabled: args.log.is_some() || args.list,
        color: color_stderr,
        output: if args.log.is_some() {
            OutputChoice::Log
        } else {
            args.output_format
        },
        print_recipe_commands: args.print_commands | args.verbose,
        print_fresh: args.print_fresh | args.verbose,
        dry_run: args.dry_run,
        quiet: args.quiet && !args.verbose,
        explain: args.explain | args.verbose,
    });

    let workspace = Workspace::new(&ast, &*io, &*watcher, workspace_dir.to_owned(), &settings)
        .map_err(display_error)?;

    if args.list {
        let mut output = AutoStream::new(std::io::stdout(), color_stdout);
        print_list(&workspace.manifest, &mut output);
        return Ok(());
    }

    let target = args.target.or_else(|| config.default_target.clone());
    let Some(target) = target else {
        return Err(Error::NoTarget);
    };

    let runner = Runner::new(&workspace);
    let result = runner.build_or_run(&target).await;

    let write_cache = match result {
        Ok(_) => true,
        Err(ref err) => err.should_still_write_werk_cache(),
    };

    if write_cache {
        if let Err(err) = workspace.finalize().await {
            eprintln!("Error writing `.werk-cache`: {err}")
        }
    }

    result.map(|_| ()).map_err(display_error)
}

pub fn print_list(doc: &werk_runner::ir::Manifest, out: &mut dyn std::io::Write) {
    let globals = doc
        .globals
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

enum Werkfile {
    Werk(Absolute<std::path::PathBuf>),
    Toml(Absolute<std::path::PathBuf>),
}

impl AsRef<std::path::Path> for Werkfile {
    fn as_ref(&self) -> &std::path::Path {
        match self {
            Werkfile::Werk(path) | Werkfile::Toml(path) => path,
        }
    }
}

fn find_werkfile() -> Result<Werkfile, Error> {
    const WERKFILE_NAMES_TOML: &[&str] = &["werk.toml"];
    const WERKFILE_NAMES: &[&str] = &["werk.toml", "Werkfile", "werkfile", "build.werk"];

    let mut current = std::env::current_dir()?;
    if !current.is_absolute() {
        return Err(Error::Workspace(current));
    }

    loop {
        for name in WERKFILE_NAMES_TOML {
            let candidate = Absolute::new_unchecked(current.join(name));
            if candidate.is_file() {
                return Ok(Werkfile::Toml(candidate));
            }
        }

        for name in WERKFILE_NAMES {
            let candidate = Absolute::new_unchecked(current.join(name));
            if candidate.is_file() {
                return Ok(Werkfile::Werk(candidate));
            }
        }

        if let Some(parent) = current.parent() {
            current = parent.to_owned();
        } else {
            return Err(Error::NoWerkfile);
        }
    }
}

fn print_error(path: &std::path::Path, werkfile: &str, err: werk_runner::Error) -> Error {
    match err {
        werk_runner::Error::Eval(eval_error) => {
            print_eval_error(path, werkfile, eval_error);
            Error::Eval
        }
        otherwise => {
            anstream::eprintln!("{otherwise}");
            Error::Runner
        }
    }
}

fn print_eval_error(path: &std::path::Path, werkfile: &str, err: werk_runner::EvalError) -> Error {
    use annotate_snippets::{Level, Snippet};

    let file_name = path.display().to_string();
    let span = err.span();

    let err_string = err.to_string();
    let err = Level::Error.title("evaluation error").snippet(
        Snippet::source(werkfile)
            .origin(&file_name)
            .fold(true)
            .annotation(Level::Error.span(span.into()).label(&err_string)),
    );
    let renderer = annotate_snippets::Renderer::styled();
    let render = renderer.render(err);
    anstream::eprintln!("{}", render);
    Error::Eval
}

fn print_parse_error(path: &std::path::Path, werkfile: &str, err: werk_parser::Error) -> Error {
    let err = err.with_location(path, werkfile);
    anstream::eprintln!("{}", err);
    Error::Parse
}
