use std::{fmt::Display, sync::Arc};

mod ansi;
mod json;
mod log;
mod stream;

pub use stream::*;

use crate::OutputChoice;

#[derive(Clone, Copy, Debug)]
pub struct OutputSettings {
    /// Logging is enabled, so don't try to modify terminal contents in-place.
    pub logging_enabled: bool,
    pub color: ColorOutputKind,
    pub output: OutputChoice,
    pub print_recipe_commands: bool,
    pub print_fresh: bool,
    pub dry_run: bool,
    pub quiet: bool,
    pub loud: bool,
    pub explain: bool,
}

pub(crate) struct Bracketed<T>(pub T);
impl<T: Display> Display for Bracketed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

pub(crate) struct Step(usize, usize);
impl Display for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.0, self.1)
    }
}

pub fn make_renderer(settings: OutputSettings) -> Arc<dyn werk_runner::Render> {
    match settings.output {
        OutputChoice::Json => Arc::new(json::JsonWatcher::new()),
        OutputChoice::Log => Arc::new(log::LogWatcher::new(settings)),
        OutputChoice::Ansi => {
            let stderr = AutoStream::new(std::io::stderr(), settings.color);
            let must_be_linear = settings.logging_enabled | !stderr.supports_nonlinear_output();
            if must_be_linear {
                Arc::new(ansi::TerminalRenderer::<true>::new(settings, stderr))
            } else {
                Arc::new(ansi::TerminalRenderer::<false>::new(settings, stderr))
            }
        }
    }
}
