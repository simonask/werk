use werk_eval::EvalError;
use werk_parser::ast;
use werk_util::DiagnosticFileId;

use crate::Edition;

#[derive(Debug, Default, PartialEq)]
pub struct Defaults {
    pub output_directory: Option<String>,
    pub print_commands: Option<bool>,
    pub print_fresh: Option<bool>,
    pub quiet: Option<bool>,
    pub loud: Option<bool>,
    pub explain: Option<bool>,
    pub verbose: Option<bool>,
    pub watch_delay: Option<i32>,
    pub jobs: Option<usize>,
    pub edition: Edition,
}

impl Defaults {
    pub fn new(ast: &ast::Root, file: DiagnosticFileId) -> Result<Self, EvalError> {
        let mut defaults = Self::default();
        for stmt in &ast.statements {
            let ast::RootStmt::Default(ref stmt) = stmt.statement else {
                continue;
            };

            match stmt {
                ast::DefaultStmt::Target(_) => {} // Evaluated on workspace creation.
                ast::DefaultStmt::OutDir(entry) => {
                    defaults.output_directory = Some(entry.value.1.clone());
                }
                ast::DefaultStmt::PrintCommands(entry) => {
                    defaults.print_commands = Some(entry.value.1);
                }
                ast::DefaultStmt::PrintFresh(entry) => defaults.print_fresh = Some(entry.value.1),
                ast::DefaultStmt::Quiet(entry) => defaults.quiet = Some(entry.value.1),
                ast::DefaultStmt::Loud(entry) => defaults.loud = Some(entry.value.1),
                ast::DefaultStmt::Explain(entry) => defaults.explain = Some(entry.value.1),
                ast::DefaultStmt::Verbose(entry) => defaults.verbose = Some(entry.value.1),
                ast::DefaultStmt::WatchDelay(entry) => defaults.watch_delay = Some(entry.value.1),
                ast::DefaultStmt::Jobs(entry) => defaults.jobs = entry.value.1.try_into().ok(),
                ast::DefaultStmt::Edition(entry) => {
                    if entry.value.1 == "v1" {
                        defaults.edition = Edition::V1;
                    } else {
                        return Err(EvalError::InvalidEdition(file.span(entry.value.0)));
                    }
                }
            }
        }

        Ok(defaults)
    }
}
