use crate::ast;

pub struct Document<'a> {
    pub root: ast::Root<'a>,
    pub source: &'a str,
    /// "Whitespace" smuggled from TOML decorations.
    pub smuggled_whitespace: Option<String>,
}

impl<'a> Document<'a> {
    pub(crate) fn new(
        root: ast::Root<'a>,
        source: &'a str,
        smuggled_whitespace: Option<String>,
    ) -> Self {
        Self {
            root,
            source,
            smuggled_whitespace,
        }
    }

    #[must_use]
    pub fn get_whitespace(&self, whitespace: ast::Whitespace) -> &str {
        let range = whitespace.0.start.0 as usize..whitespace.0.end.0 as usize;
        if let Some(ref smuggled) = self.smuggled_whitespace {
            smuggled.get(range).unwrap_or_default()
        } else {
            self.source.get(range).unwrap_or_default()
        }
    }

    pub fn task_recipes(&self) -> impl Iterator<Item = &ast::CommandRecipe<'_>> + '_ {
        self.root
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.statement {
                ast::RootStmt::Task(task) => Some(task),
                _ => None,
            })
    }

    pub fn build_recipes(&self) -> impl Iterator<Item = &ast::BuildRecipe<'_>> + '_ {
        self.root
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.statement {
                ast::RootStmt::Build(build) => Some(build),
                _ => None,
            })
    }

    pub fn config_stmts(&self) -> impl Iterator<Item = &ast::ConfigStmt<'_>> + '_ {
        self.root
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.statement {
                ast::RootStmt::Config(config) => Some(config),
                _ => None,
            })
    }

    pub fn globals(&self) -> impl Iterator<Item = &ast::LetStmt<'_>> + '_ {
        self.root
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.statement {
                ast::RootStmt::Let(let_stmt) => Some(let_stmt),
                _ => None,
            })
    }

    #[must_use]
    pub fn num_task_recipes(&self) -> usize {
        self.task_recipes().count()
    }

    #[must_use]
    pub fn num_build_recipes(&self) -> usize {
        self.build_recipes().count()
    }

    #[must_use]
    pub fn num_globals(&self) -> usize {
        self.globals().count()
    }

    #[must_use]
    pub fn num_config_stmts(&self) -> usize {
        self.config_stmts().count()
    }

    #[must_use]
    pub fn find_config(&self, name: &str) -> Option<&ast::ConfigStmt<'_>> {
        self.config_stmts().find(|stmt| stmt.ident == name)
    }

    #[must_use]
    pub fn find_global(&self, name: &str) -> Option<&ast::LetStmt<'_>> {
        self.globals().find(|stmt| stmt.ident == name)
    }

    #[must_use]
    pub fn find_task_recipe(&self, name: &str) -> Option<&ast::CommandRecipe<'_>> {
        self.task_recipes().find(|stmt| stmt.name == name)
    }
}
