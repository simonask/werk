use crate::ast;

pub struct Document<'a> {
    pub root: ast::Root<'a>,
    pub origin: &'a std::path::Path,
    pub source: &'a str,
    /// "Whitespace" smuggled from TOML decorations.
    pub smuggled_whitespace: Option<String>,
}

impl<'a> Document<'a> {
    pub(crate) fn new(
        root: ast::Root<'a>,
        origin: &'a std::path::Path,
        source: &'a str,
        smuggled_whitespace: Option<String>,
    ) -> Self {
        Self {
            root,
            origin,
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

    pub fn task_recipes<'b>(
        &'b self,
    ) -> impl Iterator<Item = &'b ast::TaskRecipe<'a>> + use<'a, 'b> {
        self.root
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.statement {
                ast::RootStmt::Task(task) => Some(task),
                _ => None,
            })
    }

    pub fn build_recipes<'b>(
        &'b self,
    ) -> impl Iterator<Item = &'b ast::BuildRecipe<'a>> + use<'a, 'b> {
        self.root
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.statement {
                ast::RootStmt::Build(build) => Some(build),
                _ => None,
            })
    }

    pub fn globals<'b>(&'b self) -> impl Iterator<Item = &'b ast::LetStmt<'a>> + use<'a, 'b> {
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
    pub fn find_default_target(&self) -> Option<&ast::StringExpr<'a>> {
        self.root.statements.iter().find_map(|stmt| {
            if let ast::RootStmt::Default(ast::DefaultStmt::Target(ref stmt)) = stmt.statement {
                Some(&stmt.value)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn find_global(&self, name: &str) -> Option<&ast::LetStmt<'_>> {
        self.globals().find(|stmt| stmt.ident == name)
    }

    #[must_use]
    pub fn find_task_recipe(&self, name: &str) -> Option<&ast::TaskRecipe<'_>> {
        self.task_recipes().find(|stmt| stmt.name == name)
    }
}

impl<'a> werk_util::DiagnosticSourceMap for &'a Document<'a> {
    #[inline]
    fn get_source(
        &self,
        id: werk_util::DiagnosticFileId,
    ) -> Option<werk_util::DiagnosticSource<'_>> {
        if id.0 == 0 {
            Some(werk_util::DiagnosticSource::new(self.origin, self.source))
        } else {
            None
        }
    }
}
