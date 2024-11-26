use std::sync::Arc;

use indexmap::IndexMap;
use werk_parser::ast;

use crate::{
    project, Error, Pattern, PatternBuilder, PatternMatch, Project, RecipeMatch, ShellCommandLine,
    ShellCommandLineBuilder, ShellError, Value,
};

pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    vars: IndexMap<String, Value>,
    pattern_match: Option<&'a PatternMatch<'a>>,
    implied_value: Option<Value>,
}

#[derive(Debug, Clone)]
pub enum StringEvalContext {
    Literal,
    Path,
    Argument,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum EvalError {
    #[error("no pattern stem in this rule")]
    NoPatternStem,
    #[error("one-of patterns not allowed in this context")]
    IllegalOneOfPattern,
    #[error("no implied interpolation value in this context; provide an identifier or a capture group index")]
    NoImpliedValue,
    #[error("no capture group with index {0}")]
    NoSuchCaptureGroup(usize),
    #[error("no identifier with name {0}")]
    NoSuchIdentifier(String),
    #[error("unexpected list; perhaps a join operation `{{var*}}` is missing?")]
    UnexpectedList,
    #[error("pattern stems `{{%}}` cannot be interpolated in patterns")]
    PatternStemInterpolationInPattern,
    #[error("path resolution `<...>` interpolations cannot be used in patterns")]
    ResolvePathInPattern,
    #[error("join interpolations `{{...*}}` cannot be used in patterns")]
    JoinInPattern,
    #[error("unexpected list in pattern")]
    ListInPattern,
    #[error("the command '{0}' was not found in the current PATH environment variable")]
    CommandNotFound(String),
    #[error("invalid path interpolation within quotes; path arguments are automatically quoted")]
    PathWithinQuotes,
    #[error("empty command")]
    EmptyCommand,
    #[error("unterminated quote")]
    UnterminatedQuote,
    #[error("`glob` expressions are only allowed in variables")]
    UnexpectedGlob,
    #[error("`which` expressions are only allowed in variables")]
    UnexpectedWhich,
    /// Shell command failed during evaluation. Note: This error is not reported
    /// when executing commands as part of a rule, only when executing commands
    /// during evaluation (settings variables etc.)
    #[error(transparent)]
    Shell(Arc<ShellError>),
    #[error(transparent)]
    Path(#[from] werk_fs::PathError),
}

impl From<ShellError> for EvalError {
    fn from(err: ShellError) -> Self {
        EvalError::Shell(Arc::new(err))
    }
}

impl Scope<'static> {
    pub fn root() -> Self {
        let mut vars = IndexMap::new();

        if cfg!(windows) {
            vars.insert("EXE_SUFFIX".to_owned(), Value::String(".exe".to_owned()));
        } else {
            vars.insert("EXE_SUFFIX".to_owned(), Value::String("".to_owned()));
        }

        Self {
            parent: None,
            vars,
            pattern_match: None,
            implied_value: None,
        }
    }
}

impl<'a> Scope<'a> {
    pub fn child<'b>(&'b self, pattern_match: Option<&'a PatternMatch<'a>>) -> Scope<'b> {
        Scope {
            parent: Some(self),
            vars: IndexMap::new(),
            pattern_match,
            implied_value: None,
        }
    }

    pub fn pattern_stem(&self) -> Option<&str> {
        self.pattern_match
            .and_then(|m| m.stem())
            .or_else(|| self.parent.and_then(|p| p.pattern_stem()))
    }

    pub fn implied_value(&self) -> Option<&Value> {
        self.implied_value
            .as_ref()
            .or_else(|| self.parent.and_then(|p| p.implied_value()))
    }

    pub fn capture_group(&self, group: usize) -> Option<&str> {
        self.pattern_match
            .and_then(|m| m.capture_group(group))
            .or_else(|| self.parent.and_then(|p| p.capture_group(group)))
    }

    pub fn get<'b>(&'b self, name: &str) -> Option<&'b Value> {
        self.vars
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.get(name)))
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
    }

    pub async fn eval(&self, expr: &ast::Expr, project: &Project) -> Result<Value, Error> {
        match expr {
            ast::Expr::StringExpr(s) => self
                .eval_string_expr(s, project)
                .map(Value::String)
                .map_err(Into::into),
            ast::Expr::Shell(s) => self.eval_shell(s, project).await.map(Value::String),
            ast::Expr::Glob(g) => self.eval_glob(g, project).await.map(Value::List),
            ast::Expr::Which(which) => {
                let string = self.eval_string_expr(which, project)?;
                project.which(&string)
            }
            ast::Expr::List(list) => {
                Box::pin(async {
                    let mut exprs = Vec::with_capacity(list.len());
                    for expr in list {
                        exprs.push(self.eval(expr, project).await?);
                    }
                    Ok(Value::List(exprs))
                })
                .await
            }
        }
    }

    pub async fn eval_collect_strings(
        &self,
        expr: &ast::Expr,
        project: &Project,
    ) -> Result<Vec<String>, Error> {
        self.eval(expr, project).await.map(Value::collect_strings)
    }

    pub fn eval_pattern(&self, expr: &ast::PatternExpr) -> Result<Pattern, EvalError> {
        let mut pattern_builder = PatternBuilder::default();

        for fragment in &expr.fragments {
            match fragment {
                ast::PatternFragment::Literal(lit) => pattern_builder.push_str(lit),
                ast::PatternFragment::PatternStem => pattern_builder.push_pattern_stem(),
                ast::PatternFragment::OneOf(one_of) => pattern_builder.push_one_of(one_of.clone()),
                ast::PatternFragment::Interpolation(interp) => {
                    if let ast::StringInterpolationStem::PatternCapture = interp.stem {
                        return Err(EvalError::PatternStemInterpolationInPattern.into());
                    }
                    if interp.interpolate_as_resolved_path {
                        return Err(EvalError::ResolvePathInPattern.into());
                    }
                    if interp.join.is_some() {
                        return Err(EvalError::JoinInPattern.into());
                    }

                    let mut value = self.eval_string_interpolation_stem(&interp.stem)?;
                    if let Some(ref op) = interp.operation {
                        value = self.eval_string_interpolation_map_operation(value, op)?;
                    }

                    match value {
                        Value::String(string) => {
                            pattern_builder.push_str(&string);
                        }
                        Value::List(_) => {
                            return Err(EvalError::ListInPattern.into());
                        }
                    }
                }
            }
        }

        Ok(pattern_builder.build())
    }

    pub fn eval_string_expr(
        &self,
        expr: &ast::StringExpr,
        project: &Project,
    ) -> Result<String, Error> {
        let mut s = String::new();

        for fragment in &expr.fragments {
            match fragment {
                ast::StringFragment::Literal(lit) => s.push_str(lit),
                ast::StringFragment::Interpolation(interp) => {
                    let mut value = self.eval_string_interpolation_stem(&interp.stem)?;

                    if let Some(ref op) = interp.operation {
                        value = self.eval_string_interpolation_map_operation(value, op)?;
                    }

                    if interp.interpolate_as_resolved_path {
                        value = recursive_resolve_path(value, werk_fs::Path::ROOT, project)?;
                    }

                    if let Some(join) = interp.join {
                        value = Value::String(recursive_join(value, join));
                    }

                    match value {
                        Value::List(_) => return Err(EvalError::UnexpectedList.into()),
                        Value::String(value) => {
                            s.push_str(&value);
                        }
                    }
                }
            }
        }

        Ok(s)
    }

    pub fn eval_shell_command(
        &self,
        expr: &ast::StringExpr,
        project: &Project,
    ) -> Result<ShellCommandLineBuilder, Error> {
        let mut builder = ShellCommandLineBuilder::default();

        for fragment in &expr.fragments {
            match fragment {
                ast::StringFragment::Literal(lit) => {
                    builder.push_lit(lit);
                }
                ast::StringFragment::Interpolation(interp) => {
                    let mut value = self.eval_string_interpolation_stem(&interp.stem)?;

                    if let Some(ref op) = interp.operation {
                        value = self.eval_string_interpolation_map_operation(value, op)?;
                    }

                    if interp.interpolate_as_resolved_path {
                        value = recursive_resolve_path(value, werk_fs::Path::ROOT, project)?;
                        builder.push_all(value);
                    } else {
                        match value {
                            Value::List(list) => match interp.join {
                                Some(' ') => {
                                    builder.push_all(Value::List(list));
                                }
                                Some(sep) => {
                                    let s = recursive_join(Value::List(list), sep);
                                    builder.push_arg(&s);
                                }
                                None => return Err(EvalError::UnexpectedList.into()),
                            },
                            Value::String(s) => {
                                builder.push_str(&s);
                            }
                        }
                    }
                }
            }
        }

        Ok(builder)
    }

    fn eval_shell_commands_into(
        &self,
        expr: &ast::Expr,
        project: &Project,
        cmds: &mut Vec<ShellCommandLineBuilder>,
    ) -> Result<(), Error> {
        match expr {
            ast::Expr::StringExpr(string_expr) | ast::Expr::Shell(string_expr) => {
                cmds.push(self.eval_shell_command(string_expr, project)?)
            }
            ast::Expr::Glob(_) => return Err(EvalError::UnexpectedGlob.into()),
            ast::Expr::Which(_) => return Err(EvalError::UnexpectedWhich.into()),
            ast::Expr::List(vec) => {
                for expr in vec {
                    self.eval_shell_commands_into(expr, project, cmds)?;
                }
            }
        }

        Ok(())
    }

    pub fn eval_shell_commands(
        &self,
        expr: &ast::Expr,
        project: &Project,
    ) -> Result<Vec<ShellCommandLineBuilder>, Error> {
        let mut cmds = Vec::new();
        self.eval_shell_commands_into(expr, project, &mut cmds)?;
        Ok(cmds)
    }

    pub fn eval_string_interpolation_stem(
        &self,
        stem: &ast::StringInterpolationStem,
    ) -> Result<Value, EvalError> {
        Ok(match stem {
            ast::StringInterpolationStem::Implied => self
                .implied_value()
                .ok_or(EvalError::NoImpliedValue)?
                .clone(),
            ast::StringInterpolationStem::PatternCapture => Value::String(
                self.pattern_stem()
                    .ok_or(EvalError::NoPatternStem)?
                    .to_owned()
                    .into(),
            ),
            ast::StringInterpolationStem::CaptureGroup(group) => Value::String(
                self.capture_group(*group)
                    .ok_or(EvalError::NoSuchCaptureGroup(*group))?
                    .to_owned()
                    .into(),
            ),
            ast::StringInterpolationStem::Ident(ref ident) => self
                .get(ident)
                .ok_or_else(|| EvalError::NoSuchIdentifier(ident.clone()))?
                .clone(),
        })
    }

    pub fn eval_string_interpolation_map_operation(
        &self,
        value: Value,
        op: &ast::StringInterpolationOperation,
    ) -> Result<Value, EvalError> {
        match op {
            ast::StringInterpolationOperation::ReplaceExtension(from, to) => {
                recursive_replace_extension(value, from, to)
            }
            ast::StringInterpolationOperation::PrependEach(prefix) => {
                recursive_prepend_each(value, prefix)
            }
            ast::StringInterpolationOperation::AppendEach(suffix) => {
                recursive_append_each(value, suffix)
            }
        }
    }

    pub async fn eval_shell(
        &self,
        expr: &ast::StringExpr,
        project: &Project,
    ) -> Result<String, Error> {
        let mut builder = self.eval_shell_command(expr, project)?;

        // Unconditionally disable color output when the command supports it,
        // because we are capturing the output as a string.
        let command = builder.set_no_color().build(&project.inner.which)?;

        let output = project.run(&command).await?;
        if !output.status.success() {
            return Err(ShellError { command, output }.into());
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(stdout.into_owned())
    }

    pub async fn eval_glob(
        &self,
        expr: &ast::Glob,
        project: &Project,
    ) -> Result<Vec<Value>, Error> {
        let mut glob_pattern_string = self.eval_string_expr(&expr.pattern, project)?;
        if !glob_pattern_string.starts_with('/') {
            glob_pattern_string.insert(0, '/');
        }
        let result = project.glob(&glob_pattern_string)?;
        if let Some(then) = expr.then.as_ref() {
            let mut scope = self.child(None);
            scope.implied_value = Some(Value::List(result));
            Ok(match Box::pin(scope.eval(then, project)).await? {
                Value::List(list) => list,
                value => vec![value],
            })
        } else {
            Ok(result)
        }
    }
}

fn flat_join(values: &[Value], sep: &str) -> String {
    fn flat_join(values: &[Value], string: &mut String, sep: &str, mut first: bool) {
        for value in values {
            match value {
                Value::String(s) => {
                    if !first {
                        string.push_str(sep);
                    }
                    string.push_str(s);
                }
                Value::List(values) => flat_join(values, string, sep, first),
            }

            first = false;
        }
    }

    let mut s = String::new();
    flat_join(values, &mut s, sep, true);
    s
}

fn recursive_join(value: Value, sep: char) -> String {
    match value {
        Value::String(s) => s,
        Value::List(values) => flat_join(&values, &sep.to_string()),
    }
}

fn recursive_resolve_path(
    mut value: Value,
    working_dir: &werk_fs::Path,
    project: &Project,
) -> Result<Value, Error> {
    value.try_recursive_modify(|string| {
        let path = werk_fs::Path::new(&string)?;
        let path = path.absolutize(working_dir)?;
        let path = project.resolve_path(&path)?;
        match path.to_str() {
            Some(path) => *string = path.to_owned(),
            None => panic!("Path resolution produced a non-UTF8 path; probably the project root path is non-UTF8"),
        }
        Ok::<_, Error>(())
    })?;

    Ok(value)
}

fn recursive_replace_extension(mut value: Value, from: &str, to: &str) -> Result<Value, EvalError> {
    value.recursive_modify(|s| {
        if s.ends_with(from) {
            s.truncate(s.len() - from.len());
            s.push_str(to);
        }
    });
    Ok(value)
}

fn recursive_prepend_each(mut value: Value, prefix: &str) -> Result<Value, EvalError> {
    value.recursive_modify(|s| {
        s.insert_str(0, prefix);
    });
    Ok(value)
}

fn recursive_append_each(mut value: Value, suffix: &str) -> Result<Value, EvalError> {
    value.recursive_modify(|s| {
        s.push_str(suffix);
    });
    Ok(value)
}
