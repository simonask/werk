use std::sync::Arc;

use werk_parser::ast;

use crate::{
    BuildStatus, Error, EvalError, Io, Pattern, PatternBuilder, RecipeScope, Scope,
    ShellCommandLine, ShellCommandLineBuilder, ShellError, SubexprScope, Value, Workspace,
};

/// Evaluated value, which keeps track of "outdatedness" with respect to cached
/// build variables. Build recipes may use the outdatedness information to
/// determine if some expression changed in a way that should cause a rebuild to
/// occur, like the result of a glob pattern, or the result of a `which`
/// expression (which is outdated if executable program path changed between
/// runs).
#[derive(Clone, Debug, Copy)]
pub struct Eval<T = Value> {
    pub value: T,
    /// When the evaluated value can impact outdatedness (like a glob
    /// expression), this is the detected build status. For expressions that
    /// don't have an outdatedness, this is always `BuildStatus::Unchanged`.
    pub status: BuildStatus,
}

impl<T> Eval<T> {
    pub fn unchanged(value: T) -> Self {
        Self {
            value,
            status: BuildStatus::Unchanged,
        }
    }

    pub fn rebuilt(value: T) -> Self {
        Self {
            value,
            status: BuildStatus::Rebuilt,
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Eval<U> {
        Eval {
            value: f(self.value),
            status: self.status,
        }
    }

    pub fn as_ref(&self) -> Eval<&T> {
        Eval {
            value: &self.value,
            status: self.status,
        }
    }

    pub fn as_deref(&self) -> &T::Target
    where
        T: std::ops::Deref,
    {
        self.value.deref()
    }
}

impl<T: Default> Default for Eval<T> {
    fn default() -> Self {
        Self {
            value: Default::default(),
            status: BuildStatus::Unchanged,
        }
    }
}

impl<T> Eval<&T> {
    pub fn cloned(&self) -> Eval<T>
    where
        T: Clone,
    {
        Eval {
            value: self.value.clone(),
            status: self.status,
        }
    }
}

impl<T> Eval<Option<T>> {
    pub fn transpose(self) -> Option<Eval<T>> {
        self.value.map(|value| Eval {
            value,
            status: self.status,
        })
    }
}

impl<T, E> Eval<Result<T, E>> {
    pub fn transpose_result(self) -> Result<Eval<T>, E> {
        self.value.map(|value| Eval {
            value,
            status: self.status,
        })
    }
}

impl<T> std::ops::Deref for Eval<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Eval<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub async fn eval(
    scope: &dyn Scope,
    io: &dyn Io,
    expr: &ast::Expr,
) -> Result<Eval<Value>, EvalError> {
    match expr {
        ast::Expr::StringExpr(expr) => Ok(eval_string_expr(scope, expr)?.map(Value::String)),
        ast::Expr::Shell(expr) => Ok(eval_shell(scope, io, expr).await?.map(Value::String)),
        ast::Expr::Glob(expr) => Ok(eval_glob(scope, expr)?.map(Value::List)),
        ast::Expr::Which(expr) => {
            let string = eval_string_expr(scope, expr)?;
            let (which, which_status) = scope
                .workspace()
                .which(io, &string.value)
                .map_err(|e| EvalError::CommandNotFound(string.value.clone(), e))?;
            let status = string.status | which_status;
            Ok(Eval {
                value: Value::String(which),
                status,
            })
        }
        ast::Expr::Env(expr) => {
            let name = eval_string_expr(scope, expr)?;
            let (env, env_status) = scope.workspace().env(io, &name.value);
            let status = name.status | env_status;
            Ok(Eval {
                value: Value::String(env),
                status,
            })
        }
        ast::Expr::Patsubst(_) => todo!(),
        ast::Expr::Match(_) => todo!(),
        ast::Expr::List(list_expr) => {
            // Boxing for recursion.
            Box::pin(async {
                let mut items = Vec::with_capacity(list_expr.len());
                let mut status = BuildStatus::Unchanged;
                for expr in list_expr {
                    let eval_item = eval(scope, io, expr).await?;
                    status |= eval_item.status;
                    items.push(eval_item.value);
                }
                Ok(Eval {
                    value: Value::List(items),
                    status,
                })
            })
            .await
        }
        ast::Expr::Ident(ident) => scope
            .get(ident)
            .as_ref()
            .map(Eval::cloned)
            .ok_or_else(|| EvalError::NoSuchIdentifier(ident.clone())),
        ast::Expr::Then(expr, string_expr) => {
            // Boxing for recursion.
            let value = Box::pin(async { eval(scope, io, expr).await }).await?;
            let scope = scope.subexpr(Some(value));
            eval_string_expr(&scope, string_expr).map(|string| string.map(Value::String))
        }
        ast::Expr::Message(message_expr) => {
            let value = Box::pin(async { eval(scope, io, &message_expr.inner).await }).await?;
            let message_scope = scope.subexpr(Some(value));
            let message = eval_string_expr(&message_scope, &message_expr.message)?;
            match message_expr.message_type {
                ast::MessageType::Info => scope.watcher().message(scope.task_id(), &message),
                ast::MessageType::Warning => scope.watcher().warning(scope.task_id(), &message),
            }
            Ok(message_scope.implied_value.unwrap())
        }
        ast::Expr::Error(string_expr) => {
            let string = eval_string_expr(scope, string_expr)?;
            Err(EvalError::ErrorExpression(string.value))
        }
    }
}

pub async fn eval_collect_strings<P: Scope>(
    scope: &P,
    io: &dyn Io,
    expr: &ast::Expr,
) -> Result<Eval<Vec<String>>, EvalError> {
    let eval = eval(scope, io, expr).await?;
    Ok(eval.map(|value| value.collect_strings()))
}

pub fn eval_pattern<P: Scope>(scope: &P, expr: &ast::PatternExpr) -> Result<Pattern, EvalError> {
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

                let mut value = eval_string_interpolation_stem(scope, &interp.stem)?;
                if let Some(ref op) = interp.operation {
                    value = eval_string_interpolation_map_operation(value, op)?;
                }

                // Note: Ignoring the build-status of the interpolation
                // stem, because we are building a pattern - it can't itself
                // be outdated.
                match value.value {
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

pub fn eval_string_expr<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr,
) -> Result<Eval<String>, EvalError> {
    let mut s = String::new();

    let mut status = BuildStatus::Unchanged;

    for fragment in &expr.fragments {
        match fragment {
            ast::StringFragment::Literal(lit) => s.push_str(lit),
            ast::StringFragment::Interpolation(interp) => {
                let mut value = eval_string_interpolation_stem(scope, &interp.stem)?;

                if let Some(ref op) = interp.operation {
                    value = eval_string_interpolation_map_operation(value, op)?;
                }

                if interp.interpolate_as_resolved_path {
                    value.value = recursive_resolve_path(
                        value.value,
                        werk_fs::Path::ROOT,
                        scope.workspace(),
                    )?;
                }

                if let Some(join) = interp.join {
                    value.value = Value::String(recursive_join(value.value, join));
                }

                match value.value {
                    Value::List(_) => return Err(EvalError::UnexpectedList.into()),
                    Value::String(value) => {
                        s.push_str(&value);
                    }
                }

                status |= value.status;
            }
        }
    }

    Ok(Eval { value: s, status })
}

pub fn eval_shell_command<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr,
) -> Result<Eval<ShellCommandLine>, EvalError> {
    let mut builder = ShellCommandLineBuilder::default();

    let mut status = BuildStatus::Unchanged;

    for fragment in &expr.fragments {
        match fragment {
            ast::StringFragment::Literal(lit) => {
                builder.push_lit(lit);
            }
            ast::StringFragment::Interpolation(interp) => {
                let mut value = eval_string_interpolation_stem(scope, &interp.stem)?;

                if let Some(ref op) = interp.operation {
                    value = eval_string_interpolation_map_operation(value, op)?;
                }
                status |= value.status;

                if interp.interpolate_as_resolved_path {
                    value.value = recursive_resolve_path(
                        value.value,
                        werk_fs::Path::ROOT,
                        scope.workspace(),
                    )?;
                    builder.push_all(value.value);
                } else {
                    match value.value {
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

    Ok(Eval {
        value: builder.build()?,
        status,
    })
}

fn eval_shell_commands_into(
    scope: &RecipeScope<'_>,
    expr: &ast::Expr,
    cmds: &mut Vec<ShellCommandLine>,
) -> Result<BuildStatus, Error> {
    match expr {
        ast::Expr::StringExpr(string_expr) | ast::Expr::Shell(string_expr) => {
            let command = eval_shell_command(scope, string_expr)?;
            cmds.push(command.value);
            Ok(command.status)
        }
        ast::Expr::Glob(_) => return Err(EvalError::UnexpectedExpressionType("glob").into()),
        ast::Expr::Which(_) => return Err(EvalError::UnexpectedExpressionType("which").into()),
        ast::Expr::Patsubst(_) => {
            return Err(EvalError::UnexpectedExpressionType("patsubst").into())
        }
        ast::Expr::Match(_) => return Err(EvalError::UnexpectedExpressionType("match").into()),
        ast::Expr::Env(_) => return Err(EvalError::UnexpectedExpressionType("env").into()),
        ast::Expr::List(vec) => {
            let mut status = BuildStatus::Unchanged;
            for expr in vec {
                status |= eval_shell_commands_into(scope, expr, cmds)?;
            }
            Ok(status)
        }
        ast::Expr::Ident(_) => return Err(EvalError::UnexpectedExpressionType("from").into()),
        ast::Expr::Then(_, _) => return Err(EvalError::UnexpectedExpressionType("then").into()),
        ast::Expr::Message(message_expr) => {
            let status = eval_shell_commands_into(scope, &message_expr.inner, cmds)?;
            let message_scope = (scope as &dyn Scope).subexpr(None);
            let message = eval_string_expr(&message_scope, &message_expr.message)?;
            match message_expr.message_type {
                ast::MessageType::Warning => scope.watcher().warning(scope.task_id(), &message),
                ast::MessageType::Info => scope.watcher().message(scope.task_id(), &message),
            }
            Ok(status)
        }
        ast::Expr::Error(string_expr) => {
            let message = eval_string_expr(scope, string_expr)?;
            return Err(EvalError::ErrorExpression(message.value).into());
        }
    }
}

pub fn eval_shell_commands(
    scope: &RecipeScope<'_>,
    expr: &ast::Expr,
) -> Result<Vec<ShellCommandLine>, Error> {
    let mut cmds = Vec::new();
    eval_shell_commands_into(scope, expr, &mut cmds)?;
    Ok(cmds)
}

/// Evaluate shell commands, run `which` on all of them (changing the `program`
/// member of the returned `ShellCommandLine`), and detect if the resolved
/// command is different from the cached path in .werk-cache.toml.
pub fn eval_shell_commands_run_which_and_detect_outdated(
    scope: &RecipeScope<'_>,
    io: &dyn Io,
    expr: &ast::Expr,
) -> Result<Eval<Vec<ShellCommandLine>>, Error> {
    let mut cmds = Vec::new();
    eval_shell_commands_into(scope, expr, &mut cmds)?;

    let mut status = BuildStatus::Unchanged;
    for cmd in cmds.iter_mut() {
        let (program, which_status) = scope
            .workspace()
            .which(io, &cmd.program)
            .map_err(|e| Error::CommandNotFound(cmd.program.clone(), e))?;
        cmd.program = program;
        status |= which_status;
    }

    Ok(Eval {
        value: cmds,
        status,
    })
}

pub fn eval_string_interpolation_stem<P: Scope + ?Sized>(
    scope: &P,
    stem: &ast::StringInterpolationStem,
) -> Result<Eval<Value>, EvalError> {
    Ok(match stem {
        ast::StringInterpolationStem::Implied => scope
            .implied_value()
            .ok_or(EvalError::NoImpliedValue)?
            .clone(),
        ast::StringInterpolationStem::PatternCapture => Eval {
            value: Value::String(
                scope
                    .pattern_stem()
                    .ok_or(EvalError::NoPatternStem)?
                    .to_owned()
                    .into(),
            ),
            status: BuildStatus::Unchanged,
        },
        ast::StringInterpolationStem::CaptureGroup(group) => Eval::unchanged(
            scope
                .capture_group(*group)
                .ok_or(EvalError::NoSuchCaptureGroup(*group))?
                .to_owned()
                .into(),
        ),
        ast::StringInterpolationStem::Ident(ref ident) => scope
            .get(ident)
            .ok_or_else(|| EvalError::NoSuchIdentifier(ident.clone()))?
            .cloned(),
    })
}

pub fn eval_string_interpolation_map_operation(
    value: Eval<Value>,
    op: &ast::StringInterpolationOperation,
) -> Result<Eval<Value>, EvalError> {
    value
        .map(|value| match op {
            ast::StringInterpolationOperation::ReplaceExtension(from, to) => {
                recursive_replace_extension(value, from, to)
            }
            ast::StringInterpolationOperation::PrependEach(prefix) => {
                recursive_prepend_each(value, prefix)
            }
            ast::StringInterpolationOperation::AppendEach(suffix) => {
                recursive_append_each(value, suffix)
            }
        })
        .transpose_result()
}

pub async fn eval_shell<P: Scope + ?Sized>(
    scope: &P,
    io: &dyn Io,
    expr: &ast::StringExpr,
) -> Result<Eval<String>, EvalError> {
    let mut command = eval_shell_command(scope, expr)?;

    // Unconditionally disable color output when the command supports it,
    // because we are capturing the output as a string.
    command.set_no_color();

    // Resolve the program path up front, in order to detect if it changed.
    let (which_program, which_status) = scope
        .workspace()
        .which(io, &command.program)
        .map_err(|e| EvalError::CommandNotFound(command.program.clone(), e))?;
    command.program = which_program;

    let output = match io
        .run_during_eval(&command, scope.workspace().project_root())
        .await
    {
        Ok(output) => output,
        Err(e) => {
            // Spawning the command failed.
            return Err(ShellError {
                command: command.value,
                result: Arc::new(Err(e)),
            }
            .into());
        }
    };

    if !output.status.success() {
        // The command itself failed.
        return Err(ShellError {
            command: command.value,
            result: Arc::new(Ok(output)),
        }
        .into());
    }

    let stdout = String::from_utf8_lossy(&output.stdout.trim_ascii());
    Ok(Eval {
        value: stdout.into_owned(),
        status: command.status | which_status,
    })
}

pub fn eval_glob(scope: &dyn Scope, expr: &ast::StringExpr) -> Result<Eval<Vec<Value>>, EvalError> {
    let Eval {
        value: mut glob_pattern_string,
        status: string_status,
    } = eval_string_expr(scope, expr)?;

    if !glob_pattern_string.starts_with('/') {
        glob_pattern_string.insert(0, '/');
    }
    let (matches, glob_status) = scope
        .workspace()
        .glob_workspace_files(&glob_pattern_string)?;
    let matches = matches
        .into_iter()
        .map(|p| Value::String(p.into()))
        .collect();

    Ok(Eval {
        value: matches,
        status: glob_status | string_status,
    })
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
        Value::String(string) => string,
        Value::List(values) => flat_join(&values, &sep.to_string()),
    }
}

fn recursive_resolve_path(
    mut value: Value,
    working_dir: &werk_fs::Path,
    workspace: &Workspace,
) -> Result<Value, EvalError> {
    value.try_recursive_modify(|string| {
        let path = werk_fs::Path::new(&string)?;
        let path = path.absolutize(working_dir)?;
        let path = workspace.resolve_path(&path)?;
        match path.to_str() {
            Some(path) => *string = path.to_owned(),
            None => panic!("Path resolution produced a non-UTF8 path; probably the project root path is non-UTF8"),
        }
        Ok::<_, EvalError>(())
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
