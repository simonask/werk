mod used;
pub use used::*;

use std::{borrow::Cow, sync::Arc};

use werk_parser::ast;

use crate::{
    Error, EvalError, Io, Pattern, PatternBuilder, PatternMatch, RecipeScope, RunCommand, Scope,
    ShellCommandLine, ShellCommandLineBuilder, ShellError, Value, Workspace,
};

/// Evaluated value, which keeps track of "outdatedness" with respect to cached
/// build variables. Build recipes may use the outdatedness information to
/// determine if some expression changed in a way that should cause a rebuild to
/// occur, like the result of a glob pattern, or the result of a `which`
/// expression (which is outdated if executable program path changed between
/// runs).
#[derive(Clone, Debug)]
pub struct Eval<T = Value> {
    pub value: T,
    /// When the evaluated value can impact outdatedness (like a glob
    /// expression), this is the variables used during evaluation.
    pub used: Used,
}

impl<T> Eval<T> {
    /// An inherent value that does not depend on any external variables.
    pub fn inherent(value: T) -> Self {
        Self {
            value,
            used: Used::none(),
        }
    }

    pub fn using_var(value: T, used: UsedVariable) -> Self {
        Self {
            value,
            used: Used::from_iter(Some(used)),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Eval<U> {
        Eval {
            value: f(self.value),
            used: self.used,
        }
    }

    pub fn as_ref(&self) -> &T {
        &self.value
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
            used: Used::none(),
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
            used: self.used.clone(),
        }
    }
}

impl<T> Eval<Option<T>> {
    pub fn transpose(self) -> Option<Eval<T>> {
        self.value.map(|value| Eval {
            value,
            used: self.used,
        })
    }
}

impl<T, E> Eval<Result<T, E>> {
    pub fn transpose_result(self) -> Result<Eval<T>, E> {
        self.value.map(|value| Eval {
            value,
            used: self.used,
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
            let Eval {
                value: string,
                mut used,
            } = eval_string_expr(scope, expr)?;

            let (which, hash) = scope
                .workspace()
                .which(io, &string)
                .map_err(|e| EvalError::CommandNotFound(string.clone(), e))?;

            let which =
                String::from_utf8(which.into_os_string().into_encoded_bytes()).map_err(|err| {
                    EvalError::NonUtf8Which(std::path::PathBuf::from(unsafe {
                        // SAFETY: These are the bytes we just got from `into_os_string()`.
                        std::ffi::OsString::from_encoded_bytes_unchecked(err.into_bytes())
                    }))
                })?;

            used.insert(UsedVariable::Which(string, hash));
            Ok(Eval {
                value: Value::String(which),
                used,
            })
        }
        ast::Expr::Env(expr) => {
            let Eval {
                value: name,
                mut used,
            } = eval_string_expr(scope, expr)?;
            let (env, hash) = scope.workspace().env(io, &name);
            used.insert(UsedVariable::Env(name, hash));
            Ok(Eval {
                value: Value::String(env),
                used,
            })
        }
        ast::Expr::Patsubst(patsubst) => {
            // Boxing for recursion.
            Box::pin(eval_patsubst(scope, io, patsubst)).await
        }
        ast::Expr::Match(match_expr) => {
            // Boxing for recursion.
            Box::pin(eval_match_expr(scope, io, match_expr)).await
        }
        ast::Expr::List(list_expr) => {
            // Boxing for recursion.
            Box::pin(async {
                let mut items = Vec::with_capacity(list_expr.len());
                let mut used = Used::none();
                for expr in list_expr {
                    let eval_item = eval(scope, io, expr).await?;
                    used |= eval_item.used;
                    items.push(eval_item.value);
                }
                Ok(Eval {
                    value: Value::List(items),
                    used,
                })
            })
            .await
        }
        ast::Expr::Join(list, sep) => {
            // Boxing for recursion.
            let list = Box::pin(eval(scope, io, list)).await?;
            let sep = eval_string_expr(scope, sep)?;
            let joined = flat_join(&[list.value], &sep.value);
            Ok(Eval {
                value: Value::String(joined),
                used: list.used | sep.used,
            })
        }
        ast::Expr::Ident(ident) => scope
            .get(ident)
            .as_ref()
            .map(Eval::cloned)
            .ok_or_else(|| EvalError::NoSuchIdentifier(ident.clone())),
        ast::Expr::Then(expr, string_expr) => {
            // Boxing for recursion.
            let mut value = Box::pin(async { eval(scope, io, expr).await }).await?;

            // Map the strings through the expression recursively.
            value.value.try_recursive_map_strings(|s| {
                let scope = scope.subexpr(Some(Eval::inherent(Value::String(s))));
                eval_string_expr(&scope, string_expr).map(|string| string.value)
            })?;
            Ok(value)
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

pub async fn eval_patsubst(
    scope: &dyn Scope,
    io: &dyn Io,
    expr: &ast::PatsubstExpr,
) -> Result<Eval<Value>, EvalError> {
    let mut value = eval(scope, io, &expr.input).await?;
    // TODO: Cache the pattern instead of compiling a regex on every evaluation.
    let pattern = eval_pattern(scope, &expr.pattern)?;
    value.value.try_recursive_map_strings(|s| {
        let Some(match_data) = pattern.match_string(&s) else {
            // Pattern does not match, ignore the string.
            return Ok::<_, EvalError>(s);
        };

        let pattern_match = PatternMatch::from_pattern_and_data(&pattern, match_data);
        let scope = scope.patsubst(&pattern_match);
        let new_value = eval_string_expr(&scope, &expr.replacement)?;
        Ok(new_value.value)
    })?;
    Ok(value)
}

pub fn will_evaluate_to_string<P: Scope + ?Sized>(scope: &P, expr: &ast::Expr) -> bool {
    match expr {
        ast::Expr::Ident(ident) => match scope.get(ident) {
            Some(value) => matches!(value.value, Value::String(_)),
            None => true,
        },
        ast::Expr::StringExpr(_)
        | ast::Expr::Shell(_)
        | ast::Expr::Which(_)
        | ast::Expr::Env(_)
        | ast::Expr::Then(_, _) => true,
        ast::Expr::List(_) | ast::Expr::Glob(_) => false,
        ast::Expr::Join(..) => false,
        ast::Expr::Patsubst(patsubst_expr) => will_evaluate_to_string(scope, &patsubst_expr.input),
        ast::Expr::Match(match_expr) => match_expr
            .patterns
            .iter()
            .all(|(_, replacement)| will_evaluate_to_string(scope, replacement)),
        ast::Expr::Message(message_expr) => will_evaluate_to_string(scope, &message_expr.inner),
        ast::Expr::Error(_) => true,
    }
}

pub async fn eval_match_expr(
    scope: &dyn Scope,
    io: &dyn Io,
    expr: &ast::MatchExpr,
) -> Result<Eval<Value>, EvalError> {
    let mut value = eval(scope, io, &expr.input).await?;

    let patterns = expr
        .patterns
        .iter()
        .map(|(pattern, replacement)| {
            let pattern = eval_pattern(scope, pattern)?;
            Ok((pattern, replacement))
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    // Apply the match recursively to the input.
    value
        .value
        .try_recursive_map_strings_async(|input_string| async {
            for (pattern, replacement_expr) in &patterns {
                tracing::trace!("trying match '{:?}' against '{}'", pattern, input_string);
                let Some(pattern_match) = pattern.match_string(&input_string) else {
                    continue;
                };

                let pattern_match = PatternMatch::from_pattern_and_data(&pattern, pattern_match);
                let scope = scope.patsubst(&pattern_match);
                let new_value = eval(&scope, io, replacement_expr).await?;
                return Ok(new_value.value);
            }

            // Unmodified.
            Ok::<_, EvalError>(Value::String(input_string))
        })
        .await?;

    Ok(value)
}

pub async fn eval_collect_strings<P: Scope>(
    scope: &P,
    io: &dyn Io,
    expr: &ast::Expr,
) -> Result<Eval<Vec<String>>, EvalError> {
    let eval = eval(scope, io, expr).await?;
    Ok(eval.map(|value| value.collect_strings()))
}

pub fn eval_pattern_builder<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::PatternExpr,
) -> Result<PatternBuilder, EvalError> {
    let mut pattern_builder = PatternBuilder::default();

    for fragment in &expr.fragments {
        match fragment {
            ast::PatternFragment::Literal(lit) => pattern_builder.push_str(lit),
            ast::PatternFragment::PatternStem => pattern_builder.push_pattern_stem(),
            ast::PatternFragment::OneOf(one_of) => pattern_builder.push_one_of(one_of.clone()),
            ast::PatternFragment::Interpolation(interp) => {
                if let ast::InterpolationStem::PatternCapture = interp.stem {
                    return Err(EvalError::PatternStemInterpolationInPattern.into());
                }
                if interp.options.join.is_some() {
                    return Err(EvalError::JoinInPattern.into());
                }

                let mut value = eval_string_interpolation_stem(scope, &interp.stem)?;
                eval_string_interpolation_ops(
                    &mut value,
                    &interp.options.ops,
                    false,
                    scope.workspace(),
                )?;

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

    Ok(pattern_builder)
}

pub fn eval_pattern<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::PatternExpr,
) -> Result<Pattern, EvalError> {
    eval_pattern_builder(scope, expr).map(PatternBuilder::build)
}

pub fn eval_string_expr<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr,
) -> Result<Eval<String>, EvalError> {
    let mut s = String::new();

    let mut used = Used::none();

    for fragment in &expr.fragments {
        match fragment {
            ast::StringFragment::Literal(lit) => s.push_str(lit),
            ast::StringFragment::Interpolation(interp) => {
                let mut value = eval_string_interpolation_stem(scope, &interp.stem)?;
                eval_string_interpolation_ops(
                    &mut value,
                    &interp.options.ops,
                    true,
                    scope.workspace(),
                )?;

                if let Some(ref join) = interp.options.join {
                    value.value = Value::String(recursive_join(value.value, join));
                }

                match value.value {
                    Value::List(_) => return Err(EvalError::UnexpectedList.into()),
                    Value::String(value) => {
                        s.push_str(&value);
                    }
                }

                used |= value.used;
            }
        }
    }

    Ok(Eval { value: s, used })
}

pub async fn eval_run_expr(
    scope: &RecipeScope<'_>,
    io: &dyn Io,
    expr: &ast::RunExpr,
    force_color: bool,
) -> Result<Eval<RunCommand>, EvalError> {
    match expr {
        ast::RunExpr::Shell(expr) => {
            let shell = eval_shell_command(scope, expr, force_color)?;
            Ok(shell.map(RunCommand::Shell))
        }
        ast::RunExpr::Write(destination, data) => {
            let destination = eval_string_expr(scope, destination)?;
            let data = eval(scope, io, data).await?;
            let used = destination.used | data.used;
            let Value::String(data) = data.value else {
                return Err(EvalError::UnexpectedList);
            };

            Ok(Eval {
                value: RunCommand::Write(destination.value.into(), data.into()),
                used,
            })
        }
        ast::RunExpr::Copy(from, to) => {
            let from = eval_string_expr(scope, from)?;
            let to = eval_string_expr(scope, to)?;
            let used = from.used | to.used;
            Ok(Eval {
                value: RunCommand::Copy(from.value.into(), to.value.into()),
                used,
            })
        }
        ast::RunExpr::Echo(message) => {
            let message = eval_string_expr(scope, message)?;
            Ok(Eval {
                value: RunCommand::Echo(message.value.into()),
                // Echo commands never contribute to outdatedness.
                used: Used::none(),
            })
        }
    }
}

pub fn eval_shell_command<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr,
    force_color: bool,
) -> Result<Eval<ShellCommandLine>, EvalError> {
    let mut builder = ShellCommandLineBuilder::default();

    let mut used = Used::none();

    for fragment in &expr.fragments {
        match fragment {
            ast::StringFragment::Literal(lit) => {
                builder.push_lit(lit);
            }
            ast::StringFragment::Interpolation(interp) => {
                let mut value = eval_string_interpolation_stem(scope, &interp.stem)?;
                eval_string_interpolation_ops(
                    &mut value,
                    &interp.options.ops,
                    true,
                    scope.workspace(),
                )?;
                used |= value.used;

                match value.value {
                    Value::List(list) => match interp.options.join.as_deref() {
                        // When the join char is a space, we treat the list as
                        // separate arguments to the command.
                        Some(" ") => {
                            builder.push_all(Value::List(list));
                        }
                        // Otherwise, we join the list into a single argument.
                        Some(sep) => {
                            let s = recursive_join(Value::List(list), sep);
                            builder.push_arg(&s);
                        }
                        // When no join operator is present take the first element of the list.
                        None => {
                            let Some(s) = find_first_string(&list) else {
                                return Err(EvalError::EmptyList);
                            };
                            builder.push_arg(s);
                        }
                    },
                    Value::String(s) => {
                        builder.push_str(&s);
                    }
                }
            }
        }
    }

    let mut command_line = builder.build()?;
    if force_color {
        command_line.set_force_color();
    }

    Ok(Eval {
        value: command_line,
        used,
    })
}

fn eval_shell_commands_into(
    scope: &RecipeScope<'_>,
    expr: &ast::Expr,
    cmds: &mut Vec<ShellCommandLine>,
    force_color: bool,
) -> Result<Used, Error> {
    match expr {
        ast::Expr::StringExpr(string_expr) | ast::Expr::Shell(string_expr) => {
            let command = eval_shell_command(scope, string_expr, force_color)?;
            cmds.push(command.value);
            Ok(command.used)
        }
        ast::Expr::Glob(_) => return Err(EvalError::UnexpectedExpressionType("glob").into()),
        ast::Expr::Which(_) => return Err(EvalError::UnexpectedExpressionType("which").into()),
        ast::Expr::Patsubst(_) => {
            return Err(EvalError::UnexpectedExpressionType("patsubst").into())
        }
        ast::Expr::Match(_) => return Err(EvalError::UnexpectedExpressionType("match").into()),
        ast::Expr::Env(_) => return Err(EvalError::UnexpectedExpressionType("env").into()),
        ast::Expr::List(vec) => {
            let mut used = Used::none();
            for expr in vec {
                used |= eval_shell_commands_into(scope, expr, cmds, force_color)?;
            }
            Ok(used)
        }
        ast::Expr::Join(..) => Err(EvalError::UnexpectedExpressionType("join").into()),
        ast::Expr::Ident(_) => return Err(EvalError::UnexpectedExpressionType("from").into()),
        ast::Expr::Then(_, _) => return Err(EvalError::UnexpectedExpressionType("then").into()),
        ast::Expr::Message(message_expr) => {
            let used = eval_shell_commands_into(scope, &message_expr.inner, cmds, force_color)?;
            let message_scope = (scope as &dyn Scope).subexpr(None);
            let message = eval_string_expr(&message_scope, &message_expr.message)?;
            match message_expr.message_type {
                ast::MessageType::Warning => scope.watcher().warning(scope.task_id(), &message),
                ast::MessageType::Info => scope.watcher().message(scope.task_id(), &message),
            }
            Ok(used)
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
    force_color: bool,
) -> Result<Vec<ShellCommandLine>, Error> {
    let mut cmds = Vec::new();
    eval_shell_commands_into(scope, expr, &mut cmds, force_color)?;
    Ok(cmds)
}

/// Evaluate shell commands, run `which` on all of them (changing the `program`
/// member of the returned `ShellCommandLine`), and detect if the resolved
/// command is different from the cached path in .werk-cache.toml.
pub fn eval_shell_commands_run_which_and_detect_outdated(
    scope: &RecipeScope<'_>,
    expr: &ast::Expr,
    force_color: bool,
) -> Result<Eval<Vec<ShellCommandLine>>, Error> {
    let mut value = Vec::new();
    let used = eval_shell_commands_into(scope, expr, &mut value, force_color)?;
    Ok(Eval { value, used })
}

pub fn eval_string_interpolation_stem<P: Scope + ?Sized>(
    scope: &P,
    stem: &ast::InterpolationStem,
) -> Result<Eval<Value>, EvalError> {
    Ok(match stem {
        ast::InterpolationStem::Implied => scope
            .implied_value()
            .ok_or(EvalError::NoImpliedValue)?
            .clone(),
        ast::InterpolationStem::PatternCapture => Eval {
            value: Value::String(
                scope
                    .pattern_stem()
                    .ok_or(EvalError::NoPatternStem)?
                    .to_owned()
                    .into(),
            ),
            used: Used::none(),
        },
        ast::InterpolationStem::CaptureGroup(group) => Eval::inherent(
            scope
                .capture_group(*group)
                .ok_or(EvalError::NoSuchCaptureGroup(*group))?
                .to_owned()
                .into(),
        ),
        ast::InterpolationStem::Ident(ref ident) => scope
            .get(ident)
            .ok_or_else(|| EvalError::NoSuchIdentifier(ident.clone()))?
            .cloned(),
    })
}

pub fn eval_string_interpolation_ops(
    value: &mut Value,
    ops: &[ast::InterpolationOp],
    allow_os_paths: bool,
    workspace: &Workspace,
) -> Result<(), EvalError> {
    for op in ops {
        match op {
            ast::InterpolationOp::ReplaceExtension(from, to) => {
                recursive_replace_extension(value, from, to)
            }
            ast::InterpolationOp::PrependEach(prefix) => recursive_prepend_each(value, prefix),
            ast::InterpolationOp::AppendEach(suffix) => recursive_append_each(value, suffix),
            ast::InterpolationOp::RegexReplace(r) => {
                recursive_regex_replace(value, &r.regex, &r.replacer)
            }
            ast::InterpolationOp::ResolveOsPath => {
                if allow_os_paths {
                    recursive_resolve_path(value, werk_fs::Path::ROOT, workspace)?
                } else {
                    return Err(EvalError::JoinInPattern);
                }
            }
        }
    }

    Ok(())
}

pub async fn eval_shell<P: Scope + ?Sized>(
    scope: &P,
    io: &dyn Io,
    expr: &ast::StringExpr,
) -> Result<Eval<String>, EvalError> {
    let mut command = eval_shell_command(scope, expr, false)?;

    // Unconditionally disable color output when the command supports it,
    // because we are capturing the output as a string.
    command.set_no_color();

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
        used: command.used,
    })
}

pub fn eval_glob(scope: &dyn Scope, expr: &ast::StringExpr) -> Result<Eval<Vec<Value>>, EvalError> {
    let Eval {
        value: mut glob_pattern_string,
        mut used,
    } = eval_string_expr(scope, expr)?;

    if !glob_pattern_string.starts_with('/') {
        glob_pattern_string.insert(0, '/');
    }
    let (matches, hash) = scope
        .workspace()
        .glob_workspace_files(&glob_pattern_string)?;
    used.insert(UsedVariable::Glob(glob_pattern_string, hash));
    let matches = matches
        .into_iter()
        .map(|p| Value::String(p.into()))
        .collect();

    Ok(Eval {
        value: matches,
        used,
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

fn recursive_join(value: Value, sep: &str) -> String {
    match value {
        Value::String(string) => string,
        Value::List(values) => flat_join(&values, sep),
    }
}

fn recursive_resolve_path(
    value: &mut Value,
    working_dir: &werk_fs::Path,
    workspace: &Workspace,
) -> Result<(), EvalError> {
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

    Ok(())
}

fn recursive_replace_extension(value: &mut Value, from: &str, to: &str) {
    value.recursive_modify(|s| {
        if s.ends_with(from) {
            s.truncate(s.len() - from.len());
            s.push_str(to);
        }
    });
}

fn recursive_prepend_each(value: &mut Value, prefix: &str) {
    value.recursive_modify(|s| {
        s.insert_str(0, prefix);
    });
}

fn recursive_append_each(value: &mut Value, suffix: &str) {
    value.recursive_modify(|s| {
        s.push_str(suffix);
    });
}

fn recursive_regex_replace(value: &mut Value, regex: &regex::Regex, replacer: &String) {
    value.recursive_modify(|s| {
        // regex guarantees Cow::Borrowed means that nothing was replaced.
        let Cow::Owned(replaced) = regex.replace(s, replacer) else {
            return;
        };
        *s = replaced;
    });
}

fn find_first_string(list: &[Value]) -> Option<&str> {
    list.iter().find_map(|value| match value {
        Value::String(s) => Some(&**s),
        Value::List(list) => find_first_string(list),
    })
}
