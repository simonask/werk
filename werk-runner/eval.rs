mod string;
mod used;
pub(crate) use string::*;
pub use used::*;

use werk_fs::Absolute;
use werk_util::Symbol;

use std::sync::Arc;

use werk_parser::{
    ast::{self},
    parser::Spanned as _,
};

use crate::{
    BuildRecipeScope, Env, EvalError, Lookup, LookupValue, MatchScope, Pattern, RunCommand, Scope,
    ShellCommandLine, ShellError, StringFlags, StringValue, SubexprScope, TaskRecipeScope, Value,
    Warning,
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
    pub const fn inherent(value: T) -> Self {
        Self {
            value,
            used: Used::none(),
        }
    }

    pub fn using_var(value: T, used: UsedVariable) -> Self {
        Self::using_vars(value, [used])
    }

    pub fn using_vars(value: T, used: impl IntoIterator<Item = UsedVariable>) -> Self {
        Self {
            value,
            used: Used::from_iter(used),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Eval<U> {
        Eval {
            value: f(self.value),
            used: self.used,
        }
    }

    pub fn as_deref(&self) -> &T::Target
    where
        T: std::ops::Deref,
    {
        &self.value
    }
}

impl<T> AsRef<T> for Eval<T> {
    fn as_ref(&self) -> &T {
        &self.value
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
    #[must_use]
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

pub fn eval(scope: &dyn Scope, expr: &ast::Expr<'_>) -> Result<Eval<Value>, EvalError> {
    match expr {
        ast::Expr::SubExpr(expr) => eval_chain(scope, &expr.expr),
        ast::Expr::StringExpr(expr) => Ok(eval_string_expr(scope, expr)?.map(Value::String)),
        ast::Expr::Shell(expr) => Ok(eval_shell(scope, &expr.param)?.map(Value::String)),
        ast::Expr::Read(expr) => Ok(eval_read(scope, &expr.param)?.map(Value::String)),
        ast::Expr::Glob(expr) => Ok(eval_glob(scope, expr)?.map(Value::List)),
        ast::Expr::Which(expr) => {
            let Eval {
                value: string,
                mut used,
            } = eval_string_expr(scope, &expr.param)?;

            let (which, hash) = scope
                .workspace()
                .which(&string)
                .map_err(|e| EvalError::CommandNotFound(expr.span, string.string.clone(), e))?;
            // Note: TryFrom sets the correct StringFlags.
            let which: StringValue = match which.into_owned().try_into() {
                Ok(value) => value,
                Err(path) => {
                    return Err(EvalError::NonUtf8Which(expr.span, path.into_inner()));
                }
            };

            if let Some(hash) = hash {
                used.insert(UsedVariable::Which(Symbol::new(&string), hash));
            }

            Ok(Eval {
                value: Value::String(which),
                used,
            })
        }
        ast::Expr::Env(expr) => {
            let Eval {
                value: name,
                mut used,
            } = eval_string_expr(scope, &expr.param)?;
            let (env, hash) = scope.workspace().env(&name);
            used.insert(UsedVariable::Env(Symbol::new(&name), hash));
            Ok(Eval {
                value: Value::String(env.into()),
                used,
            })
        }
        ast::Expr::List(list_expr) => {
            let mut items = Vec::with_capacity(list_expr.items.len());
            let mut used = Used::none();
            for expr in &list_expr.items {
                let eval_item = eval_chain(scope, &expr.item)?;
                used |= eval_item.used;
                items.push(eval_item.value);
            }
            Ok(Eval {
                value: Value::List(items),
                used,
            })
        }
        ast::Expr::Ident(ident) => scope
            .get(Lookup::Ident(ident.ident))
            .ok_or_else(|| EvalError::NoSuchIdentifier(ident.span, ident.ident.to_string()))
            .map(LookupValue::into_owned),
        ast::Expr::Error(expr) => {
            let message = eval_string_expr(scope, &expr.param)?;
            Err(EvalError::ErrorExpression(expr.span, message.value.string))
        }
    }
}

pub fn eval_chain(scope: &dyn Scope, expr: &ast::ExprChain<'_>) -> Result<Eval<Value>, EvalError> {
    let mut value = eval(scope, &expr.expr)?;
    for entry in &expr.ops {
        value = eval_op(scope, &entry.expr, value)?;
    }
    Ok(value)
}

pub fn eval_op(
    scope: &dyn Scope,
    expr: &ast::ExprOp,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    match expr {
        ast::ExprOp::SubExpr(expr) => {
            let subscope = SubexprScope::new(scope, &param);
            eval_chain(&subscope, &expr.expr)
        }
        ast::ExprOp::StringExpr(expr) => {
            let subscope = SubexprScope::new(scope, &param);
            eval_string_expr(&subscope, expr).map(|eval| eval.map(Value::String))
        }
        ast::ExprOp::Match(match_expr) => eval_match_expr(scope, match_expr, param),
        ast::ExprOp::Map(expr) => eval_map(scope, expr, param),
        ast::ExprOp::Flatten(_) => Ok(eval_flatten(scope, param)),
        ast::ExprOp::Filter(expr) => eval_filter(scope, expr, param),
        ast::ExprOp::FilterMatch(expr) => eval_filter_match(scope, expr, param),
        ast::ExprOp::Discard(expr) => eval_discard(scope, expr, param),
        ast::ExprOp::Join(expr) => eval_join(scope, expr, param),
        ast::ExprOp::Split(expr) => eval_split(scope, expr, param),
        ast::ExprOp::Dedup(_) => Ok(eval_dedup(param)),
        ast::ExprOp::Lines(_) => Ok(eval_split_lines(scope, param)),
        ast::ExprOp::Info(expr) => {
            let scope = SubexprScope::new(scope, &param);
            let message = eval_string_expr(&scope, &expr.param)?;
            scope.render().message(scope.task_id(), &message.value);
            Ok(param)
        }
        ast::ExprOp::Warn(expr) => {
            let scope = SubexprScope::new(scope, &param);
            let message = eval_string_expr(&scope, &expr.param)?;
            scope.render().warning(
                scope.task_id(),
                &Warning::WarningExpression(expr.span, message.value.string),
            );
            Ok(param)
        }
        ast::ExprOp::Error(error_expr) => {
            let string = eval_string_expr(scope, &error_expr.param)?;
            Err(EvalError::ErrorExpression(
                error_expr.span,
                string.value.string,
            ))
        }
        ast::ExprOp::AssertEq(expr) => eval_assert_eq(scope, expr, param),
        ast::ExprOp::AssertMatch(expr) => eval_assert_match(scope, expr, param),
    }
}

pub fn eval_match_expr(
    scope: &dyn Scope,
    expr: &ast::MatchExpr<'_>,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    // Apply the match recursively to the input.
    fn apply_match_recursively(
        scope: &dyn Scope,
        patterns: &[(Pattern, &ast::ExprChain<'_>)],
        value: Value,
        used: &mut Used,
    ) -> Result<Value, EvalError> {
        match value {
            Value::String(s) => apply_match(scope, patterns, s.string, used),
            Value::List(list) => {
                if list.is_empty() {
                    return Ok(Value::List(Vec::new()));
                }

                let mut new_list = Vec::with_capacity(list.len());
                for item in list {
                    new_list.push(apply_match_recursively(scope, patterns, item, used)?);
                }
                Ok(Value::List(new_list))
            }
        }
    }

    fn apply_match(
        scope: &dyn Scope,
        patterns: &[(Pattern, &ast::ExprChain<'_>)],
        input_string: String,
        used: &mut Used,
    ) -> Result<Value, EvalError> {
        for (pattern, replacement_expr) in patterns {
            tracing::trace!("trying match '{:?}' against '{}'", pattern, input_string);
            let Some(pattern_match) = pattern.match_whole_string(&input_string) else {
                continue;
            };

            // Don't need to forward used variables here, because
            // we are manually collecting used variables
            let matched_string = Eval::inherent(Value::from(input_string));
            let scope = MatchScope::new(scope, &pattern_match, &matched_string);
            let new_value = eval_chain(&scope, replacement_expr)?;
            *used |= new_value.used;
            return Ok(new_value.value);
        }

        // Unmodified.
        Ok(Value::String(input_string.into()))
    }

    let mut used = param.used;

    // Evaluate patterns.
    let mut patterns = Vec::with_capacity(expr.param.len());
    for stmt in &expr.param {
        let pattern = eval_pattern(scope, &stmt.pattern)?;
        used |= pattern.used;
        patterns.push((pattern.value, &stmt.expr));
    }

    let value = apply_match_recursively(scope, &patterns, param.value, &mut used)?;

    Ok(Eval { value, used })
}

pub fn eval_filter_match(
    scope: &dyn Scope,
    expr: &ast::FilterMatchExpr<'_>,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    // Apply the match recursively to the input.
    fn apply_filter_match_recursively(
        scope: &dyn Scope,
        patterns: &[(Pattern, &ast::ExprChain<'_>)],
        value: Value,
        used: &mut Used,
        result: &mut Vec<Value>,
    ) -> Result<(), EvalError> {
        match value {
            Value::String(s) => apply_filter_match(scope, patterns, s.string, used, result),
            Value::List(list) => {
                for item in list {
                    apply_filter_match_recursively(scope, patterns, item, used, result)?;
                }
                Ok(())
            }
        }
    }

    fn apply_filter_match(
        scope: &dyn Scope,
        patterns: &[(Pattern, &ast::ExprChain<'_>)],
        input_string: String,
        used: &mut Used,
        result: &mut Vec<Value>,
    ) -> Result<(), EvalError> {
        for (pattern, replacement_expr) in patterns {
            tracing::trace!("trying match '{:?}' against '{}'", pattern, input_string);
            let Some(pattern_match) = pattern.match_whole_string(&input_string) else {
                continue;
            };

            // Don't need to forward used variables here, because
            // we are manually collecting used variables
            let matched_string = Eval::inherent(Value::from(input_string));
            let scope = MatchScope::new(scope, &pattern_match, &matched_string);
            let new_value = eval_chain(&scope, replacement_expr)?;
            *used |= new_value.used;
            result.push(new_value.value);
            break;
        }

        // No match.
        Ok(())
    }

    let mut used = param.used;

    // Evaluate patterns.
    let mut patterns = Vec::with_capacity(expr.param.len());
    for stmt in &expr.param {
        let pattern = eval_pattern(scope, &stmt.pattern)?;
        used |= pattern.used;
        patterns.push((pattern.value, &stmt.expr));
    }

    let mut result = Vec::new();
    apply_filter_match_recursively(scope, &patterns, param.value, &mut used, &mut result)?;

    Ok(Eval {
        value: Value::List(result),
        used,
    })
}

fn eval_join(
    scope: &dyn Scope,
    expr: &ast::JoinExpr<'_>,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    let sep = eval_string_expr(scope, &expr.param)?;
    let used = param.used | sep.used;
    Ok(Eval {
        value: Value::String(flat_join(&param.value, &sep.value)),
        used,
    })
}

fn eval_map(
    scope: &dyn Scope,
    expr: &ast::MapExpr<'_>,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    fn apply_map_recursively(
        scope: &dyn Scope,
        value: Value,
        map: &ast::Expr<'_>,
        used: &mut Used,
    ) -> Result<Value, EvalError> {
        match value {
            Value::List(vec) => {
                let mut result = Vec::with_capacity(vec.len());
                for item in vec {
                    let new_value = apply_map_recursively(scope, item, map, used)?;
                    result.push(new_value);
                }
                Ok(Value::List(result))
            }
            value @ Value::String(_) => {
                let input = Eval::inherent(value);
                let subscope = SubexprScope::new(scope, &input);
                let new_value = eval(&subscope, map)?;
                *used |= new_value.used;
                Ok(new_value.value)
            }
        }
    }

    let mut used = param.used;
    let value = apply_map_recursively(scope, param.value, &expr.param, &mut used)?;
    Ok(Eval { value, used })
}

fn eval_flatten(_scope: &dyn Scope, param: Eval<Value>) -> Eval<Value> {
    fn apply_flatten_recursive(value: Value, flattened: &mut Vec<Value>) {
        match value {
            Value::List(vec) => {
                for item in vec {
                    apply_flatten_recursive(item, flattened);
                }
            }
            Value::String(_) => flattened.push(value),
        }
    }

    let used = param.used;
    let mut flat = Vec::new();
    apply_flatten_recursive(param.value, &mut flat);
    Eval {
        value: Value::List(flat),
        used,
    }
}

fn eval_filter(
    scope: &dyn Scope,
    expr: &ast::FilterExpr,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    fn eval_filter_recursive(pattern: &Pattern, value: Value, result: &mut Vec<Value>) {
        match value {
            Value::List(vec) => {
                for item in vec {
                    eval_filter_recursive(pattern, item, result);
                }
            }
            Value::String(ref s) => {
                if pattern.match_whole_string(s).is_some() {
                    result.push(value);
                }
            }
        }
    }

    let pattern = eval_pattern(scope, &expr.param)?;
    let used = param.used | pattern.used;
    let mut result = Vec::new();
    eval_filter_recursive(&pattern.value, param.value, &mut result);
    Ok(Eval {
        value: Value::List(result),
        used,
    })
}

fn eval_discard(
    scope: &dyn Scope,
    expr: &ast::DiscardExpr,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    fn eval_discard_recursive(pattern: &Pattern, value: Value, result: &mut Vec<Value>) {
        match value {
            Value::List(vec) => {
                for item in vec {
                    eval_discard_recursive(pattern, item, result);
                }
            }
            Value::String(ref s) => {
                if pattern.match_whole_string(s).is_none() {
                    result.push(value);
                }
            }
        }
    }

    let pattern = eval_pattern(scope, &expr.param)?;
    let used = param.used | pattern.used;
    let mut result = Vec::new();
    eval_discard_recursive(&pattern.value, param.value, &mut result);
    Ok(Eval {
        value: Value::List(result),
        used,
    })
}

fn eval_split(
    scope: &dyn Scope,
    expr: &ast::SplitExpr,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    let pattern_builder = eval_pattern_builder(scope, &expr.param)?;
    let pattern_regex = pattern_builder.build_partial_regex();
    let regex = pattern_regex.value.regex;
    let used = param.used | pattern_regex.used;

    let mut parts = vec![];
    param.value.visit_ref(|s| {
        for part in regex.split(s) {
            parts.push(part.to_owned().into());
        }
    });
    Ok(Eval {
        value: Value::List(parts),
        used,
    })
}

fn eval_dedup(param: Eval<Value>) -> Eval<Value> {
    let new_value = dedup_recursive(param.value);
    Eval {
        value: new_value,
        used: param.used,
    }
}

fn eval_split_lines(_scope: &dyn Scope, param: Eval<Value>) -> Eval<Value> {
    let mut lines = vec![];
    param.visit_ref(|s| {
        for line in s.lines() {
            lines.push(Value::String(StringValue {
                string: line.to_owned(),
                // Paths can never contain newlines, but the string may contain
                // a list of paths, so inherit the flags.
                flags: s.flags,
            }));
        }
    });

    Eval {
        value: Value::List(lines),
        used: param.used,
    }
}

pub(crate) fn eval_pattern_builder<'a, P: Scope + ?Sized>(
    scope: &'a P,
    expr: &'a ast::PatternExpr,
) -> Result<PatternBuilder<'a, P>, EvalError> {
    let mut pattern_builder = PatternBuilder::new(scope, expr.span);
    pattern_builder.eval(expr)?;
    Ok(pattern_builder)
}

pub fn eval_pattern<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::PatternExpr,
) -> Result<Eval<Pattern>, EvalError> {
    Ok(eval_pattern_builder(scope, expr)?.build())
}

pub fn eval_string_expr<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<StringValue>, EvalError> {
    let mut builder = StringBuilder::new(scope);
    builder.eval(expr)?;
    Ok(builder.build())
}

#[expect(clippy::too_many_lines)]
pub(crate) fn eval_run_exprs<S: Scope>(
    scope: &S,
    expr: &ast::RunExpr<'_>,
    commands: &mut Vec<RunCommand>,
) -> Result<Used, EvalError> {
    fn eval_run_exprs_recursively<S: Scope>(
        scope: &S,
        expr: &ast::RunExpr<'_>,
        commands: &mut Vec<RunCommand>,
        used: &mut Used,
    ) -> Result<(), EvalError> {
        match expr {
            ast::RunExpr::Shell(expr) => {
                let shell = eval_shell_command(scope, &expr.param)?;
                *used |= shell.used;
                commands.push(RunCommand::Shell(shell.value));
            }
            ast::RunExpr::Write(expr) => {
                let destination = eval(scope, &expr.path)?;
                let Value::String(dest_path) = destination.value else {
                    return Err(EvalError::UnexpectedList(expr.path.span()));
                };
                let dest_path = werk_fs::Path::new(&dest_path)
                    .and_then(|path| scope.workspace().get_output_file_path(path))
                    .map_err(|err| EvalError::Path(expr.span, err))?;
                let data = eval(scope, &expr.value)?;
                let write_used = destination.used | data.used;
                let Value::String(data) = data.value else {
                    return Err(EvalError::UnexpectedList(expr.value.span()));
                };

                *used |= write_used;
                commands.push(RunCommand::Write(dest_path, data.string.into()));
            }
            ast::RunExpr::Copy(expr) => {
                let from = eval_string_expr(scope, &expr.src)?;
                let to = eval_string_expr(scope, &expr.dest)?;
                let from_path = werk_fs::PathBuf::new(from.value.string)
                    .and_then(|path| {
                        path.absolutize(werk_fs::Path::ROOT)
                            .map(std::borrow::Cow::into_owned)
                    })
                    .map_err(|err| EvalError::Path(expr.src.span, err))?;
                let to_path = werk_fs::Path::new(&to)
                    .and_then(|path| scope.workspace().get_output_file_path(path))
                    .map_err(|err| EvalError::Path(expr.dest.span, err))?;
                let copy_used = from.used | to.used;
                *used |= copy_used;
                commands.push(RunCommand::Copy(from_path, to_path));
            }
            ast::RunExpr::Delete(expr) => {
                let evaluated_paths = eval(scope, &expr.param)?;
                let mut paths = Vec::new();
                evaluated_paths.value.visit(|s| {
                    if s.flags.contains(StringFlags::CONTAINS_PATHS) {
                        let path = std::path::PathBuf::from(s.string);
                        let path = match Absolute::new(path) {
                            Ok(path) => path,
                            Err(path) => {
                                scope.warning(&Warning::IgnoringNonAbsolutePath(expr.span, path));
                                return;
                            }
                        };
                        paths.push(path);
                    } else {
                        scope.warning(&Warning::IgnoringUnresolvedPath(expr.span, s.string));
                    }
                });
                *used |= evaluated_paths.used;
                commands.push(RunCommand::Delete(expr.span, paths));
            }
            ast::RunExpr::Env(expr) => {
                let key = eval_string_expr(scope, &expr.key)?;
                let value = eval_string_expr(scope, &expr.value)?;
                *used |= key.used;
                *used |= value.used;
                commands.push(RunCommand::SetEnv(key.value.string, value.value.string));
            }
            ast::RunExpr::EnvRemove(expr) => {
                let key = eval_string_expr(scope, &expr.param)?;
                *used |= key.used;
                commands.push(RunCommand::RemoveEnv(key.value.string));
            }
            ast::RunExpr::Info(expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                *used |= message.used;
                commands.push(RunCommand::Info(expr.span, message.value.string));
            }
            ast::RunExpr::Warn(expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                *used |= message.used;
                commands.push(RunCommand::Warn(expr.span, message.value.string));
            }
            ast::RunExpr::List(exprs) => {
                for expr in &exprs.items {
                    eval_run_exprs_recursively(scope, &expr.item, commands, used)?;
                }
            }
            ast::RunExpr::Block(block) => {
                for stmt in &block.statements {
                    eval_run_exprs_recursively(scope, &stmt.statement, commands, used)?;
                }
            }
        }

        Ok(())
    }

    let mut used = Used::none();
    eval_run_exprs_recursively(scope, expr, commands, &mut used)?;
    Ok(used)
}

pub fn eval_shell_command<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr,
) -> Result<Eval<ShellCommandLine>, EvalError> {
    let mut builder = CommandLineBuilder::new(scope, expr.span);
    builder.eval(expr)?;
    builder.build()
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ResolvePathMode {
    /// Infer from whether or not the path exists in the workspace.
    Infer,
    /// The `:out-dir` operation was present.
    OutDir,
    /// The `:workspace` operation was present.
    Workspace,
    /// Cannot resolve paths here (e.g., in patterns)
    Illegal,
}

pub fn eval_shell<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<StringValue>, EvalError> {
    let command = eval_shell_command(scope, expr)?;

    // Unconditionally disable color output when executing shell command during eval.
    let mut env = Env::default();
    env.set_no_color();

    let output = match scope
        .io()
        .run_during_eval(&command, scope.workspace().project_root(), &env)
    {
        Ok(output) => output,
        Err(e) => {
            // Spawning the command failed.
            return Err(EvalError::Shell(
                expr.span,
                Arc::new(ShellError {
                    command: command.value,
                    result: Arc::new(Err(e)),
                }),
            ));
        }
    };

    if !output.status.success() {
        // The command itself failed.
        return Err(EvalError::Shell(
            expr.span,
            Arc::new(ShellError {
                command: command.value,
                result: Arc::new(Ok(output)),
            }),
        ));
    }

    let stdout = String::from_utf8_lossy(output.stdout.trim_ascii());
    Ok(Eval {
        value: stdout.into_owned().into(),
        used: command.used,
    })
}

pub fn eval_read<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<StringValue>, EvalError> {
    let path = eval_string_expr(scope, expr)?;

    let path_err = |err| EvalError::Path(expr.span, err);

    let path = werk_fs::Path::new(&path).map_err(path_err)?;
    let path = path.absolutize(werk_fs::Path::ROOT).map_err(path_err)?;
    let Some(fs_entry) = scope.workspace().get_project_file(&path) else {
        return Err(EvalError::Io(
            expr.span,
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("file not found during `read`: {path}"),
            )
            .into(),
        ));
    };

    let contents = scope
        .io()
        .read_file(&fs_entry.path)
        .map_err(|err| EvalError::Io(expr.span, err.into()))?;

    let Ok(string) = String::from_utf8(contents) else {
        return Err(EvalError::NonUtf8Read(
            expr.span,
            fs_entry.path.clone().into_inner(),
        ));
    };

    let used = UsedVariable::WorkspaceFile(Absolute::symbolicate(&path), fs_entry.metadata.mtime);

    Ok(Eval {
        value: string.into(),
        used: Used::from_iter([used]),
    })
}

pub fn eval_glob(
    scope: &dyn Scope,
    expr: &ast::GlobExpr<'_>,
) -> Result<Eval<Vec<Value>>, EvalError> {
    let Eval {
        value: mut glob_pattern_string,
        mut used,
    } = eval_string_expr(scope, &expr.param)?;

    if !glob_pattern_string.starts_with('/') {
        glob_pattern_string.string.insert(0, '/');
    }
    let (matches, hash) = scope
        .workspace()
        .glob_workspace_files(&glob_pattern_string)
        .map_err(|err| EvalError::Glob(expr.span, Arc::new(err)))?;
    used.insert(UsedVariable::Glob(Symbol::new(&glob_pattern_string), hash));
    let matches = matches.into_iter().map(|p| p.into_inner().into()).collect();

    Ok(Eval {
        value: matches,
        used,
    })
}

pub(crate) struct EvaluatedBuildRecipe {
    pub explicit_dependencies: Vec<StringValue>,
    pub depfile: Option<StringValue>,
    pub commands: Vec<RunCommand>,
    pub env: Env,
}

pub(crate) fn eval_build_recipe_statements(
    scope: &mut BuildRecipeScope<'_>,
    body: &[ast::BodyStmt<ast::BuildRecipeStmt<'_>>],
) -> Result<Eval<EvaluatedBuildRecipe>, EvalError> {
    let mut evaluated = EvaluatedBuildRecipe {
        explicit_dependencies: Vec::new(),
        depfile: None,
        commands: Vec::new(),
        env: Env::default(),
    };
    let mut used = Used::none();

    for stmt in body {
        match stmt.statement {
            ast::BuildRecipeStmt::Let(ref let_stmt) => {
                let value = eval_chain(scope, &let_stmt.value)?;
                scope.set(let_stmt.ident.ident, value);
            }
            ast::BuildRecipeStmt::From(ref expr) => {
                let value = eval_chain(scope, &expr.param)?;
                used |= value.used;
                let offset = evaluated.explicit_dependencies.len();
                value
                    .value
                    .collect_strings_into(&mut evaluated.explicit_dependencies);

                // Populate the `in` variable.
                scope.push_input_files(
                    evaluated.explicit_dependencies[offset..]
                        .iter()
                        .map(|s| s.string.clone()),
                );
            }
            ast::BuildRecipeStmt::Depfile(ref expr) => {
                let value = eval_chain(scope, &expr.param)?;
                used |= &value.used;
                match value.value {
                    Value::String(ref depfile) => {
                        evaluated.depfile = Some(depfile.clone());
                        scope.set(Symbol::from("depfile"), value);
                    }
                    Value::List(_) => {
                        return Err(EvalError::UnexpectedList(expr.span));
                    }
                }
            }
            ast::BuildRecipeStmt::Env(ref expr) => {
                let key = eval_string_expr(scope, &expr.key)?;
                let value = eval_string_expr(scope, &expr.value)?;
                used |= key.used;
                used |= value.used;
                evaluated.env.env(key.value, value.value);
            }
            ast::BuildRecipeStmt::EnvRemove(ref expr) => {
                let key = eval_string_expr(scope, &expr.param)?;
                used |= key.used;
                evaluated.env.env_remove(key.value);
            }
            ast::BuildRecipeStmt::Run(ref expr) => {
                used |= eval_run_exprs(scope, &expr.param, &mut evaluated.commands)?;
            }
            ast::BuildRecipeStmt::Info(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated
                    .commands
                    .push(RunCommand::Info(expr.span, message.value.string));
            }
            ast::BuildRecipeStmt::Warn(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated
                    .commands
                    .push(RunCommand::Warn(expr.span, message.value.string));
            }
            ast::BuildRecipeStmt::SetCapture(ref kw_expr) => {
                evaluated
                    .commands
                    .push(RunCommand::SetCapture(kw_expr.param.1));
            }
            ast::BuildRecipeStmt::SetNoCapture(ref kw_expr) => {
                evaluated
                    .commands
                    .push(RunCommand::SetCapture(!kw_expr.param.1));
            }
        }
    }

    Ok(Eval {
        value: evaluated,
        used,
    })
}

pub(crate) struct EvaluatedTaskRecipe {
    pub build: Vec<StringValue>,
    pub commands: Vec<RunCommand>,
    pub env: Env,
}

pub(crate) fn eval_task_recipe_statements(
    scope: &mut TaskRecipeScope<'_>,
    body: &[ast::BodyStmt<ast::TaskRecipeStmt<'_>>],
) -> Result<EvaluatedTaskRecipe, EvalError> {
    let mut evaluated = EvaluatedTaskRecipe {
        build: Vec::new(),
        commands: Vec::new(),
        env: Env::default(),
    };

    for stmt in body {
        match stmt.statement {
            ast::TaskRecipeStmt::Let(ref let_stmt) => {
                let value = eval_chain(scope, &let_stmt.value)?;
                scope.set(let_stmt.ident.ident, Eval::inherent(value.value));
            }
            ast::TaskRecipeStmt::Build(ref expr) => {
                let value = eval_chain(scope, &expr.param)?;
                value.value.collect_strings_into(&mut evaluated.build);
            }
            ast::TaskRecipeStmt::Env(ref expr) => {
                let key = eval_string_expr(scope, &expr.key)?;
                let value = eval_string_expr(scope, &expr.value)?;
                evaluated.env.env(key.value, value.value);
            }
            ast::TaskRecipeStmt::EnvRemove(ref expr) => {
                let key = eval_string_expr(scope, &expr.param)?;
                evaluated.env.env_remove(key.value);
            }
            ast::TaskRecipeStmt::Run(ref expr) => {
                eval_run_exprs(scope, &expr.param, &mut evaluated.commands)?;
            }
            ast::TaskRecipeStmt::Info(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated
                    .commands
                    .push(RunCommand::Info(expr.span, message.value.string));
            }
            ast::TaskRecipeStmt::Warn(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated
                    .commands
                    .push(RunCommand::Warn(expr.span, message.value.string));
            }
            ast::TaskRecipeStmt::SetCapture(ref kw_expr) => evaluated
                .commands
                .push(RunCommand::SetCapture(kw_expr.param.1)),
            ast::TaskRecipeStmt::SetNoCapture(ref kw_expr) => evaluated
                .commands
                .push(RunCommand::SetCapture(!kw_expr.param.1)),
        }
    }

    Ok(evaluated)
}

fn eval_assert_eq(
    scope: &dyn Scope,
    expr: &ast::AssertEqExpr<'_>,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    let scope = SubexprScope::new(scope, &param);
    let rhs = eval(&scope, &expr.param)?;
    let used = param.used | rhs.used;
    if param.value != rhs.value {
        return Err(EvalError::AssertEqFailed(
            expr.span,
            Box::new((param.value, rhs.value)),
        ));
    }
    Ok(Eval {
        value: param.value,
        used,
    })
}

fn eval_assert_match(
    scope: &dyn Scope,
    expr: &ast::AssertMatchExpr<'_>,
    param: Eval<Value>,
) -> Result<Eval<Value>, EvalError> {
    fn get_mismatch<'a>(pattern: &Pattern, value: &'a Value) -> Option<&'a str> {
        match value {
            Value::List(vec) => vec.iter().find_map(|item| get_mismatch(pattern, item)),
            Value::String(s) => {
                if pattern.match_whole_string(s).is_none() {
                    Some(s)
                } else {
                    None
                }
            }
        }
    }

    let scope = SubexprScope::new(scope, &param);
    let pattern = eval_pattern(&scope, &expr.param)?;
    let used = param.used | pattern.used;

    if let Some(mismatch) = get_mismatch(&pattern.value, &param.value) {
        return Err(EvalError::AssertMatchFailed(
            expr.span,
            Box::new((mismatch.to_owned(), pattern.value.string)),
        ));
    }

    Ok(Eval {
        value: param.value,
        used,
    })
}
