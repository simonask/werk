mod used;
pub use used::*;
use werk_fs::Absolute;
use werk_util::Symbol;

use std::{borrow::Cow, sync::Arc};

use werk_parser::{
    ast,
    parser::{Span, Spanned as _},
};

use crate::{
    AmbiguousPatternError, BuildRecipeScope, Env, EvalError, Lookup, LookupValue, MatchScope,
    Pattern, PatternBuilder, RunCommand, Scope, ShellCommandLine, ShellCommandLineBuilder,
    ShellError, SubexprScope, TaskRecipeScope, Value, Workspace,
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
                .map_err(|e| EvalError::CommandNotFound(expr.span, string.clone(), e))?;

            let which = String::from_utf8(
                which
                    .into_owned()
                    .into_inner()
                    .into_os_string()
                    .into_encoded_bytes(),
            )
            .map_err(|err| {
                EvalError::NonUtf8Which(
                    expr.span,
                    std::path::PathBuf::from(unsafe {
                        // SAFETY: These are the bytes we just got from `into_os_string()`.
                        std::ffi::OsString::from_encoded_bytes_unchecked(err.into_bytes())
                    }),
                )
            })?;

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
                value: Value::String(env),
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
            Err(EvalError::ErrorExpression(expr.span, message.value))
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
        ast::ExprOp::Match(match_expr) => eval_match_expr(scope, match_expr, param).map(Into::into),
        ast::ExprOp::Map(expr) => eval_map(scope, expr, param),
        ast::ExprOp::Flatten(_) => Ok(eval_flatten(scope, param)),
        ast::ExprOp::Filter(expr) => eval_filter(scope, expr, param),
        ast::ExprOp::FilterMatch(expr) => eval_filter_match(scope, expr, param),
        ast::ExprOp::Discard(expr) => eval_discard(scope, expr, param),
        ast::ExprOp::Join(expr) => eval_join(scope, expr, param),
        ast::ExprOp::Split(expr) => eval_split(scope, expr, param),
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
            scope.render().warning(scope.task_id(), &message.value);
            Ok(param)
        }
        ast::ExprOp::Error(error_expr) => {
            let string = eval_string_expr(scope, &error_expr.param)?;
            Err(EvalError::ErrorExpression(error_expr.span, string.value))
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
        patterns: &[(Pattern<'_>, &ast::ExprChain<'_>)],
        value: Value,
        used: &mut Used,
    ) -> Result<Value, EvalError> {
        match value {
            Value::String(s) => apply_match(scope, patterns, s.clone(), used),
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
        patterns: &[(Pattern<'_>, &ast::ExprChain<'_>)],
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
            let matched_string = Eval::inherent(Value::String(input_string));
            let scope = MatchScope::new(scope, &pattern_match, &matched_string);
            let new_value = eval_chain(&scope, replacement_expr)?;
            *used |= new_value.used;
            return Ok(new_value.value);
        }

        // Unmodified.
        Ok(Value::String(input_string))
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
        patterns: &[(Pattern<'_>, &ast::ExprChain<'_>)],
        value: Value,
        used: &mut Used,
        result: &mut Vec<Value>,
    ) -> Result<(), EvalError> {
        match value {
            Value::String(s) => apply_filter_match(scope, patterns, s, used, result),
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
        patterns: &[(Pattern<'_>, &ast::ExprChain<'_>)],
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
            let matched_string = Eval::inherent(Value::String(input_string));
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
    fn split_recursive(value: &Value, regex: &regex::Regex, result: &mut Vec<Value>) {
        match value {
            Value::List(vec) => {
                for item in vec {
                    split_recursive(item, regex, result);
                }
            }
            Value::String(s) => {
                for split in regex.split(s) {
                    result.push(Value::String(split.to_owned()));
                }
            }
        }
    }

    let pattern_builder = eval_pattern_builder(scope, &expr.param)?;
    let used = param.used | pattern_builder.used;
    let mut pattern_builder = pattern_builder.value;
    pattern_builder.set_match_substrings(true);
    let pattern = pattern_builder.build();

    let mut result = Vec::new();
    split_recursive(
        &param.value,
        pattern
            .regex()
            .expect("split-pattern did not produce a regex"),
        &mut result,
    );
    Ok(Eval {
        value: Value::List(result),
        used,
    })
}

fn eval_split_lines(_scope: &dyn Scope, param: Eval<Value>) -> Eval<Value> {
    fn split_lines_recursive(value: &Value, result: &mut Vec<Value>) {
        match value {
            Value::List(vec) => {
                for item in vec {
                    split_lines_recursive(item, result);
                }
            }
            Value::String(s) => {
                for line in s.lines() {
                    result.push(Value::String(line.to_owned()));
                }
            }
        }
    }

    let used = param.used;
    let mut result = Vec::new();
    split_lines_recursive(&param.value, &mut result);
    Eval {
        value: Value::List(result),
        used,
    }
}

pub fn eval_pattern_builder<'a, P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::PatternExpr<'a>,
) -> Result<Eval<PatternBuilder<'a>>, EvalError> {
    let mut pattern_builder = PatternBuilder::default();
    pattern_builder.set_span(expr.span);

    let mut used = Used::none();

    for fragment in &expr.fragments {
        match fragment {
            ast::PatternFragment::Literal(lit) => pattern_builder.push_str(lit),
            ast::PatternFragment::PatternStem => pattern_builder.push_pattern_stem(),
            ast::PatternFragment::OneOf(one_of) => pattern_builder.push_one_of(one_of.clone()),
            ast::PatternFragment::Interpolation(interp) => {
                if let ast::InterpolationStem::PatternCapture = interp.stem {
                    return Err(EvalError::PatternStemInterpolationInPattern(expr.span));
                }

                let value = eval_string_interpolation_stem(scope, expr.span, interp.stem)?;
                used |= value.used();

                let mut value_owned;
                let value = if let Some(ref options) = interp.options {
                    if options.join.is_some() {
                        return Err(EvalError::JoinInPattern(expr.span));
                    }

                    value_owned = value.into_value();
                    eval_string_interpolation_ops(
                        expr.span,
                        &mut value_owned,
                        &options.ops,
                        false,
                        scope.workspace(),
                        ResolvePathMode::Illegal,
                    )?;
                    &value_owned
                } else {
                    &*value
                };

                // Note: Ignoring the build-status of the interpolation
                // stem, because we are building a pattern - it can't itself
                // be outdated.
                match value {
                    Value::String(string) => {
                        pattern_builder.push_str(string);
                    }
                    Value::List(_) => {
                        return Err(EvalError::ListInPattern(expr.span));
                    }
                }
            }
        }
    }

    Ok(Eval {
        value: pattern_builder,
        used,
    })
}

pub fn eval_pattern<'a, P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::PatternExpr<'a>,
) -> Result<Eval<Pattern<'a>>, EvalError> {
    Ok(eval_pattern_builder(scope, expr)?.map(PatternBuilder::build))
}

pub fn eval_string_expr<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<String>, EvalError> {
    let mut s = String::new();

    let mut used = Used::none();

    for fragment in &expr.fragments {
        match fragment {
            ast::StringFragment::Literal(lit) => s.push_str(lit),
            ast::StringFragment::Interpolation(interp) => {
                let value = eval_string_interpolation_stem(scope, expr.span, interp.stem)?;
                used |= value.used();

                let mut value_owned;
                let value = if let Some(ref options) = interp.options {
                    value_owned = value.into_value();
                    eval_string_interpolation_ops(
                        expr.span,
                        &mut value_owned,
                        &options.ops,
                        true,
                        scope.workspace(),
                        ResolvePathMode::Infer,
                    )?;

                    if let Some(ref join) = options.join {
                        value_owned = Value::String(recursive_join(value_owned, join));
                    }
                    &value_owned
                } else {
                    &*value
                };

                match value {
                    Value::List(list) => {
                        if let Some(first) = find_first_string(list) {
                            s.push_str(first);
                        }
                    }
                    Value::String(value) => {
                        s.push_str(value);
                    }
                }
            }
        }
    }

    Ok(Eval { value: s, used })
}

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
                commands.push(RunCommand::Write(dest_path, data.into()));
            }
            ast::RunExpr::Copy(expr) => {
                let from = eval_string_expr(scope, &expr.src)?;
                let to = eval_string_expr(scope, &expr.dest)?;
                let from_path = werk_fs::PathBuf::new(from.value)
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
                evaluated_paths
                    .value
                    .try_collect_strings_recursive(|path| {
                        let path = werk_fs::PathBuf::new(path)?;
                        let path = path.absolutize(werk_fs::Path::ROOT)?;
                        let path = scope.workspace().get_output_file_path(&path)?;
                        paths.push(path);
                        Ok(())
                    })
                    .map_err(|err| EvalError::Path(expr.param.span(), err))?;
                *used |= evaluated_paths.used;
                commands.push(RunCommand::Delete(paths));
            }
            ast::RunExpr::Env(expr) => {
                let key = eval_string_expr(scope, &expr.key)?;
                let value = eval_string_expr(scope, &expr.value)?;
                *used |= key.used;
                *used |= value.used;
                commands.push(RunCommand::SetEnv(key.value, value.value));
            }
            ast::RunExpr::EnvRemove(expr) => {
                let key = eval_string_expr(scope, &expr.param)?;
                *used |= key.used;
                commands.push(RunCommand::RemoveEnv(key.value));
            }
            ast::RunExpr::Info(expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                *used |= message.used;
                commands.push(RunCommand::Info(message.value));
            }
            ast::RunExpr::Warn(expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                *used |= message.used;
                // TODO: Specific warn command.
                commands.push(RunCommand::Info(message.value));
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
    let mut builder = ShellCommandLineBuilder::default();

    let mut used = Used::none();

    for fragment in &expr.fragments {
        match fragment {
            ast::StringFragment::Literal(lit) => {
                builder.push_lit(lit);
            }
            ast::StringFragment::Interpolation(interp) => {
                let value = eval_string_interpolation_stem(scope, expr.span, interp.stem)?;
                used |= value.used();

                let mut value_owned;
                let value = if let Some(ref options) = interp.options {
                    value_owned = value.into_value();
                    eval_string_interpolation_ops(
                        expr.span,
                        &mut value_owned,
                        &options.ops,
                        true,
                        scope.workspace(),
                        ResolvePathMode::Infer,
                    )?;
                    &value_owned
                } else {
                    &*value
                };

                match value {
                    Value::List(list) => match interp
                        .options
                        .as_ref()
                        .and_then(|options| options.join.as_deref())
                    {
                        // When the join char is a space, we treat the list as
                        // separate arguments to the command.
                        Some(" ") => {
                            builder.push_all(value);
                        }
                        // Otherwise, we join the list into a single argument.
                        Some(sep) => {
                            let s = flat_join(value, sep);
                            builder.push_arg(&s);
                        }
                        // When no join operator is present take the first element of the list.
                        None => {
                            let Some(s) = find_first_string(list) else {
                                return Err(EvalError::EmptyList(expr.span));
                            };
                            builder.push_arg(s);
                        }
                    },
                    Value::String(s) => {
                        builder.push_str(s);
                    }
                }
            }
        }
    }

    let (command_line, used_which) = builder.build(expr.span, scope.workspace())?;

    if let Some(used_which) = used_which {
        used.insert(used_which);
    }

    Ok(Eval {
        value: command_line,
        used,
    })
}

fn eval_string_interpolation_stem<P: Scope + ?Sized>(
    scope: &P,
    span: Span,
    stem: ast::InterpolationStem,
) -> Result<LookupValue, EvalError> {
    Ok(match stem {
        ast::InterpolationStem::Implied => scope
            .get(Lookup::Implied)
            .ok_or(EvalError::NoImpliedValue(span))?,
        ast::InterpolationStem::PatternCapture => scope
            .get(Lookup::PatternStem)
            .ok_or(EvalError::NoPatternStem(span))?,
        ast::InterpolationStem::CaptureGroup(group) => scope
            .get(Lookup::CaptureGroup(group))
            .ok_or(EvalError::NoSuchCaptureGroup(span, group))?,
        ast::InterpolationStem::Ident(ident) => scope
            .get(Lookup::Ident(ident))
            .ok_or_else(|| EvalError::NoSuchIdentifier(span, ident.as_ref().to_owned()))?,
    })
}

#[derive(Clone, Copy)]
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

fn eval_string_interpolation_ops(
    span: Span,
    value: &mut Value,
    ops: &[ast::InterpolationOp],
    allow_os_paths: bool,
    workspace: &Workspace,
    default_resolve_mode: ResolvePathMode,
) -> Result<(), EvalError> {
    let mut resolve_mode = default_resolve_mode;

    for op in ops {
        match op {
            ast::InterpolationOp::ReplaceExtension { from, to } => {
                recursive_replace_extension(value, from, to);
            }
            ast::InterpolationOp::PrependEach(prefix) => recursive_prepend_each(value, prefix),
            ast::InterpolationOp::AppendEach(suffix) => recursive_append_each(value, suffix),
            ast::InterpolationOp::RegexReplace(r) => {
                recursive_regex_replace(value, &r.regex, &r.replacer);
            }
            ast::InterpolationOp::ResolveOsPath => {
                if allow_os_paths {
                    recursive_resolve_path(
                        span,
                        value,
                        werk_fs::Path::ROOT,
                        workspace,
                        resolve_mode,
                    )?;
                } else {
                    return Err(EvalError::JoinInPattern(span));
                }
            }
            ast::InterpolationOp::ResolveOutDir => {
                resolve_mode = ResolvePathMode::OutDir;
            }
            ast::InterpolationOp::ResolveWorkspace => {
                resolve_mode = ResolvePathMode::Workspace;
            }
        }
    }

    Ok(())
}

pub fn eval_shell<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<String>, EvalError> {
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
        value: stdout.into_owned(),
        used: command.used,
    })
}

pub fn eval_read<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<String>, EvalError> {
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
        value: string,
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
        glob_pattern_string.insert(0, '/');
    }
    let (matches, hash) = scope
        .workspace()
        .glob_workspace_files(&glob_pattern_string)
        .map_err(|err| EvalError::Glob(expr.span, Arc::new(err)))?;
    used.insert(UsedVariable::Glob(Symbol::new(&glob_pattern_string), hash));
    let matches = matches
        .into_iter()
        .map(|p| Value::String(p.into_inner().into()))
        .collect();

    Ok(Eval {
        value: matches,
        used,
    })
}

pub(crate) struct EvaluatedBuildRecipe {
    pub explicit_dependencies: Vec<String>,
    pub depfile: Option<String>,
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
                scope.push_input_files(&evaluated.explicit_dependencies[offset..]);
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
                evaluated.commands.push(RunCommand::Info(message.value));
            }
            ast::BuildRecipeStmt::Warn(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated.commands.push(RunCommand::Warn(message.value));
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
    pub build: Vec<String>,
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
                evaluated.commands.push(RunCommand::Info(message.value));
            }
            ast::TaskRecipeStmt::Warn(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated.commands.push(RunCommand::Warn(message.value));
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
    fn get_mismatch<'a>(pattern: &Pattern, value: &'a Value) -> Option<&'a String> {
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
            Box::new((mismatch.clone(), pattern.value.string)),
        ));
    }

    Ok(Eval {
        value: param.value,
        used,
    })
}

fn flat_join(values: &Value, sep: &str) -> String {
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

    match values {
        Value::String(s) => s.clone(),
        Value::List(l) => {
            let mut s = String::new();
            flat_join(l, &mut s, sep, true);
            s
        }
    }
}

fn recursive_join(value: Value, sep: &str) -> String {
    match value {
        Value::String(s) => s,
        ref value @ Value::List(_) => flat_join(value, sep),
    }
}

fn recursive_resolve_path(
    span: Span,
    value: &mut Value,
    working_dir: &Absolute<werk_fs::Path>,
    workspace: &Workspace,
    resolve_mode: ResolvePathMode,
) -> Result<(), EvalError> {
    value.try_recursive_modify(|string| {
        let path = werk_fs::Path::new(string).map_err(|err| EvalError::Path(span, err))?;
        let path = path.absolutize(working_dir).map_err(|err| EvalError::Path(span, err))?;
        let path = match resolve_mode {
            ResolvePathMode::Infer => resolve_path_infer(span, &path, workspace)?,
            ResolvePathMode::OutDir => path.resolve(workspace.output_directory()),
            ResolvePathMode::Workspace => path.resolve(workspace.project_root()),
            ResolvePathMode::Illegal => return Err(EvalError::ResolvePathInPattern(span)),
        };
        match path.to_str() {
            Some(path) => path.clone_into(string),
            None => panic!("Path resolution produced a non-UTF8 path; probably the project root path is non-UTF8"),
        }
        Ok::<_, EvalError>(())
    })
}

fn resolve_path_infer(
    span: Span,
    path: &Absolute<werk_fs::Path>,
    workspace: &Workspace,
) -> Result<Absolute<std::path::PathBuf>, EvalError> {
    if let Some(workspace_file) = workspace.get_project_file(path) {
        // Check if the path also matches a build recipe, and must be disambiguated.
        match workspace.manifest.match_build_recipe(path) {
            Ok(Some(recipe)) => Err(EvalError::AmbiguousPathResolution(
                span,
                Arc::new(crate::AmbiguousPathError {
                    path: path.to_path_buf(),
                    build_recipe: recipe.recipe.pattern.span,
                }),
            )),
            Err(AmbiguousPatternError { pattern1, .. }) => Err(EvalError::AmbiguousPathResolution(
                span,
                Arc::new(crate::AmbiguousPathError {
                    path: path.to_path_buf(),
                    build_recipe: pattern1,
                }),
            )),
            Ok(None) => Ok(workspace_file.path.clone()),
        }
    } else {
        Ok(path.resolve(workspace.output_directory()))
    }
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

fn recursive_regex_replace(value: &mut Value, regex: &regex::Regex, replacer: &str) {
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
