mod used;
pub use used::*;
use werk_fs::{Absolute, PathError};

use std::{borrow::Cow, sync::Arc};

use werk_parser::{
    ast,
    parser::{Span, Spanned as _},
};

use crate::{
    BuildRecipeScope, Error, EvalError, Lookup, LookupValue, MatchScope, Pattern, PatternBuilder,
    RunCommand, Scope, ShellCommandLine, ShellCommandLineBuilder, ShellError, SubexprScope,
    TaskRecipeScope, Value, Workspace,
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

pub async fn eval(scope: &dyn Scope, expr: &ast::Expr<'_>) -> Result<Eval<Value>, EvalError> {
    match eval_inner(scope, expr).await? {
        EvalInner::Produced(eval) => Ok(eval),
        // EvalInner::ReturnImplicit => Ok(Eval::inherent(Value::String(String::new()))),
        EvalInner::ReturnImplicit => {
            let Some(implied) = scope.get(Lookup::Implied) else {
                return Err(EvalError::NoImpliedValue(expr.span()));
            };
            Ok(implied.into_owned())
        }
    }
}

enum EvalInner {
    Produced(Eval<Value>),
    ReturnImplicit,
}

impl From<Eval<Value>> for EvalInner {
    fn from(value: Eval<Value>) -> Self {
        EvalInner::Produced(value)
    }
}

async fn eval_inner(scope: &dyn Scope, expr: &ast::Expr<'_>) -> Result<EvalInner, EvalError> {
    match expr {
        ast::Expr::StringExpr(expr) => Ok(eval_string_expr(scope, expr)?.map(Value::String).into()),
        ast::Expr::Shell(expr) => Ok(eval_shell(scope, &expr.param)
            .await?
            .map(Value::String)
            .into()),
        ast::Expr::Glob(expr) => Ok(eval_glob(scope, expr)?.map(Value::List).into()),
        ast::Expr::Which(expr) => {
            let Eval {
                value: string,
                mut used,
            } = eval_string_expr(scope, &expr.param)?;

            let (which, hash) = scope
                .workspace()
                .which(&string)
                .map_err(|e| EvalError::CommandNotFound(expr.span, string.clone(), e))?;

            let which = String::from_utf8(which.into_inner().into_os_string().into_encoded_bytes())
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
                used.insert(UsedVariable::Which(string, hash));
            }

            Ok(Eval {
                value: Value::String(which),
                used,
            }
            .into())
        }
        ast::Expr::Env(expr) => {
            let Eval {
                value: name,
                mut used,
            } = eval_string_expr(scope, &expr.param)?;
            let (env, hash) = scope.workspace().env(&name);
            used.insert(UsedVariable::Env(name, hash));
            Ok(Eval {
                value: Value::String(env),
                used,
            }
            .into())
        }
        ast::Expr::Match(match_expr) => {
            // Boxing for recursion.
            Box::pin(eval_match_expr(scope, match_expr))
                .await
                .map(Into::into)
        }
        ast::Expr::List(list_expr) => {
            // Boxing for recursion.
            Box::pin(async {
                let mut items = Vec::with_capacity(list_expr.items.len());
                let mut used = Used::none();
                for expr in &list_expr.items {
                    let eval_item = match eval_inner(scope, &expr.item).await? {
                        EvalInner::Produced(eval) => eval,
                        // If the list element is an expression that forwards an
                        // implicit value, produce the empty string -- we don't
                        // want to duplicate the implicit value in list
                        // expressions.
                        EvalInner::ReturnImplicit => Eval {
                            value: Value::String(String::new()),
                            used: Used::none(),
                        },
                    };
                    used |= eval_item.used;
                    items.push(eval_item.value);
                }
                Ok(Eval {
                    value: Value::List(items),
                    used,
                }
                .into())
            })
            .await
        }
        ast::Expr::Join(join) => {
            let sep = eval_string_expr(scope, &join.param)?;
            let joined = if let Some(val) = scope.get(Lookup::Implied) {
                let mut used = sep.used;
                used.vars.extend(val.used().iter().cloned());
                Eval {
                    value: flat_join(&*val, &sep.value),
                    used,
                }
            } else {
                Eval {
                    value: String::new(),
                    used: Used::none(),
                }
            };
            Ok(joined.map(Value::String).into())
        }
        ast::Expr::Ident(ident) => scope
            .get(Lookup::Ident(&ident.ident))
            .ok_or_else(|| EvalError::NoSuchIdentifier(ident.span, ident.ident.to_owned()))
            .map(LookupValue::into_owned)
            .map(EvalInner::Produced),
        ast::Expr::Then(then) => {
            // Boxing for recursion.
            Box::pin(async {
                let lhs_value = eval(scope, &then.expr).await?;
                let scope = SubexprScope::new(scope, &lhs_value);
                match eval_inner(&scope, &then.then).await? {
                    // The rhs expression produced a new value, return that.
                    EvalInner::Produced(rhs_value) => Ok(EvalInner::Produced(rhs_value)),
                    // The lhs expression did not produce a new value, return the lhs value.
                    EvalInner::ReturnImplicit => Ok(EvalInner::Produced(lhs_value)),
                }
            })
            .await
        }
        ast::Expr::Info(info_expr) => {
            let message = eval_string_expr(scope, &info_expr.param)?;
            scope.watcher().message(scope.task_id(), &message.value);
            Ok(EvalInner::ReturnImplicit)
        }
        ast::Expr::Warn(warn_expr) => {
            let message = eval_string_expr(scope, &warn_expr.param)?;
            scope.watcher().warning(scope.task_id(), &message.value);
            Ok(EvalInner::ReturnImplicit)
        }
        ast::Expr::Error(error_expr) => {
            let string = eval_string_expr(scope, &error_expr.param)?;
            Err(EvalError::ErrorExpression(error_expr.span, string.value))
        }
    }
}

pub async fn eval_match_expr(
    scope: &dyn Scope,
    expr: &ast::MatchExpr<'_>,
) -> Result<Eval<Value>, EvalError> {
    let implied_value = scope
        .get(Lookup::Implied)
        .ok_or(EvalError::NoImpliedValue(expr.span))?;
    let mut used = implied_value.used().clone();

    // Evaluate patterns.
    let mut patterns = Vec::with_capacity(expr.body.statements.len());
    for stmt in expr.body.statements.iter() {
        let pattern = eval_pattern(scope, &stmt.statement.pattern)?;
        used |= pattern.used;
        patterns.push((pattern.value, &stmt.statement.expr));
    }

    // Apply the match recursively to the input.
    async fn apply_match_recursively(
        scope: &dyn Scope,
        patterns: &[(Pattern<'_>, &ast::Expr<'_>)],
        value: &Value,
        used: &mut Used,
    ) -> Result<Value, EvalError> {
        match value {
            Value::String(s) => apply_match(scope, patterns, s.clone(), used).await,
            Value::List(list) => {
                if list.is_empty() {
                    return Ok(Value::List(Vec::new()));
                }

                // Boxing for recursion
                Box::pin(async move {
                    let mut new_list = Vec::with_capacity(list.len());
                    for item in list {
                        new_list.push(apply_match_recursively(scope, patterns, item, used).await?);
                    }
                    Ok(Value::List(new_list))
                })
                .await
            }
        }
    }

    async fn apply_match(
        scope: &dyn Scope,
        patterns: &[(Pattern<'_>, &ast::Expr<'_>)],
        input_string: String,
        used: &mut Used,
    ) -> Result<Value, EvalError> {
        for (pattern, replacement_expr) in patterns {
            tracing::trace!("trying match '{:?}' against '{}'", pattern, input_string);
            let Some(pattern_match) = pattern.match_string(&input_string) else {
                continue;
            };

            let matched_string = Eval {
                value: Value::String(input_string.clone()),
                // Don't need to forward used variables here, because
                // we are manually collecting used variables
                used: Used::none(),
            };
            let scope = MatchScope::new(scope, &pattern_match, &matched_string);
            let new_value = match eval_inner(&scope, replacement_expr).await? {
                EvalInner::Produced(eval) => {
                    *used |= eval.used;
                    eval.value
                }
                EvalInner::ReturnImplicit => matched_string.value,
            };
            return Ok(new_value);
        }

        // Unmodified.
        Ok::<_, EvalError>(Value::String(input_string))
    }

    let value = apply_match_recursively(scope, &patterns, &*implied_value, &mut used).await?;

    Ok(Eval { value, used })
}

pub async fn eval_collect_strings<P: Scope>(
    scope: &P,
    expr: &ast::Expr<'_>,
) -> Result<Eval<Vec<String>>, EvalError> {
    let eval = eval(scope, expr).await?;
    Ok(eval.map(|value| value.collect_strings()))
}

pub fn eval_pattern_builder<'a, P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::PatternExpr<'a>,
) -> Result<Eval<PatternBuilder<'a>>, EvalError> {
    let mut pattern_builder = PatternBuilder::default();

    let mut used = Used::none();

    for fragment in &expr.fragments {
        match fragment {
            ast::PatternFragment::Literal(lit) => pattern_builder.push_str(lit),
            ast::PatternFragment::PatternStem => pattern_builder.push_pattern_stem(),
            ast::PatternFragment::OneOf(one_of) => pattern_builder.push_one_of(one_of.clone()),
            ast::PatternFragment::Interpolation(interp) => {
                if let ast::InterpolationStem::PatternCapture = interp.stem {
                    return Err(EvalError::PatternStemInterpolationInPattern(expr.span).into());
                }

                let value = eval_string_interpolation_stem(scope, expr.span, &interp.stem)?;
                used |= value.used();

                let mut value_owned;
                let value = if let Some(ref options) = interp.options {
                    if options.join.is_some() {
                        return Err(EvalError::JoinInPattern(expr.span).into());
                    }

                    value_owned = value.into_value();
                    eval_string_interpolation_ops(
                        expr.span,
                        &mut value_owned,
                        &options.ops,
                        false,
                        scope.workspace(),
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
                        pattern_builder.push_str(&string);
                    }
                    Value::List(_) => {
                        return Err(EvalError::ListInPattern(expr.span).into());
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
                let value = eval_string_interpolation_stem(scope, expr.span, &interp.stem)?;
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
                    )?;

                    if let Some(ref join) = options.join {
                        value_owned = Value::String(recursive_join(value_owned, join));
                    }
                    &value_owned
                } else {
                    &*value
                };

                match value {
                    Value::List(_) => return Err(EvalError::UnexpectedList(expr.span).into()),
                    Value::String(value) => {
                        s.push_str(&value);
                    }
                }
            }
        }
    }

    Ok(Eval { value: s, used })
}

pub(crate) async fn eval_run_exprs<S: Scope>(
    scope: &S,
    expr: &ast::RunExpr<'_>,
    commands: &mut Vec<RunCommand>,
) -> Result<Used, EvalError> {
    async fn eval_run_exprs_recursively<S: Scope>(
        scope: &S,
        expr: &ast::RunExpr<'_>,
        commands: &mut Vec<RunCommand>,
        used: &mut Used,
    ) -> Result<(), EvalError> {
        match expr {
            ast::RunExpr::Shell(expr) => {
                let shell = eval_shell_command(scope, &expr.param, scope.workspace().force_color)?;
                *used |= shell.used;
                commands.push(RunCommand::Shell(shell.value));
            }
            ast::RunExpr::Write(expr) => {
                let destination = eval(scope, &expr.path).await?;
                let Value::String(dest_path) = destination.value else {
                    return Err(EvalError::UnexpectedList(expr.path.span()));
                };
                let dest_path = werk_fs::Path::new(&dest_path)
                    .and_then(|path| scope.workspace().get_output_file_path(path))
                    .map_err(|err| EvalError::Path(expr.span, err))?;
                let data = eval(scope, &expr.value).await?;
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
                let from_path = werk_fs::Path::new(&from)
                    .and_then(|path| scope.workspace().get_output_file_path(path))
                    .map_err(|err| EvalError::Path(expr.src.span, err))?;
                let to_path = werk_fs::Path::new(&to)
                    .and_then(|path| scope.workspace().get_output_file_path(path))
                    .map_err(|err| EvalError::Path(expr.dest.span, err))?;
                let copy_used = from.used | to.used;
                *used |= copy_used;
                commands.push(RunCommand::Copy(from_path, to_path));
            }
            ast::RunExpr::Delete(expr) => {
                let path = eval_string_expr(scope, &expr.param)?;
                let delete_path = werk_fs::Path::new(&path.value)
                    .and_then(|path| scope.workspace().get_output_file_path(path))
                    .map_err(|err| EvalError::Path(expr.param.span, err))?;
                *used |= path.used;
                commands.push(RunCommand::Delete(delete_path));
            }
            ast::RunExpr::Info(expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                *used |= message.used;
                commands.push(RunCommand::Info(message.value.into()));
            }
            ast::RunExpr::Warn(expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                *used |= message.used;
                // TODO: Specific warn command.
                commands.push(RunCommand::Info(message.value.into()));
            }
            ast::RunExpr::List(exprs) => {
                // Boxing for recursion
                Box::pin(async {
                    for expr in &exprs.items {
                        eval_run_exprs_recursively(scope, &expr.item, commands, used).await?;
                    }
                    Ok::<_, EvalError>(())
                })
                .await?;
            }
            ast::RunExpr::Block(block) => {
                // Boxing for recursion
                Box::pin(async {
                    for stmt in &block.statements {
                        eval_run_exprs_recursively(scope, &stmt.statement, commands, used).await?;
                    }
                    Ok::<_, EvalError>(())
                })
                .await?;
            }
        }

        Ok(())
    }

    let mut used = Used::none();
    eval_run_exprs_recursively(scope, expr, commands, &mut used).await?;
    Ok(used)
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
                let value = eval_string_interpolation_stem(scope, expr.span, &interp.stem)?;
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
                            let Some(s) = find_first_string(&list) else {
                                return Err(EvalError::EmptyList(expr.span));
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

    let (mut command_line, used_which) = builder.build(expr.span, scope.workspace())?;
    if force_color {
        command_line.set_force_color();
    }

    if let Some(used_which) = used_which {
        used.insert(used_which);
    }

    Ok(Eval {
        value: command_line,
        used,
    })
}

fn eval_shell_commands_into<S: Scope>(
    scope: &S,
    expr: &ast::Expr,
    cmds: &mut Vec<ShellCommandLine>,
    force_color: bool,
) -> Result<Used, Error> {
    let span = expr.span();

    match expr {
        ast::Expr::StringExpr(string_expr)
        | ast::Expr::Shell(ast::ShellExpr {
            param: string_expr, ..
        }) => {
            let command = eval_shell_command(scope, string_expr, force_color)?;
            cmds.push(command.value);
            Ok(command.used)
        }
        ast::Expr::Glob(_) => return Err(EvalError::UnexpectedExpressionType(span, "glob").into()),
        ast::Expr::Which(_) => {
            return Err(EvalError::UnexpectedExpressionType(span, "which").into())
        }
        ast::Expr::Match(_) => {
            return Err(EvalError::UnexpectedExpressionType(span, "match").into())
        }
        ast::Expr::Env(_) => return Err(EvalError::UnexpectedExpressionType(span, "env").into()),
        ast::Expr::List(list) => {
            let mut used = Used::none();
            for item in &list.items {
                used |= eval_shell_commands_into(scope, &item.item, cmds, force_color)?;
            }
            Ok(used)
        }
        ast::Expr::Join(..) => Err(EvalError::UnexpectedExpressionType(span, "join").into()),
        ast::Expr::Ident(_) => return Err(EvalError::UnexpectedExpressionType(span, "from").into()),
        ast::Expr::Then(_) => return Err(EvalError::UnexpectedExpressionType(span, "then").into()),
        ast::Expr::Info(expr) => {
            let message = eval_string_expr(scope, &expr.param)?;
            scope.watcher().message(scope.task_id(), &message);
            // Messages in shell commands do not contribute to outdatedness.
            Ok(Used::none())
        }
        ast::Expr::Warn(expr) => {
            let message = eval_string_expr(scope, &expr.param)?;
            scope.watcher().warning(scope.task_id(), &message);
            // Messages in shell commands do not contribute to outdatedness.
            Ok(Used::none())
        }
        ast::Expr::Error(expr) => {
            let message = eval_string_expr(scope, &expr.param)?;
            return Err(EvalError::ErrorExpression(span, message.value).into());
        }
    }
}

pub fn eval_shell_commands<S: Scope>(
    scope: &S,
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
pub fn eval_shell_commands_run_which_and_detect_outdated<S: Scope>(
    scope: &S,
    expr: &ast::Expr,
    force_color: bool,
) -> Result<Eval<Vec<ShellCommandLine>>, Error> {
    let mut value = Vec::new();
    let used = eval_shell_commands_into(scope, expr, &mut value, force_color)?;
    Ok(Eval { value, used })
}

fn eval_string_interpolation_stem<'a, P: Scope + ?Sized>(
    scope: &'a P,
    span: Span,
    stem: &ast::InterpolationStem,
) -> Result<LookupValue<'a>, EvalError> {
    Ok(match stem {
        ast::InterpolationStem::Implied => scope
            .get(Lookup::Implied)
            .ok_or(EvalError::NoImpliedValue(span))?,
        ast::InterpolationStem::PatternCapture => scope
            .get(Lookup::PatternStem)
            .ok_or(EvalError::NoPatternStem(span))?,
        ast::InterpolationStem::CaptureGroup(group) => scope
            .get(Lookup::CaptureGroup(*group as u32))
            .ok_or(EvalError::NoSuchCaptureGroup(span, *group))?,
        ast::InterpolationStem::Ident(ref ident) => scope
            .get(Lookup::Ident(ident))
            .ok_or_else(|| EvalError::NoSuchIdentifier(span, ident.as_ref().to_owned()))?,
    })
}

fn eval_string_interpolation_ops(
    span: Span,
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
                    recursive_resolve_path(span, value, werk_fs::Path::ROOT, workspace)?
                } else {
                    return Err(EvalError::JoinInPattern(span));
                }
            }
        }
    }

    Ok(())
}

pub async fn eval_shell<P: Scope + ?Sized>(
    scope: &P,
    expr: &ast::StringExpr<'_>,
) -> Result<Eval<String>, EvalError> {
    let mut command = eval_shell_command(scope, expr, false)?;

    // Unconditionally disable color output when the command supports it,
    // because we are capturing the output as a string.
    command.set_no_color();

    let output = match scope
        .io()
        .run_during_eval(&command, scope.workspace().project_root())
        .await
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

    let stdout = String::from_utf8_lossy(&output.stdout.trim_ascii());
    Ok(Eval {
        value: stdout.into_owned(),
        used: command.used,
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
    used.insert(UsedVariable::Glob(glob_pattern_string, hash));
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
}

pub(crate) async fn eval_build_recipe_statements(
    scope: &mut BuildRecipeScope<'_>,
    body: &[ast::BodyStmt<ast::BuildRecipeStmt<'_>>],
) -> Result<Eval<EvaluatedBuildRecipe>, EvalError> {
    let mut evaluated = EvaluatedBuildRecipe {
        explicit_dependencies: Vec::new(),
        depfile: None,
        commands: Vec::new(),
    };
    let mut used = Used::none();

    for stmt in body {
        match stmt.statement {
            ast::BuildRecipeStmt::Let(ref let_stmt) => {
                let value = eval(scope, &let_stmt.value).await?;
                scope.set(let_stmt.ident.ident.to_string(), value);
            }
            ast::BuildRecipeStmt::From(ref expr) => {
                let value = eval(scope, &expr.param).await?;
                used |= value.used;
                let offset = evaluated.explicit_dependencies.len();
                value
                    .value
                    .collect_strings_into(&mut evaluated.explicit_dependencies);

                // Populate the `in` variable.
                scope.push_input_files(&evaluated.explicit_dependencies[offset..]);
            }
            ast::BuildRecipeStmt::Depfile(ref expr) => {
                let value = eval(scope, &expr.param).await?;
                used |= value.used;
                match value.value {
                    Value::String(depfile) => {
                        evaluated.depfile = Some(depfile);
                    }
                    Value::List(_) => {
                        return Err(EvalError::UnexpectedList(expr.span));
                    }
                }
            }
            ast::BuildRecipeStmt::Run(ref expr) => {
                used |= eval_run_exprs(scope, &expr.param, &mut evaluated.commands).await?;
            }
            ast::BuildRecipeStmt::Info(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated.commands.push(RunCommand::Info(message.value));
            }
            ast::BuildRecipeStmt::Warn(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated.commands.push(RunCommand::Warn(message.value));
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
}

pub(crate) async fn eval_task_recipe_statements(
    scope: &mut TaskRecipeScope<'_>,
    body: &[ast::BodyStmt<ast::TaskRecipeStmt<'_>>],
) -> Result<EvaluatedTaskRecipe, EvalError> {
    let mut evaluated = EvaluatedTaskRecipe {
        build: Vec::new(),
        commands: Vec::new(),
    };

    for stmt in body {
        match stmt.statement {
            ast::TaskRecipeStmt::Let(ref let_stmt) => {
                let value = eval(scope, &let_stmt.value).await?;
                scope.set(
                    let_stmt.ident.ident.to_string(),
                    Eval::inherent(value.value),
                );
            }
            ast::TaskRecipeStmt::Build(ref expr) => {
                let value = eval(scope, &expr.param).await?;
                value.value.collect_strings_into(&mut evaluated.build);
            }
            ast::TaskRecipeStmt::Run(ref expr) => {
                eval_run_exprs(scope, &expr.param, &mut evaluated.commands).await?;
            }
            ast::TaskRecipeStmt::Info(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated.commands.push(RunCommand::Info(message.value));
            }
            ast::TaskRecipeStmt::Warn(ref expr) => {
                let message = eval_string_expr(scope, &expr.param)?;
                evaluated.commands.push(RunCommand::Warn(message.value));
            }
        }
    }

    Ok(evaluated)
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
        ref value => flat_join(value, sep),
    }
}

fn recursive_resolve_path(
    span: Span,
    value: &mut Value,
    working_dir: &Absolute<werk_fs::Path>,
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
        Ok::<_, PathError>(())
    }).map_err(|err| EvalError::Path(span,err))?;

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
