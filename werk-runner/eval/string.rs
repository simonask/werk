use std::{borrow::Cow, fmt::Write as _, sync::Arc};

use indexmap::IndexMap;
use werk_fs::Absolute;
use werk_parser::ast;
use werk_util::{DiagnosticFileId, DiagnosticSpan, Symbol};

use crate::{
    AmbiguousPatternError, EvalError, Lookup, LookupValue, Pattern, PatternMatcher, PatternRegex,
    PatternRegexCaptures, Scope, ShellCommandLine, StringFlags, StringValue, Value, Warning,
    Workspace,
};

use super::{Eval, ResolvePathMode, Used, UsedVariable};

/// Normal string builder, not sensitive to argument quoting.
pub(crate) struct StringBuilder<'a, P: ?Sized> {
    scope: &'a P,
    string: String,
    flags: StringFlags,
    used: Used,
}

impl<'a, P: Scope + ?Sized> StringBuilder<'a, P> {
    pub fn new(scope: &'a P) -> Self {
        Self {
            scope,
            string: String::new(),
            flags: StringFlags::EMPTY,
            used: Used::default(),
        }
    }

    pub fn eval_fragment(
        &mut self,
        fragment: &ast::StringFragment,
        span: DiagnosticSpan,
    ) -> Result<(), EvalError> {
        match fragment {
            ast::StringFragment::Literal(lit) => self.string.push_str(lit),
            ast::StringFragment::PatternStem => {
                if let Some(value) = self.scope.get(Lookup::PatternStem) {
                    let (string, flags) = find_first_string(&value);
                    self.string.push_str(string);
                    self.used |= value.used();
                    self.flags |= flags;
                } else {
                    self.scope.warning(&Warning::NoPatternStem(span));
                }
            }
            ast::StringFragment::Interpolation(interp) => {
                self.flags |= eval_string_interpolation(
                    self.scope,
                    span,
                    interp,
                    ResolvePathMode::Infer,
                    &mut self.string,
                    &mut self.used,
                )?;
            }
        }

        Ok(())
    }

    pub fn eval(
        &mut self,
        file: DiagnosticFileId,
        expr: &ast::StringExpr,
    ) -> Result<(), EvalError> {
        for fragment in &expr.fragments {
            self.eval_fragment(fragment, expr.span.with_file(file))?;
        }
        Ok(())
    }

    pub fn build(self) -> Eval<StringValue> {
        Eval {
            value: StringValue {
                string: self.string,
                flags: self.flags,
            },
            used: self.used,
        }
    }
}

pub(crate) struct PatternBuilder<'a, P: ?Sized> {
    scope: &'a P,
    fragments: Vec<PatternFragment>,
    used: Used,
    span: DiagnosticSpan,
}

enum PatternFragment {
    Literal(String),
    PatternStem,
    OneOf(Vec<String>),
}

impl<'a, P: Scope + ?Sized> PatternBuilder<'a, P> {
    pub fn new(scope: &'a P, span: DiagnosticSpan) -> Self {
        Self {
            scope,
            fragments: vec![PatternFragment::Literal(String::new())],
            used: Used::default(),
            span,
        }
    }

    pub fn ensure_absolute_path(&mut self) {
        match self.fragments.get_mut(0) {
            Some(PatternFragment::Literal(s)) => {
                if !s.starts_with(werk_fs::Path::SEPARATOR) {
                    s.insert(0, werk_fs::Path::SEPARATOR);
                }
            }
            _ => self
                .fragments
                .insert(0, PatternFragment::Literal(werk_fs::Path::SEPARATOR.into())),
        }
    }

    fn push_str(&mut self, s: &str) {
        let buf = if let Some(PatternFragment::Literal(last)) = self.fragments.last_mut() {
            last
        } else {
            self.fragments.push(PatternFragment::Literal(String::new()));
            let Some(PatternFragment::Literal(last)) = self.fragments.last_mut() else {
                unreachable!()
            };
            last
        };
        buf.push_str(s);
    }

    pub fn eval(
        &mut self,
        file: DiagnosticFileId,
        expr: &ast::PatternExpr,
    ) -> Result<(), EvalError> {
        self.span = expr.span.with_file(file);
        for fragment in &expr.fragments {
            self.eval_fragment(fragment)?;
        }
        Ok(())
    }

    pub fn eval_fragment(&mut self, fragment: &ast::PatternFragment) -> Result<(), EvalError> {
        match fragment {
            ast::PatternFragment::Literal(lit) => self.push_str(lit),
            ast::PatternFragment::PatternStem => self.fragments.push(PatternFragment::PatternStem),
            ast::PatternFragment::OneOf(alternatives) => {
                self.fragments
                    .push(PatternFragment::OneOf(alternatives.clone()));
            }
            ast::PatternFragment::Interpolation(interp) => {
                let buf = if let Some(PatternFragment::Literal(last)) = self.fragments.last_mut() {
                    last
                } else {
                    self.fragments.push(PatternFragment::Literal(String::new()));
                    let Some(PatternFragment::Literal(last)) = self.fragments.last_mut() else {
                        unreachable!()
                    };
                    last
                };

                let flags = eval_string_interpolation(
                    self.scope,
                    self.span,
                    interp,
                    ResolvePathMode::Illegal,
                    buf,
                    &mut self.used,
                )?;

                if flags.contains(StringFlags::CONTAINS_PATHS) {
                    return Err(EvalError::ResolvePathInPattern(self.span));
                }
            }
        }

        Ok(())
    }

    fn append_regex_fragments(&self, buf: &mut String) -> PatternRegexCaptures {
        let mut capture_count = 0;
        let mut stem_capture_index = None;
        let mut num_normal_capture_groups = 0;
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(lit) => regex_syntax::escape_into(lit, buf),
                PatternFragment::PatternStem => {
                    buf.push_str("(.*)");
                    stem_capture_index = Some(capture_count);
                    capture_count += 1;
                }
                PatternFragment::OneOf(variants) => {
                    buf.push('(');
                    for (i, variant) in variants.iter().enumerate() {
                        if i != 0 {
                            buf.push('|');
                        }
                        regex_syntax::escape_into(variant, buf);
                    }
                    buf.push(')');
                    capture_count += 1;
                    num_normal_capture_groups += 1;
                }
            }
        }

        PatternRegexCaptures {
            stem_capture_index,
            num_normal_capture_groups,
        }
    }

    /// Build the pattern as a regex that matches substrings, like in `split`.
    fn build_partial_regex_inner(&self) -> PatternRegex {
        let mut buf = String::new();
        let captures = self.append_regex_fragments(&mut buf);
        PatternRegex {
            regex: regex::Regex::new(&buf).expect("pattern builder did not produce a valid regex"),
            captures,
        }
    }

    /// Build the pattern as a regex that only matches whole strings, like in
    /// `match` arms and build recipes.
    fn build_whole_regex_inner(&self) -> PatternRegex {
        let mut buf = String::from("^");
        let captures = self.append_regex_fragments(&mut buf);
        buf.push('$');
        PatternRegex {
            regex: regex::Regex::new(&buf).expect("pattern builder did not produce a valid regex"),
            captures,
        }
    }

    pub fn build_partial_regex(self) -> Eval<PatternRegex> {
        let value = self.build_partial_regex_inner();
        Eval {
            value,
            used: self.used,
        }
    }

    /// Build the pattern either as a literal-match pattern or a regex pattern
    /// that matches the whole string (if it contains stems or capture groups)
    pub fn build(self) -> Eval<Pattern> {
        let span = self.span;

        // The builder guarantees that if there are no capture groups or a stem,
        // the builder contains exactly one literal fragment.
        if matches!(&*self.fragments, [PatternFragment::Literal(_)]) {
            let Some(PatternFragment::Literal(lit)) = self.fragments.into_iter().next() else {
                unreachable!()
            };
            return Eval {
                value: Pattern {
                    span,
                    string: lit.clone(),
                    matcher: PatternMatcher::Literal(lit),
                },
                used: self.used,
            };
        }

        let string = self.to_string();
        let pattern_regex = self.build_whole_regex_inner();

        Eval {
            value: Pattern {
                span,
                string,
                matcher: PatternMatcher::Regex(pattern_regex),
            },
            used: self.used,
        }
    }
}

impl<P: ?Sized> std::fmt::Display for PatternBuilder<'_, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            match fragment {
                PatternFragment::Literal(lit) => {
                    werk_parser::parser::escape_pattern_literal(lit).fmt(f)?;
                }
                PatternFragment::PatternStem => f.write_char('%')?,
                PatternFragment::OneOf(cows) => {
                    f.write_char('(')?;
                    for (i, cow) in cows.iter().enumerate() {
                        if i != 0 {
                            f.write_char('|')?;
                        }
                        f.write_str(cow)?;
                    }
                    f.write_char(')')?;
                }
            }
        }
        Ok(())
    }
}

pub(crate) struct CommandLineBuilder<'a, P: ?Sized> {
    scope: &'a P,
    in_quotes: Option<InQuotes>,
    escape: bool,
    parts: Vec<String>,
    used: Used,
    span: DiagnosticSpan,
}

enum InQuotes {
    Single,
    Double,
}

impl<'a, P: Scope + ?Sized> CommandLineBuilder<'a, P> {
    pub fn new(scope: &'a P, span: DiagnosticSpan) -> Self {
        Self {
            scope,
            in_quotes: None,
            escape: false,
            parts: Vec::new(),
            used: Used::default(),
            span,
        }
    }

    fn push_char_data(&mut self, ch: char) {
        if let Some(last) = self.parts.last_mut() {
            last.push(ch);
        } else {
            self.parts.push(ch.to_string());
        }
    }

    fn push_string_data(&mut self, s: &str) {
        if let Some(last) = self.parts.last_mut() {
            last.push_str(s);
        } else if !s.is_empty() {
            self.parts.push(s.to_owned());
        }
    }

    fn push_string_interpreting_quotes_and_spaces(&mut self, s: &str) {
        for ch in s.chars() {
            if self.escape {
                self.escape = false;
                self.push_char_data(ch);
            } else if ch == '\\' {
                self.escape = true;
            } else if ch == '"' {
                match self.in_quotes {
                    Some(InQuotes::Single) => self.push_char_data('"'),
                    Some(InQuotes::Double) => self.in_quotes = None,
                    None => self.in_quotes = Some(InQuotes::Double),
                }
            } else if ch == '\'' {
                match self.in_quotes {
                    Some(InQuotes::Single) => self.in_quotes = None,
                    Some(InQuotes::Double) => self.push_char_data('\''),
                    None => self.in_quotes = Some(InQuotes::Single),
                }
            } else if ch.is_whitespace() && self.in_quotes.is_none() {
                // Saw whitespace, and not currently inside quotes, so start a
                // new argument.
                if !self.parts.last().is_some_and(std::string::String::is_empty) {
                    self.parts.push(String::new());
                }
            } else {
                self.push_char_data(ch);
            }
        }
    }

    pub fn eval_fragment(&mut self, fragment: &ast::StringFragment) -> Result<(), EvalError> {
        match fragment {
            ast::StringFragment::Literal(lit) => {
                self.push_string_interpreting_quotes_and_spaces(lit);
            }
            ast::StringFragment::PatternStem => {
                if let Some(value) = self.scope.get(Lookup::PatternStem) {
                    let (string, _) = find_first_string(&value);
                    self.push_string_data(string);
                    self.used |= value.used();
                } else {
                    self.scope.warning(&Warning::NoPatternStem(self.span));
                }
            }
            ast::StringFragment::Interpolation(interp) => {
                // Pass as separate arguments when there is a join operation and its separator is whitespace.
                let pass_as_separate_arguments = interp
                    .options
                    .as_deref()
                    .and_then(|ops| ops.join.as_deref())
                    .is_some_and(|sep| sep.chars().all(char::is_whitespace));

                if self.in_quotes.is_some() || !pass_as_separate_arguments {
                    // Inside quotes: Evaluate the interpolation normally,
                    // appending it to the current argument.
                    let buf = if let Some(last) = self.parts.last_mut() {
                        last
                    } else {
                        self.parts.push(String::new());
                        let Some(last) = self.parts.last_mut() else {
                            unreachable!()
                        };
                        last
                    };
                    eval_string_interpolation(
                        self.scope,
                        self.span,
                        interp,
                        ResolvePathMode::Infer,
                        buf,
                        &mut self.used,
                    )?;
                } else {
                    // Outside quotes, and if there is a join separator, it is
                    // whitespace. Separate the interpolated values into
                    // separate arguments.
                    let value = eval_string_interpolation_stem(self.scope, self.span, interp.stem)?;
                    self.used |= value.used();

                    let value = eval_string_interpolation_ops(
                        self.span,
                        value.into_value(),
                        interp.options.as_deref().map_or(&[], |ops| &*ops.ops),
                        self.scope.workspace(),
                        ResolvePathMode::Infer,
                    )?;

                    // If we just saw an unquoted space that started a new argument,
                    if self.parts.last().is_some_and(std::string::String::is_empty) {
                        self.parts.pop();
                    }

                    value.visit(|arg| {
                        if !arg.trim().is_empty() {
                            // If the last argument is empty, replace it. Otherwise, push a new argument.
                            if let Some(last) = self.parts.last_mut() {
                                if last.is_empty() {
                                    *last = arg.string;
                                    return;
                                }
                            }

                            self.parts.push(arg.string);
                        }
                    });
                }
            }
        }

        Ok(())
    }

    pub fn eval(&mut self, expr: &ast::StringExpr) -> Result<(), EvalError> {
        self.span.span = expr.span;
        for fragment in &expr.fragments {
            self.eval_fragment(fragment)?;
        }
        Ok(())
    }

    pub fn build(mut self) -> Result<Eval<ShellCommandLine>, EvalError> {
        if self.in_quotes.is_some() {
            Err(EvalError::UnterminatedQuote(self.span))
        } else {
            let mut parts = self.parts.into_iter();
            let Some(program) = parts.next() else {
                return Err(EvalError::EmptyCommand(self.span));
            };

            let (program_path, hash) = self
                .scope
                .workspace()
                .which(&program)
                .map_err(|err| EvalError::CommandNotFound(self.span, program.clone(), err))?;
            self.used
                .vars
                .extend(hash.map(|hash| UsedVariable::Which(Symbol::new(&program), hash)));

            Ok(Eval {
                value: ShellCommandLine {
                    program: program_path.into_owned(),
                    arguments: parts.collect(),
                },
                used: self.used,
            })
        }
    }
}

fn eval_string_interpolation<P: Scope + ?Sized>(
    scope: &P,
    span: DiagnosticSpan,
    interp: &ast::Interpolation,
    resolve_mode: ResolvePathMode,
    buf: &mut String,
    used: &mut Used,
) -> Result<StringFlags, EvalError> {
    let value = eval_string_interpolation_stem(scope, span, interp.stem)?;
    *used |= value.used();

    if let Some(ref options) = interp.options {
        if !options.ops.is_empty() || options.join.is_some() {
            return eval_string_interpolation_ops_and_join(
                span,
                value.into_value(),
                buf,
                options,
                scope.workspace(),
                resolve_mode,
            );
        }
    }

    let value = if let Some(ref index) = interp.index {
        let index = match index {
            ast::InterpolationIndex::Const(index) => *index,
            ast::InterpolationIndex::Ident(ident) => {
                let lookup = scope
                    .get(Lookup::Ident(*ident))
                    .ok_or(EvalError::NoSuchIdentifier(span, ident.to_string()))?;
                *used |= lookup.used();
                let (index_string, _) = find_first_string(&lookup);
                index_string
                    .parse()
                    .map_err(|_| EvalError::ExpectedInt(span, index_string.to_owned()))?
            }
        };

        value
            .index(index)
            .ok_or(EvalError::IndexOutOfBounds(span, index, value.len()))?
    } else {
        &value
    };

    let (s, flags) = find_first_string(value);
    buf.push_str(s);

    if flags.contains(StringFlags::CONTAINS_PATHS) && resolve_mode == ResolvePathMode::Illegal {
        return Err(EvalError::ResolvePathInPattern(span));
    }

    Ok(flags)
}

fn eval_string_interpolation_stem<P: Scope + ?Sized>(
    scope: &P,
    span: DiagnosticSpan,
    stem: ast::InterpolationStem,
) -> Result<LookupValue<'_>, EvalError> {
    Ok(match stem {
        ast::InterpolationStem::Implied => scope
            .get(Lookup::Implied)
            .ok_or(EvalError::NoImpliedValue(span))?,
        ast::InterpolationStem::PatternCapture => {
            if let Some(stem) = scope.get(Lookup::PatternStem) {
                stem
            } else {
                scope.warning(&Warning::NoPatternStem(span));
                LookupValue::Owned(Eval::inherent(Value::String(StringValue::default())))
            }
        }
        ast::InterpolationStem::CaptureGroup(group) => scope
            .get(Lookup::CaptureGroup(group))
            .ok_or(EvalError::NoSuchCaptureGroup(span, group))?,
        ast::InterpolationStem::Ident(ident) => scope
            .get(Lookup::Ident(ident))
            .ok_or_else(|| EvalError::NoSuchIdentifier(span, ident.as_ref().to_owned()))?,
    })
}

fn eval_string_interpolation_ops_and_join(
    span: DiagnosticSpan,
    mut value: Value,
    buf: &mut String,
    options: &ast::InterpolationOptions,
    workspace: &Workspace,
    default_resolve_mode: ResolvePathMode,
) -> Result<StringFlags, EvalError> {
    value =
        eval_string_interpolation_ops(span, value, &options.ops, workspace, default_resolve_mode)?;

    if let (Value::List(_), Some(sep)) = (&value, options.join.as_deref()) {
        Ok(recursive_join_push(&value, sep, buf))
    } else {
        let (s, flags) = find_first_string(&value);
        buf.push_str(s);
        Ok(flags)
    }
}

fn eval_string_interpolation_ops(
    span: DiagnosticSpan,
    mut value: Value,
    options: &[ast::InterpolationOp],
    workspace: &Workspace,
    default_resolve_mode: ResolvePathMode,
) -> Result<Value, EvalError> {
    let mut resolve_mode = default_resolve_mode;

    for op in options {
        match op {
            ast::InterpolationOp::Dedup => {
                value = dedup_recursive(value);
            }
            ast::InterpolationOp::Filename => {
                recursive_into_filename(&mut value);
            }
            ast::InterpolationOp::Dirname => {
                recursive_into_dirname(&mut value);
            }
            ast::InterpolationOp::Ext => {
                recursive_into_ext(&mut value);
            }
            ast::InterpolationOp::ReplaceExtension { from, to } => {
                recursive_replace_extension(&mut value, from, to);
            }
            ast::InterpolationOp::PrependEach(prefix) => recursive_prepend_each(&mut value, prefix),
            ast::InterpolationOp::AppendEach(suffix) => recursive_append_each(&mut value, suffix),
            ast::InterpolationOp::RegexReplace(r) => {
                recursive_regex_replace(&mut value, &r.regex, &r.replacer);
            }
            ast::InterpolationOp::ResolveOsPath => {
                recursive_resolve_path(
                    span,
                    &mut value,
                    werk_fs::Path::ROOT,
                    workspace,
                    resolve_mode,
                )?;
            }
            ast::InterpolationOp::ResolveOutDir => {
                resolve_mode = ResolvePathMode::OutDir;
            }
            ast::InterpolationOp::ResolveWorkspace => {
                resolve_mode = ResolvePathMode::Workspace;
            }
        }
    }

    Ok(value)
}

fn find_first_string(value: &Value) -> (&str, StringFlags) {
    match value {
        Value::List(values) => {
            if let Some(first) = values.first() {
                find_first_string(first)
            } else {
                ("", StringFlags::EMPTY)
            }
        }
        Value::String(string_value) => (&string_value.string, string_value.flags),
    }
}

fn recursive_join_push(value: &Value, sep: &str, buf: &mut String) -> StringFlags {
    let mut first = true;
    let mut flags = StringFlags::EMPTY;
    value.visit_ref(|s| {
        if !first {
            buf.push_str(sep);
        }
        first = false;
        buf.push_str(&s.string);
        flags |= s.flags;
    });
    flags
}

pub(crate) fn flat_join(values: &Value, sep: &str) -> StringValue {
    let mut flattened = String::new();
    let mut flags = StringFlags::EMPTY;
    let mut num_strings = 0;
    values.visit_ref(|s| {
        if num_strings != 0 {
            flattened.push_str(sep);
        }
        flags |= s.flags;
        num_strings += 1;
        flattened.push_str(s);
    });

    StringValue {
        string: flattened,
        flags,
    }
}

fn recursive_resolve_path(
    span: DiagnosticSpan,
    value: &mut Value,
    working_dir: &Absolute<werk_fs::Path>,
    workspace: &Workspace,
    resolve_mode: ResolvePathMode,
) -> Result<(), EvalError> {
    value.try_visit_mut(&mut |string: &mut StringValue| -> Result<(), EvalError> {
        if string.flags.contains(StringFlags::CONTAINS_PATHS) {
            return Err(EvalError::DoubleResolvePath(span));
        }

        let path = werk_fs::Path::new(string).map_err(|err| EvalError::Path(span, err))?;
        let path = path.absolutize(working_dir).map_err(|err| EvalError::Path(span, err))?;
        let path = match resolve_mode {
            ResolvePathMode::Infer => resolve_path_infer(span, &path, workspace)?,
            ResolvePathMode::OutDir => path.resolve(workspace.output_directory()),
            ResolvePathMode::Workspace => path.resolve(workspace.project_root()),
            ResolvePathMode::Illegal => return Err(EvalError::ResolvePathInPattern(span)),
        };
        let Some(path) = path.to_str() else {
            panic!("Path resolution produced a non-UTF8 path; probably the project root path is non-UTF8")
        };

        path.clone_into(&mut string.string);
        string.flags = StringFlags::CONTAINS_PATHS;
        Ok::<_, EvalError>(())
    })
}

fn resolve_path_infer(
    span: DiagnosticSpan,
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
    value.visit_mut(&mut |s: &mut StringValue| {
        if s.ends_with(from) {
            s.string.truncate(s.len() - from.len());
            s.string.push_str(to);
        }
    });
}

fn recursive_into_filename(value: &mut Value) {
    value.visit_mut(&mut |s: &mut StringValue| {
        if s.flags.contains(StringFlags::CONTAINS_PATHS) {
            let path = std::path::Path::new(s);
            if let Some(filename) = path.file_name() {
                s.string = filename.to_string_lossy().into_owned();
            }
            s.flags = StringFlags::EMPTY;
        } else if let Ok(path) = werk_fs::Path::new(s) {
            let filename = path.file_name();
            s.string = filename.to_string();
        }
    });
}

fn recursive_into_dirname(value: &mut Value) {
    value.visit_mut(&mut |s: &mut StringValue| {
        if s.flags.contains(StringFlags::CONTAINS_PATHS) {
            let path = std::path::Path::new(s);
            if let Some(parent) = path.parent() {
                s.string = parent.to_string_lossy().into_owned();
            }
        } else if let Ok(path) = werk_fs::Path::new(s) {
            if let Some(parent) = path.parent() {
                s.string = parent.to_string();
            } else {
                s.string = werk_fs::Path::ROOT.to_string();
            }
        }
    });
}

fn recursive_into_ext(value: &mut Value) {
    value.visit_mut(&mut |s: &mut StringValue| {
        if s.flags.contains(StringFlags::CONTAINS_PATHS) {
            let path = std::path::Path::new(s);
            if let Some(ext) = path.extension() {
                s.string = ext.to_string_lossy().into_owned();
            }
            s.flags = StringFlags::EMPTY;
        } else if let Ok(path) = werk_fs::Path::new(s) {
            if let Some(ext) = path.extension() {
                s.string = ext.to_string();
            }
        }
    });
}

fn recursive_prepend_each(value: &mut Value, prefix: &str) {
    value.visit_mut(|s| {
        s.string.insert_str(0, prefix);
    });
}

fn recursive_append_each(value: &mut Value, suffix: &str) {
    value.visit_mut(|s| {
        s.string.push_str(suffix);
    });
}

fn recursive_regex_replace(value: &mut Value, regex: &regex::Regex, replacer: &str) {
    value.visit_mut(|s| {
        // regex guarantees Cow::Borrowed means that nothing was replaced.
        let Cow::Owned(replaced) = regex.replace(s, replacer) else {
            return;
        };
        s.string = replaced;
    });
}

pub(crate) fn dedup_recursive(value: Value) -> Value {
    match value {
        Value::String(_) => value,
        Value::List(_) => {
            let mut map = IndexMap::<String, StringFlags, ahash::RandomState>::default();
            value.visit(|s| {
                map.entry(s.string)
                    .and_modify(|flags| *flags |= s.flags)
                    .or_insert(s.flags);
            });
            Value::List(
                map.into_iter()
                    .map(|(string, flags)| Value::String(StringValue { string, flags }))
                    .collect(),
            )
        }
    }
}
