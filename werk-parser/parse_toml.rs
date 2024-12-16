use indexmap::IndexMap;

use crate::{ast, parse_string, Error};

pub fn parse_toml(input: &str) -> Result<ast::Root, Error> {
    let toml: toml_edit::DocumentMut = input.parse()?;
    parse_toml_document(&toml)
}

pub fn parse_toml_document(toml: &toml_edit::DocumentMut) -> Result<ast::Root, Error> {
    let root = toml.as_table();

    let mut config = ast::Config::default();
    let mut global = IndexMap::new();
    let mut command_rules = IndexMap::new();
    let mut out_rules = IndexMap::new();

    for (key, value) in root {
        let path = TomlPath {
            here: TomlPathComponent::Ident(key),
            parent: None,
        };

        match key {
            "config" => {
                let config_table = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("config".to_owned()))?;
                parse_config_table(&path, config_table, &mut config)?;
            }
            "global" => {
                let global_table = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("global".to_owned()))?;
                for (key, value) in global_table {
                    let path = path.ident(key);
                    global.insert(
                        parse_ident(&path, key)?,
                        parse_commented_item_expr(&path, value)?,
                    );
                }
            }
            "command" => {
                let command = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("command".to_owned()))?;
                for (key, value) in command {
                    let path = path.ident(key);
                    command_rules.insert(
                        parse_ident(&path, key)?,
                        parse_command_recipe(&path, value)?,
                    );
                }
            }
            "build" => {
                let out = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("out".to_owned()))?;
                for (key, value) in out {
                    let path = path.ident(key);
                    out_rules.insert(
                        parse_pattern_expr(&path, key)?,
                        parse_build_recipe(&path, value)?,
                    );
                }
            }
            _ => return Err(Error::InvalidKey(path.to_string())),
        }
    }

    Ok(ast::Root {
        config,
        global,
        commands: command_rules,
        recipes: out_rules,
    })
}

fn parse_config_table(
    path: &TomlPath,
    table: &toml_edit::Table,
    config: &mut ast::Config,
) -> Result<(), Error> {
    for (key, value) in table.iter() {
        match key {
            "out-dir" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.ident("out-dir").to_string()));
                };
                config.output_directory = Some(value.to_owned());
            }
            "edition" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.ident("edition").to_string()));
                };
                config.edition = Some(value.to_owned());
            }
            "print-commands" => {
                let Some(value) = value.as_bool() else {
                    return Err(Error::ExpectedString(
                        path.ident("print-commands").to_string(),
                    ));
                };
                config.print_commands = Some(value);
            }
            "default" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.ident("default").to_string()));
                };
                config.default = Some(value.to_owned());
            }
            _ => {
                return Err(Error::UnknownConfigKey(key.to_owned()));
            }
        }
    }

    Ok(())
}

fn parse_ident(path: &TomlPath, s: &str) -> Result<String, Error> {
    parse_string::parse_ident(s).map_err(|e| Error::InvalidIdent(path.to_string(), e))
}

fn parse_string_expr(path: &TomlPath, s: &str) -> Result<ast::StringExpr, Error> {
    parse_string::parse_string_expr(s).map_err(|e| Error::InvalidStringExpr(path.to_string(), e))
}

fn parse_pattern_expr(path: &TomlPath, s: &str) -> Result<ast::PatternExpr, Error> {
    parse_string::parse_pattern_expr(s).map_err(|e| Error::InvalidPatternExpr(path.to_string(), e))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExprType {
    String,
    From,
    Env,
    Shell,
    Which,
    Glob,
    Error,
}

impl ExprType {
    pub fn all_strs() -> &'static [&'static str] {
        &["env", "shell", "which", "glob", "from", "error"]
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "string" => Some(Self::String),
            "env" => Some(Self::Env),
            "shell" => Some(Self::Shell),
            "which" => Some(Self::Which),
            "glob" => Some(Self::Glob),
            "from" => Some(Self::From),
            "error" => Some(Self::Error),
            _ => None,
        }
    }
}

impl std::fmt::Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ExprType::String => "string",
            ExprType::From => "from",
            ExprType::Env => "env",
            ExprType::Shell => "shell",
            ExprType::Which => "which",
            ExprType::Glob => "glob",
            ExprType::Error => "error",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RunExprType {
    Shell,
    Write,
    Copy,
    Echo,
}

impl RunExprType {
    pub fn all_strs() -> &'static [&'static str] {
        &["shell", "write", "copy", "echo"]
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "shell" => Some(Self::Shell),
            "write" => Some(Self::Write),
            "copy" => Some(Self::Copy),
            "echo" => Some(Self::Echo),
            _ => None,
        }
    }
}

impl std::fmt::Display for RunExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            RunExprType::Shell => "shell",
            RunExprType::Write => "write",
            RunExprType::Copy => "copy",
            RunExprType::Echo => "echo",
        })
    }
}

fn find_main_expr_type<'a, I>(keys: I) -> Result<(ExprType, &'a toml_edit::Item), Error>
where
    I: IntoIterator<Item = (&'a str, &'a toml_edit::Item)>,
{
    let mut found = None;
    let mut iter = keys.into_iter();
    while let Some((key, item)) = iter.next() {
        if let Some(ty) = ExprType::from_str(key) {
            found = Some((ty, item));
            break;
        }
    }

    let Some(found) = found else {
        return Err(Error::ExpectedMainExpression);
    };

    while let Some((tail, _)) = iter.next() {
        if let Some(duplicate) = ExprType::from_str(&tail) {
            return Err(Error::AmbiguousMainExpression(found.0, duplicate));
        }
    }

    Ok(found)
}

fn find_main_run_expr_type<'a, I>(keys: I) -> Result<(RunExprType, &'a toml_edit::Item), Error>
where
    I: IntoIterator<Item = (&'a str, &'a toml_edit::Item)>,
{
    let mut found = None;
    let mut iter = keys.into_iter();
    while let Some((key, item)) = iter.next() {
        if let Some(ty) = RunExprType::from_str(key) {
            found = Some((ty, item));
            break;
        }
    }

    let Some(found) = found else {
        return Err(Error::ExpectedMainExpression);
    };

    while let Some((tail, _)) = iter.next() {
        if let Some(duplicate) = RunExprType::from_str(&tail) {
            return Err(Error::AmbiguousRunExpression(found.0, duplicate));
        }
    }

    Ok(found)
}

fn parse_table_expr<T: toml_edit::TableLike + ?Sized>(
    path: &TomlPath,
    table: &T,
) -> Result<ast::Expr, Error> {
    let expr_ty = find_main_expr_type(table.iter())?;
    let mut expr = match expr_ty {
        (ExprType::String, item) => {
            parse_item_string_expr(&path.ident("string"), item).map(ast::Expr::StringExpr)?
        }
        (ExprType::From, toml_edit::Item::Value(toml_edit::Value::String(s))) => {
            let ident = parse_ident(&path.ident("from"), s.value())?;
            ast::Expr::Ident(ident)
        }
        (ExprType::From, item) => parse_item_expr(&path.ident("from"), item)?,
        (ExprType::Env, item) => {
            parse_item_string_expr(&path.ident("env"), item).map(ast::Expr::Env)?
        }
        (ExprType::Shell, item) => {
            parse_item_string_expr(&path.ident("shell"), item).map(ast::Expr::Shell)?
        }
        (ExprType::Which, item) => {
            parse_item_string_expr(&path.ident("which"), item).map(ast::Expr::Which)?
        }
        (ExprType::Glob, item) => {
            parse_item_string_expr(&path.ident("glob"), item).map(ast::Expr::Glob)?
        }
        (ExprType::Error, item) => parse_item_string_expr(&path, item).map(ast::Expr::Error)?,
    };

    // Chaining expressions
    for (key, item) in table.iter() {
        // Skip the main expression - note that duplicates and ambiguous
        // expressions have already been detected.
        if let Some(_ty) = ExprType::from_str(key) {
            continue;
        }

        let path = path.ident(key);
        match key {
            "then" => {
                let then = parse_item_string_expr(&path, item)?;
                expr = ast::Expr::Then(Box::new(expr), Box::new(then));
            }
            "match" => {
                let Some(table) = item.as_table_like() else {
                    return Err(Error::ExpectedTable(path.to_string()));
                };

                let mut patterns = IndexMap::new();
                for (pattern, value) in table.iter() {
                    let path = path.ident(pattern);
                    let pattern = parse_pattern_expr(&path, pattern)?;
                    let value = parse_item_expr(&path, value)?;
                    if patterns.insert(pattern, value).is_some() {
                        return Err(Error::DuplicatePatternExpr(path.to_string()));
                    }
                }

                expr = ast::Expr::Match(Box::new(ast::MatchExpr {
                    input: expr,
                    patterns,
                }));
            }
            "join" => {
                let sep = parse_item_string_expr(&path, item)?;
                expr = ast::Expr::Join(Box::new(expr), Box::new(sep));
            }
            "warn" => {
                let message = parse_item_string_expr(&path, item)?;
                expr = ast::Expr::Message(Box::new(ast::MessageExpr {
                    inner: expr,
                    message,
                    message_type: ast::MessageType::Warning,
                }));
            }
            "info" => {
                let message = parse_item_string_expr(&path, item)?;
                expr = ast::Expr::Message(Box::new(ast::MessageExpr {
                    inner: expr,
                    message,
                    message_type: ast::MessageType::Info,
                }));
            }
            "patsubst" => {
                let Some(table) = item.as_table_like() else {
                    return Err(Error::ExpectedTable(path.to_string()));
                };

                let mut pattern = None;
                let mut replacement = None;

                for (key, item) in table.iter() {
                    let path = path.ident(key);
                    match key {
                        "pattern" => pattern = Some(parse_item_pattern_expr(&path, item)?),
                        "replacement" => replacement = Some(parse_item_string_expr(&path, item)?),
                        _ => return Err(Error::InvalidKey(path.to_string())),
                    }
                }

                let Some(pattern) = pattern else {
                    return Err(Error::ExpectedKey(path.to_string(), "pattern".to_owned()));
                };
                let Some(replacement) = replacement else {
                    return Err(Error::ExpectedKey(
                        path.to_string(),
                        "replacement".to_owned(),
                    ));
                };

                expr = ast::Expr::Patsubst(Box::new(ast::PatsubstExpr {
                    input: expr,
                    pattern,
                    replacement,
                }))
            }
            _ => return Err(Error::UnknownExpressionChain(path.to_string())),
        }
    }

    Ok(expr)
}

fn parse_value_expr(path: &TomlPath, toml: &toml_edit::Value) -> Result<ast::Expr, Error> {
    match toml {
        toml_edit::Value::String(formatted) => {
            parse_string_expr(path, formatted.value()).map(ast::Expr::StringExpr)
        }
        toml_edit::Value::Integer(_)
        | toml_edit::Value::Float(_)
        | toml_edit::Value::Boolean(_)
        | toml_edit::Value::Datetime(_) => Err(Error::ExpectedStringOrTable(path.to_string())),
        toml_edit::Value::Array(array) => {
            let mut exprs = Vec::with_capacity(array.len());
            for (i, value) in array.iter().enumerate() {
                let path = path.index(i);
                exprs.push(parse_value_expr(&path, value)?);
            }
            Ok(ast::Expr::List(exprs))
        }
        toml_edit::Value::InlineTable(inline_table) => parse_table_expr(path, inline_table),
    }
}

fn parse_item_expr(path: &TomlPath, toml: &toml_edit::Item) -> Result<ast::Expr, Error> {
    match toml {
        toml_edit::Item::None => Err(Error::ExpectedString(path.to_string())),
        toml_edit::Item::Value(value) => parse_value_expr(path, value),
        toml_edit::Item::Table(table) => parse_table_expr(path, table),
        toml_edit::Item::ArrayOfTables(array_of_tables) => {
            let mut exprs = Vec::with_capacity(array_of_tables.len());
            for (i, table) in array_of_tables.iter().enumerate() {
                let path = path.index(i);
                exprs.push(parse_table_expr(&path, table)?);
            }
            Ok(ast::Expr::List(exprs))
        }
    }
}

fn get_item_doc(toml: &toml_edit::Item) -> &str {
    let raw_str = match toml {
        toml_edit::Item::None | toml_edit::Item::ArrayOfTables(_) => return "",
        toml_edit::Item::Value(value) => value.decor().prefix(),
        toml_edit::Item::Table(table) => table.decor().prefix(),
    };
    raw_str
        .and_then(|raw| raw.as_str())
        .map(|s| s.trim())
        .unwrap_or("")
}

fn parse_commented_item_expr(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<ast::Commented<ast::Expr>, Error> {
    let doc_string = get_item_doc(toml);
    parse_item_expr(path, toml).map(|expr| ast::Commented {
        comment: doc_string.to_owned(),
        item: expr,
    })
}

fn parse_item_run_expr(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<Vec<ast::RunExpr>, Error> {
    let mut vec = Vec::new();
    parse_item_run_exprs_into(path, toml, &mut vec)?;
    Ok(vec)
}

fn parse_item_run_exprs_into(
    path: &TomlPath,
    toml: &toml_edit::Item,
    exprs: &mut Vec<ast::RunExpr>,
) -> Result<(), Error> {
    match toml {
        toml_edit::Item::None => return Ok(()),
        toml_edit::Item::Value(value) => parse_value_run_exprs_into(path, value, exprs),
        toml_edit::Item::Table(table) => {
            exprs.push(parse_table_run_expr(path, table)?);
            Ok(())
        }
        toml_edit::Item::ArrayOfTables(array_of_tables) => {
            for (index, table) in array_of_tables.iter().enumerate() {
                let path = path.index(index);
                let run_expr = parse_table_run_expr(&path, table)?;
                exprs.push(run_expr);
            }
            Ok(())
        }
    }
}

fn parse_value_run_exprs_into(
    path: &TomlPath,
    value: &toml_edit::Value,
    exprs: &mut Vec<ast::RunExpr>,
) -> Result<(), Error> {
    match value {
        toml_edit::Value::String(formatted) => {
            let string = parse_string_expr(path, formatted.value())?;
            exprs.push(ast::RunExpr::Shell(string));
            Ok(())
        }
        toml_edit::Value::Integer(_)
        | toml_edit::Value::Float(_)
        | toml_edit::Value::Boolean(_)
        | toml_edit::Value::Datetime(_) => Err(Error::ExpectedStringOrArray(path.to_string())),
        toml_edit::Value::Array(array) => {
            for (index, element) in array.iter().enumerate() {
                let path = path.index(index);
                parse_value_run_exprs_into(&path, element, exprs)?;
            }
            Ok(())
        }
        toml_edit::Value::InlineTable(table) => {
            exprs.push(parse_table_run_expr(path, table)?);
            Ok(())
        }
    }
}

fn parse_table_run_expr<T: toml_edit::TableLike>(
    path: &TomlPath,
    table: &T,
) -> Result<ast::RunExpr, Error> {
    let run_expr_ty = find_main_run_expr_type(table.iter())?;
    match run_expr_ty {
        (RunExprType::Shell, item) => {
            // TODO: Validate that there are no other keys.
            let path = path.ident("shell");
            parse_item_string_expr(&path, item).map(ast::RunExpr::Shell)
        }
        (RunExprType::Write, item) => {
            let path = path.ident("write");
            let target = parse_item_string_expr(&path, item)?;
            if let Some(data) = table.get("data") {
                let path = path.ident("data");
                let data = parse_item_expr(&path, data)?;
                Ok(ast::RunExpr::Write(target, data))
            } else {
                Err(Error::ExpectedKey(path.to_string(), "data".to_owned()))
            }
        }
        (RunExprType::Copy, item) => {
            let path = path.ident("copy");
            let source = parse_item_string_expr(&path, item)?;
            if let Some(destination) = table.get("to") {
                let path = path.ident("to");
                let to = parse_item_string_expr(&path, destination)?;
                Ok(ast::RunExpr::Copy(source, to))
            } else {
                Err(Error::ExpectedKey(path.to_string(), "to".to_owned()))
            }
        }
        (RunExprType::Echo, item) => {
            let path = path.ident("echo");
            let message = parse_item_string_expr(&path, item)?;
            Ok(ast::RunExpr::Echo(message))
        }
    }
}

fn parse_item_string_expr(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<ast::StringExpr, Error> {
    match toml {
        toml_edit::Item::Value(toml_edit::Value::String(s)) => parse_string_expr(path, s.value()),
        _ => Err(Error::ExpectedString(path.to_string())),
    }
}

fn parse_item_pattern_expr(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<ast::PatternExpr, Error> {
    match toml {
        toml_edit::Item::Value(toml_edit::Value::String(s)) => parse_pattern_expr(path, s.value()),
        _ => Err(Error::ExpectedString(path.to_string())),
    }
}

fn parse_command_recipe(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<ast::Commented<ast::CommandRecipe>, Error> {
    let Some(table) = toml.as_table_like() else {
        return Err(Error::ExpectedTable(path.to_string()));
    };

    let doc = get_item_doc(toml);
    let mut build = None;
    let mut command = Vec::new();
    let mut pre_message = None;
    let mut post_message = None;
    let mut capture = None;

    for (key, value) in table.iter() {
        let path = path.ident(key);
        match key {
            "build" => {
                build = Some(parse_item_expr(&path, value)?);
            }
            "command" => {
                command = parse_item_run_expr(&path, value)?;
            }
            "pre-message" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.to_string()));
                };
                pre_message = Some(parse_string_expr(&path, value)?);
            }
            "post-message" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.to_string()));
                };
                post_message = Some(parse_string_expr(&path, value)?);
            }
            "capture" => {
                let Some(value) = value.as_bool() else {
                    return Err(Error::ExpectedBool(path.to_string()));
                };
                capture = Some(value);
            }
            _ => {
                return Err(Error::InvalidKey(path.to_string()));
            }
        }
    }

    Ok(ast::Commented {
        comment: doc.to_owned(),
        item: ast::CommandRecipe {
            build,
            command,
            pre_message,
            post_message,
            capture,
        },
    })
}

fn parse_build_recipe(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<ast::Commented<ast::BuildRecipe>, Error> {
    let Some(table) = toml.as_table_like() else {
        return Err(Error::ExpectedTable(path.to_string()));
    };

    let doc = get_item_doc(toml);
    let mut in_files = None;
    let mut depfile = None;
    let mut command = Vec::new();
    let mut pre_message = None;
    let mut post_message = None;

    for (key, value) in table.iter() {
        let path = path.ident(key);
        match key {
            "in" => {
                in_files = Some(parse_item_expr(&path, value)?);
            }
            "depfile" | "depfiles" => {
                depfile = Some(parse_item_string_expr(&path, value)?);
            }
            "pre-message" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.to_string()));
                };
                pre_message = Some(parse_string_expr(&path, value)?);
            }
            "post-message" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(path.to_string()));
                };
                post_message = Some(parse_string_expr(&path, value)?);
            }
            "command" => {
                command = parse_item_run_expr(&path, value)?;
            }
            _ => {
                return Err(Error::InvalidKey(path.to_string()));
            }
        }
    }

    Ok(ast::Commented {
        comment: doc.to_owned(),
        item: ast::BuildRecipe {
            in_files,
            depfile,
            command,
            pre_message,
            post_message,
        },
    })
}

/// Path to a TOML value, for diagnostics.
struct TomlPath<'a> {
    pub here: TomlPathComponent<'a>,
    pub parent: Option<&'a TomlPath<'a>>,
}

enum TomlPathComponent<'a> {
    Ident(&'a str),
    Index(usize),
}

impl<'a> std::fmt::Display for TomlPathComponent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TomlPathComponent::Ident(ident) => ident.fmt(f),
            TomlPathComponent::Index(index) => index.fmt(f),
        }
    }
}

impl<'a> TomlPath<'a> {
    pub fn to_string(&self) -> String {
        if let Some(parent) = self.parent {
            let mut s = parent.to_string();
            match self.here {
                TomlPathComponent::Ident(ident) => {
                    s.push('.');
                    s.push_str(ident);
                }
                TomlPathComponent::Index(index) => {
                    s.push('[');
                    s.push_str(index.to_string().as_str());
                    s.push(']');
                }
            }
            s
        } else {
            self.here.to_string()
        }
    }

    pub fn ident<'b>(&'b self, here: &'b str) -> TomlPath<'b> {
        TomlPath {
            here: TomlPathComponent::Ident(here),
            parent: Some(self),
        }
    }

    pub fn index<'b>(&'b self, here: usize) -> TomlPath<'b> {
        TomlPath {
            here: TomlPathComponent::Index(here),
            parent: Some(self),
        }
    }
}
