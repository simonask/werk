pub mod ast;
pub mod parse;
mod pattern;

use indexmap::IndexMap;
pub use pattern::*;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Toml(#[from] toml_edit::TomlError),
    #[error("unknown root key: {0}")]
    InvalidKey(String),
    #[error("expected table: {0}")]
    ExpectedTable(String),
    #[error("expected table containing 'shell', 'glob', or 'string': {0}")]
    InvalidExprTable(String),
    #[error("expected string: {0}")]
    ExpectedString(String),
    #[error("expected string or table: {0}")]
    ExpectedStringOrTable(String),
    #[error("expected string or array: {0}")]
    ExpectedStringOrArray(String),
    #[error("invalid identifier '{0}': {1}")]
    InvalidIdent(String, parse::Error),
    #[error("invalid string expression '{0}': {1}")]
    InvalidStringExpr(String, parse::Error),
    #[error("invalid pattern expression '{0}': {1}")]
    InvalidPatternExpr(String, parse::Error),
}

pub fn parse_toml(input: &str) -> Result<ast::Root, Error> {
    let toml: toml_edit::DocumentMut = input.parse()?;
    parse_toml_document(&toml)
}

pub fn parse_toml_document(toml: &toml_edit::DocumentMut) -> Result<ast::Root, Error> {
    let root = toml.as_table();

    let mut global = IndexMap::new();
    let mut command_rules = IndexMap::new();
    let mut out_rules = IndexMap::new();

    for (key, value) in root {
        let path = TomlPath {
            here: TomlPathComponent::Ident(key),
            parent: None,
        };

        match key {
            "global" => {
                let global_table = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("global".to_owned()))?;
                for (key, value) in global_table {
                    let path = path.ident(key);
                    global.insert(parse_ident(&path, key)?, parse_item_expr(&path, value)?);
                }
            }
            "command" => {
                let command = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("command".to_owned()))?;
                for (key, value) in command {
                    let path = path.ident(key);
                    command_rules
                        .insert(parse_ident(&path, key)?, parse_command_rule(&path, value)?);
                }
            }
            "out" => {
                let out = value
                    .as_table()
                    .ok_or_else(|| Error::ExpectedTable("out".to_owned()))?;
                for (key, value) in out {
                    let path = path.ident(key);
                    out_rules.insert(
                        parse_pattern_expr(&path, key)?,
                        parse_out_rule(&path, value)?,
                    );
                }
            }
            _ => return Err(Error::InvalidKey(path.to_string())),
        }
    }

    Ok(ast::Root {
        global,
        commands: command_rules,
        recipes: out_rules,
    })
}

fn parse_ident(path: &TomlPath, s: &str) -> Result<String, Error> {
    parse::parse_ident(s).map_err(|e| Error::InvalidIdent(path.to_string(), e))
}

fn parse_string_expr(path: &TomlPath, s: &str) -> Result<ast::StringExpr, Error> {
    parse::parse_string_expr(s).map_err(|e| Error::InvalidStringExpr(path.to_string(), e))
}

fn parse_pattern_expr(path: &TomlPath, s: &str) -> Result<ast::PatternExpr, Error> {
    parse::parse_pattern_expr(s).map_err(|e| Error::InvalidPatternExpr(path.to_string(), e))
}

fn parse_table_expr<T: toml_edit::TableLike + ?Sized>(
    path: &TomlPath,
    table: &T,
) -> Result<ast::Expr, Error> {
    if let Some(shell) = table.get("shell") {
        if table.len() != 1 {
            return Err(Error::InvalidExprTable(path.to_string()));
        }
        let path = path.ident("shell");
        if let Some(shell) = shell.as_str() {
            Ok(ast::Expr::Shell(parse_string_expr(&path, shell)?))
        } else {
            Err(Error::ExpectedString(path.to_string()))
        }
    } else if let Some(string) = table.get("string") {
        if table.len() != 1 {
            return Err(Error::InvalidExprTable(path.to_string()));
        }
        let path = path.ident("string");
        if let Some(string) = string.as_str() {
            Ok(ast::Expr::StringExpr(parse_string_expr(&path, string)?))
        } else {
            Err(Error::ExpectedString(path.to_string()))
        }
    } else if let Some(glob) = table.get("glob") {
        let then = if let Some(then) = table.get("then") {
            if table.len() != 2 {
                return Err(Error::InvalidExprTable(path.to_string()));
            }
            Some(Box::new(parse_item_expr(&path.ident("then"), then)?))
        } else {
            None
        };

        let path = path.ident("glob");
        if let Some(glob) = glob.as_str() {
            Ok(ast::Expr::Glob(ast::Glob {
                pattern: parse_string_expr(&path, glob)?,
                then,
            }))
        } else {
            Err(Error::ExpectedString(path.to_string()))
        }
    } else {
        return Err(Error::InvalidExprTable(path.to_string()));
    }
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

fn parse_command_rule(
    path: &TomlPath,
    toml: &toml_edit::Item,
) -> Result<ast::CommandRecipe, Error> {
    let Some(table) = toml.as_table_like() else {
        return Err(Error::ExpectedTable(path.to_string()));
    };

    let mut build = None;
    let mut command = None;
    let mut pre_message = None;
    let mut post_message = None;

    for (key, value) in table.iter() {
        let path = path.ident(key);
        match key {
            "build" => {
                build = Some(parse_item_expr(&path, value)?);
            }
            "command" => {
                command = Some(parse_item_expr(&path, value)?);
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
            _ => {
                return Err(Error::InvalidKey(path.to_string()));
            }
        }
    }

    Ok(ast::CommandRecipe {
        build,
        command,
        pre_message,
        post_message,
    })
}

fn parse_out_rule(path: &TomlPath, toml: &toml_edit::Item) -> Result<ast::Recipe, Error> {
    let Some(table) = toml.as_table_like() else {
        return Err(Error::ExpectedTable(path.to_string()));
    };

    let mut in_files = None;
    let mut depfiles = None;
    let mut command = None;
    let mut pre_message = None;
    let mut post_message = None;

    for (key, value) in table.iter() {
        let path = path.ident(key);
        match key {
            "in" => {
                in_files = Some(parse_item_expr(&path, value)?);
            }
            "depfile" | "depfiles" => {
                depfiles = Some(parse_item_expr(&path, value)?);
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
                command = Some(parse_item_expr(&path, value)?);
            }
            _ => {
                return Err(Error::InvalidKey(path.to_string()));
            }
        }
    }

    Ok(ast::Recipe {
        in_files,
        depfiles,
        command,
        pre_message,
        post_message,
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
