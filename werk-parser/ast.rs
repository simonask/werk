use indexmap::IndexMap;

#[derive(Debug, PartialEq)]
pub struct Root {
    pub config: Config,
    pub global: IndexMap<String, Expr>,
    pub commands: IndexMap<String, CommandRecipe>,
    pub recipes: IndexMap<PatternExpr, Recipe>,
}

/// The `[config]` section of werk.toml.
#[derive(Debug, Default, PartialEq)]
pub struct Config {
    pub edition: Option<String>,
    pub output_directory: Option<String>,
    pub print_commands: Option<bool>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Look up variable in scope.
    Ident(String),
    StringExpr(StringExpr),
    Shell(StringExpr),
    Glob(StringExpr),
    Which(StringExpr),
    Env(StringExpr),
    List(Vec<Expr>),
    Patsubst(Box<PatsubstExpr>),
    Match(Box<MatchExpr>),
    Then(Box<Expr>, Box<StringExpr>),
    Message(Box<MessageExpr>),
    Error(StringExpr),
}

impl Expr {
    pub fn literal(s: impl Into<String>) -> Self {
        Self::StringExpr(StringExpr::literal(s))
    }

    pub fn shell(s: impl Into<String>) -> Self {
        Self::Shell(StringExpr::literal(s))
    }
}

#[derive(Debug, PartialEq)]
pub struct MessageExpr {
    pub inner: Expr,
    pub message: StringExpr,
    pub message_type: MessageType,
}

#[derive(Debug, PartialEq)]
pub struct PatsubstExpr {
    pub input: Expr,
    pub pattern: PatternExpr,
    pub replacement: StringExpr,
}

#[derive(Debug, PartialEq)]
pub struct MatchExpr {
    pub input: Expr,
    pub when: IndexMap<PatternExpr, Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MessageType {
    Info,
    Warning,
}

#[derive(Debug, PartialEq)]
pub struct CommandRecipe {
    pub build: Option<Expr>,
    pub command: Option<Expr>,
    pub pre_message: Option<StringExpr>,
    pub post_message: Option<StringExpr>,
}

#[derive(Debug, PartialEq)]
pub struct Recipe {
    pub in_files: Option<Expr>,
    pub depfiles: Option<Expr>,
    pub command: Option<Expr>,
    pub pre_message: Option<StringExpr>,
    pub post_message: Option<StringExpr>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct StringExpr {
    pub fragments: Vec<StringFragment>,
}

impl StringExpr {
    pub fn literal(s: impl Into<String>) -> Self {
        Self {
            fragments: vec![StringFragment::Literal(s.into())],
        }
    }
}

/// Interpolated string fragment.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringFragment {
    Literal(String),
    /// `{...}`
    Interpolation(StringInterpolation),
}

impl Default for StringFragment {
    #[inline]
    fn default() -> Self {
        StringFragment::Literal(String::new())
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PatternExpr {
    pub fragments: Vec<PatternFragment>,
}

/// Interpolated pattern fragment (i.e., can have capture patterns like `%` and `(a|b|c)`).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PatternFragment {
    Literal(String),
    /// `%`
    PatternStem,
    /// `(a|b|c)`
    OneOf(Vec<String>),
    /// `{...}`
    Interpolation(StringInterpolation),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringInterpolation {
    pub stem: StringInterpolationStem,
    /// `{stem:operation}`
    pub operation: Option<StringInterpolationOperation>,
    /// `<...>` instead of `{...}`.
    pub interpolate_as_resolved_path: bool,
    /// `<...*>`
    pub join: Option<char>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringInterpolationStem {
    /// Empty stem; inherit output type from the interpolated value.
    Implied,
    /// `{%}` - output is string.
    PatternCapture,
    /// `{1}` - output is string.
    CaptureGroup(usize),
    /// `{ident}` - output is string.
    Ident(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringInterpolationOperation {
    /// Replace extension - input must be path.
    ReplaceExtension(String, String),
    PrependEach(String),
    AppendEach(String),
}
