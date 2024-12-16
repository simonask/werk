use indexmap::IndexMap;

#[derive(Debug, PartialEq)]
pub struct Root {
    pub config: Config,
    pub global: IndexMap<String, Commented<Expr>>,
    pub commands: IndexMap<String, Commented<CommandRecipe>>,
    pub recipes: IndexMap<PatternExpr, Commented<BuildRecipe>>,
}

/// The `[config]` section of werk.toml.
#[derive(Debug, Default, PartialEq)]
pub struct Config {
    pub edition: Option<String>,
    pub output_directory: Option<String>,
    pub print_commands: Option<bool>,
    pub default: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct Commented<T> {
    pub comment: String,
    pub item: T,
}

impl<T> std::ops::Deref for Commented<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> std::ops::DerefMut for Commented<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Commented<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
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
    /// Given a list expression, flatten the list and join each element with
    /// separator.
    Join(Box<Expr>, Box<StringExpr>),
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

#[derive(Debug, PartialEq, Eq)]
pub struct MessageExpr {
    pub inner: Expr,
    pub message: StringExpr,
    pub message_type: MessageType,
}

impl std::hash::Hash for MessageExpr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Disregard the actual message in the hash, because the hash is used to
        // calculate outdatedness.
        self.inner.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatsubstExpr {
    pub input: Expr,
    pub pattern: PatternExpr,
    pub replacement: StringExpr,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchExpr {
    pub input: Expr,
    pub patterns: IndexMap<PatternExpr, Expr>,
}

impl std::hash::Hash for MatchExpr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.input.hash(state);
        for (pattern, expr) in &self.patterns {
            pattern.hash(state);
            expr.hash(state);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MessageType {
    Info,
    Warning,
}

/// Things that can appear in the `command` part of recipes.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RunExpr {
    /// Run shell command.
    Shell(StringExpr),
    /// Write the result of the expression to the path. The string is an OS path.
    Write(StringExpr, Expr),
    /// Copy one file to another.
    Copy(StringExpr, StringExpr),
    /// Print a message while running the command.
    Echo(StringExpr),
}

#[derive(Debug, PartialEq)]
pub struct CommandRecipe {
    pub build: Option<Expr>,
    pub command: Vec<RunExpr>,
    pub pre_message: Option<StringExpr>,
    pub post_message: Option<StringExpr>,
    pub capture: Option<bool>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct BuildRecipe {
    pub in_files: Option<Expr>,
    pub depfile: Option<StringExpr>,
    pub command: Vec<RunExpr>,
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
    Interpolation(Interpolation),
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
    Interpolation(Interpolation),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Interpolation {
    pub stem: InterpolationStem,
    pub options: InterpolationOptions,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct InterpolationOptions {
    /// `{stem:operation}`
    pub ops: Vec<InterpolationOp>,
    /// `{...*}` - This is not a normal operation because we need to treat it
    /// specially when building shell commands.
    pub join: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolationStem {
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
pub enum InterpolationOp {
    /// Replace extension - input must be path.
    ReplaceExtension(String, String),
    PrependEach(String),
    AppendEach(String),
    RegexReplace(RegexInterpolationOp),
    // Interpret the string as an OS path and resolve it. This is the `<..>`
    // interpolation syntax.
    ResolveOsPath,
}

#[derive(Clone, Debug)]
pub struct RegexInterpolationOp {
    pub regex: regex::Regex,
    pub replacer: String,
}

impl PartialEq for RegexInterpolationOp {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.replacer == other.replacer
    }
}

impl Eq for RegexInterpolationOp {}

impl std::hash::Hash for RegexInterpolationOp {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.regex.as_str().hash(state);
        self.replacer.hash(state);
    }
}
