use werk_fs::Absolute;
use werk_util::{AnnotateLevelExt as _, DiagnosticSpan, Level};

#[derive(Clone, Debug, thiserror::Error)]
pub enum Warning {
    #[error("no patterns in scope containing a stem `%`")]
    NoPatternStem(DiagnosticSpan),
    #[error("no implicit value in scope")]
    NoImpliedValue(DiagnosticSpan),
    #[error("ignoring unresolved path")]
    IgnoringUnresolvedPath(DiagnosticSpan, String),
    #[error("ignoring non-absolute OS path")]
    IgnoringNonAbsolutePath(DiagnosticSpan, std::path::PathBuf),
    #[error("ignoring path outside output directory")]
    IgnoringPathOutsideOutputDirectory(DiagnosticSpan, Absolute<std::path::PathBuf>),
    #[error("ignoring file not found")]
    IgnoringFileNotFound(DiagnosticSpan, Absolute<std::path::PathBuf>),
    #[error("depfile was not generated by the recipe, and there was no rule to generate it")]
    DepfileNotGenerated(DiagnosticSpan, Absolute<std::path::PathBuf>),
    #[error("{1}")]
    WarningExpression(DiagnosticSpan, String),
    #[error("unused define `{0}`")]
    UnusedDefine(String),
    #[error("output directory changed; was `{0}`, is now `{1}`")]
    OutputDirectoryChanged(Absolute<std::path::PathBuf>, Absolute<std::path::PathBuf>),
}

impl werk_util::AsDiagnostic for Warning {
    fn as_diagnostic(&self) -> werk_util::Diagnostic {
        let level = Level::Warning;

        match self {
            Warning::NoPatternStem(span) => level
                .diagnostic("W0010")
                .annotation(span.annotation(level,
                    "this string uses `%`, but it will evaluate to an empty string",
                ))
                .footer("perhaps you meant to escape the literal character: `\\%`"),
            Warning::NoImpliedValue(span) => level
                .diagnostic("W0011")
                .annotation(span.annotation(level,

                "this string uses the implicit value `{}` or `<>`, but the current expression has no implicit value",
            )).footer("use an expression chain `lhs | rhs` to introduce an implicit value"),
            Warning::IgnoringUnresolvedPath(span, path) => level
                .diagnostic("W0020")
                .annotation(span.annotation(level,

                    format!("path `{path}` is not resolved"),
                )),
            Warning::IgnoringNonAbsolutePath(span, path_buf) => level
                .diagnostic("W0021")
                .annotation(span.annotation(level,

                    format!("path `{}` is not absolute", path_buf.display()),
                ))
                .footer("use \"<...>\" to convert abstract paths to native OS paths"),
            Warning::IgnoringPathOutsideOutputDirectory(span, absolute) => level
                .diagnostic("W0022")
                .annotation(span.annotation(level,

                   format!("path `{}` is outside the output directory", absolute.display()),
                ))
                .footer("use \"<...:out-dir>\" to unambiguously produce an absolute path in the output directory"),
            Warning::IgnoringFileNotFound(span, absolute) => level
                .diagnostic("W0023")
                .annotation(span.annotation(level,

                    format!("file `{}` not found", absolute.display()),
                )),
            Warning::DepfileNotGenerated(span, absolute) => level
                .diagnostic("W0030")
                .annotation(span.annotation(level,

                    format!("expected this rule to write `{}`", absolute.display()),
                ))
                .footer(
                    "pass `-MMD`, `-MF`, or similar flags to your compiler to implicitly generate a depfile",)
                    .footer(
                    "or add a recipe to generate the depfile"
                ),
            Warning::WarningExpression(span, message) => level
                .diagnostic("W9999")
                .annotation(span.annotation(level, message)),
            Warning::UnusedDefine(key) => level
                .diagnostic("W1000")
                .footer(format_args!("no `config` statement exists with the name `{key}`"))
                .footer("maybe a `let` statement should be changed to a `config` statement?"),
            Warning::OutputDirectoryChanged(..) => level
                .diagnostic("W1001")
        }.title(self) // Use Display impl from thiserror
    }
}
