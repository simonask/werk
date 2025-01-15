use std::sync::OnceLock;

use tests::mock_io::*;
use toml_edit::de;
use werk_parser::ast;
use werk_runner::{Metadata, Runner};

struct PragmaRegexes {
    pub file: regex::Regex,
    pub assert_file: regex::Regex,
}

impl Default for PragmaRegexes {
    fn default() -> Self {
        Self {
            file: regex::Regex::new(r#"^#\!file (.*)=(.*)$"#).unwrap(),
            assert_file: regex::Regex::new(r#"^#\!assert-file (.*)=(.*)$"#).unwrap(),
        }
    }
}

fn regexes() -> &'static PragmaRegexes {
    static REGEXES: OnceLock<PragmaRegexes> = OnceLock::new();
    REGEXES.get_or_init(PragmaRegexes::default)
}

async fn evaluate_check(file: &std::path::Path) -> Result<(), anyhow::Error> {
    let source = std::fs::read_to_string(file).unwrap();
    let path = std::path::Path::new(file);
    let test = Test::new(&source)
        .map_err(|err| anyhow::Error::msg(err.with_location(path, &source).to_string()))?;

    // Interpret pragmas in the trailing comment of the werkfile.
    let trailing_whitespace = test
        .ast
        .get_whitespace(test.ast.root.ws_trailing)
        .trim()
        .lines();
    let regexes = regexes();
    let mut check_files = Vec::new();
    let mut fs = test.io.filesystem.lock();
    for line in trailing_whitespace {
        if let Some(captures) = regexes.file.captures(line) {
            let filename = captures.get(1).unwrap().as_str();
            let content = captures.get(2).unwrap().as_str();
            let path = workspace_file(filename);
            println!("INSERTING FILE {} with contents {content}", path.display());
            insert_fs(
                &mut *fs,
                &path,
                (
                    Metadata {
                        mtime: default_mtime(),
                        is_file: true,
                        is_symlink: false,
                    },
                    content.as_bytes().to_owned(),
                ),
            )
            .unwrap();
        } else if let Some(captures) = regexes.assert_file.captures(line) {
            let filename = captures.get(1).unwrap().as_str();
            let content = captures.get(2).unwrap().as_str();
            check_files.push((filename, content.as_bytes()));
        }
    }
    std::mem::drop(fs);

    let workspace = test.create_workspace(&[])?;

    // Invoke the runner if there is a default target.
    if let Some(ast::ConfigStmt {
        value: ast::ConfigValue::String(ast::ConfigString(_, default_target)),
        ..
    }) = test.ast.find_config("default")
    {
        let runner = Runner::new(&workspace);
        runner.build_or_run(default_target).await?;

        let fs = test.io.filesystem.lock();
        for (filename, contents) in &check_files {
            let out_file = output_file(filename);
            let (_entry, data) = read_fs(&fs, out_file.as_deref())?;
            assert_eq!(
                data, *contents,
                "assert-file failed: contents of output file `{filename}` do not match"
            );
        }
    }

    Ok(())
}

macro_rules! success_case {
    ($name:ident) => {
        #[macro_rules_attribute::apply(smol_macros::test)]
        async fn $name() {
            _ = tracing_subscriber::fmt::try_init();
            evaluate_check(std::path::Path::new(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/cases/",
                stringify!($name),
                ".werk"
            )))
            .await
            .unwrap();
        }
    };
}

success_case!(map);
success_case!(match_expr);
success_case!(flatten);
success_case!(join);
success_case!(split);
success_case!(discard);
success_case!(filter);
success_case!(write);
success_case!(copy);
success_case!(read);
