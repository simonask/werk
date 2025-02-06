use tests::mock_io::*;
use werk_parser::ast;
use werk_runner::Runner;

async fn evaluate_check(file: &std::path::Path) -> Result<(), anyhow::Error> {
    let source = std::fs::read_to_string(file).unwrap();
    let test = Test::new(&source).map_err(|err| anyhow::Error::msg(err.to_string()))?;
    let ast = &test.ast;

    let workspace = match test.create_workspace(&[]) {
        Ok(workspace) => workspace,
        Err(err) => {
            eprintln!("{}", err);
            panic!("evaluation failed")
        }
    };

    // Invoke the runner if there is a default target.
    if let Some(ast::ConfigStmt {
        value: ast::ConfigValue::String(ast::ConfigString(_, default_target)),
        ..
    }) = ast.find_config("default")
    {
        let runner = Runner::new(&workspace);
        runner.build_or_run(default_target).await?;
        test.run_pragma_tests();
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
success_case!(env);
