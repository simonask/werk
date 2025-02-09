use tests::mock_io::*;
use werk_runner::Runner;

fn strip_colors(s: &str) -> String {
    use std::io::Write as _;
    let mut buf = Vec::new();
    let mut stream = anstream::StripStream::new(&mut buf);
    stream.write_all(s.as_bytes()).unwrap();
    String::from_utf8(buf).unwrap()
}

fn fix_newlines(s: &str) -> String {
    s.replace('\r', "")
}

async fn evaluate_check(file: &std::path::Path) -> Result<(), anyhow::Error> {
    let source = std::fs::read_to_string(file).unwrap();
    let test = Test::new(&source).map_err(|err| anyhow::Error::msg(err.to_string()))?;

    let workspace = test
        .create_workspace(&[])
        .map_err(|err| anyhow::Error::msg(err.to_string()))?;

    // Invoke the runner if there is a default target.
    if let Some(ref default_target) = workspace.default_target {
        let runner = Runner::new(&workspace);
        runner
            .build_or_run(default_target)
            .await
            .map_err(|err| anyhow::Error::msg(err.to_string()))?;
        test.run_pragma_tests()?;
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

macro_rules! error_case {
    ($name:ident) => {
        #[macro_rules_attribute::apply(smol_macros::test)]
        async fn $name() {
            _ = tracing_subscriber::fmt::try_init();

            let case_path = std::path::Path::new(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/fail/",
                stringify!($name),
                ".werk"
            ));
            let expected_path = std::path::Path::new(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/fail/",
                stringify!($name),
                ".txt"
            ));
            let expected_text = std::fs::read_to_string(expected_path).unwrap();

            let output = match evaluate_check(case_path).await {
                Ok(_) => panic!("expected error"),
                Err(err) => err.to_string(),
            };

            let output_stripped = fix_newlines(&strip_colors(&output));
            let expected_lf = fix_newlines(&expected_text);

            if output_stripped.trim() != expected_lf.trim() {
                eprintln!("Error message mismatch!");
                eprintln!("Got:\n{}", output);
                eprintln!("Expected:\n{}", expected_text);
                panic!("Error message mismatch");
            }
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
success_case!(string_interp);
success_case!(dedup);

error_case!(ambiguous_build_recipe);
error_case!(ambiguous_path_resolution);
error_case!(capture_group_out_of_bounds);
error_case!(duplicate_config);
