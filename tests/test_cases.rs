use std::sync::Arc;

use tests::mock_io::*;

fn evaluate_check(file: &std::path::Path) -> Result<(), werk_runner::EvalError> {
    let source = std::fs::read_to_string(file).unwrap();
    let path = std::path::Path::new(file);
    let ast = werk_parser::parse_werk(&source)
        .map_err(|err| anyhow::Error::msg(err.with_location(path, &source).to_string()))
        .unwrap();
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(MockIo::default().with_default_workspace_dir());
    match werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    ) {
        Ok(_) => Ok(()),
        Err(werk_runner::Error::Eval(err)) => Err(err),
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

macro_rules! success_case {
    ($name:ident) => {
        #[test]
        fn $name() {
            evaluate_check(std::path::Path::new(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/cases/",
                stringify!($name),
                ".werk"
            )))
            .unwrap();
        }
    };
}

macro_rules! success_cases {
    ($($name:ident),* $(,)?) => {
        $(success_case!($name);)*
    };
}

success_cases!(map, match_expr, flatten, join, split, discard, filter);
