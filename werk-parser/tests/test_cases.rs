use std::io::Write as _;

use werk_parser::*;
use werk_util::{AsDiagnostic as _, DiagnosticSource};

fn strip_colors(s: &str) -> String {
    let mut buf = Vec::new();
    let mut stream = anstream::StripStream::new(&mut buf);
    stream.write_all(s.as_bytes()).unwrap();
    String::from_utf8(buf).unwrap()
}

fn fix_newlines(s: &str) -> String {
    s.replace('\r', "")
}

macro_rules! error_case {
    ($t:ident) => {
        #[test]
        fn $t() {
            let input_path = concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/fail/",
                stringify!($t),
                ".werk"
            );
            let expected_path = concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/fail/",
                stringify!($t),
                ".txt"
            );

            let input = std::fs::read_to_string(input_path).unwrap();
            let expected = std::fs::read_to_string(expected_path).unwrap();
            let Err(err) = parse_werk(std::path::Path::new(input_path), &input) else {
                panic!("expected error, got Ok")
            };

            let rendered = err
                .into_diagnostic_error(DiagnosticSource::new(std::path::Path::new("INPUT"), &input))
                .to_string();

            let rendered_stripped = fix_newlines(&strip_colors(&rendered));
            let expected_lf = fix_newlines(&expected);

            if rendered_stripped.trim() != expected_lf.trim() {
                eprintln!("Error message mismatch!");
                eprintln!("Got:\n{rendered}\n");
                eprintln!("Expected:\n{expected}");
                panic!("Error message mismatch");
            }
        }
    };
}

macro_rules! success_case {
    ($t:ident) => {
        #[test]
        fn $t() {
            let input_path = concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/succeed/",
                stringify!($t),
                ".werk"
            );
            let expected_path = concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/succeed/",
                stringify!($t),
                ".json"
            );

            let input = std::fs::read_to_string(input_path).unwrap();
            let expected_json = std::fs::read_to_string(expected_path).unwrap();
            let input = match parse_werk(std::path::Path::new(input_path), &input) {
                Ok(input) => input,
                Err(err) => {
                    let rendered = err
                        .into_diagnostic_error(DiagnosticSource::new(
                            std::path::Path::new("INPUT"),
                            &input,
                        ))
                        .to_string();
                    eprintln!("Error message:\n{}", rendered);
                    panic!("error parsing input");
                }
            };

            let expected_ast = serde_json::from_str::<ast::Root>(&expected_json).unwrap();

            if input.root != expected_ast {
                let input_json = serde_json::to_string(&input.root).unwrap();
                eprintln!("AST mismatch!");
                // eprintln!("Expected:\n{:?}", expected_ast);
                // eprintln!("Got:\n{:?}\n", input.root);
                eprintln!("Got:\n{}\n", input_json);
                eprintln!("Expected:\n{}", expected_json);
                panic!("AST mismatch");
            }
        }
    };
}

error_case!(let_no_ident);
error_case!(let_no_eq);
error_case!(let_no_value);
error_case!(invalid_escape);
error_case!(root_invalid);
error_case!(expr_trailing_pipe);
error_case!(task_string_name);
error_case!(build_ident_name);
error_case!(match_unterminated);
error_case!(match_no_arrow);
error_case!(default_unknown_key);
error_case!(default_invalid_value);

success_case!(c);
success_case!(config);
success_case!(let_simple);
success_case!(let_simple_interp);
success_case!(let_match);
success_case!(let_match_inline);
success_case!(let_map);
success_case!(let_list);
success_case!(expr_parens);
