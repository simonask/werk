use std::io::Write as _;

use werk_parser::*;

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
                "/tests/cases/",
                stringify!($t),
                ".werk"
            );
            let expected_path = concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/cases/",
                stringify!($t),
                ".txt"
            );

            let input = std::fs::read_to_string(input_path).unwrap();
            let expected = std::fs::read_to_string(expected_path).unwrap();
            let Err(err) = parse_werk(&input) else {
                panic!("expected error, got Ok")
            };

            let rendered = LocatedError {
                file_name: std::path::Path::new("INPUT"),
                source_code: &input,
                error: err,
            }
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

error_case!(let_no_ident);
error_case!(let_no_eq);
error_case!(invalid_escape);
