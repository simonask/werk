use tests::mock_io::*;
use werk_parser::parser::{pattern_expr_inside_quotes, Input};
use werk_runner::{Pattern, PatternMatchData, RootScope};

fn parse_and_compile_pattern(scope: &RootScope, pattern: &str) -> Pattern {
    let expr = pattern_expr_inside_quotes(&mut Input::new(pattern)).unwrap();
    werk_runner::eval::eval_pattern(scope, &expr).unwrap().value
}

#[test]
fn test_pattern_match() -> anyhow::Result<()> {
    let test = Test::new("").unwrap();
    let workspace = test.create_workspace(&[]).unwrap();
    let scope = RootScope::new(&workspace);

    let empty = parse_and_compile_pattern(&scope, "");
    let all = parse_and_compile_pattern(&scope, "%");
    let specific = parse_and_compile_pattern(&scope, "foo");
    let c_ext = parse_and_compile_pattern(&scope, "%.c");

    assert_eq!(
        empty.match_whole_string(""),
        Some(PatternMatchData::default())
    );
    assert_eq!(empty.match_whole_string("a"), None);

    assert_eq!(
        all.match_whole_string(""),
        Some(PatternMatchData::new(Some(""), None::<&str>))
    );
    assert_eq!(
        all.match_whole_string("Hello, World!"),
        Some(PatternMatchData::new(Some("Hello, World!"), None::<&str>))
    );

    assert_eq!(
        specific.match_whole_string("foo"),
        Some(PatternMatchData::default())
    );
    assert_eq!(specific.match_whole_string("bar"), None);

    assert_eq!(
        c_ext.match_whole_string(".c"),
        Some(PatternMatchData::new(Some(""), None::<&str>))
    );
    assert_eq!(
        c_ext.match_whole_string("a.c"),
        Some(PatternMatchData::new(Some("a"), None::<&str>))
    );

    Ok(())
}

#[test]
fn test_capture_groups() -> anyhow::Result<()> {
    let test = Test::new("").unwrap();
    let workspace = test.create_workspace(&[]).unwrap();
    let scope = RootScope::new(&workspace);

    let abc = parse_and_compile_pattern(&scope, "(a|b|c)");

    assert_eq!(
        abc.match_whole_string("a"),
        Some(PatternMatchData::new(None::<&str>, [String::from("a")]))
    );
    assert_eq!(
        abc.match_whole_string("b"),
        Some(PatternMatchData::new(None::<&str>, [String::from("b")]))
    );
    assert_eq!(
        abc.match_whole_string("c"),
        Some(PatternMatchData::new(None::<&str>, [String::from("c")]))
    );

    let stem_abc = parse_and_compile_pattern(&scope, "%(a|b|c)");
    assert_eq!(
        stem_abc.match_whole_string("aaa"),
        Some(PatternMatchData::new(Some("aa"), [String::from("a")]))
    );
    assert_eq!(
        stem_abc.match_whole_string("abc"),
        Some(PatternMatchData::new(Some("ab"), [String::from("c")]))
    );
    assert_eq!(
        stem_abc.match_whole_string("bbc"),
        Some(PatternMatchData::new(Some("bb"), [String::from("c")]))
    );
    assert_eq!(stem_abc.match_whole_string("bbd"), None);

    let abc_stem = parse_and_compile_pattern(&scope, "(a|b|c)%");
    assert_eq!(
        abc_stem.match_whole_string("aaa"),
        Some(PatternMatchData::new(Some("aa"), [String::from("a")]))
    );
    assert_eq!(
        abc_stem.match_whole_string("abc"),
        Some(PatternMatchData::new(Some("bc"), [String::from("a")]))
    );
    assert_eq!(
        abc_stem.match_whole_string("bbc"),
        Some(PatternMatchData::new(Some("bc"), [String::from("b")]))
    );
    assert_eq!(
        abc_stem.match_whole_string("bbd"),
        Some(PatternMatchData::new(Some("bd"), [String::from("b")]))
    );
    assert_eq!(abc_stem.match_whole_string("dbb"), None,);

    Ok(())
}
