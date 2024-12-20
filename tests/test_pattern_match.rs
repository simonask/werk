use werk_runner::{AmbiguousPatternError, Pattern, PatternMatch, PatternMatchData, PatternSet};

#[test]
fn test_pattern_match() -> anyhow::Result<()> {
    let empty = Pattern::parse("")?;
    let all = Pattern::parse("%")?;
    let specific = Pattern::parse("foo")?;
    let c_ext = Pattern::parse("%.c")?;

    assert_eq!(
        empty.match_string(""),
        Some(PatternMatchData::new(None::<&str>, []))
    );
    assert_eq!(empty.match_string("a"), None);

    assert_eq!(
        all.match_string(""),
        Some(PatternMatchData::new(Some(""), []))
    );
    assert_eq!(
        all.match_string("Hello, World!"),
        Some(PatternMatchData::new(Some("Hello, World!"), []))
    );

    assert_eq!(
        specific.match_string("foo"),
        Some(PatternMatchData::new(None::<&str>, []))
    );
    assert_eq!(specific.match_string("bar"), None);

    assert_eq!(
        c_ext.match_string(".c"),
        Some(PatternMatchData::new(Some(""), []))
    );
    assert_eq!(
        c_ext.match_string("a.c"),
        Some(PatternMatchData::new(Some("a"), []))
    );

    Ok(())
}

#[test]
fn test_capture_groups() -> anyhow::Result<()> {
    let abc = Pattern::parse("(a|b|c)")?;

    assert_eq!(
        abc.match_string("a"),
        Some(PatternMatchData::new(None::<&str>, [String::from("a")]))
    );
    assert_eq!(
        abc.match_string("b"),
        Some(PatternMatchData::new(None::<&str>, [String::from("b")]))
    );
    assert_eq!(
        abc.match_string("c"),
        Some(PatternMatchData::new(None::<&str>, [String::from("c")]))
    );

    let stem_abc = Pattern::parse("%(a|b|c)")?;
    assert_eq!(
        stem_abc.match_string("aaa"),
        Some(PatternMatchData::new(Some("aa"), [String::from("a")]))
    );
    assert_eq!(
        stem_abc.match_string("abc"),
        Some(PatternMatchData::new(Some("ab"), [String::from("c")]))
    );
    assert_eq!(
        stem_abc.match_string("bbc"),
        Some(PatternMatchData::new(Some("bb"), [String::from("c")]))
    );
    assert_eq!(stem_abc.match_string("bbd"), None);

    let abc_stem = Pattern::parse("(a|b|c)%")?;
    assert_eq!(
        abc_stem.match_string("aaa"),
        Some(PatternMatchData::new(Some("aa"), [String::from("a")]))
    );
    assert_eq!(
        abc_stem.match_string("abc"),
        Some(PatternMatchData::new(Some("bc"), [String::from("a")]))
    );
    assert_eq!(
        abc_stem.match_string("bbc"),
        Some(PatternMatchData::new(Some("bc"), [String::from("b")]))
    );
    assert_eq!(
        abc_stem.match_string("bbd"),
        Some(PatternMatchData::new(Some("bd"), [String::from("b")]))
    );
    assert_eq!(abc_stem.match_string("dbb"), None,);

    Ok(())
}

#[test]
fn ambiguous_pattern_set() -> anyhow::Result<()> {
    let pattern_set = PatternSet::new([Pattern::parse("foo/%/a.c")?, Pattern::parse("%/foo/a.c")?]);

    assert_eq!(
        pattern_set.best_match_string("foo/bar/a.c"),
        Ok(Some((
            0,
            PatternMatch {
                pattern: &pattern_set[0],
                data: PatternMatchData::new(Some("bar"), [])
            }
        )))
    );

    assert_eq!(
        pattern_set.best_match_string("bar/foo/a.c"),
        Ok(Some((
            1,
            PatternMatch {
                pattern: &pattern_set[1],
                data: PatternMatchData::new(Some("bar"), [])
            }
        )))
    );

    assert_eq!(
        pattern_set.best_match_string("foo/foo/a.c"),
        Err(AmbiguousPatternError {
            pattern1: String::from("foo/%/a.c"),
            pattern2: String::from("%/foo/a.c"),
            path: String::from("foo/foo/a.c"),
        })
    );

    Ok(())
}
