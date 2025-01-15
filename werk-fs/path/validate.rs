use super::PathError;

pub fn validate(path: &str) -> Result<(), PathError> {
    if path.is_empty() {
        return Err(PathError::Empty);
    }

    if path == "/" {
        return Ok(());
    }

    let mut components = path.split('/');

    let Some(first) = components.next() else {
        return Err(PathError::Empty);
    };

    let mut current;

    // The first path component is allowed to be empty only if there are
    // subsequent components (note that the root path is special-cased above).
    if first.is_empty() {
        // Require at least one non-empty component.
        let Some(second) = components.next() else {
            // No path separators present.
            return Err(PathError::Empty);
        };
        current = second;
    } else {
        current = first;
    }

    loop {
        let next = components.next();
        if next.is_none() && current.is_empty() {
            return Err(PathError::EndsWithSeparator);
        }
        validate_component(current)?;
        if let Some(next) = next {
            current = next;
        } else {
            break;
        }
    }

    Ok(())
}

fn validate_component(component: &str) -> Result<(), PathError> {
    validate_reserved(component)?;

    let mut chars = component.chars();
    let Some(mut ch) = chars.next() else {
        return Err(PathError::Empty);
    };

    if ch.is_whitespace() {
        return Err(PathError::StartsWithWhitespace);
    }

    loop {
        let next = chars.next();
        validate_char(ch)?;
        if let Some(next) = next {
            ch = next;
        } else {
            if ch.is_whitespace() {
                return Err(PathError::EndsWithWhitespace);
            } else if ch == '/' {
                return Err(PathError::EndsWithSeparator);
            } else if ch == '.' {
                return Err(PathError::EndsWithDot);
            }

            break;
        }
    }

    Ok(())
}

const fn validate_char(ch: char) -> Result<(), PathError> {
    // Must be a printable char.
    match ch {
        // C0 control characters
        '\0'..='\x1F' | '\x7F' => return Err(PathError::IllegalChar(ch)),
        // C1 control characters
        '\u{80}'..='\u{9F}' => return Err(PathError::IllegalChar(ch)),
        // Specially disallowed
        '\\' | ':' => return Err(PathError::IllegalChar(ch)),
        _ => Ok(()),
    }
}

/// Validate whether a path component is a reserved Windows filename (ignoring its extension, if any).
fn validate_reserved(component: &str) -> Result<(), PathError> {
    const CON: &'static str = "CON";
    const PRN: &'static str = "PRN";
    const AUX: &'static str = "AUX";
    const NUL: &'static str = "NUL";
    const COM: &'static str = "COM";
    const LPT: &'static str = "LPT";

    const CON_BYTES: &'static [u8; 3] = b"CON";
    const PRN_BYTES: &'static [u8; 3] = b"PRN";
    const AUX_BYTES: &'static [u8; 3] = b"AUX";
    const NUL_BYTES: &'static [u8; 3] = b"NUL";
    const COM_BYTES: &'static [u8; 3] = b"COM";
    const LPT_BYTES: &'static [u8; 3] = b"LPT";

    let stem = component
        .split_once('.')
        .map(|(stem, _)| stem)
        .unwrap_or(component);

    let len = stem.len();

    // Fast exit for trivially good paths.
    if len < 3 || len > 5 {
        return Ok(());
    }
    let stem_bytes = stem.as_bytes();

    let prefix: [u8; 3] = [
        stem_bytes[0].to_ascii_uppercase(),
        stem_bytes[1].to_ascii_uppercase(),
        stem_bytes[2].to_ascii_uppercase(),
    ];

    match &prefix {
        CON_BYTES if len == 3 => Err(PathError::ReservedStem(&CON)),
        PRN_BYTES if len == 3 => Err(PathError::ReservedStem(&PRN)),
        AUX_BYTES if len == 3 => Err(PathError::ReservedStem(&AUX)),
        NUL_BYTES if len == 3 => Err(PathError::ReservedStem(&NUL)),
        COM_BYTES if len >= 4 && is_rest_digit(&stem[3..]) => Err(PathError::ReservedStem(&COM)),
        LPT_BYTES if len >= 4 && is_rest_digit(&stem[3..]) => Err(PathError::ReservedStem(&LPT)),
        _ => Ok(()),
    }
}

const fn is_superscript_digit(s: &str) -> bool {
    let b = s.as_bytes();
    match b {
        [0xc2, 0xb9 | 0xb2 | 0xb3] => true,
        _ => false,
    }
}

const fn is_ascii_digit(s: &str) -> bool {
    s.len() == 1 && {
        let b = s.as_bytes()[0];
        matches!(b, b'0'..=b'9')
    }
}

const fn is_rest_digit(s: &str) -> bool {
    is_ascii_digit(s) || is_superscript_digit(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fail_empty() {
        assert_eq!(validate(""), Err(PathError::Empty));
        assert_eq!(validate("/"), Ok(()));
        assert_eq!(validate("foo/"), Err(PathError::EndsWithSeparator));
        assert_eq!(validate("/foo"), Ok(()));
        assert_eq!(validate("foo//bar"), Err(PathError::Empty));
        assert_eq!(validate("foo/bar"), Ok(()));
        assert_eq!(validate("/foo/bar"), Ok(()));
        assert_eq!(validate("/foo/bar/"), Err(PathError::EndsWithSeparator));
    }

    #[test]
    fn fail_space() {
        assert_eq!(validate(" foo"), Err(PathError::StartsWithWhitespace));
        assert_eq!(validate("foo "), Err(PathError::EndsWithWhitespace));
        assert_eq!(validate("foo bar"), Ok(()));
        assert_eq!(validate("foo bar "), Err(PathError::EndsWithWhitespace));
        assert_eq!(validate("foo bar/baz"), Ok(()));
        assert_eq!(validate("foo bar/baz "), Err(PathError::EndsWithWhitespace));
    }

    #[test]
    fn fail_non_printable() {
        assert_eq!(validate("foo\x00bar"), Err(PathError::IllegalChar('\x00')));
        assert_eq!(validate("foo\x1Fbar"), Err(PathError::IllegalChar('\x1F')));
        assert_eq!(validate("foo\x7Fbar"), Err(PathError::IllegalChar('\x7F')));
        assert_eq!(
            validate("foo\u{80}bar"),
            Err(PathError::IllegalChar('\u{80}'))
        );
        assert_eq!(
            validate("foo\u{9F}bar"),
            Err(PathError::IllegalChar('\u{9F}'))
        );
    }

    #[test]
    fn fail_windows_reserved() {
        assert_eq!(
            validate_reserved("CON"),
            Err(PathError::ReservedStem(&"CON"))
        );
        assert_eq!(
            validate_reserved("PRN"),
            Err(PathError::ReservedStem(&"PRN"))
        );
        assert_eq!(
            validate_reserved("AUX"),
            Err(PathError::ReservedStem(&"AUX"))
        );
        assert_eq!(
            validate_reserved("NUL"),
            Err(PathError::ReservedStem(&"NUL"))
        );
        assert_eq!(
            validate_reserved("COM1"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM2"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM3"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM4"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM5"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM6"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM7"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM8"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM9"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(validate_reserved("COM10"), Ok(()));
        assert_eq!(validate_reserved("COM11"), Ok(()));
        assert_eq!(validate_reserved("COM12"), Ok(()));
        assert_eq!(validate_reserved("COM13"), Ok(()));
        assert_eq!(validate_reserved("COM14"), Ok(()));
        assert_eq!(validate_reserved("COM15"), Ok(()));
        assert_eq!(validate_reserved("COM16"), Ok(()));

        assert_eq!(
            validate_reserved("LPT1"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT2"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT3"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT4"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT5"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT6"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT7"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT8"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("LPT9"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(validate_reserved("LPT10"), Ok(()));

        assert_eq!(
            validate_reserved("CON.txt"),
            Err(PathError::ReservedStem(&"CON"))
        );
        assert_eq!(
            validate_reserved("CON.txt.txt"),
            Err(PathError::ReservedStem(&"CON"))
        );
        assert_eq!(
            validate_reserved("COM1.txt"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("COM1.txt.txt"),
            Err(PathError::ReservedStem(&"COM"))
        );
        assert_eq!(
            validate_reserved("lpt1.txt"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("lpt\u{00B9}.txt"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("lpt\u{00B2}.txt"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(
            validate_reserved("lpt\u{00B3}.txt"),
            Err(PathError::ReservedStem(&"LPT"))
        );
        assert_eq!(validate_reserved("lpt\u{00B9}æ.txt"), Ok(()));

        assert_eq!(validate_reserved("æø"), Ok(()))
    }
}
