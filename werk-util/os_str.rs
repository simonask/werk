use std::{
    ffi::OsStr,
    fmt::{Debug, Write},
};

/// Display an command-line argument, in quotes it if it contains whitespace,
/// quotes, or special characters.
pub struct DisplayArg<'a>(pub &'a str);

impl std::fmt::Display for DisplayArg<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if contains_whitespace_or_control(self.0) {
            DisplayArgQuoted(self.0).fmt(f)
        } else {
            write_escape_control::<false>(self.0, f)
        }
    }
}

pub struct DisplayArgQuoted<'a>(pub &'a str);

impl std::fmt::Display for DisplayArgQuoted<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('"')?;
        write_escape_control::<true>(self.0, f)?;
        f.write_char('"')
    }
}

#[inline]
fn contains_whitespace_or_control(s: &str) -> bool {
    s.as_bytes()
        .iter()
        .any(|&b| b.is_ascii_whitespace() || b.is_ascii_control() || b == b'"')
}

#[inline]
fn write_escape_control<const ESCAPE_QUOTES: bool>(
    s: &str,
    f: &mut std::fmt::Formatter,
) -> std::fmt::Result {
    for ch in s.chars() {
        if ch.is_ascii_control() {
            ch.escape_default().fmt(f)?;
        } else if ESCAPE_QUOTES && ch == '"' {
            f.write_str("\\\"")?;
        } else {
            f.write_char(ch)?;
        }
    }
    Ok(())
}

#[inline]
#[must_use]
pub fn trim_os_str(s: &OsStr) -> &OsStr {
    let bytes = s.as_encoded_bytes().trim_ascii();
    unsafe {
        // SAFETY: Trimming ASCII whitespace cannot produce an invalid OsStr
        OsStr::from_encoded_bytes_unchecked(bytes)
    }
}
