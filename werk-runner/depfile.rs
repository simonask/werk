use std::path::PathBuf;

#[derive(Default)]
pub struct Depfile {
    pub target: PathBuf,
    pub deps: Vec<PathBuf>,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum DepfileError {
    #[error("depfile is empty")]
    Empty,
    #[error("error parsing depfile: {0}")]
    ParseError(&'static str),
    #[error("depfile is not valid UTF-8")]
    InvalidUtf8(#[from] std::str::Utf8Error),
}

impl Depfile {
    pub fn parse(depfile_contents: &[u8]) -> Result<Depfile, DepfileError> {
        let depfile_contents = std::str::from_utf8(depfile_contents)?;
        let mut lines = depfile_contents.lines();

        let Some(first) = lines.next() else {
            return Err(DepfileError::Empty);
        };

        // Note: `:` cannot be followed by a space in Windows paths, so it
        // should work to split the string by `: `.
        let (target, rest) = first.split_once(": ").ok_or_else(|| {
            DepfileError::ParseError("character sequence ': ' not found on the first line")
        })?;

        let mut deps = Vec::new();
        deps.push(parse_dep_line(rest.trim()));
        for line in lines {
            let line = line.trim();
            if !line.is_empty() {
                deps.push(parse_dep_line(line));
            }
        }

        Ok(Depfile {
            target: PathBuf::from(target.to_owned()),
            deps,
        })
    }
}

fn parse_dep_line(trimmed: &str) -> PathBuf {
    if let Some(stripped) = trimmed.strip_suffix('\\') {
        PathBuf::from(stripped.trim().to_owned())
    } else {
        PathBuf::from(trimmed.to_owned())
    }
}
