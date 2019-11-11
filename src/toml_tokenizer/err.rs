use std::{fmt, io};

#[derive(PartialEq)]
pub(crate) enum TomlErrorKind {
    UnexpectedToken(String),
    InternalParseError(String),
}

#[derive(PartialEq)]
pub(crate) struct ParseTomlError {
    pub(super) kind: TomlErrorKind,
    pub(super) info: String,
}

impl ParseTomlError {
    pub(crate) fn new(s: String, t_err: TomlErrorKind) -> ParseTomlError {
        ParseTomlError { kind: t_err, info: s }
    }
}

impl From<io::Error> for ParseTomlError {
    fn from(e: io::Error) -> ParseTomlError {
        let msg = e.to_string();
        ParseTomlError::new(
            msg,
            TomlErrorKind::InternalParseError("? opperator returned error".to_owned()),
        )
    }
}

impl From<ParseTomlError> for io::Error {
    fn from(e: ParseTomlError) -> io::Error {
        match e.kind {
            TomlErrorKind::InternalParseError(info) => io::Error::new(io::ErrorKind::Other, info),
            TomlErrorKind::UnexpectedToken(info) => io::Error::new(io::ErrorKind::Other, info),
        }
    }
}

impl fmt::Debug for ParseTomlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for ParseTomlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = match &self.kind {
            TomlErrorKind::InternalParseError(ref span) => span,
            TomlErrorKind::UnexpectedToken(ref span) => span,
        };
        write!(f, "{}, found {:?}", self.info, span)
    }
}

impl std::error::Error for ParseTomlError {}
