use kaleidoscope_error as error;

use nom::error::ParseError;

use crate::located::LocatedInput;

#[derive(Debug)]
pub enum ErrorKind {
    Nom(nom::error::ErrorKind),
    Char(char),
}

impl ErrorKind {
    pub fn message(&self) -> String {
        match self {
            ErrorKind::Nom(err) => format!("Expected {}", err.description()),
            ErrorKind::Char(c) => format!("Expected `{}`", c),
        }
    }
}

#[derive(Debug)]
pub struct Error<I> {
    primary_error: (I, ErrorKind),
    secondary_errors: Vec<(I, ErrorKind)>,
}
impl<L: LocatedInput + std::fmt::Debug> Error<L> {
    pub fn to_error(self) -> error::Error {
        let (slice, errorkind) = self.primary_error;
            let position = slice.position();
            error::Error::new(position..position, errorkind.message())
    }
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Error {
            primary_error: (input, ErrorKind::Nom(kind)),
            secondary_errors: vec![],
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.secondary_errors.push((input, ErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Error {
            primary_error: (input, ErrorKind::Char(c)),
            secondary_errors: vec![],
        }
    }
}
