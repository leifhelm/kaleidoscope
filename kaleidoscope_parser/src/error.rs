use kaleidoscope_error as error;

use nom::error::ParseError;

use crate::located::LocatedInput;

#[derive(Debug)]
pub enum ErrorKind {
    Nom(nom::error::ErrorKind),
    Char(char),
}

#[derive(Debug)]
pub struct Error<I> {
    errors: Vec<(I, ErrorKind)>,
}
impl<L: LocatedInput + std::fmt::Debug> Error<L> {
    pub fn to_error(self) -> Option<error::Error> {
        if self.errors.len() != 0 {
            let (slice, errorkind) = &self.errors[0];
            let error_str = format!("{:?}", self.errors);
            let position = slice.position();
            Some(error::Error::new(position..position, error_str))
        } else {
            None
        }
    }
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Error {
            errors: vec![(input, ErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, ErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Error {
            errors: vec![(input, ErrorKind::Char(c))],
        }
    }
}
