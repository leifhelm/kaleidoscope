use std::io::ErrorKind;

use bunt::termcolor::Buffer;

macro_rules! error {
    ($target:expr, $format_str:literal $(, $arg:expr)*) => {
        if let Err(err) = bunt::writeln!($target, ["{$red+bold}error:{/$} ", $format_str] $(, $arg)*) {
            print_logging_error(&err);
        }
    };
}
pub fn print_logging_error(error: &std::io::Error) {
    eprintln!("error: An error occured during logging:\n{:?}", error);
}
#[derive(Debug)]
pub enum ApplicationError {
    FileSystem {
        error: std::io::Error,
        file_name: String,
        operation: FileOperation,
    },
    LoggingError(std::io::Error),
}

impl ApplicationError {
    pub fn log(&self, mut buffer: &mut Buffer) {
        match self {
            ApplicationError::FileSystem {
                error,
                file_name,
                operation,
            } => match error.kind() {
                ErrorKind::NotFound => error!(
                    buffer,
                    "The file {[blue+underline]} was not found", file_name
                ),
                ErrorKind::AlreadyExists => error!(
                    buffer,
                    "The file {[blue+underline]} already exists", file_name
                ),
                ErrorKind::PermissionDenied => error!(
                    buffer,
                    "Permission denied when {} the file {[blue+underline]}",
                    operation.to_error_str(),
                    file_name
                ),
                _ => error!(
                    buffer,
                    "An error occured when {} the file {[blue+underline]}:\n{[italic]:?}",
                    operation.to_error_str(),
                    file_name,
                    error
                ),
            },
            ApplicationError::LoggingError(err) => print_logging_error(err),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum FileOperation {
    Read,
    Write,
}

impl FileOperation {
    fn to_error_str(&self) -> &'static str {
        match self {
            Self::Read => "reading from",
            Self::Write => "writing to",
        }
    }
}
