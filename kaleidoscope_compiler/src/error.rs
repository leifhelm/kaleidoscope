use std::io::ErrorKind;

use bunt::termcolor::Buffer;
use kaleidoscope_ast::Identifier;
use kaleidoscope_codegen::CodeGenError;
use kaleidoscope_error as error;
use kaleidoscope_parser::located::Located;

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

pub fn codegen_error_to_error<'ctx, L: Located>(error: &CodeGenError<'ctx, L>) -> error::Error {
    match error {
        CodeGenError::VariableNotFound(ident) => ident_error(ident, "Variable not found"),
        CodeGenError::FunctionNotFound(ident) => ident_error(ident, "Function not found"),
        CodeGenError::ArgumentLengthMismatch {
            function_name,
            expected,
            actual,
        } => ident_error(
            function_name,
            format!(
                "Argument mismatch: expected {}, actual {}",
                expected, actual
            ),
        ),
        CodeGenError::InvalidFunctionCall(ident) => ident_error(ident, "Invalid function call"),
        CodeGenError::InvalidGeneratedFunction(ident) => {
            ident_error(ident, "Invalid generated LLVM function")
        }
        CodeGenError::InvalidMainFunction => {
            error::Error::global("The method name `main` is reserved".into())
        }
        CodeGenError::InvalidGeneratedModule(error_str) => error::Error::global(format!(
            "An error occured when genereating the code unit: {}",
            error_str
        )),
    }
}

fn ident_error<M: Into<String>, L: Located>(ident: &Identifier<L>, message: M) -> error::Error {
    error::Error::new(ident.extra.position().clone(), message.into())
}
