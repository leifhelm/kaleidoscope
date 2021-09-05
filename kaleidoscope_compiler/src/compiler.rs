use super::error::{ApplicationError, FileOperation};
use super::Logger;

use bunt::termcolor::Buffer;

use kaleidoscope_codegen as codegen;
use kaleidoscope_parser as parser;

macro_rules! writeln {
    ($target:expr, $format_str:literal $(, $arg:expr)* $(,)?) => {
        bunt::writeln!($target, [$format_str] $(, $arg)*).map_err(ApplicationError::LoggingError)
    };
}

pub struct Compiler {
    logger: Logger,
    file_name: String,
    stdout: Buffer,
    stderr: Buffer,
}

impl Compiler {
    pub fn new(logger: Logger, file_name: String) -> Compiler {
        let stdout = logger.stdout.buffer();
        let stderr = logger.stderr.buffer();
        Compiler {
            logger,
            file_name,
            stdout,
            stderr,
        }
    }
    pub fn run(&mut self) -> Result<(), ApplicationError> {
        writeln!(&mut self.stdout, "Compiling file {[green]}", self.file_name)?;
        let file_contents = std::fs::read_to_string(&self.file_name).map_err(|error| {
            ApplicationError::FileSystem {
                error,
                file_name: self.file_name.clone(),
                operation: FileOperation::Read,
            }
        })?;
        let parser_result = parser::parse::<nom::error::VerboseError<&str>>(&file_contents);
        match parser_result {
            Ok((_, ast_list)) => {
                writeln!(&mut self.stdout, "{:?}", ast_list)?;
                let context = codegen::GlobalContext::new();
                let module = codegen::codegen(&context, &ast_list);
                match module {
                    Ok(module) => {
                        if let Err(err) = kaleidoscope_codegen::write_executable(module) {
                            writeln!(self.stderr, "{$red+bold}error:{/$} {}", err)?;
                        }
                        writeln!(&mut self.stdout, "{$green}Build sucessful{/$}")?;
                    }
                    Err(err) => {
                        writeln!(&mut self.stdout, "{[red+bold]:?}", err)?;
                    }
                }
            }
            Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
                writeln!(
                    &mut self.stdout,
                    "{[red+bold]}",
                    nom::error::convert_error(&*file_contents, err)
                )?;
            }
            Err(err) => {
                writeln!(&mut self.stdout, "{[red+bold]:?}", err)?;
            }
        }
        self.logger
            .stdout
            .as_ref()
            .print(&self.stdout)
            .map_err(ApplicationError::LoggingError)?;
        Ok(())
    }
}
