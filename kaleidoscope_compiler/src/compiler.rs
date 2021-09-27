use crate::error::codegen_error_to_error;
use crate::error::{ApplicationError, FileOperation};
use crate::Logger;

use bunt::termcolor::Buffer;

use kaleidoscope_codegen as codegen;
use kaleidoscope_error as error;
use kaleidoscope_parser as parser;
use parser::ast::AST;
use parser::located::LocatedSlice;
use parser::located::Position;

macro_rules! writeln {
    ($target:expr, $format_str:literal $(, $arg:expr)* $(,)?) => {
        bunt::writeln!($target, [$format_str] $(, $arg)*).map_err(ApplicationError::LoggingError)
    };
}

pub struct Compiler {
    logger: Logger,
    file_name: String,
    codegen_context: codegen::GlobalContext,
    stdout: Buffer,
    stderr: Buffer,
}

impl Compiler {
    pub fn new(
        logger: Logger,
        file_name: String,
        codegen_context: codegen::GlobalContext,
    ) -> Compiler {
        let stdout = logger.stdout.buffer();
        let stderr = logger.stderr.buffer();
        Compiler {
            logger,
            file_name,
            codegen_context,
            stdout,
            stderr,
        }
    }
    pub fn run(mut self) -> Result<(), ApplicationError> {
        writeln!(&mut self.stdout, "Compiling file {[green]}", self.file_name)?;
        let file_contents = std::fs::read_to_string(&self.file_name).map_err(|error| {
            ApplicationError::FileSystem {
                error,
                file_name: self.file_name.clone(),
                operation: FileOperation::Read,
            }
        })?;
        self.parse(&file_contents)?;
        self.logger
            .stdout
            .as_ref()
            .print(&self.stdout)
            .map_err(ApplicationError::LoggingError)?;
        Ok(())
    }

    fn parse(&mut self, input: &str) -> Result<(), ApplicationError> {
        // let located_slice = LocatedSlice::new(input);
        // let parser_result =
        //     parser::parse::<LocatedSlice, parser::error::Error<LocatedSlice>, Position>(
        //         located_slice,
        //     ).map_err(|e| e.to_error());
        let parser_result = kaleidoscope_parser_pest::parse::<Position>(input);
        match parser_result {
            Ok(ast_list) => {
                writeln!(&mut self.stdout, "{:#?}", ast_list)?;
                return self.codegen(input, ast_list);
            }
            // Err(err) => writeln!(
            //     &mut self.stdout,
            //     "{$red+intense+bold}error:{/$}\n{:#?}",
            //     err
            // )?,
            Err(err) => self.print_error(err, input)?,
        }
        Ok(())
    }
    fn codegen(
        &mut self,
        input: &str,
        ast_list: Vec<AST<Position>>,
    ) -> Result<(), ApplicationError> {
        let ctx = self.codegen_context.clone();
        let module = codegen::codegen(&ctx, &ast_list);
        match module {
            Ok(cu) => return self.write_files(cu),
            Err(errors) => {
                for err in errors {
                    self.print_error(codegen_error_to_error(&err), input)?;
                }
            }
        }
        Ok(())
    }
    fn write_files<'ctx>(
        &'ctx mut self,
        cu: codegen::CodeGenUnit<'ctx>,
    ) -> Result<(), ApplicationError> {
        if let Err(err) = codegen::write_executable(cu) {
            writeln!(self.stderr, "{$red+bold}error:{/$} {}", err)?;
        } else {
            writeln!(&mut self.stdout, "{$green}Build sucessful{/$}")?;
        }
        Ok(())
    }

    fn print_error(&mut self, error: error::Error, input: &str) -> Result<(), ApplicationError> {
        error
            .print_codespan_reporting(self.file_name.as_str(), input, &mut self.stdout)
            .map_err(ApplicationError::LoggingError)
    }
}
