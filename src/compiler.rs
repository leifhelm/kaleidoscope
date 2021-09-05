use super::error::{ApplicationError, FileOperation};
use super::Logger;

use std::path::Path;

use bunt::termcolor::Buffer;
use inkwell::{
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};

mod ast;
mod codegen;
mod parser;

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
                let context = Context::create();
                let module = codegen::codegen(&context, &ast_list);
                match module {
                    Ok(module) => {
                        self.write_executable(module)?;
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
    fn write_executable<'ctx>(&mut self, module: Module<'ctx>) -> Result<(), ApplicationError> {
        Target::initialize_x86(&InitializationConfig::default());

        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-pc-linux-gnu"),
                "x86-64",
                "+avx2",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        if let Err(err) = target_machine.write_to_file(&module, FileType::Object, Path::new("a.o"))
        {
            writeln!(self.stderr, "{$red+bold}error:{/$} {}", err)?;
        }
        Ok(())
    }
}
