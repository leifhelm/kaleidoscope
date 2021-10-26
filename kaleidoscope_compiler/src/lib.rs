mod compiler;
mod error;
#[cfg(test)]
mod tests;

use atty::Stream;
use bunt::termcolor::{BufferWriter, ColorChoice};
use clap::ArgMatches;
use kaleidoscope_codegen::GlobalContext;
use std::sync::Arc;

use compiler::Compiler;

pub fn run(matches: ArgMatches) {
    match parse_cliargs(matches) {
        Ok(cli_args) => {
            let logger = Logger::new(cli_args.color_setting);
            let global_context = GlobalContext::new();
            if let Err(err) =
                Compiler::new(Logger::clone(&logger), cli_args.file_name, global_context).run()
            {
                let mut stderr = logger.stderr.buffer();
                err.log(&mut stderr);
                if let Err(logging_err) = logger.stderr.print(&stderr) {
                    error::print_logging_error(&logging_err);
                }
            }
        }
        Err(CLIArgsError::Color(wrong_input)) => {
            eprintln!(
                "\"{0}\" is not valid for the color option.\n\
             Please specify one of auto, always or never",
                wrong_input
            );
            std::process::exit(64);
        }
    }
}

pub struct CLIArgs {
    file_name: String,
    color_setting: ColorSetting,
}

enum CLIArgsError {
    Color(String),
}

fn parse_cliargs(matches: ArgMatches) -> Result<CLIArgs, CLIArgsError> {
    let file_name = matches.value_of("FILE").unwrap();
    let color_setting = match matches.value_of("color") {
        Some("auto") => ColorSetting::Auto,
        Some("always") => ColorSetting::Always,
        Some("never") => ColorSetting::Never,
        Some(wrong_input) => return Err(CLIArgsError::Color(String::from(wrong_input))),
        None => ColorSetting::Auto,
    };
    Ok(CLIArgs {
        file_name: String::from(file_name),
        color_setting,
    })
}

enum ColorSetting {
    Always,
    Auto,
    Never,
}

pub struct Logger {
    stdout: Arc<BufferWriter>,
    stderr: Arc<BufferWriter>,
}

impl Logger {
    fn new(color_setting: ColorSetting) -> Logger {
        let stdout = Arc::new(BufferWriter::stdout(get_color_choice(
            &color_setting,
            Stream::Stdout,
        )));
        let stderr = Arc::new(BufferWriter::stderr(get_color_choice(
            &color_setting,
            Stream::Stderr,
        )));
        Logger { stdout, stderr }
    }
    fn clone(logger: &Logger) -> Logger {
        Logger {
            stdout: Arc::clone(&logger.stdout),
            stderr: Arc::clone(&logger.stderr),
        }
    }
}

fn get_color_choice(color_setting: &ColorSetting, stream: Stream) -> ColorChoice {
    match (color_setting, atty::is(stream)) {
        (ColorSetting::Always, true) => ColorChoice::Always,
        (ColorSetting::Always, false) => ColorChoice::AlwaysAnsi,
        (ColorSetting::Auto, true) => ColorChoice::Auto,
        (ColorSetting::Auto, false) => ColorChoice::Never,
        (ColorSetting::Never, _) => ColorChoice::Never,
    }
}
