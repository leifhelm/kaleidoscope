use std::ops::Range;

#[cfg(feature = "bunt")]
use bunt::termcolor::WriteColor;
#[cfg(feature = "codespan-reporting")]
use codespan_reporting::files::Error as FilesError;
#[cfg(feature = "codespan-reporting")]
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorLevel {
    Bug,
    Error,
    Info,
    Help,
}

#[derive(Debug, PartialEq)]
pub struct Error {
    pub range: Option<Range<usize>>,
    pub message: String,
    pub error_level: ErrorLevel,
}

macro_rules! ranged_err {
    ($level:expr, $name:ident) => {
        #[inline]
        pub fn $name(range: Range<usize>, message: impl Into<String>) -> Self {
            Error {
                range: Some(range),
                message: message.into(),
                error_level: $level,
            }
        }
    };
}
macro_rules! global_err {
    ($level:expr, $name:ident) => {
        #[inline]
        pub fn $name(message: impl Into<String>) -> Self {
            Error {
                range: None,
                message: message.into(),
                error_level: $level,
            }
        }
    };
}

impl Error {
    ranged_err!(ErrorLevel::Bug, bug);
    ranged_err!(ErrorLevel::Error, error);
    ranged_err!(ErrorLevel::Info, info);
    ranged_err!(ErrorLevel::Help, help);
    global_err!(ErrorLevel::Bug, global_bug);
    global_err!(ErrorLevel::Error, global_error);
    global_err!(ErrorLevel::Info, global_info);
    global_err!(ErrorLevel::Help, global_help);

    #[cfg(all(feature = "codespan-reporting", feature = "bunt"))]
    pub fn print_codespan_reporting(
        &self,
        name: &str,
        input: &str,
        mut buffer: &mut dyn WriteColor,
    ) -> Result<(), std::io::Error> {
        let config = codespan_reporting::term::Config::default();
        let (files, diagnostic) = self.into_diagnostic(name, input);
        match codespan_reporting::term::emit(&mut buffer, &config, &files, &diagnostic) {
            Ok(()) => Ok(()),
            Err(FilesError::Io(err)) => Err(err),
            // TODO constuct better error messages
            Err(err) => bunt::writeln!(
                &mut buffer,
                "{$red+bold}error{/$} when constucting the error message: {[bold]:?}",
                err
            ),
        }
    }
    #[cfg(feature = "codespan-reporting")]
    pub fn into_diagnostic<'a>(
        &self,
        name: &'a str,
        input: &'a str,
    ) -> (SimpleFile<&'a str, &'a str>, Diagnostic<()>) {
        let file = SimpleFile::new(name, input);
        let labels = match &self.range {
            Some(range) => vec![Label::primary((), range.clone())],
            None => vec![],
        };
        let diagnostic = Diagnostic::error()
            .with_message(self.message.as_str())
            .with_labels(labels);
        (file, diagnostic)
    }
    pub fn message(&self) -> &str {
        self.message.as_str()
    }
    pub fn range(&self) -> Option<&Range<usize>> {
        self.range.as_ref()
    }
}
