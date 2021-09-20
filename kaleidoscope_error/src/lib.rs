use std::ops::Range;

#[cfg(feature = "bunt")]
use bunt::termcolor::Buffer;
#[cfg(feature = "codespan-reporting")]
use codespan_reporting::files::Error as FilesError;
#[cfg(feature = "codespan-reporting")]
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
};

pub struct Error {
    range: Option<Range<usize>>,
    message: String,
}

impl Error {
    pub fn new(range: Range<usize>, message: String) -> Self {
        Error {
            range: Some(range),
            message,
        }
    }
    pub fn global(message: String) -> Self {
        Error {
            range: None,
            message,
        }
    }
    #[cfg(all(feature = "codespan-reporting", feature = "bunt"))]
    pub fn print_codespan_reporting(
        &self,
        name: &str,
        input: &str,
        mut buffer: &mut Buffer,
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
