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
    range: Range<usize>,
    message: String,
}

impl Error {
    pub fn new(range: Range<usize>, message: String) -> Self {
        Error {
            range,
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
        let diagnostic = Diagnostic::error()
            .with_message(self.message.as_str())
            .with_labels(vec![Label::primary((), self.range.clone())]);
        (file, diagnostic)
    }
    pub fn message(&self) -> &str {
        self.message.as_str()
    }
}

fn range_from_str_slice(first: &str, second: &str) -> Range<usize> {
    let fst = first.as_ptr() as usize;
    let snd = second.as_ptr() as usize;

    let start = snd - fst;
    let end = start + second.len();
    Range { start, end }
}
