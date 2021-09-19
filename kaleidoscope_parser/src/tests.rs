use nom::{InputLength, Slice};

use crate::located::{Located, LocatedSlice, Position};
use kaleidoscope_ast::XWrapper;

#[test]
fn identifier() {
    let input = LocatedSlice::new("hello");
    assert_eq!(
        Ok((
            input.slice(input.input_len()..),
            XWrapper::new(String::from("hello"), Located::new(0..5))
        )),
        super::identifier::<LocatedSlice, nom::error::Error<LocatedSlice>, Position>(input)
    );
    let input = LocatedSlice::new("hello ");
    assert_eq!(
        Ok((input.slice(5..), XWrapper::new(String::from("hello"), Located::new(0..5)))),
        super::identifier::<LocatedSlice, nom::error::Error<LocatedSlice>, Position>(input)
    );
    let input = LocatedSlice::new("64");
    assert_eq!(
        Err(nom::Err::Error(nom::error::Error {
            input: input.clone(),
            code: nom::error::ErrorKind::Tag
        })),
        super::identifier::<LocatedSlice, nom::error::Error<LocatedSlice>, Position>(input)
    );
}
