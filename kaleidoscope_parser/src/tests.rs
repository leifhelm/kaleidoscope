use nom::{InputLength, Slice};

use crate::located::{Located, LocatedSlice, Position};
use kaleidoscope_ast::XWrapper;

#[test]
fn identifier() {
    let input = LocatedSlice::new("hello");
    assert_eq!(
        Ok((
            input.slice(input.input_len()..),
            XWrapper::new(String::from("hello"), Position::new(0..5))
        )),
        super::identifier::<_, (), _>(input)
    );
    let input = LocatedSlice::new("hello ");
    assert_eq!(
        Ok((
            input.slice(5..),
            XWrapper::new(String::from("hello"), Position::new(0..5))
        )),
        super::identifier::<_, (), _>(input)
    );
    let input = LocatedSlice::new("64");
    assert_eq!(
        Err(nom::Err::Error((
            input.clone(),
            nom::error::ErrorKind::Char
        ))),
        super::identifier::<_, _, Position>(input)
    );
}
