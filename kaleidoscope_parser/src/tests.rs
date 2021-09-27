use std::ops::RangeFrom;

use nom::{
    character::complete::{alpha1, char, multispace0},
    combinator::cut,
    error::{convert_error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, preceded},
    AsChar, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice,
};

use crate::{
    error::ErrorTree,
    located::{Located, LocatedSlice, Position},
};
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
        Err(nom::Err::Error((input.clone(), nom::error::ErrorKind::Tag))),
        super::identifier::<_, _, Position>(input)
    );
}

fn parse_array<I, E>(i: I) -> IResult<I, Vec<I>, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + InputLength + InputTake + InputTakeAtPosition + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: nom::error::ParseError<I>,
{
    delimited(
        char('['),
        cut(separated_list0(
            preceded(multispace0, char(',')),
            preceded(multispace0, alpha1),
        )),
        // char(']'),
        cut(preceded(multispace0, char(']'))),
    )(i)
}
// #[test]
fn test_parse_array() {
    let parser = parse_array;
    assert_eq!(parser("[ ]"), Ok(("", vec![])));
    assert_eq!(parser("[a ]"), Ok(("", vec!["a"])));
    assert_eq!(parser("[a,b"), Err(Err::Failure(("", ErrorKind::Char))));
    assert_eq!(parser("[;]"), Err(Err::Failure((";]", ErrorKind::Char))));
    assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Char))));
    let input = "[;]";
    if let Err(Err::Failure(e)) = parse_array::<_, ErrorTree<_>>(input) {
        println!("{:?}", e);
        assert!(false);
    }
}
