pub use kaleidoscope_ast as ast;
use kaleidoscope_ast::*;
pub mod error;
pub mod located;
#[cfg(test)]
mod tests;

use crate::located::{locate, Located, LocatedInput};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, not_line_ending},
    combinator::{eof, map, recognize},
    error::ParseError,
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
    AsBytes, AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
    Offset, Parser, Slice,
};
use nom_unicode::{
    complete::{alpha1, alphanumeric1},
    IsChar,
};
use std::ops::{Range, RangeFrom, RangeTo};

pub trait Input:
    InputTakeAtPosition
    + InputTake
    + InputIter
    + InputLength
    + AsBytes
    + for<'a> Compare<&'a [u8]>
    + Compare<&'static str>
    + Slice<RangeFrom<usize>>
    + Slice<RangeTo<usize>>
    + Slice<Range<usize>>
    + Offset
    + LocatedInput
    + Into<String>
    + PartialEq
    + Clone
// https://github.com/rust-lang/rust/issues/54149
// where
//     <Self as InputTakeAtPosition>::Item: AsChar + Clone,
//     <Self as InputIter>::Item: AsChar,
{
}
impl<T> Input for T where
    T: InputTakeAtPosition
        + InputTake
        + InputIter
        + InputLength
        + AsBytes
        + for<'a> Compare<&'a [u8]>
        + Compare<&'static str>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Slice<Range<usize>>
        + Offset
        + LocatedInput
        + Into<String>
        + PartialEq
        + Clone // https://github.com/rust-lang/rust/issues/54149
                // <T as InputTakeAtPosition>::Item: AsChar + Clone,
                // <T as InputIter>::Item: AsChar
{
}

/// A combinator that takes a parser `inner` and produces a parser
// that also consumes leading whitespace, returning the output of `inner`.
fn ws<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    preceded(pair(multispace0, many0(pair(comment, multispace0))), inner)
}

fn comment<I, E>(i: I) -> IResult<I, I, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    preceded(tag("//"), not_line_ending)(i)
}

fn literal_number<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: ParseError<I>,
    L: Located,
{
    map(locate(double), Expression::LiteralNumber)(i)
}

fn identifier<I, E, L>(i: I) -> IResult<I, Identifier<L>, E>
where
    I: Input,
    <I as InputTakeAtPosition>::Item: IsChar,
    E: ParseError<I>,
    L: Located,
{
    locate(map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        Into::into,
    ))(i)
}

fn args<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    delimited(
        char('('),
        separated_list0(ws(char(',')), ws(inner)),
        ws(char(')')),
    )
}

fn variable_call<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    let (i, ident) = identifier(i)?;
    if let Ok((i, args)) = args::<_, _, E, _>(expression)(i.clone()) {
        Ok((
            i,
            Expression::Call(Call {
                callee: ident,
                args,
            }),
        ))
    } else {
        Ok((i, Expression::Variable(ident)))
    }
}

fn primary<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    alt((
        literal_number,
        variable_call,
        delimited(char('('), ws(expression), ws(char(')'))),
    ))(i)
}

fn op_additive<I, E>(i: I) -> IResult<I, Op, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    alt((
        map(char('-'), |_| Op::Subtract),
        map(char('+'), |_| Op::Add),
    ))(i)
}

fn op_multiplicative<I, E>(i: I) -> IResult<I, Op, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    alt((
        map(char('/'), |_| Op::Divide),
        map(char('*'), |_| Op::Multiply),
    ))(i)
}

fn expression<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    let (i, lhs) = term(i)?;
    fold_many0(
        pair(ws(locate(op_additive)), ws(term)),
        lhs,
        |lhs, (op, rhs)| {
            Expression::BinaryOperation(Box::new(BinaryOperation {
                left_hand_side: lhs,
                right_hand_side: rhs,
                operation: op,
            }))
        },
    )(i)
}

fn term<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    let (i, lhs) = primary(i)?;
    fold_many0(
        pair(ws(locate(op_multiplicative)), ws(primary)),
        lhs,
        |lhs, (op, rhs)| {
            Expression::BinaryOperation(Box::new(BinaryOperation {
                left_hand_side: lhs,
                right_hand_side: rhs,
                operation: op,
            }))
        },
    )(i)
}

fn fold_many0<I, O, E, F, G, R>(mut f: F, init: R, mut g: G) -> impl FnOnce(I) -> IResult<I, R, E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    G: FnMut(R, O) -> R,
    E: ParseError<I>,
{
    move |i: I| {
        let mut res = init;
        let mut input = i;

        loop {
            let i_ = input.clone();
            match f.parse(i_) {
                Ok((i, o)) => {
                    // loop trip must always consume (otherwise infinite loops)
                    if i == input {
                        return Err(nom::Err::Error(E::from_error_kind(
                            input,
                            nom::error::ErrorKind::Many0,
                        )));
                    }

                    res = g(res, o);
                    input = i;
                }
                Err(nom::Err::Error(_)) => {
                    return Ok((input, res));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}

fn keyword<I, E>(keyword: &'static str) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: Input,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
{
    move |i: I| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = multispace1(i)?;
        Ok((i, ()))
    }
}
fn function_prototype<I, E, L>(i: I) -> IResult<I, FunctionPrototype<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    map(pair(identifier, args(identifier)), |(name, args)| {
        FunctionPrototype { name, args }
    })(i)
}

fn function_definition<I, E, L>(i: I) -> IResult<I, Function<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    map(
        pair(function_prototype, ws(expression)),
        |(prototype, body)| Function {
            prototype,
            body: Box::new(body),
        },
    )(i)
}

pub fn parse_item<I, E, L>(i: I) -> IResult<I, AST<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    alt((
        map(preceded(keyword("def"), function_definition), |fn_def| {
            AST::Function(fn_def)
        }),
        map(preceded(keyword("extern"), function_prototype), |protype| {
            AST::ExternFunction(protype)
        }),
        map(expression, |expr| AST::Expression(expr)),
    ))(i)
}

pub fn parse<I, E, L>(i: I) -> Result<(I, Vec<AST<L>>), Option<E>>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    terminated(many0(ws(parse_item)), pair(multispace0, eof))(i).map_err(|err| match err {
        nom::Err::Error(err) | nom::Err::Failure(err) => Some(err),
        _ => None,
    })
}
