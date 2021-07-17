use super::ast::*;

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
    AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Parser,
    Slice,
};
use nom_unicode::complete::{alpha1, alphanumeric1};
use std::{
    fmt::Display,
    ops::{Range, RangeFrom, RangeTo},
};
/// A combinator that takes a parser `inner` and produces a parser
// that also consumes leading whitespace, returning the output of `inner`.
fn ws<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition
        + InputTake
        + InputIter
        + InputLength
        + Compare<&'static str>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Slice<Range<usize>>
        + PartialEq
        + Display
        + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    preceded(pair(multispace0, many0(pair(comment, multispace0))), inner)
}

fn comment<I, E>(i: I) -> IResult<I, I, E>
where
    I: InputTake
        + InputIter
        + InputLength
        + Compare<&'static str>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Slice<Range<usize>>
        + Display,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    preceded(tag("//"), not_line_ending)(i)
}

fn literal_number<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    map(double, Expression::LiteralNumber)(i)
}

fn identifier<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&str, Identifier, E> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        String::from,
    )(i)
}

fn args<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: InputTakeAtPosition
        + InputTake
        + InputIter
        + InputLength
        + Compare<&'static str>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Slice<Range<usize>>
        + Display
        + Clone
        + PartialEq,
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

fn variable_call<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    let (i, ident) = identifier(i)?;
    if let Ok((i, args)) = args::<_, _, E, _>(expression)(i) {
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

fn primary<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    alt((
        literal_number,
        variable_call,
        delimited(char('('), ws(expression), ws(char(')'))),
    ))(i)
}

fn op_additive<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Op, E> {
    alt((
        map(char('-'), |_| Op::Subtract),
        map(char('+'), |_| Op::Add),
    ))(i)
}

fn op_multiplicative<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Op, E> {
    alt((
        map(char('/'), |_| Op::Divide),
        map(char('*'), |_| Op::Multiply),
    ))(i)
}

fn expression<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    let (i, lhs) = term(i)?;
    fold_many0(pair(ws(op_additive), ws(term)), lhs, |lhs, (op, rhs)| {
        Expression::BinaryOperation(Box::new(BinaryOperation {
            left_hand_side: lhs,
            right_hand_side: rhs,
            operation: op,
        }))
    })(i)
}

fn term<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    let (i, lhs) = primary(i)?;
    fold_many0(
        pair(ws(op_multiplicative), ws(primary)),
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
    I: InputTakeAtPosition + InputTake + Compare<&'static str>,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
{
    move |i: I| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = multispace1(i)?;
        Ok((i, ()))
    }
}
fn function_prototype<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, FunctionPrototype, E> {
    map(pair(identifier, args(identifier)), |(name, args)| {
        FunctionPrototype { name, args }
    })(i)
}

fn function_definition<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Function, E> {
    map(
        pair(function_prototype, ws(expression)),
        |(prototype, body)| Function {
            prototype,
            body: Box::new(body),
        },
    )(i)
}

pub fn parse_item<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
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

pub fn parse<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<AST>, E> {
    terminated(many0(ws(parse_item)), pair(multispace0, eof))(i)
}
