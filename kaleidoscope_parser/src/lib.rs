/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use crate::error::{extended_context, ExtendedContext, ExtendedContextError};
use crate::located::{locate, strip_location, Located, LocatedInput};
pub use kaleidoscope_ast as ast;
use kaleidoscope_ast::*;

extern crate nom;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, multispace0, multispace1, not_line_ending};
use nom::combinator::{all_consuming, cut, map, opt, recognize, success, value};
use nom::error::ContextError;
use nom::multi::{many0, separated_list1};
use nom::number::complete::double;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{
    AsBytes, AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
    Offset, ParseTo, Parser, Slice,
};
use nom_unicode::{
    complete::{alpha1, alphanumeric1},
    IsChar,
};
use std::ops::{Range, RangeFrom, RangeTo};

pub mod error;
pub mod located;
#[cfg(test)]
mod tests;

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
    + ParseTo<f64>
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
        + ParseTo<f64>
        + PartialEq
        + Clone // https://github.com/rust-lang/rust/issues/54149
                // <T as InputTakeAtPosition>::Item: AsChar + Clone,
                // <T as InputIter>::Item: AsChar
{
}

pub trait ParseError<I>: nom::error::ParseError<I> + ExtendedContextError<I> {}
impl<I, T> ParseError<I> for T where T: nom::error::ParseError<I> + ExtendedContextError<I> {}

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

/// comments start with `//` or with `#`
fn comment<I, E>(i: I) -> IResult<I, I, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    preceded(
        alt((map(tag("//"), |_| ()), map(char('#'), |_| ()))),
        not_line_ending,
    )(i)
}

fn literal_number<I, E, L>(i: I) -> IResult<I, Expr<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: ParseError<I>,
    L: Located,
{
    extended_context(
        ExtendedContext::Number,
        map(locate(double), Expr::LiteralNumber),
    )(i)
}

fn identifier<I, E, L>(i: I) -> IResult<I, Identifier<L>, E>
where
    I: Input,
    <I as InputTakeAtPosition>::Item: IsChar,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
    L: Located,
{
    extended_context(
        ExtendedContext::Identifier,
        locate(map(
            recognize(pair(
                alt((value((), alpha1), value((), char('_')))),
                many0(alt((value((), alphanumeric1), value((), char('_'))))),
            )),
            Into::into,
        )),
    )(i)
}

fn args<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    extended_context(
        ExtendedContext::ArgumentList,
        delimited(
            char('('),
            cut(map(
                opt(terminated(
                    separated_list1(ws(char(',')), ws(inner)),
                    opt(ws(char(','))),
                )),
                |args| args.unwrap_or_default(),
            )),
            cut(ws(char(')'))),
        ),
    )
}

fn variable_call<I, E, L>(i: I) -> IResult<I, Expr<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    extended_context(ExtendedContext::VariableOrFunctionCall, move |i: I| {
        let (i, ident) = identifier(i)?;
        match args::<_, _, E, _>(expression)(i.clone()) {
            Ok((i, args)) => Ok((
                i,
                Expr::Call(Call {
                    callee: ident,
                    args,
                }),
            )),
            Err(nom::Err::Error(_)) => Ok((i, Expr::Variable(ident))),
            Err(e) => Err(e),
        }
    })(i)
}

fn operator<I, E>(i: I) -> IResult<I, Op, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    alt((
        map(char('+'), |_| Op::Add),
        map(char('-'), |_| Op::Subtract),
        map(char('*'), |_| Op::Multiply),
        map(char('/'), |_| Op::Divide),
        map(char('>'), |_| Op::Greater),
        map(char('<'), |_| Op::Less),
        map(tag(">="), |_| Op::GreaterOrEqual),
        map(tag("<="), |_| Op::LessOrEqual),
        map(tag("=="), |_| Op::Equal),
        map(tag("!="), |_| Op::Equal),
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
    extended_context(ExtendedContext::Expression, |i: I| {
        let (mut i, mut lhs) = term(i)?;
        let (i, expr) = loop {
            match pair::<_, _, _, (), _, _>(ws(locate(operator)), ws(term))(i.clone()) {
                Err(_) => break Ok((i, lhs)),
                Ok((i_next, (op, rhs))) => {
                    i = i_next;
                    let XWrapper {
                        wrapped: lhs_expr,
                        extra: lhs_extra,
                    } = lhs;
                    match lhs_expr {
                        Expr::BinaryOperation(bin_op_lhs) => {
                            if bin_op_lhs.operation.wrapped < op.wrapped {
                                let range = range_from_located::<L>(&lhs_extra, &rhs.extra);
                                lhs = wrap(
                                    Expr::BinaryOperation(Box::new(BinaryOperation {
                                        operation: op,
                                        left_hand_side: XWrapper::new(
                                            Expr::BinaryOperation(Box::new(BinaryOperation {
                                                operation: bin_op_lhs.operation,
                                                left_hand_side: bin_op_lhs.left_hand_side,
                                                right_hand_side: bin_op_lhs.right_hand_side,
                                            })),
                                            lhs_extra,
                                        ),
                                        right_hand_side: rhs,
                                    })),
                                    range,
                                );
                            } else {
                                let range = range_from_located::<L>(&lhs_extra, &rhs.extra);
                                let range_inner = range_from_located::<L>(
                                    &bin_op_lhs.right_hand_side.extra,
                                    &rhs.extra,
                                );
                                lhs = wrap(
                                    Expr::BinaryOperation(Box::new(BinaryOperation {
                                        operation: bin_op_lhs.operation,
                                        left_hand_side: bin_op_lhs.left_hand_side,
                                        right_hand_side: wrap(
                                            Expr::BinaryOperation(Box::new(BinaryOperation {
                                                operation: op,
                                                left_hand_side: bin_op_lhs.right_hand_side,
                                                right_hand_side: rhs,
                                            })),
                                            range_inner,
                                        ),
                                    })),
                                    range,
                                );
                            }
                        }
                        expr => {
                            let range = range_from_located::<L>(&rhs.extra, &lhs_extra);
                            lhs = wrap(
                                Expr::BinaryOperation(Box::new(BinaryOperation {
                                    operation: op,
                                    left_hand_side: XWrapper::new(expr, lhs_extra),
                                    right_hand_side: rhs,
                                })),
                                range,
                            );
                        }
                    }
                }
            }
        }?;
        Ok((i, expr))
    })(i)
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
    locate(alt((
        literal_number,
        if_then_else,
        for_expression,
        variable_call,
        delimited(char('('), ws(strip_location(expression)), ws(char(')'))),
    )))(i)
}

fn if_then_else<I, E, L>(i: I) -> IResult<I, Expr<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    extended_context(
        ExtendedContext::IfExpression,
        preceded(
            keyword("if"),
            cut(move |i: I| -> IResult<I, Expr<L>, E> {
                let (i, condition) = ws(expression)(i)?;
                let (i, _) = ws(keyword("then"))(i)?;
                let (i, then_expr) = ws(expression)(i)?;
                let (i, _) = ws(keyword("else"))(i)?;
                let (i, else_expr) = ws(expression)(i)?;
                Ok((
                    i,
                    Expr::If(Box::new(IfExpression {
                        condition,
                        then: then_expr,
                        r#else: else_expr,
                    })),
                ))
            }),
        ),
    )(i)
}

fn for_expression<I, E, L>(i: I) -> IResult<I, Expr<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    extended_context(
        ExtendedContext::ForExpression,
        preceded(
            keyword("for"),
            cut(move |i: I| {
                let (i, variable_name) = ws(identifier)(i)?;
                let (i, start) = preceded(ws(char('=')), ws(expression))(i)?;
                let (i, end) = preceded(ws(char(',')), ws(expression))(i)?;
                let (i, step) = opt(preceded(ws(char(',')), ws(expression)))(i)?;
                let (i, body) = preceded(ws(keyword("in")), ws(expression))(i)?;
                Ok((
                    i,
                    Expr::For(Box::new(ForExpression {
                        variable: variable_name,
                        start,
                        end,
                        step,
                        body,
                    })),
                ))
            }),
        ),
    )(i)
}
fn keyword<I, E>(keyword: &'static str) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
{
    extended_context(ExtendedContext::Keyword(keyword), move |i: I| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = alt((multispace1, comment))(i)?;
        Ok((i, ()))
    })
}
fn function_prototype<I, E, L>(i: I) -> IResult<I, FunProto<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    map(pair(identifier, args(identifier)), |(name, args)| {
        FunProto { name, args }
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
    locate(preceded(
        keyword("def"),
        cut(ws(map(
            pair(locate(function_prototype), ws(expression)),
            |(prototype, body)| Fun {
                prototype,
                body: Box::new(body),
            },
        ))),
    ))(i)
}

fn extern_function<I, E, L>(i: I) -> IResult<I, FunctionPrototype<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: IsChar + Clone,
    E: ParseError<I> + ContextError<I>,
    L: Located,
{
    locate(preceded(keyword("extern"), cut(ws(function_prototype))))(i)
}

pub fn statement<I, E, L>(i: I) -> IResult<I, AST<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I> + ContextError<I>,
    L: Located,
{
    extended_context(
        ExtendedContext::Statement,
        terminated(
            alt((
                map(function_definition, |fn_def| AST::Function(fn_def)),
                map(extern_function, |protoype| AST::ExternFunction(protoype)),
                map(expression, |expr| AST::Expression(expr)),
            )),
            opt(char(';')),
        ),
    )(i)
}

fn parse_statements<I, E, L>(i: I) -> IResult<I, Vec<AST<L>>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I> + ContextError<I>,
    L: Located,
{
    all_consuming(terminated(many0(ws(statement)), ws(success(()))))(i)
}

pub fn parse<I, E, L>(i: I) -> Result<Vec<AST<L>>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I> + ContextError<I>,
    L: Located,
{
    match parse_statements(i) {
        Err(err) => Err(match err {
            nom::Err::Error(err) | nom::Err::Failure(err) => err,
            _ => unreachable!(),
        }),
        Ok((_, ok)) => Ok(ok),
    }
}

fn range_from_located<L: Located>(start: &L, end: &L) -> Range<usize> {
    start.position().start..end.position().end
}
fn wrap<T, L: Located>(wrapped: T, range: Range<usize>) -> XWrapper<T, L> {
    XWrapper::new(wrapped, Located::new(range))
}
