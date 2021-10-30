use crate::error::{extended_context, ExtendedContext, ExtendedContextError};
use crate::located::{locate, strip_location, Located, LocatedInput};
pub use kaleidoscope_ast as ast;
use kaleidoscope_ast::*;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, not_line_ending},
    combinator::{cut, map, opt, recognize, success, value},
    error::ContextError,
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded},
    AsBytes, AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
    Offset, Parser, Slice,
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

fn comment<I, E>(i: I) -> IResult<I, I, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    preceded(tag("//"), not_line_ending)(i)
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
    delimited(
        char('('),
        cut(separated_list0(ws(char(',')), ws(inner))),
        cut(ws(char(')'))),
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
        if let Ok((i, args)) = args::<_, _, E, _>(expression)(i.clone()) {
            Ok((
                i,
                Expr::Call(Call {
                    callee: ident,
                    args,
                }),
            ))
        } else {
            Ok((i, Expr::Variable(ident)))
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
        let (i, _) = opt(char(';'))(i)?;
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

fn keyword<I, E>(keyword: &'static str) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: Input,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
{
    extended_context(ExtendedContext::Keyword(keyword), move |i: I| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = multispace1(i)?;
        Ok((i, ()))
    })
}
fn function_prototype<I, E, L>(i: I) -> IResult<I, FunProto<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    map(pair(identifier, args(identifier)), |(name, args)| {
        FunProto { name, args }
    })(i)
}

fn function_definition<I, E, L>(i: I) -> IResult<I, Fun<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I>,
    L: Located,
{
    map(
        pair(locate(function_prototype), ws(expression)),
        |(prototype, body)| Fun {
            prototype,
            body: Box::new(body),
        },
    )(i)
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
        alt((
            map(
                locate(preceded(keyword("def"), cut(function_definition))),
                |fn_def| AST::Function(fn_def),
            ),
            map(
                locate(preceded(keyword("extern"), cut(function_prototype))),
                |protoype| AST::ExternFunction(protoype),
            ),
            map(expression, |expr| AST::Expression(expr)),
        )),
    )(i)
}

fn many0_until_eof<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    move |mut i: I| {
        let mut acc = Vec::with_capacity(4);
        loop {
            let len = i.input_len();
            match f.parse(i.clone()) {
                Err(e @ nom::Err::Error(_)) => {
                    let i = match ws::<_, _, E, _>(success(()))(i.clone()) {
                        Ok((i, _)) => i,
                        Err(_) => i,
                    };
                    return if i.input_len() == 0 {
                        Ok((i, acc))
                    } else {
                        Err(e)
                    };
                }
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    // infinite loop check: the parser must always consume
                    if i1.input_len() == len {
                        return Err(nom::Err::Error(E::from_error_kind(
                            i,
                            nom::error::ErrorKind::Many0,
                        )));
                    }

                    i = i1;
                    acc.push(o);
                }
            }
        }
    }
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
    match many0_until_eof(ws(statement))(i) {
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
