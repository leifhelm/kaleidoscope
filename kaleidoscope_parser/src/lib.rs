use crate::error::{extended_context, ExtendedContext, ExtendedContextError};
use crate::located::{locate, Located, LocatedInput};
pub use kaleidoscope_ast as ast;
use kaleidoscope_ast::*;

extern crate nom;
use nom::combinator::success;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, not_line_ending},
    combinator::{cut, map, recognize},
    error::{context, ContextError},
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

fn literal_number<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
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
        map(locate(double), Expression::LiteralNumber),
    )(i)
}

fn identifier<I, E, L>(i: I) -> IResult<I, Identifier<L>, E>
where
    I: Input,
    <I as InputTakeAtPosition>::Item: IsChar,
    E: ParseError<I>,
    L: Located,
{
    extended_context(
        ExtendedContext::Identifier,
        locate(map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
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

fn variable_call<I, E, L>(i: I) -> IResult<I, Expression<L>, E>
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
        // if let Some('(') = i.iter_elements().next().map(AsChar::as_char) {
        //     let (i, args) = args(expression)(i)?;
        //     Ok((
        //         i,
        //         Expression::Call(Call {
        //             callee: ident,
        //             args,
        //         }),
        //     ))
        // } else {
        //     Ok((i, Expression::Variable(ident)))
        // }
        // // match char('(')(i) {

        // // }
        // // let ident2 = ident.clone();
        // // case((
        // //     (
        // //         map(char('('),|_|()),
        // //     ),
        // //     move |i: I|Ok((i, ))),
        // // ))(i)
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
    })(i)
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
    extended_context(ExtendedContext::Expression, |i: I| {
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
    extended_context(ExtendedContext::Keyword(keyword), move |i: I| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = multispace1(i)?;
        Ok((i, ()))
    })
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

pub trait Case<I, O, E> {
    fn case(&mut self, i: I) -> IResult<I, O, E>;
}

macro_rules! succ (
  (0, $submac:ident ! ($($rest:tt)*)) => ($submac!(1, $($rest)*));
  (1, $submac:ident ! ($($rest:tt)*)) => ($submac!(2, $($rest)*));
  (2, $submac:ident ! ($($rest:tt)*)) => ($submac!(3, $($rest)*));
  (3, $submac:ident ! ($($rest:tt)*)) => ($submac!(4, $($rest)*));
  (4, $submac:ident ! ($($rest:tt)*)) => ($submac!(5, $($rest)*));
  (5, $submac:ident ! ($($rest:tt)*)) => ($submac!(6, $($rest)*));
  (6, $submac:ident ! ($($rest:tt)*)) => ($submac!(7, $($rest)*));
  (7, $submac:ident ! ($($rest:tt)*)) => ($submac!(8, $($rest)*));
  (8, $submac:ident ! ($($rest:tt)*)) => ($submac!(9, $($rest)*));
  (9, $submac:ident ! ($($rest:tt)*)) => ($submac!(10, $($rest)*));
  (10, $submac:ident ! ($($rest:tt)*)) => ($submac!(11, $($rest)*));
  (11, $submac:ident ! ($($rest:tt)*)) => ($submac!(12, $($rest)*));
  (12, $submac:ident ! ($($rest:tt)*)) => ($submac!(13, $($rest)*));
  (13, $submac:ident ! ($($rest:tt)*)) => ($submac!(14, $($rest)*));
  (14, $submac:ident ! ($($rest:tt)*)) => ($submac!(15, $($rest)*));
  (15, $submac:ident ! ($($rest:tt)*)) => ($submac!(16, $($rest)*));
  (16, $submac:ident ! ($($rest:tt)*)) => ($submac!(17, $($rest)*));
  (17, $submac:ident ! ($($rest:tt)*)) => ($submac!(18, $($rest)*));
  (18, $submac:ident ! ($($rest:tt)*)) => ($submac!(19, $($rest)*));
  (19, $submac:ident ! ($($rest:tt)*)) => ($submac!(20, $($rest)*));
  (20, $submac:ident ! ($($rest:tt)*)) => ($submac!(21, $($rest)*));
);
macro_rules! case_trait(
  (($first0:ident, $first1:ident) $(($id0: ident, $id1: ident))+) => (
    case_trait!(__impl ($first0, $first1); $(($id0, $id1))+);
  );
  (__impl $(($current0:ident, $current1:ident))*; ($head0:ident, $head1:ident) $(($id0: ident, $id1: ident))+) => (
    case_trait_impl!($(($current0, $current1))* $head1);

    case_trait!(__impl $(($current0, $current1))* ($head0, $head1); $(($id0, $id1))+);
  );
  (__impl $(($current0:ident, $current1:ident))*; ($head0:ident, $head1:ident)) => (
    case_trait_impl!($(($current0, $current1))* $head1);
  );
);

macro_rules! case_trait_impl(
  ($(($id0:ident, $id1:ident))+ $last:ident) => (
    impl<
      Input: Clone, Output, Error: ParseError<Input>,
      $($id0: Parser<Input, (), Error>),+,
      $($id1: Parser<Input, Output, Error>),+,
      $last: Parser<Input, Output, Error>,
    > Case<Input, Output, Error> for ( $(($id0, $id1)),+, $last ) {

      fn case(&mut self, input: Input) -> IResult<Input, Output, Error> {
        match self.0.0.parse(input.clone()) {
          Err(nom::Err::Error(e)) => case_trait_inner!(1, self, input, e, $(($id0, $id1))+ $last),
          Ok((input, _)) => self.0.1.parse(input.clone()),
          Err(e) => Err(e),
        }
      }
    }
  );
);

macro_rules! case_trait_inner(
  ($it:tt, $self:expr, $input:expr, $err:expr, ($head0:ident, $head1:ident) $(($id0:ident, $id1:ident))+ $last:ident) => (
    match $self.$it.0.parse($input.clone()) {
      Err(nom::Err::Error(e)) => {
        let err = $err.or(e);
        succ!($it, case_trait_inner!($self, $input, err, $(($id0, $id1))+ $last))
      }
      Ok((input, _)) => $self.$it.1.parse(input.clone()),
      Err(e) => Err(e),
    }
  );
  ($it:tt, $self:expr, $input:expr, $err:expr, ($head0:ident, $head1:ident) $last:ident) => (
    match $self.$it.parse($input.clone()){
      Err(nom::Err::Error(e)) => {
        let err = $err.or(e);
        Err(nom::Err::Error(Error::append($input, nom::error::ErrorKind::Alt, err)))
      }
      Ok(ok) => Ok(ok),
      Err(e) => Err(e),
    }
  );
);

case_trait!((A0, A1)(B0, B1)(C0, C1)(D0, D1)(E0, E1)(F0, F1)(G0, G1)(
    H0, H1
)(I0, I1)(J0, J1)(K0, K1)(L0, L1)(M0, M1)(N0, N1)(O0, O1)(
    P0, P1
)(Q0, Q1)(R0, R1)(S0, S1)(T0, T1)(U0, U1));

fn case<I, O, E, List>(mut list: List) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Input,
    E: ParseError<I>,
    List: Case<I, O, E>,
{
    move |i: I| list.case(i)
}

pub fn parse_item<I, E, L>(i: I) -> IResult<I, AST<L>, E>
where
    I: Input,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputIter>::IterElem: Clone,
    <I as InputTakeAtPosition>::Item: AsChar + IsChar + Clone,
    E: ParseError<I> + ContextError<I>,
    L: Located,
{
    context(
        "parse_item",
        alt((
            map(
                preceded(keyword("def"), cut(function_definition)),
                |fn_def| AST::Function(fn_def),
            ),
            map(
                preceded(keyword("extern"), cut(function_prototype)),
                |protype| AST::ExternFunction(protype),
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
    match many0_until_eof(ws(parse_item))(i) {
        Err(err) => Err(match err {
            nom::Err::Error(err) | nom::Err::Failure(err) => err,
            _ => unreachable!(),
        }),
        Ok((_, ok)) => Ok(ok),
    }
}
