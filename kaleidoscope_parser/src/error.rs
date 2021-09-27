use kaleidoscope_error as error;

use nom::{
    error::{ContextError, ParseError},
    IResult, Parser,
};

use crate::located::LocatedInput;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Context(ExtendedContext),
    Char(char),
    Nom(nom::error::ErrorKind),
}

impl ErrorKind {
    pub fn message(&self) -> String {
        match self {
            ErrorKind::Context(ctx) => ctx.message().into(),
            ErrorKind::Char(c) => format!("`{}`", c),
            ErrorKind::Nom(err) => err.description().into(),
        }
    }
}

#[derive(Debug)]
pub struct ErrorData<I> {
    context: Option<ExtendedContext>,
    errorkind: ErrorKind,
    slice: I,
}

#[derive(Debug)]
pub enum ErrorTree<I> {
    And(Vec<ErrorTree<I>>, Option<ExtendedContext>),
    Or(Vec<ErrorTree<I>>, Option<ExtendedContext>),
    Leaf(ErrorData<I>),
}

impl<I: std::fmt::Debug> ParseError<I> for ErrorTree<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        ErrorTree::Leaf(ErrorData {
            context: None,
            errorkind: ErrorKind::Nom(kind),
            slice: input,
        })
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        // if let nom::error::ErrorKind::Alt = kind{
        //     return other
        // }
        // println!("append {:?}", kind);
        let new_leaf = Self::from_error_kind(input, kind);
        match other {
            ErrorTree::And(mut list, ctx) => {
                list.push(new_leaf);
                ErrorTree::And(list, ctx)
            }
            other => ErrorTree::And(vec![other, new_leaf], None),
        }
    }

    fn from_char(input: I, c: char) -> Self {
        ErrorTree::Leaf(ErrorData {
            context: None,
            errorkind: ErrorKind::Char(c),
            slice: input,
        })
    }

    fn or(self, other: Self) -> Self {
        // println!("or {:?}", self);
        match other {
            ErrorTree::Or(mut list, ctx) => {
                list.push(self);
                ErrorTree::Or(list, ctx)
            }
            other => ErrorTree::Or(vec![other, self], None),
        }
    }
}
impl<I: std::fmt::Debug> ContextError<I> for ErrorTree<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        Self::add_extended_context(input, ExtendedContext::Context(ctx), other)
    }
}
impl<I: std::fmt::Debug> ExtendedContextError<I> for ErrorTree<I> {
    fn add_extended_context(_input: I, ctx: ExtendedContext, other: Self) -> Self {
        // println!("context {:?}", ctx);
        match other {
            ErrorTree::And(list, None) => ErrorTree::And(list, Some(ctx)),
            ErrorTree::Or(list, None) => ErrorTree::Or(list, Some(ctx)),
            ErrorTree::Leaf(mut leaf) => {
                if leaf.context.is_none() {
                    leaf.context = Some(ctx);
                }
                ErrorTree::Leaf(leaf)
            }
            _ => other,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Error<I> {
    primary_error: (I, ErrorKind),
    secondary_errors: Vec<(I, ErrorKind)>,
}
impl<L: LocatedInput> Error<L> {
    pub fn to_error(self) -> error::Error {
        let (slice, errorkind) = self.primary_error;
        let position = slice.position();
        let mut message = errorkind.message();
        for (_, errorkind) in self.secondary_errors {
            message = format!("{}, {}", message, errorkind.message());
        }
        let message = format!("Expected {}", message);
        error::Error::error(position..position, message)
    }
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Error {
            primary_error: (input, ErrorKind::Nom(kind)),
            secondary_errors: vec![],
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.secondary_errors.push((input, ErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Error {
            primary_error: (input, ErrorKind::Char(c)),
            secondary_errors: vec![],
        }
    }

    fn or(self, mut other: Self) -> Self {
        other.secondary_errors.push(self.primary_error);
        for error in self.secondary_errors {
            other.secondary_errors.push(error);
        }
        other
    }
}

impl<I> ExtendedContextError<I> for Error<I> {
    fn add_extended_context(input: I, ctx: ExtendedContext, mut other: Self) -> Self {
        other
            .secondary_errors
            .push((input, ErrorKind::Context(ctx)));
        other
    }
}
impl<I> ContextError<I> for Error<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        Self::add_extended_context(input, ExtendedContext::Context(ctx), other)
    }
}

pub trait ExtendedContextError<I>: ContextError<I> {
    fn add_extended_context(_input: I, _ctx: ExtendedContext, other: Self) -> Self {
        other
    }
}

impl<I> ExtendedContextError<I> for () {}
impl<I> ExtendedContextError<I> for (I, nom::error::ErrorKind) {}

#[derive(Debug, PartialEq, Clone)]
pub enum ExtendedContext {
    Expression,
    Number,
    Identifier,
    VariableOrFunctionCall,
    Keyword(&'static str),
    Context(&'static str),
}

impl ExtendedContext {
    fn message(&self) -> String {
        use ExtendedContext::*;
        match self {
            Expression => "expression".into(),
            Number => "float number".into(),
            VariableOrFunctionCall => "varaiable or function call".into(),
            Identifier => "identifier".into(),
            Keyword(keyword) => format!("keyword `{}`", keyword),
            &Context(ctx) => ctx.into(),
        }
    }
}

pub fn extended_context<I, O, E, F>(
    context: ExtendedContext,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone,
    E: ExtendedContextError<I>,
    F: Parser<I, O, E>,
{
    move |i: I| match f.parse(i.clone()) {
        Ok(o) => Ok(o),
        Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(E::add_extended_context(
            i,
            context.clone(),
            e,
        ))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(E::add_extended_context(
            i,
            context.clone(),
            e,
        ))),
    }
}
