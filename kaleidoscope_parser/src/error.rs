use std::{fmt::Display, iter::FromIterator};

use kaleidoscope_error as error;

use nom::{
    error::{ContextError, ParseError},
    AsChar, IResult, InputIter, Parser,
};
use termtree::Tree;

use crate::located::LocatedInput;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Char(char),
    Nom(nom::error::ErrorKind),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Char(c) => write!(f, "`{}`", c),
            ErrorKind::Nom(err) => write!(f, "{}", err.description()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ErrorData<I> {
    context: Option<ExtendedContext>,
    errorkind: ErrorKind,
    slice: I,
}

#[derive(Debug, PartialEq)]
pub enum ErrorTree<I> {
    Or(Vec<ErrorTree<I>>, Option<ExtendedContext>),
    Leaf(ErrorData<I>),
}

#[derive(Debug, PartialEq)]
pub struct Error<I> {
    slice: I,
    tree: ErrorTree<I>,
}

impl<I> Error<I> {
    fn new(slice: I, tree: ErrorTree<I>) -> Self {
        Error { slice, tree }
    }
}

impl<L: Clone + LocatedInput> ParseError<L> for Error<L> {
    fn from_error_kind(input: L, kind: nom::error::ErrorKind) -> Self {
        Error::new(
            input.clone(),
            ErrorTree::Leaf(ErrorData {
                context: None,
                errorkind: ErrorKind::Nom(kind),
                slice: input,
            }),
        )
    }

    fn append(_input: L, kind: nom::error::ErrorKind, other: Self) -> Self {
        if kind == nom::error::ErrorKind::Fail {
            Error::new(other.slice, ErrorTree::Or(vec![other.tree], None))
        } else {
            other
        }
    }

    fn from_char(input: L, c: char) -> Self {
        Error::new(
            input.clone(),
            ErrorTree::Leaf(ErrorData {
                context: None,
                errorkind: ErrorKind::Char(c),
                slice: input,
            }),
        )
    }

    fn or(mut self, other: Self) -> Self {
        match self.tree {
            /*
            Keep only the last error. All previous error are discarded
            by the following guards.
            */
            _ if self.slice.position() > other.slice.position() => self,
            _ if other.slice.position() > self.slice.position() => other,
            ErrorTree::Or(mut list, ctx) => {
                match other.tree {
                    ErrorTree::Or(mut sub_tree, _) => list.append(&mut sub_tree),
                    tree => list.push(tree),
                }
                self.tree = ErrorTree::Or(list, ctx);
                self
            }
            me => Error::new(self.slice, ErrorTree::Or(vec![me, other.tree], None)),
        }
    }
}
impl<I> ContextError<I> for Error<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        Self::add_extended_context(input, ExtendedContext::Context(ctx), other)
    }
}
impl<I> ExtendedContextError<I> for Error<I> {
    fn add_extended_context(_input: I, ctx: ExtendedContext, mut other: Self) -> Self {
        match other.tree {
            ErrorTree::Or(list, None) => {
                other.tree = ErrorTree::Or(list, Some(ctx));
                other
            }
            ErrorTree::Leaf(mut leaf) => {
                if leaf.context.is_none() {
                    leaf.context = Some(ctx);
                }
                Error::new(other.slice, ErrorTree::Leaf(leaf))
            }
            _ => other,
        }
    }
}

impl<L> From<Error<L>> for error::Error
where
    L: LocatedInput + InputIter + std::fmt::Debug,
    <L as InputIter>::Item: AsChar,
{
    fn from(err: Error<L>) -> Self {
        println!("{:#?}", err);
        let message = err.tree.to_string();
        let position = LocatedInput::position(&err.slice);
        let message = if let Some(unexpected_char) = err.slice.iter_elements().next() {
            format!(
                "Unexpected character `{}`. Expected:\n{}",
                unexpected_char.as_char(),
                message
            )
        } else {
            format!("Unexpected end of input. Expected:\n{}", message)
        };
        error::Error::error(position..position, message)
    }
}

impl<I> Display for ErrorTree<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for tree in self.to_trees() {
            writeln!(f, "{}", tree)?;
        }
        Ok(())
    }
}

impl<I> ErrorTree<I> {
    fn to_trees(&self) -> Box<dyn Iterator<Item = Tree<String>> + '_> {
        match self {
            ErrorTree::Or(sub_trees, Some(context)) => {
                Box::new(IntoIterator::into_iter([Tree::new(
                    context.to_string(),
                    FromIterator::from_iter(sub_trees.into_iter().flat_map(Self::to_trees)),
                )]))
            }
            ErrorTree::Or(sub_trees, None) => {
                Box::new(sub_trees.into_iter().flat_map(Self::to_trees))
            }
            ErrorTree::Leaf(ErrorData {
                context, errorkind, ..
            }) => {
                use nom::error::ErrorKind::*;
                let sub_tree = match errorkind {
                    ErrorKind::Char(_)
                    | ErrorKind::Nom(Char | Alpha | AlphaNumeric | Space | MultiSpace) => {
                        vec![Tree::new(errorkind.to_string(), vec![])]
                    }
                    _ => vec![],
                };
                match context {
                    Some(context) => Box::new(IntoIterator::into_iter([Tree::new(
                        context.to_string(),
                        sub_tree,
                    )])),
                    None => Box::new(sub_tree.into_iter()),
                }
            }
        }
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
    IfExpression,
    ForExpression,
    Statement,
    Keyword(&'static str),
    Context(&'static str),
}

impl Display for ExtendedContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExtendedContext::*;
        match self {
            Expression => write!(f, "expression"),
            Number => write!(f, "float number"),
            VariableOrFunctionCall => write!(f, "varaiable or function call"),
            Identifier => write!(f, "identifier"),
            IfExpression => write!(f, "if then else expression"),
            ForExpression => write!(f, "for expression"),
            Statement => write!(f, "statement"),
            Keyword(keyword) => write!(f, "keyword `{}`", keyword),
            &Context(ctx) => write!(f, "{}", ctx),
        }
    }
}

pub fn extended_context<I, O, E, F>(
    context: ExtendedContext,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone,
    E: ExtendedContextError<I> + ParseError<I>,
    F: Parser<I, O, E>,
{
    move |i: I| match f.parse(i.clone()) {
        Ok(o) => Ok(o),
        Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(ParseError::append(
            i.clone(),
            nom::error::ErrorKind::Fail,
            E::add_extended_context(i, context.clone(), e),
        ))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(ParseError::append(
            i.clone(),
            nom::error::ErrorKind::Fail,
            E::add_extended_context(i, context.clone(), e),
        ))),
    }
}
