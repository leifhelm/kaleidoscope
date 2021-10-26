pub mod located;
#[cfg(test)]
mod tests;
use crate::located::Located;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::{fmt::Display, ops::Range};

use kaleidoscope_ast::{
    BinaryOperation, Call, Expr, Expression, Fun, FunProto, FunctionPrototype, Identifier,
    IfExpression, LiteralNumber, Op, Operator, XWrapper, AST,
};
use kaleidoscope_error::Error;
use lexical_parse_float::FromLexical;
use pest::{
    error::{ErrorVariant, InputLocation},
    iterators::{Pair, Pairs},
    Parser,
};

#[derive(Parser)]
#[grammar = "kaleidoscope.pest"]
pub struct KaleidoscopeParser;

pub fn parse<L: Located>(input: &str) -> Result<Vec<AST<L>>, Error> {
    match KaleidoscopeParser::parse(Rule::file, input) {
        Err(err) => Err(handle_parser_error(err)),
        Ok(mut pairs) => {
            let file = pairs.next().ok_or(global_internal_parser_error(
                "Parser did not return the file token",
            ))?;
            let mut ast = Vec::with_capacity(10);
            for pair in file.into_inner() {
                match pair.as_rule() {
                    Rule::toplevel => ast.push(toplevel(pair)?),
                    Rule::EOI => (),
                    _ => unexpected_pair::<()>(pair, Rule::file)?,
                }
            }
            Ok(ast)
        }
    }
}

fn handle_parser_error(error: pest::error::Error<Rule>) -> Error {
    let range = match error.location {
        InputLocation::Pos(p) => p..p,
        InputLocation::Span((start, end)) => start..end,
    };
    let message = match error.variant {
        ErrorVariant::ParsingError {
            positives,
            negatives,
        } => match (negatives.is_empty(), positives.is_empty()) {
            (false, false) => format!(
                "unexpected {}; expected {}",
                Rules::from(negatives),
                Rules::from(positives),
            ),
            (false, true) => format!("unexpected {}", Rules::from(negatives)),
            (true, false) => format!("expected {}", Rules::from(positives)),
            (true, true) => "unknown parsing error".to_owned(),
        },
        ErrorVariant::CustomError { message } => message,
    };
    Error::error(range, message)
}

struct Rules {
    rules: Vec<Rule>,
}

impl Display for Rules {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.rules.len() {
            1 => self.rules[0].fmt(f),
            2 => write!(f, "{} or {}", self.rules[0], self.rules[1]),
            l => {
                for r in self.rules.iter().take(l - 2) {
                    write!(f, "{}, ", r)?;
                }
                write!(f, "{} or {}", self.rules[l - 2], self.rules[l - 1])
            }
        }
    }
}

impl From<Vec<Rule>> for Rules {
    fn from(rules: Vec<Rule>) -> Self {
        Rules { rules }
    }
}

fn toplevel<L: Located>(pair: Pair<Rule>) -> Result<AST<L>, Error> {
    expect_rule(&pair, Rule::toplevel)?;
    if let Some(inner_pair) = pair.into_inner().next() {
        return match inner_pair.as_rule() {
            Rule::function_decleration => function_decleration(inner_pair),
            Rule::extern_function => extern_function(inner_pair),
            Rule::expression => Ok(AST::Expression(expression(inner_pair)?)),
            _ => unexpected_pair(inner_pair, Rule::toplevel),
        };
    }
    Err(global_internal_parser_error(
        "toplevel expression was empty",
    ))
}

fn function_decleration<L: Located>(pair: Pair<Rule>) -> Result<AST<L>, Error> {
    expect_rule(&pair, Rule::function_decleration)?;
    let range = range_from_pair(&pair);
    let mut iter = pair.into();
    safe_next(&mut iter, Rule::def_keyword)?;
    let prototype = function_prototype(safe_next(&mut iter, Rule::function_prototype)?)?;
    let body = expression(safe_next(&mut iter, Rule::expression)?)?;
    Ok(AST::Function(wrap(
        Fun {
            prototype,
            body: Box::new(body),
        },
        range,
    )))
}

fn function_prototype<L: Located>(pair: Pair<Rule>) -> Result<FunctionPrototype<L>, Error> {
    expect_rule(&pair, Rule::function_prototype)?;
    let range = range_from_pair(&pair);
    let mut iter = pair.into();
    let name = function_name(safe_next(&mut iter, Rule::function_name)?)?;
    let args =
        function_prototype_arguments(safe_next(&mut iter, Rule::function_prototype_arguments)?)?;
    Ok(wrap(FunProto { name, args }, range))
}

fn function_name<L: Located>(pair: Pair<Rule>) -> Result<Identifier<L>, Error> {
    expect_rule(&pair, Rule::function_name)?;
    let range = range_from_pair(&pair);
    let name = String::from(pair.as_str());
    Ok(wrap(name, range))
}
fn function_prototype_arguments<L: Located>(pair: Pair<Rule>) -> Result<Vec<Identifier<L>>, Error> {
    expect_rule(&pair, Rule::function_prototype_arguments)?;
    range_from_pair(&pair);
    let mut args = Vec::with_capacity(4);
    for var in pair.into_inner() {
        args.push(variable_name(var)?);
    }
    Ok(args)
}
fn variable_name<L: Located>(pair: Pair<Rule>) -> Result<Identifier<L>, Error> {
    expect_rule(&pair, Rule::variable_name)?;
    let range = range_from_pair(&pair);
    let name = String::from(pair.as_str());
    Ok(wrap(name, range))
}

fn extern_function<L: Located>(pair: Pair<Rule>) -> Result<AST<L>, Error> {
    expect_rule(&pair, Rule::extern_function)?;
    range_from_pair(&pair);
    let mut iter = pair.into();
    safe_next(&mut iter, Rule::extern_keyword)?;
    let prototype = function_prototype(safe_next(&mut iter, Rule::function_prototype)?)?;
    Ok(AST::ExternFunction(prototype))
}

fn expression<L: Located>(pair: Pair<Rule>) -> Result<Expression<L>, Error> {
    expect_rule(&pair, Rule::expression)?;
    let mut iter = pair.into();
    let mut lhs = term(safe_next(&mut iter, Rule::term)?)?;
    while let Some(op) = iter.pairs.next() {
        let op = operator(op)?;
        let rhs = term(safe_next(&mut iter, Rule::term)?)?;
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
                    let range_inner =
                        range_from_located::<L>(&bin_op_lhs.right_hand_side.extra, &rhs.extra);
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
    Ok(lhs)
}

fn term<L: Located>(pair: Pair<Rule>) -> Result<Expression<L>, Error> {
    expect_rule(&pair, Rule::term)?;
    let range = range_from_pair(&pair);
    let mut iter = pair.into();
    let term = safe_next(&mut iter, Rule::term)?;
    match term.as_rule() {
        Rule::literal_number => Ok(wrap(Expr::LiteralNumber(literal_number(term)?), range)),
        Rule::if_then_else => Ok(wrap(Expr::If(Box::new(if_then_else(term)?)), range)),
        Rule::function_call => Ok(wrap(Expr::Call(function_call(term)?), range)),
        Rule::variable_name => Ok(wrap(Expr::Variable(variable_name(term)?), range)),
        Rule::expression => Ok(expression(term)?),
        _ => unexpected_pair(term, Rule::term),
    }
}

fn operator<L: Located>(pair: Pair<Rule>) -> Result<Operator<L>, Error> {
    expect_rule(&pair, Rule::operator)?;
    let range = range_from_pair(&pair);
    let operator = match pair.as_str() {
        "+" => Ok(Op::Add),
        "-" => Ok(Op::Subtract),
        "*" => Ok(Op::Multiply),
        "/" => Ok(Op::Divide),
        ">" => Ok(Op::Greater),
        "<" => Ok(Op::Less),
        ">=" => Ok(Op::GreaterOrEqual),
        "<=" => Ok(Op::LessOrEqual),
        "==" => Ok(Op::Equal),
        "!=" => Ok(Op::NotEqual),
        op => internal_parser_error(
            range_from_pair(&pair),
            format!("Unexpected operator `{}`, expected {}", op, Rule::operator).as_str(),
        ),
    }?;
    Ok(wrap(operator, range))
}

fn literal_number<L: Located>(pair: Pair<Rule>) -> Result<LiteralNumber<L>, Error> {
    expect_rule(&pair, Rule::literal_number)?;
    let range = range_from_pair(&pair);
    match f64::from_lexical(pair.as_str().as_bytes()) {
        Ok(number) => Ok(wrap(number, range)),
        Err(_) => internal_parser_error(range_from_pair(&pair), "Invalid float literal"),
    }
}

fn if_then_else<L: Located>(pair: Pair<Rule>) -> Result<IfExpression<L>, Error> {
    expect_rule(&pair, Rule::if_then_else)?;
    let mut iter = pair.into();
    safe_next(&mut iter, Rule::if_keyword)?;
    let condition = expression(safe_next(&mut iter, Rule::expression)?)?;
    safe_next(&mut iter, Rule::then_keyword)?;
    let then = expression(safe_next(&mut iter, Rule::expression)?)?;
    safe_next(&mut iter, Rule::else_keyword)?;
    let r#else = expression(safe_next(&mut iter, Rule::expression)?)?;
    Ok(IfExpression {
        condition,
        then,
        r#else,
    })
}

fn function_call<L: Located>(pair: Pair<Rule>) -> Result<Call<L>, Error> {
    expect_rule(&pair, Rule::function_call)?;
    let mut iter = pair.into();
    let name = function_name(safe_next(&mut iter, Rule::function_name)?)?;
    let args = function_call_arguments(safe_next(&mut iter, Rule::function_call_arguments)?)?;
    Ok(Call { callee: name, args })
}

fn function_call_arguments<L: Located>(pair: Pair<Rule>) -> Result<Vec<Expression<L>>, Error> {
    expect_rule(&pair, Rule::function_call_arguments)?;
    let mut args = Vec::with_capacity(4);
    for expr in pair.into_inner() {
        args.push(expression(expr)?);
    }
    Ok(args)
}

fn unexpected_pair<T>(unexpected_pair: Pair<Rule>, rule: Rule) -> Result<T, Error> {
    internal_parser_error(
        range_from_pair(&unexpected_pair),
        format!(
            "Unexpected token {} while parsing {}",
            unexpected_pair.as_rule(),
            rule
        )
        .as_str(),
    )
}

fn internal_parser_error<T>(range: Range<usize>, error: &str) -> Result<T, Error> {
    Err(Error::bug(
        range,
        format!("Internal parser error: {}", error),
    ))
    /*TODO add help: This should not happen. Please open an issue*/
}

fn global_internal_parser_error(error: &str) -> Error {
    Error::global_bug(format!("Internal parser error: {}", error))
}

fn range_from_pair(pair: &Pair<Rule>) -> Range<usize> {
    let span = pair.as_span();
    span.start()..span.end()
}

fn range_from_located<L: Located>(start: &L, end: &L) -> Range<usize> {
    start.position().start..end.position().end
}

fn safe_next<'a>(iter: &mut Iter<'a>, expected_rule: Rule) -> Result<Pair<'a, Rule>, Error> {
    match iter.pairs.next() {
        Some(pair) => Ok(pair),
        None => Err(Error::bug(
            iter.range(),
            format!(
                "Internal parser error: Unexpected end of token stream, expected {}",
                expected_rule
            ),
        )),
    }
}

fn expect_rule(pair: &Pair<Rule>, expected_rule: Rule) -> Result<(), Error> {
    let rule = pair.as_rule();
    if rule == expected_rule {
        Ok(())
    } else {
        Err(Error::bug(
            range_from_pair(pair),
            format!(
                "Internal parser error: Expected token {} but was {}",
                expected_rule, rule
            ),
        ))
    }
}

fn wrap<T, L: Located>(wrapped: T, range: Range<usize>) -> XWrapper<T, L> {
    XWrapper::new(wrapped, Located::new(range))
}

#[derive(Debug, PartialEq, Clone)]
struct Iter<'a> {
    start: usize,
    pairs: Pairs<'a, Rule>,
}

impl<'a> Iter<'a> {
    fn range(&self) -> Range<usize> {
        self.start..self.start + self.pairs.as_str().len()
    }
}

impl<'a> From<Pair<'a, Rule>> for Iter<'a> {
    fn from(pair: Pair<'a, Rule>) -> Self {
        Iter {
            start: pair.as_span().start(),
            pairs: pair.into_inner(),
        }
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rule::WHITESPACE => f.write_str("whitespace"),
            Rule::COMMENT => f.write_str("commnet"),
            Rule::comma => f.write_str("`,`"),
            Rule::literal_number => f.write_str("literal number"),
            Rule::variable_name => f.write_str("variable name"),
            Rule::expression => f.write_str("expression"),
            Rule::term => f.write_str("term of an expression"),
            Rule::operator => {
                f.write_str("operator: (`+`, `-`, `*`, `/`, `>`, `<`, `>=`, `<=`, `==` or `!=`)")
            }
            Rule::if_then_else => f.write_str("if expression"),
            Rule::function_call => f.write_str("function call"),
            Rule::function_call_arguments => f.write_str("arguments for a function call"),
            Rule::function_name => f.write_str("function name"),
            Rule::keyword_after => f.write_str("whitespace or comment after a keyword"),
            Rule::def_keyword => f.write_str("`def` keyword"),
            Rule::extern_keyword => f.write_str("`extern` keyword"),
            Rule::if_keyword => f.write_str("`if` keyword"),
            Rule::then_keyword => f.write_str("then` keyword"),
            Rule::else_keyword => f.write_str("else` keyword"),
            Rule::function_decleration => f.write_str("function decleration"),
            Rule::extern_function => f.write_str("extern function"),
            Rule::function_prototype => f.write_str("function prototype"),
            Rule::function_prototype_arguments => f.write_str("arguments for a function prototype"),
            Rule::toplevel => f.write_str(
                "toplevel expression: function definition, extern function or expression",
            ),
            Rule::file => f.write_str("the whole file containing one or more toplevel expressions"),
            Rule::EOI => f.write_str("EOI: end of input"),
        }
    }
}
