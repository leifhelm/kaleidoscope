use nom::{combinator::all_consuming, InputLength, Parser, Slice};

use crate::{
    args,
    error::Error,
    expression, extern_function, function_definition, identifier, if_then_else, literal_number,
    located::{Located, LocatedSlice, Position},
    parse_statements, variable_call,
};
use kaleidoscope_ast::XWrapper;
use std::fmt::Debug;

#[test]
fn test_identifier() {
    let input = LocatedSlice::new("hello");
    assert_eq!(
        Ok((
            input.slice(input.input_len()..),
            XWrapper::new(String::from("hello"), Position::new(0..5))
        )),
        identifier::<_, (), _>(input)
    );
    let input = LocatedSlice::new("hello ");
    assert_eq!(
        Ok((
            input.slice(5..),
            XWrapper::new(String::from("hello"), Position::new(0..5))
        )),
        identifier::<_, (), _>(input)
    );
    let input = LocatedSlice::new("64");
    assert_eq!(
        Err(nom::Err::Error((
            input.clone(),
            nom::error::ErrorKind::Char
        ))),
        identifier::<_, _, Position>(input)
    );
}
#[test]
fn test_literal_number() {
    let literal_number = literal_number::<_, _, Position>;
    good(literal_number, "99");
    good(literal_number, ".1");
    good(literal_number, "1.");
    good(literal_number, "1.e0");
    good(literal_number, "1.5678643E+59");
    good(literal_number, "5678643e-59");
    good(literal_number, "-673");
    good(literal_number, "-.314159");
    good(literal_number, "-21.");
    good(literal_number, "-23.E-1");
    good(literal_number, "-1.685431E+4");
    good(literal_number, "+55");
    good(literal_number, "+.0045");
    good(literal_number, "+7.");
    good(literal_number, "+87.e9");
    good(literal_number, "+3.14159E-1");
    good(literal_number, "+182e+59");

    bad(literal_number, ".");
    bad(literal_number, "E");
    bad(literal_number, "e");
    bad(literal_number, "E0");
    bad(literal_number, "1E");
    bad(literal_number, "-");
    bad(literal_number, "E-");
    bad(literal_number, "E+");
    bad(literal_number, "e-");
    bad(literal_number, "e+");
    bad(literal_number, "-.");
    bad(literal_number, "1 . 5 5 5");
    bad(literal_number, ". 1");
    bad(literal_number, ".//\n5");
}
#[test]
fn test_args() {
    let identifier = identifier::<_, _, Position>;
    good(args(identifier), "()");
    good(args(identifier), "(a)");
    good(args(identifier), "(_i,\t test)");
    good(args(identifier), "(\n  ident\n)");
    good(args(identifier), "(\n  i1,\n  i2\n)");
    good(args(identifier), "(a,b,)");
    good(args(identifier), "(a , )");

    bad(args(identifier), "(");
    bad(args(identifier), ")");
    bad(args(identifier), "asdf");
    bad(args(identifier), "(,");
    bad(args(identifier), "(,)");
    bad(args(identifier), "(,a)");
    bad(args(identifier), "(a ; a)");
}

#[test]
fn test_function_call() {
    let variable_call = variable_call::<_, _, Position>;
    good(variable_call, "hi()");
    good(variable_call, "times(a, b)");
    good(
        variable_call,
        "massive_product(a1, a2, a3, a4, a5, a6, a7, a8, a9)",
    );
    good(variable_call, "times(a, 5.4456)");
    good(
        variable_call,
        "newline(\n  super_long_and_complicated_name,\n  (5 + 6/7)/4.3568896\n  )",
    );
    good(variable_call, "_underline()");
    good(variable_call, "extra_comma(a,b,)");

    bad(variable_call, "()");
    bad(variable_call, "invalid_comma(,)");
    bad(variable_call, "power(+5, ");
    bad(variable_call, "space (5)");
    bad(variable_call, "comma(a; b + 5)");
    bad(variable_call, "parenthesis(a, b))");
    bad(variable_call, "nested(inner(4), -4 + 4))");
}

#[test]
fn test_if_then_else() {
    let if_then_else = if_then_else::<_, _, Position>;
    good(if_then_else, "if 1 then 1 else 0");
    good(if_then_else, "if var then 2 else var");
    good(
        if_then_else,
        "if\nnewline\nthen\nif//comment\ncomment then 3 else 9\nelse a(9)",
    );
    good(if_then_else, "if if double_if then 0 else 1 then a else b");
    good(
        if_then_else,
        "if if_else then\n  8.3\nelse if pi then\n  3.14\nelse\n  0",
    );
    good(if_then_else, "if (parenthesis)then (64)else (brackets)");
    good(
        if_then_else,
        "if (if parens_nested then p else n) then 8 else 43",
    );
    good(if_then_else, "if f(x) then f(y) else f(z)");
    good(if_then_else, "if double_then then then else 7");
    good(if_then_else, "if double_else then 78 else else");
    good(if_then_else, "if then_else then else else 09");
    good(if_then_else, "if else_then then 65 else then");
    good(if_then_else, "if then then else else else");
    good(if_then_else, "if then then then else else");
    good(if_then_else, "if else then else else then");
    good(
        if_then_else,
        "if if then then then else else then else else else",
    );

    bad(if_then_else, "if too_short then then else");
    bad(if_then_else, "if too_long then then else else else");
    bad(if_then_else, "if wrong_way_round else 83 then 55");
    bad(if_then_else, "if(parenthesis)then 437 else abc");
    bad(if_then_else, "if if wrong_nest then 4 then 41 else 8");
    bad(if_then_else, "if then_nest then if c then b else a");
}

#[test]
fn test_expression() {
    let expression = expression::<_, _, Position>;
    good(expression, "4");
    good(expression, "_a");
    good(expression, "foo(5)");
    good(expression, "no_args()");
    good(expression, "(9.3)");
    good(expression, "(foo(c))");
    good(expression, "(x)");
    good(expression, "(5 - foo(y) / (8 + r))");
    good(expression, "5 + a");
    good(expression, "foo(5, 4) + 67 / 34.3");
    good(expression, "7 - -4");
    good(expression, "7 -4");
    good(expression, "foo(4) - 6 /\n  (d + 48)\n  * 0.3");

    bad(expression, "2 ^ 24");
    bad(expression, "()");
    bad(expression, "((5)");
    bad(expression, "(func(-6.3)");
    bad(expression, "6 -7)");
}

#[test]
fn test_function_definition() {
    let function_definition = function_definition::<_, _, Position>;
    good(function_definition, "def func() 4");
    good(
        function_definition,
        "def //function_decleration\n  half(x)\n    x / 2",
    );
    good(function_definition, "def func(x)x");
    good(
        function_definition,
        "def power(base, //The base number
exponent, //The exponent used to raise the base
)//Raise the base by the exponent
base*exponent",
    );
    good(
        function_definition,
        "def//invalid function decleration\nfunc() 4",
    );
    good(function_definition, "def camelCase(_x) 6.5");
    good(function_definition, "def\nnewline()\n1");
    good(function_definition, "def\ttab()\t7+pow(3,\t6.4)");
    good(function_definition, "def def() 5");

    bad(function_definition, "deffunc() x");
    bad(function_definition, "def func () x");
    bad(function_definition, "def two() 2 2");
    bad(function_definition, "define times_two(x) x*2");
}
#[test]
fn test_extern_function() {
    let extern_function = extern_function::<_, _, Position>;
    good(extern_function, "extern pi()");
    good(
        extern_function,
        "extern//a external function
sqrt(x//the number the square root gets calculated for
)",
    );
    good(extern_function, "extern pow(base, exponent,)");
    good(extern_function, "extern extern(pi)");

    bad(extern_function, "extern");
    bad(extern_function, "external print(number)");
    bad(extern_function, "externfoo(n)");
    bad(extern_function, "extern foobar(,)");
    bad(extern_function, "(extern foobar(a,b,))");
    bad(extern_function, "def foo(a,b,c)");
}

#[test]
fn test_parse_statements() {
    let parse_statements = parse_statements::<_, _, Position>;
    good(parse_statements, "2 f()");
    good(parse_statements, "x f()");
    good(parse_statements, "xdef //hi\nfoo() 4\n");
    good(parse_statements, "foo()\ndef xyz() 5\n");
}

fn good<'a, T>(parser: impl Parser<LocatedSlice<'a>, T, Error<LocatedSlice<'a>>>, i: &'a str) {
    let input = LocatedSlice::new(i);
    match all_consuming(parser).parse(input) {
        Ok(_) => (),
        Err(e) => panic!("`{}`: Did not expect Err({:#?})", i, e),
    }
}
fn bad<'a, T: Debug>(
    parser: impl Parser<LocatedSlice<'a>, T, Error<LocatedSlice<'a>>>,
    i: &'a str,
) {
    let input = LocatedSlice::new(i);
    match all_consuming(parser).parse(input) {
        Ok(ok) => panic!("`{}`: Did not expect Ok({:#?})", i, ok),
        Err(_) => (),
    }
}
