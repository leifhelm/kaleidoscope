use crate::pest::Parser;
use crate::{KaleidoscopeParser, Rule};

#[test]
fn variable_name() {
    good(Rule::variable_name, "_Käse99");
    bad(Rule::variable_name, "99Käse");
}
#[test]
fn literal_number() {
    good(Rule::literal_number, "99");
    good(Rule::literal_number, ".1");
    good(Rule::literal_number, "1.");
    good(Rule::literal_number, "1.e0");
    good(Rule::literal_number, "1.5678643E+59");
    good(Rule::literal_number, "5678643e-59");
    good(Rule::literal_number, "-673");
    good(Rule::literal_number, "-.314159");
    good(Rule::literal_number, "-21.");
    good(Rule::literal_number, "-23.E-1");
    good(Rule::literal_number, "-1.685431E+4");
    good(Rule::literal_number, "+55");
    good(Rule::literal_number, "+.0045");
    good(Rule::literal_number, "+7.");
    good(Rule::literal_number, "+87.e9");
    good(Rule::literal_number, "+3.14159E-1");
    good(Rule::literal_number, "+182e+59");

    bad(Rule::literal_number, ".");
    bad(Rule::literal_number, "E");
    bad(Rule::literal_number, "e");
    bad(Rule::literal_number, "E0");
    bad(Rule::literal_number, "1E");
    bad(Rule::literal_number, "-");
    bad(Rule::literal_number, "E-");
    bad(Rule::literal_number, "E+");
    bad(Rule::literal_number, "e-");
    bad(Rule::literal_number, "e+");
    bad(Rule::literal_number, "-.");
    bad(Rule::literal_number, "1 . 5 5 5");
    bad(Rule::literal_number, ". 1");
    bad(Rule::literal_number, ".//\n5");
}

#[test]
fn function_prototype_arguments() {
    good(Rule::function_prototype_arguments, "()");
    good(Rule::function_prototype_arguments, "(a)");
    good(Rule::function_prototype_arguments, "(_i,\t test)");
    good(Rule::function_prototype_arguments, "(\n  ident\n)");
    good(Rule::function_prototype_arguments, "(\n  i1,\n  i2\n)");
    good(Rule::function_prototype_arguments, "(a,b,)");
    good(Rule::function_prototype_arguments, "(a , )");

    bad(Rule::function_prototype_arguments, "(");
    bad(Rule::function_prototype_arguments, ")");
    bad(Rule::function_prototype_arguments, "asdf");
    bad(Rule::function_prototype_arguments, "(,");
    bad(Rule::function_prototype_arguments, "(,)");
    bad(Rule::function_prototype_arguments, "(,a)");
    bad(Rule::function_prototype_arguments, "(a ; a)");
}

#[test]
fn function_call() {
    good(Rule::function_call, "hi()");
    good(Rule::function_call, "times(a, b)");
    good(
        Rule::function_call,
        "massive_product(a1, a2, a3, a4, a5, a6, a7, a8, a9)",
    );
    good(Rule::function_call, "times(a, 5.4456)");
    good(
        Rule::function_call,
        "newline(\n  super_long_and_complicated_name,\n  (5 + 6/7)/4.3568896\n  )",
    );
    good(Rule::function_call, "extra_comma(a,b,)");

    bad(Rule::function_call, "()");
    bad(Rule::function_call, "invalid_comma(,)");
    bad(Rule::function_call, "power(+5, ");
    bad(Rule::function_call, "space (5)");
    bad(Rule::function_call, "comma(a; b + 5)");
    bad(Rule::function_call, "parenthesis(a, b))");
    bad(Rule::function_call, "_underline()");
    bad(Rule::function_call, "nested(inner(4), -4 + 4))");
}

#[test]
fn expression() {
    good(Rule::expression, "4");
    good(Rule::expression, "_a");
    good(Rule::expression, "foo(5)");
    good(Rule::expression, "no_args()");
    good(Rule::expression, "(9.3)");
    good(Rule::expression, "(foo(c))");
    good(Rule::expression, "(x)");
    good(Rule::expression, "(5 - foo(y) / (8 + r))");
    good(Rule::expression, "5 + a");
    good(Rule::expression, "foo(5, 4) + 67 / 34.3");
    good(Rule::expression, "7 - -4");
    good(Rule::expression, "7 -4");
    good(Rule::expression, "foo(4) - 6 /\n  (d + 48)\n  * 0.3");

    bad(Rule::expression, "2 ^ 24");
    bad(Rule::expression, "()");
    bad(Rule::expression, "((5)");
    bad(Rule::expression, "(func(-6.3)");
    bad(Rule::expression, "6 -7)");
}

#[test]
fn function_decleration() {
    good(Rule::function_decleration, "def func() 4");
    good(
        Rule::function_decleration,
        "def //function_decleration\n  half(x)\n    x / 2",
    );
    good(Rule::function_decleration, "def func(x)x");
    good(
        Rule::function_decleration,
        "def power(base, //The base number
exponent, //The exponent used to raise the base
)//Raise the base by the exponent
base*exponent",
    );
    good(
        Rule::function_decleration,
        "def//invalid function decleration\nfunc() 4",
    );
    good(Rule::function_decleration, "def camelCase(_x) 6.5");
    good(Rule::function_decleration, "def\nnewline()\n1");
    good(Rule::function_decleration, "def\ttab()\t7+pow(3,\t6.4)");

    bad(Rule::function_decleration, "deffunc() x");
    bad(Rule::function_decleration, "def func () x");
    bad(Rule::function_decleration, "def two() 2 2");
    bad(Rule::function_decleration, "define times_two(x) x*2");
}

#[test]
fn extern_function() {
    good(Rule::extern_function, "extern pi()");
    good(
        Rule::extern_function,
        "extern//a external function
sqrt(x//the number the square root gets calculated for
)",
    );
    good(Rule::extern_function, "extern pow(base, exponent,)");

    bad(Rule::extern_function, "extern");
    bad(Rule::extern_function, "external print(number)");
    bad(Rule::extern_function, "externfoo(n)");
    bad(Rule::extern_function, "extern foobar(,)");
    bad(Rule::extern_function, "(extern foobar(a,b,))");
    bad(Rule::extern_function, "def foo(a,b,c)");
}

#[test]
fn file() {
    good(Rule::file, "2 f()");
    good(Rule::file, "x f()");
    good(Rule::file, "xdef //hi\nfoo() 4\n");
    good(Rule::file, "foo()\ndef xyz() 5\n");
}

fn good(rule: Rule, input: &str) {
    assert_eq!(
        input,
        KaleidoscopeParser::parse(rule, input)
            .expect("parsing error")
            .as_str(),
        "did not consume all input"
    );
}

fn bad(rule: Rule, input: &str) {
    match KaleidoscopeParser::parse(rule, input) {
        Ok(pairs) => assert_ne!(input, pairs.as_str(), "No error and consumed all input"),
        _ => (),
    }
}

#[test]
fn parse() {
    let input = "def //hi\nfoo(x) +0003.1415e-3\n";
    // let input = "extern foo(x, y,)";
    match super::parse::<kaleidoscope_parser::located::Position>(input) {
        Err(err) => {
            err.print_codespan_reporting(
                "tests::parse",
                input,
                &mut termcolor::BufferedStandardStream::stdout(termcolor::ColorChoice::Auto),
            )
            .unwrap();
            panic!()
        }
        Ok(ast) => println!("{:#?}", ast),
    }
}
