#[derive(Debug, PartialEq)]
pub enum AST {
    Expression(Expression),
    ExternFunction(FunctionPrototype),
    Function(Function),
}

pub type Identifier = String;

#[derive(Debug, PartialEq)]
pub enum Expression {
    LiteralNumber(f64),
    Variable(Identifier),
    BinaryOperation(Box<BinaryOperation>),
    Call(Call),
}

#[derive(Debug, PartialEq)]
pub struct BinaryOperation {
    pub operation: Op,
    pub left_hand_side: Expression,
    pub right_hand_side: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub callee: Identifier,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionPrototype {
    pub name: Identifier,
    pub args: Vec<Identifier>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub prototype: FunctionPrototype,
    pub body: Box<Expression>,
}
