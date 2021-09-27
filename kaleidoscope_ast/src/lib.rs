#[derive(Debug, Clone, PartialEq)]
pub enum AST<X = ()> {
    Expression(Expression<X>),
    ExternFunction(FunctionPrototype<X>),
    Function(Function<X>),
}

pub type Identifier<X> = XWrapper<String, X>;
pub type LiteralNumber<X> = XWrapper<f64, X>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<X> {
    LiteralNumber(LiteralNumber<X>),
    Variable(Identifier<X>),
    BinaryOperation(Box<BinaryOperation<X>>),
    Call(Call<X>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation<X> {
    pub operation: Operator<X>,
    pub left_hand_side: Expression<X>,
    pub right_hand_side: Expression<X>,
}

pub type Operator<X> = XWrapper<Op, X>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op {
    Divide,
    Multiply,
    Subtract,
    Add,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call<X> {
    pub callee: Identifier<X>,
    pub args: Vec<Expression<X>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionPrototype<X> {
    pub name: Identifier<X>,
    pub args: Vec<Identifier<X>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<X> {
    pub prototype: FunctionPrototype<X>,
    pub body: Box<Expression<X>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XWrapper<Wrapped, X> {
    pub wrapped: Wrapped,
    pub extra: X,
}

impl<Wrapped, X> XWrapper<Wrapped, X> {
    pub fn new(wrapped: Wrapped, extra: X) -> Self {
        XWrapper { wrapped, extra }
    }
}
