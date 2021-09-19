#[derive(Debug, PartialEq)]
pub enum AST<X = ()> {
    Expression(Expression<X>),
    ExternFunction(FunctionPrototype<X>),
    Function(Function<X>),
}

pub type Identifier<X> = XWrapper<String, X>;

#[derive(Debug, PartialEq)]
pub enum Expression<X> {
    LiteralNumber(XWrapper<f64, X>),
    Variable(Identifier<X>),
    BinaryOperation(Box<BinaryOperation<X>>),
    Call(Call<X>),
}

#[derive(Debug, PartialEq)]
pub struct BinaryOperation<X> {
    pub operation: XWrapper<Op, X>,
    pub left_hand_side: Expression<X>,
    pub right_hand_side: Expression<X>,
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq)]
pub struct Call<X> {
    pub callee: Identifier<X>,
    pub args: Vec<Expression<X>>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionPrototype<X> {
    pub name: Identifier<X>,
    pub args: Vec<Identifier<X>>,
}

#[derive(Debug, PartialEq)]
pub struct Function<X> {
    pub prototype: FunctionPrototype<X>,
    pub body: Box<Expression<X>>,
}

#[derive(Debug, PartialEq)]
pub struct XWrapper<Wrapped, X> {
    pub wrapped: Wrapped,
    pub extra: X,
}

impl<Wrapped, X> XWrapper<Wrapped, X> {
    pub fn new(wrapped: Wrapped, extra: X) -> Self {
        XWrapper { wrapped, extra }
    }
    // pub fn wrap(wrapped: Wrapped) -> Self {
    //     XWrapper {
    //         wrapped,
    //         extra: None,
    //     }
    // }
}
