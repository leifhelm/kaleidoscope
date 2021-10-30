use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub enum AST<X = ()> {
    Expression(Expression<X>),
    ExternFunction(FunctionPrototype<X>),
    Function(Function<X>),
}

pub type Identifier<X> = XWrapper<String, X>;
pub type LiteralNumber<X> = XWrapper<f64, X>;
pub type Expression<X> = XWrapper<Expr<X>, X>;
pub type Operator<X> = XWrapper<Op, X>;
pub type FunctionPrototype<X> = XWrapper<FunProto<X>, X>;
pub type Function<X> = XWrapper<Fun<X>, X>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<X> {
    LiteralNumber(LiteralNumber<X>),
    Variable(Identifier<X>),
    BinaryOperation(Box<BinaryOperation<X>>),
    If(Box<IfExpression<X>>),
    Call(Call<X>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation<X> {
    pub operation: Operator<X>,
    pub left_hand_side: Expression<X>,
    pub right_hand_side: Expression<X>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op {
    Divide,
    Multiply,
    Subtract,
    Add,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call<X> {
    pub callee: Identifier<X>,
    pub args: Vec<Expression<X>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression<X> {
    pub condition: Expression<X>,
    pub then: Expression<X>,
    pub r#else: Expression<X>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunProto<X> {
    pub name: Identifier<X>,
    pub args: Vec<Identifier<X>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun<X> {
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

    pub fn map<T>(self, f: impl FnOnce(Wrapped) -> T) -> XWrapper<T, X> {
        XWrapper {
            wrapped: f(self.wrapped),
            extra: self.extra,
        }
    }
}

impl<Wrapped, X> Deref for XWrapper<Wrapped, X> {
    type Target = Wrapped;

    fn deref(&self) -> &Self::Target {
        &self.wrapped
    }
}
