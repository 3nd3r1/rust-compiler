#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Addition,
    Substraction,
    Multiplication,
    Division,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal {
        value: i32,
    },
    Identifier {
        value: String,
    },
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operation,
    },
}
