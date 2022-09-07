use crate::{lexer::Token, parser::BMinorType};

#[derive(Debug)]
pub enum Expression<'a> {
    Variable {
        name: Token<'a>,
    },
    Literal {
        value: Token<'a>,
        kind: BMinorType,
    },
    Grouping {
        expression: Box<Expression<'a>>,
    },
    Unary {
        operator: Token<'a>,
        value: Box<Expression<'a>>,
    },
    Exponent {
        base: Box<Expression<'a>>,
        power: Box<Expression<'a>>,
    },
    Binary {
        left: Box<Expression<'a>>,
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Logical {
        left: Box<Expression<'a>>,
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Assignment {
        target: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
    },
    Call {
        target: Token<'a>,
        args: Vec<Expression<'a>>,
    },
    Subscript {
        target: Token<'a>,
        key: Box<Expression<'a>>,
    },
}
