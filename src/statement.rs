use crate::{
    expression::Expression,
    lexer::Token,
    parser::{Param, VariableType},
};

#[derive(Debug)]
pub enum Statement<'a> {
    Block {
        statements: Vec<Statement<'a>>,
    },
    Expression {
        expression: Expression<'a>,
    },
    FunctionDeclaration {
        name: Token<'a>,
        return_kind: Token<'a>,
        params: Vec<Param<'a>>,
        body: Box<Statement<'a>>,
    },
    If {
        condition: Expression<'a>,
        then: Box<Statement<'a>>,
        otherwise: Option<Box<Statement<'a>>>,
    },
    For {
        initializer: Option<Box<Statement<'a>>>,
        condition: Option<Expression<'a>>,
        increment: Option<Expression<'a>>,
        body: Box<Statement<'a>>,
    },
    Print {
        expressions: Vec<Expression<'a>>,
    },
    PrototypeDeclaration {
        name: Token<'a>,
        return_kind: Token<'a>,
        params: Vec<Param<'a>>,
    },
    Return {
        value: Option<Expression<'a>>,
    },
    VariableDeclaration {
        name: Token<'a>,
        variable_type: VariableType<'a>,
    },
}
