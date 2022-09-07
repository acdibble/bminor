use crate::{
    expression::Expression,
    lexer::{Lexer, Token, TokenKind},
    statement::Statement,
};
use std::{
    convert::{TryFrom, TryInto},
    fmt::Display,
    iter::Peekable,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedParserError,
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum BMinorType {
    Void,
    Boolean,
    Char,
    Integer,
    String,
    Array,
    Map,
    Function,
}

impl Display for BMinorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            BMinorType::Void => "void",
            BMinorType::Boolean => "boolean",
            BMinorType::Char => "char",
            BMinorType::Integer => "integer",
            BMinorType::String => "string",
            BMinorType::Array => "array",
            BMinorType::Map => "map",
            BMinorType::Function => "function",
        };
        write!(f, "{}", value)
    }
}

impl TryFrom<&Token<'_>> for BMinorType {
    type Error = ParseError;

    fn try_from(token: &Token<'_>) -> ParseResult<Self> {
        match token.kind {
            TokenKind::Void => Ok(Self::Void),
            TokenKind::Boolean | TokenKind::True | TokenKind::False => Ok(Self::Boolean),
            TokenKind::CharLiteral | TokenKind::Char => Ok(Self::Char),
            TokenKind::IntegerLiteral | TokenKind::Integer => Ok(Self::Integer),
            TokenKind::StringLiteral | TokenKind::String => Ok(Self::String),
            TokenKind::Array => Ok(Self::Array),
            TokenKind::Map => Ok(Self::Map),
            TokenKind::Function => Ok(Self::Function),
            _ => Err(ParseError::UnexpectedToken(format!(
                "failed to convert {:?} to type",
                token
            ))),
        }
    }
}

impl TryFrom<Token<'_>> for BMinorType {
    type Error = ParseError;

    fn try_from(token: Token<'_>) -> ParseResult<Self> {
        Self::try_from(&token)
    }
}

const ATOMIC_TYPES: [TokenKind; 4] = [
    TokenKind::Boolean,
    TokenKind::Char,
    TokenKind::Integer,
    TokenKind::String,
];

#[derive(Debug)]
pub enum ParamType {
    Atomic {
        kind: BMinorType,
    },
    Array {
        kind: BMinorType,
    },
    Map {
        key_kind: BMinorType,
        value_kind: BMinorType,
    },
}

#[derive(Debug)]
pub struct Param<'a> {
    pub name: Token<'a>,
    pub kind: ParamType,
}

#[derive(Debug)]
pub enum VariableType<'a> {
    Atomic {
        kind: BMinorType,
        initializer: Option<Expression<'a>>,
    },
    Array {
        kind: BMinorType,
        size: Expression<'a>,
        initializer: Option<Vec<Expression<'a>>>,
    },
    Map {
        key_kind: BMinorType,
        value_kind: BMinorType,
        initializer: Option<Vec<(Expression<'a>, Expression<'a>)>>,
    },
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    done: bool,
    outstanding: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let tokens = Lexer::new(input).into_iter().peekable();

        Self {
            tokens,
            done: false,
            outstanding: None,
        }
    }

    fn advance(&mut self) -> ParseResult<Token<'a>> {
        self.outstanding
            .take()
            .or_else(|| self.tokens.next())
            .ok_or(ParseError::UnexpectedParserError)
    }

    fn peek(&mut self) -> Option<&Token<'a>> {
        self.outstanding.as_ref().or_else(|| self.tokens.peek())
    }

    fn matches(&mut self, expected_kind: TokenKind) -> bool {
        match self.peek() {
            Some(token) => token.kind == expected_kind,
            _ => false,
        }
    }

    fn consume(&mut self, expected_kinds: &[TokenKind]) -> Option<Token<'a>> {
        match self.outstanding.as_ref() {
            Some(token) if expected_kinds.contains(&token.kind) => self.outstanding.take(),
            Some(_) => None,
            None => self.tokens.next_if(|t| expected_kinds.contains(&t.kind)),
        }
    }

    fn expect(
        &mut self,
        expected_kinds: &[TokenKind],
        message: &'static str,
    ) -> ParseResult<Token<'a>> {
        if let Some(token) = self.consume(expected_kinds) {
            Ok(token)
        } else {
            println!("{:?}", self.peek());
            Err(ParseError::UnexpectedToken(message.to_owned()))
        }
    }

    fn error_message<T>(&mut self, message: &str) -> ParseResult<T> {
        Err(ParseError::UnexpectedToken(message.to_owned()))
    }

    fn statement(&mut self) -> ParseResult<Statement<'a>> {
        let next = if let Some(token) = self.outstanding.take() {
            token
        } else {
            self.advance()?
        };

        let stmt = match &next.kind {
            TokenKind::LeftBrace => self.block_statement(),
            TokenKind::Identifier if self.consume(&[TokenKind::Colon]).is_some() => {
                self.declaration_statement(next)
            }
            TokenKind::For => self.for_statement(),
            TokenKind::If => self.if_statement(),
            TokenKind::Print => self.print_statement(),
            TokenKind::Return => self.return_statement(),
            _ => {
                self.outstanding = Some(next);
                self.expression_statement()
            }
        }?;

        Ok(stmt)
    }

    fn block_statement(&mut self) -> ParseResult<Statement<'a>> {
        let mut statements = Vec::new();

        while self.consume(&[TokenKind::RightBrace]).is_none() {
            statements.push(self.statement()?);
        }

        Ok(Statement::Block { statements })
    }

    fn declaration_statement(&mut self, name: Token<'a>) -> ParseResult<Statement<'a>> {
        let kind = match self.consume(&[
            TokenKind::Boolean,
            TokenKind::Char,
            TokenKind::Integer,
            TokenKind::String,
            TokenKind::Array,
            TokenKind::Map,
            TokenKind::Function,
        ]) {
            Some(token) => token,
            _ => return self.error_message("expect type after variable declaration"),
        };

        match &kind.kind {
            TokenKind::Boolean | TokenKind::Char | TokenKind::Integer | TokenKind::String => {
                let initializer = if self.matches(TokenKind::Semicolon) {
                    None
                } else {
                    self.expect(&[TokenKind::Equal], "expect '=' after declaration")?;
                    Some(self.expression()?)
                };

                self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;

                Ok(Statement::VariableDeclaration {
                    name,
                    variable_type: VariableType::Atomic {
                        kind: kind.try_into()?,
                        initializer,
                    },
                })
            }
            TokenKind::Array => {
                self.expect(&[TokenKind::LeftBracket], "expect left bracket")?;
                let size = self.expression()?;
                self.expect(&[TokenKind::RightBracket], "expect right bracket")?;
                let kind = self.expect(&ATOMIC_TYPES, "expect array type")?;

                let initializer = if self.matches(TokenKind::Semicolon) {
                    None
                } else {
                    self.expect(&[TokenKind::Equal], "expect '=' after declaration")?;
                    self.expect(&[TokenKind::LeftBrace], "expect '{' before array elements")?;

                    let mut elements = Vec::new();

                    loop {
                        elements.push(self.expression()?);

                        if self.consume(&[TokenKind::Comma]).is_none() {
                            break;
                        }
                    }

                    self.expect(&[TokenKind::RightBrace], "expect '}' after array elements")?;

                    Some(elements)
                };

                self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;

                Ok(Statement::VariableDeclaration {
                    name,
                    variable_type: VariableType::Array {
                        kind: kind.try_into()?,
                        size,
                        initializer,
                    },
                })
            }
            TokenKind::Map => {
                let key = self.expect(&ATOMIC_TYPES, "expect map key type")?;
                let value = self.expect(&ATOMIC_TYPES, "expect map value type")?;

                let initializer = if self.matches(TokenKind::Semicolon) {
                    None
                } else {
                    // VariableType::Map(key, value)
                    let mut elements = Vec::new();

                    self.expect(&[TokenKind::Equal], "expect '=' after declaration")?;
                    self.expect(&[TokenKind::LeftBrace], "expect '{' before map values")?;

                    loop {
                        let key = self.expression()?;
                        self.expect(&[TokenKind::Colon], "expect ';' after key")?;
                        let value = self.expression()?;
                        elements.push((key, value));

                        if self.consume(&[TokenKind::Comma]).is_none() {
                            break;
                        }
                    }

                    self.expect(&[TokenKind::RightBrace], "expect '}' after map values")?;

                    Some(elements)
                };

                self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;

                Ok(Statement::VariableDeclaration {
                    name,
                    variable_type: VariableType::Map {
                        key_kind: key.try_into()?,
                        value_kind: value.try_into()?,
                        initializer,
                    },
                })
            }
            TokenKind::Function => {
                let return_kind = self.expect(
                    &[
                        TokenKind::Boolean,
                        TokenKind::Char,
                        TokenKind::Integer,
                        TokenKind::String,
                        TokenKind::Void,
                    ],
                    "expect return type after 'function'",
                )?;

                self.expect(
                    &[TokenKind::LeftParen],
                    "expect '(' after function return type",
                )?;

                let mut params = Vec::new();

                if !self.matches(TokenKind::RightParen) {
                    loop {
                        let name =
                            self.expect(&[TokenKind::Identifier], "expect parameter identifier")?;
                        self.expect(&[TokenKind::Colon], "expect ':' after param identifier")?;
                        let token = self.expect(
                            &[
                                TokenKind::Boolean,
                                TokenKind::Char,
                                TokenKind::Integer,
                                TokenKind::String,
                                TokenKind::Array,
                                TokenKind::Map,
                            ],
                            "expect param type",
                        )?;

                        let kind = match token.kind {
                            TokenKind::Boolean
                            | TokenKind::Char
                            | TokenKind::Integer
                            | TokenKind::String => ParamType::Atomic {
                                kind: token.try_into()?,
                            },
                            TokenKind::Array => {
                                self.expect(&[TokenKind::LeftBracket], "expect '[' after 'array")?;
                                self.expect(&[TokenKind::RightBracket], "expect ']'")?;
                                let kind = self.expect(&ATOMIC_TYPES, "expect array type")?;
                                ParamType::Array {
                                    kind: kind.try_into()?,
                                }
                            }
                            TokenKind::Map => {
                                let key_kind = self.expect(&ATOMIC_TYPES, "expect map key type")?;
                                let value_kind =
                                    self.expect(&ATOMIC_TYPES, "expect map value type")?;
                                ParamType::Map {
                                    key_kind: key_kind.try_into()?,
                                    value_kind: value_kind.try_into()?,
                                }
                            }
                            _ => unreachable!(),
                        };

                        params.push(Param { name, kind });

                        if self.consume(&[TokenKind::Comma]).is_none() {
                            break;
                        }
                    }
                }

                self.expect(&[TokenKind::RightParen], "expect ')' after param list")?;

                if self.consume(&[TokenKind::Semicolon]).is_some() {
                    return Ok(Statement::PrototypeDeclaration {
                        name,
                        return_kind,
                        params,
                    });
                }

                self.expect(&[TokenKind::Equal], "expect '=' after param list")?;
                self.expect(&[TokenKind::LeftBrace], "expect function body")?;

                let body = self.block_statement()?;

                Ok(Statement::FunctionDeclaration {
                    name,
                    return_kind,
                    params,
                    body: Box::from(body),
                })
            }
            _ => unreachable!(),
        }
    }

    fn expression_statement(&mut self) -> ParseResult<Statement<'a>> {
        let expression = self
            .expression()
            .or_else(|_| self.error_message("expect statement"))?;
        self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;
        Ok(Statement::Expression { expression })
    }

    fn for_statement(&mut self) -> ParseResult<Statement<'a>> {
        self.expect(&[TokenKind::LeftParen], "expect '(' after for")?;

        let token = self.advance()?;

        let initializer = match &token.kind {
            TokenKind::Semicolon => None,
            TokenKind::Identifier if self.consume(&[TokenKind::Colon]).is_some() => {
                self.declaration_statement(token).ok()
            }
            _ => {
                self.outstanding = Some(token);
                self.expression_statement().ok()
            }
        }
        .map(Box::from);

        let condition = self.expression().ok();

        self.expect(&[TokenKind::Semicolon], "expect ';' after condition")?;

        let increment = self.expression().ok();

        self.expect(&[TokenKind::RightParen], "expect ')' after increment")?;

        let body = Box::from(self.statement()?);

        Ok(Statement::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    fn if_statement(&mut self) -> ParseResult<Statement<'a>> {
        self.expect(&[TokenKind::LeftParen], "expect '(' after if")?;

        let condition = self.expression()?;

        self.expect(&[TokenKind::RightParen], "expect ')' after condition")?;

        let then = self.statement()?;

        let otherwise = if self.consume(&[TokenKind::Else]).is_some() {
            Some(Box::from(self.statement()?))
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then: Box::from(then),
            otherwise,
        })
    }

    fn print_statement(&mut self) -> ParseResult<Statement<'a>> {
        let mut expressions = Vec::new();

        loop {
            expressions.push(self.expression()?);

            if self.consume(&[TokenKind::Comma]).is_none() {
                break;
            }
        }

        self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;

        Ok(Statement::Print { expressions })
    }

    fn return_statement(&mut self) -> ParseResult<Statement<'a>> {
        let value = self.expression().ok();

        self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;

        Ok(Statement::Return { value })
    }

    fn expression(&mut self) -> ParseResult<Expression<'a>> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.or()?;

        if let Some(_) = self.consume(&[TokenKind::Equal]) {
            expr = match expr {
                Expression::Variable { name } => Expression::Assignment {
                    target: name,
                    value: Box::from(self.assignment()?),
                },
                _ => return self.error_message("Invalid assignment target."),
            };
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.and()?;

        while let Some(operator) = self.consume(&[TokenKind::Or]) {
            expr = Expression::Logical {
                left: Box::from(expr),
                operator,
                right: Box::from(self.and()?),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.consume(&[TokenKind::And]) {
            expr = Expression::Logical {
                left: Box::from(expr),
                operator,
                right: Box::from(self.comparison()?),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.term()?;

        while let Some(operator) = self.consume(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::EqualEqual,
            TokenKind::BangEqual,
        ]) {
            expr = Expression::Binary {
                left: Box::from(expr),
                operator,
                right: Box::from(self.term()?),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.factor()?;

        while let Some(operator) = self.consume(&[TokenKind::Plus, TokenKind::Minus]) {
            expr = Expression::Binary {
                left: Box::from(expr),
                operator,
                right: Box::from(self.factor()?),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.exponent()?;

        while let Some(operator) =
            self.consume(&[TokenKind::Star, TokenKind::Slash, TokenKind::Modulo])
        {
            expr = Expression::Binary {
                left: Box::from(expr),
                operator,
                right: Box::from(self.exponent()?),
            }
        }

        Ok(expr)
    }

    fn exponent(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.unary()?;

        if let Some(_) = self.consume(&[TokenKind::Exponent]) {
            let power = self.unary()?;

            expr = Expression::Exponent {
                base: Box::from(expr),
                power: Box::from(power),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expression<'a>> {
        if let Some(operator) = self.consume(&[TokenKind::Bang, TokenKind::Plus, TokenKind::Minus])
        {
            Ok(Expression::Unary {
                operator,
                value: Box::from(self.call()?),
            })
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.primary()?;

        if let Expression::Variable { name } = expr {
            expr = if self.consume(&[TokenKind::LeftParen]).is_some() {
                let mut args = Vec::new();

                if !self.matches(TokenKind::RightParen) {
                    loop {
                        args.push(self.expression()?);
                        if self.consume(&[TokenKind::Comma]).is_none() {
                            break;
                        }
                    }
                }

                self.expect(&[TokenKind::RightParen], "expect ')' after arg list")?;

                Expression::Call { name, args }
            } else {
                Expression::Variable { name }
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> ParseResult<Expression<'a>> {
        if let Some(value) = self.consume(&[
            TokenKind::CharLiteral,
            TokenKind::IntegerLiteral,
            TokenKind::StringLiteral,
        ]) {
            Ok(Expression::Literal {
                kind: BMinorType::try_from(&value)?,
                value,
            })
        } else if let Some(value) = self.consume(&[TokenKind::True, TokenKind::False]) {
            Ok(Expression::Literal {
                kind: BMinorType::try_from(&value)?,
                value,
            })
        } else if let Some(name) = self.consume(&[TokenKind::Identifier]) {
            self.handle_postfix(Expression::Variable { name })
        } else if let Some(_) = self.consume(&[TokenKind::LeftParen]) {
            let expression = self.expression()?;

            self.expect(
                &[TokenKind::RightParen],
                "expect ')' after grouping expression",
            )?;

            self.handle_postfix(Expression::Grouping {
                expression: Box::from(expression),
            })
        } else {
            self.error_message("expect expression")
        }
    }

    fn handle_postfix(&mut self, expr: Expression<'a>) -> ParseResult<Expression<'a>> {
        if let Some(operator) = self.consume(&[TokenKind::PlusPlus, TokenKind::MinusMinus]) {
            Ok(Expression::Unary {
                operator,
                value: Box::from(expr),
            })
        } else {
            Ok(expr)
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = ParseResult<Statement<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        if let Some(_) = self.consume(&[TokenKind::EOF]) {
            self.done = true;
            return None;
        }

        Some(self.statement())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use insta;

    fn parse_expression(string: &str) -> ParseResult<Expression> {
        Parser::new(string).expression()
    }

    fn parse_statement(string: &str) -> ParseResult<Statement> {
        Parser::new(string).statement()
    }

    #[test]
    fn test_primary_expression() {
        insta::assert_debug_snapshot!(parse_expression("'a'"));
        insta::assert_debug_snapshot!(parse_expression("1"));
        insta::assert_debug_snapshot!(parse_expression("\"string literal\""));
        insta::assert_debug_snapshot!(parse_expression("x"));
        insta::assert_debug_snapshot!(parse_expression("(2)"));
    }

    #[test]
    fn test_unary_expression() {
        insta::assert_debug_snapshot!(parse_expression("+1"));
        insta::assert_debug_snapshot!(parse_expression("-1"));
        insta::assert_debug_snapshot!(parse_expression("x++"));
        insta::assert_debug_snapshot!(parse_expression("x--"));
        insta::assert_debug_snapshot!(parse_expression("!true"));
    }

    #[test]
    fn test_exponent_expression() {
        insta::assert_debug_snapshot!(parse_expression("-1 ^ x++"));
        insta::assert_debug_snapshot!(parse_expression("(-1) ^ (x++)"));
    }

    #[test]
    fn test_binary_expression() {
        insta::assert_debug_snapshot!(parse_expression("1 * 2"));
        insta::assert_debug_snapshot!(parse_expression("1 / 3"));
        insta::assert_debug_snapshot!(parse_expression("1 % 4"));
        insta::assert_debug_snapshot!(parse_expression("1 * 2 + 3 / 4 - 5"));
    }

    #[test]
    fn test_comparison_expression() {
        insta::assert_debug_snapshot!(parse_expression("a == b"));
        insta::assert_debug_snapshot!(parse_expression("a != b"));
        insta::assert_debug_snapshot!(parse_expression("a > b"));
        insta::assert_debug_snapshot!(parse_expression("a >= b"));
        insta::assert_debug_snapshot!(parse_expression("a < b"));
        insta::assert_debug_snapshot!(parse_expression("a <= b"));
        insta::assert_debug_snapshot!(parse_expression("a <= b == c"));
    }

    #[test]
    fn test_logical_expression() {
        insta::assert_debug_snapshot!(parse_expression("a && b"));
        insta::assert_debug_snapshot!(parse_expression("a && b && c"));
        insta::assert_debug_snapshot!(parse_expression("a || b"));
        insta::assert_debug_snapshot!(parse_expression("a || b || c"));
        insta::assert_debug_snapshot!(parse_expression("a && b || c"));
        insta::assert_debug_snapshot!(parse_expression("a || b && c"));
    }

    #[test]
    fn test_assignment_expression() {
        insta::assert_debug_snapshot!(parse_expression("x = a && b"));
    }

    #[test]
    fn test_call_expression() {
        insta::assert_debug_snapshot!(parse_expression("x()"));
        insta::assert_debug_snapshot!(parse_expression("x(a)"));
        insta::assert_debug_snapshot!(parse_expression("x(a, 1))"));
        insta::assert_debug_snapshot!(parse_expression("x(a, 1, y = 2, z())"));
        insta::assert_debug_snapshot!(parse_expression("x(a, 1, y = 2, z(g()))"));
        insta::assert_debug_snapshot!(parse_expression("x(a,"));
        insta::assert_debug_snapshot!(parse_expression("x(a"));
        insta::assert_debug_snapshot!(parse_expression("x("));
    }

    #[test]
    fn test_declaration_statement() {
        insta::assert_debug_snapshot!(parse_statement("int: integer;"));
        insta::assert_debug_snapshot!(parse_statement("int: integer = 2;"));
        insta::assert_debug_snapshot!(parse_statement("int: integer = 2 * 2;"));

        insta::assert_debug_snapshot!(parse_statement("bool: boolean;"));
        insta::assert_debug_snapshot!(parse_statement("ch: char;"));
        insta::assert_debug_snapshot!(parse_statement("str: string;"));

        insta::assert_debug_snapshot!(parse_statement("arr: array;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1];"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] integer;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] integer =;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] integer = {;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] integer = {1;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] integer = {1};"));
        insta::assert_debug_snapshot!(parse_statement(
            "arr: array [1] integer = {1,
            2,
            3
        };"
        ));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] char;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] boolean;"));
        insta::assert_debug_snapshot!(parse_statement("arr: array [1] string;"));

        insta::assert_debug_snapshot!(parse_statement("m: map;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string int;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string integer;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string integer =;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string integer = {;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string integer = {\"key\";"));
        insta::assert_debug_snapshot!(parse_statement("m: map string integer = {\"key\":;"));
        insta::assert_debug_snapshot!(parse_statement("m: map string integer = {\"key\":0;"));
        insta::assert_debug_snapshot!(parse_statement(
            "m: map string integer = {
                \"key\": 0,
                \"key2\":1
            };"
        ));

        insta::assert_debug_snapshot!(parse_statement("fn: function;"));
        insta::assert_debug_snapshot!(parse_statement("fn: function void;"));
        insta::assert_debug_snapshot!(parse_statement("fn: function char;"));
        insta::assert_debug_snapshot!(parse_statement("fn: function integer;"));
        insta::assert_debug_snapshot!(parse_statement("fn: function string;"));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean;"));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean ()"));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean () ="));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean () = {"));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean () = {}"));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean (a: integer) = {}"));
        insta::assert_debug_snapshot!(parse_statement(
            "fn: function boolean (a: integer, b: string, c: char, d: array [] integer, e: boolean, f: map string string) = {}"
        ));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean (a: array ["));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean (a: array []"));
        insta::assert_debug_snapshot!(parse_statement(
            "fn: function boolean (a: array [] string);"
        ));
        insta::assert_debug_snapshot!(parse_statement("fn: function boolean (a: array [] char);"));
        insta::assert_debug_snapshot!(parse_statement(
            "fn: function boolean (a: array [] integer);"
        ));
        insta::assert_debug_snapshot!(parse_statement(
            "fn: function boolean (a: array [] boolean);"
        ));
    }

    #[test]
    fn test_block_statements() {
        insta::assert_debug_snapshot!(parse_statement("{}"));
        insta::assert_debug_snapshot!(parse_statement(
            "{
    x: integer;
    y: string = \"hello world\";
}"
        ));
    }

    #[test]
    fn test_print_statements() {
        insta::assert_debug_snapshot!(parse_statement("print;"));
        insta::assert_debug_snapshot!(parse_statement("print 1;"));
        insta::assert_debug_snapshot!(parse_statement("print 1, 2 + 2, \"string\";"));
        insta::assert_debug_snapshot!(parse_statement("print 1"));
        insta::assert_debug_snapshot!(parse_statement("print fun();"));
    }

    #[test]
    fn test_return_statements() {
        insta::assert_debug_snapshot!(parse_statement("return"));
        insta::assert_debug_snapshot!(parse_statement("return;"));
        insta::assert_debug_snapshot!(parse_statement("return 1;"));
        insta::assert_debug_snapshot!(parse_statement("return fn(0);"));
    }

    #[test]
    fn test_if_statement() {
        insta::assert_debug_snapshot!(parse_statement("if true"));
        insta::assert_debug_snapshot!(parse_statement("if ()"));
        insta::assert_debug_snapshot!(parse_statement("if (false;"));
        insta::assert_debug_snapshot!(parse_statement("if (true) return;"));
        insta::assert_debug_snapshot!(parse_statement("if (true) return 1; else return 0;"));
        insta::assert_debug_snapshot!(parse_statement(
            "if (true) {
    return 1;
} else {
    return 0;
}"
        ));
        insta::assert_debug_snapshot!(parse_statement(
            "if (true) if (false) return 2; else return 0;"
        ));
    }

    #[test]
    fn test_expression_statement() {
        insta::assert_debug_snapshot!(parse_statement("true;"));
        insta::assert_debug_snapshot!(parse_statement("false;"));
        insta::assert_debug_snapshot!(parse_statement("1;"));
        insta::assert_debug_snapshot!(parse_statement("'c';"));
        insta::assert_debug_snapshot!(parse_statement("\"string\";"));
        insta::assert_debug_snapshot!(parse_statement("1 + 1;"));
        insta::assert_debug_snapshot!(parse_statement("true || false;"));
        insta::assert_debug_snapshot!(parse_statement("fn();"));
        insta::assert_debug_snapshot!(parse_statement("x = fn();"));
        insta::assert_debug_snapshot!(parse_statement("1 * 1;"));
        insta::assert_debug_snapshot!(parse_statement("x = 1 + 1;"));
    }

    #[test]
    fn test_for_statement() {
        insta::assert_debug_snapshot!(parse_statement("for"));
        insta::assert_debug_snapshot!(parse_statement("for ("));
        insta::assert_debug_snapshot!(parse_statement("for (;"));
        insta::assert_debug_snapshot!(parse_statement("for (;;"));
        insta::assert_debug_snapshot!(parse_statement("for (;;)"));
        insta::assert_debug_snapshot!(parse_statement("for (;;) {}"));
        insta::assert_debug_snapshot!(parse_statement("for (x: integer = 1;;) {}"));
        insta::assert_debug_snapshot!(parse_statement("for (x: integer = 1; x < 2;) {}"));
        insta::assert_debug_snapshot!(parse_statement("for (x: integer = 1; x < 2; x = x + 1) {}"));
        insta::assert_debug_snapshot!(parse_statement("for (x: integer = 1;; x = x + 1) {}"));
        insta::assert_debug_snapshot!(parse_statement("for (; x < 2;) {}"));
        insta::assert_debug_snapshot!(parse_statement("for (x = 1;;) {}"));
    }
}
