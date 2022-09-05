use crate::lexer::{Lexer, Token, TokenKind};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Expression<'a> {
    Variable {
        name: Token<'a>,
    },
    Literal {
        value: Token<'a>,
    },
    Boolean {
        expression: Box<Expression<'a>>,
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
        target: Token<'a>,
        value: Box<Expression<'a>>,
    },
}

#[derive(Debug)]
pub enum VariableType<'a> {
    Atomic {
        kind: Token<'a>,
        initializer: Option<Expression<'a>>,
    },
    Array {
        kind: Token<'a>,
        size: Expression<'a>,
        initializer: Option<Vec<Expression<'a>>>,
    },
    Map {
        key_kind: Token<'a>,
        value_kind: Token<'a>,
        initializer: Option<Vec<(Expression<'a>, Expression<'a>)>>,
    },
}

#[derive(Debug)]
pub enum Statement<'a> {
    Block {
        statements: Vec<Statement<'a>>,
    },
    Declaration {
        name: Token<'a>,
        variable_type: VariableType<'a>,
    },
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedParserError,
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    done: bool,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let tokens = Lexer::new(input).into_iter().peekable();

        Self {
            tokens,
            done: false,
        }
    }

    fn advance(&mut self) -> ParseResult<Token<'a>> {
        self.tokens.next().ok_or(ParseError::UnexpectedParserError)
    }

    fn matches(&mut self, expected_kind: TokenKind) -> bool {
        match self.tokens.peek() {
            Some(token) => token.kind == expected_kind,
            _ => false,
        }
    }

    fn consume(&mut self, expected_kinds: &[TokenKind]) -> Option<Token<'a>> {
        self.tokens.next_if(|t| expected_kinds.contains(&t.kind))
    }

    fn expect(
        &mut self,
        expected_kinds: &[TokenKind],
        message: &'static str,
    ) -> ParseResult<Token<'a>> {
        if let Some(token) = self.consume(expected_kinds) {
            Ok(token)
        } else {
            println!("{:?}", self.tokens.peek());
            Err(ParseError::UnexpectedToken(message.to_owned()))
        }
    }

    fn error<T>(&mut self) -> ParseResult<T> {
        match self.tokens.peek() {
            Some(token) => Err(ParseError::UnexpectedToken(format!("{:?}", token))),
            None => Err(ParseError::UnexpectedParserError),
        }
    }

    fn error_message<T>(&mut self, message: &str) -> ParseResult<T> {
        Err(ParseError::UnexpectedToken(message.to_owned()))
    }

    fn statement(&mut self) -> ParseResult<Statement<'a>> {
        let next = self.advance()?;

        let stmt = match &next.kind {
            TokenKind::LeftBrace => self.block_statement(),
            TokenKind::Identifier if self.consume(&[TokenKind::Colon]).is_some() => {
                let result = self.declaration_statement(next)?;
                self.expect(&[TokenKind::Semicolon], "expect ';' after statement")?;
                Ok(result)
            }
            _ => todo!(),
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

                Ok(Statement::Declaration {
                    name,
                    variable_type: VariableType::Atomic { kind, initializer },
                })
            }
            TokenKind::Array => {
                self.expect(&[TokenKind::LeftBracket], "expect left bracket")?;
                let size = self.expression()?;
                self.expect(&[TokenKind::RightBracket], "expect right bracket")?;
                let kind = self.expect(
                    &[
                        TokenKind::Boolean,
                        TokenKind::Char,
                        TokenKind::Integer,
                        TokenKind::String,
                    ],
                    "expect array type",
                )?;

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

                Ok(Statement::Declaration {
                    name,
                    variable_type: VariableType::Array {
                        kind,
                        size,
                        initializer,
                    },
                })
            }
            TokenKind::Map => {
                let key = self.expect(
                    &[
                        TokenKind::Boolean,
                        TokenKind::Char,
                        TokenKind::Integer,
                        TokenKind::String,
                    ],
                    "expect map key type",
                )?;
                let value = self.expect(
                    &[
                        TokenKind::Boolean,
                        TokenKind::Char,
                        TokenKind::Integer,
                        TokenKind::String,
                    ],
                    "expect map value type",
                )?;

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

                Ok(Statement::Declaration {
                    name,
                    variable_type: VariableType::Map {
                        key_kind: key,
                        value_kind: value,
                        initializer,
                    },
                })
            }
            _ => unreachable!(),
        }
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
            expr = Expression::Boolean {
                expression: Box::from(Expression::Logical {
                    left: Box::from(expr),
                    operator,
                    right: Box::from(self.and()?),
                }),
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParseResult<Expression<'a>> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.consume(&[TokenKind::And]) {
            expr = Expression::Boolean {
                expression: Box::from(Expression::Logical {
                    left: Box::from(expr),
                    operator,
                    right: Box::from(self.comparison()?),
                }),
            }
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
            expr = Expression::Boolean {
                expression: Box::from(Expression::Binary {
                    left: Box::from(expr),
                    operator,
                    right: Box::from(self.term()?),
                }),
            }
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
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expression<'a>> {
        if let Some(value) = self.consume(&[
            TokenKind::CharLiteral,
            TokenKind::IntegerLiteral,
            TokenKind::StringLiteral,
        ]) {
            Ok(Expression::Literal { value })
        } else if let Some(value) = self.consume(&[TokenKind::True, TokenKind::False]) {
            Ok(Expression::Boolean {
                expression: Box::from(Expression::Literal { value }),
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
            self.error()
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

    fn parse_expression(string: &str) -> Expression {
        Parser::new(string).expression().unwrap()
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
}
