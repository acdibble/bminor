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

    fn consume(&mut self, expected_kinds: &[TokenKind]) -> Option<Token<'a>> {
        if self.matches(expected_kinds) {
            self.tokens.next()
        } else {
            None
        }
    }

    fn expect(&mut self, expected_kind: TokenKind, message: &'static str) -> ParseResult<()> {
        if let None = self.consume(&[expected_kind]) {
            Err(ParseError::UnexpectedToken(message.to_owned()))
        } else {
            Ok(())
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

    fn matches(&mut self, expected_kinds: &[TokenKind]) -> bool {
        self.tokens
            .peek()
            .map(|t| expected_kinds.contains(&t.kind))
            .unwrap_or(false)
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
                TokenKind::RightParen,
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
    type Item = ParseResult<Expression<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        if let Some(_) = self.consume(&[TokenKind::EOF]) {
            self.done = true;
            return None;
        }

        Some(self.expression())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use insta;

    fn parse_expression(string: &str) -> Expression {
        Parser::new(string).expression().ok().unwrap()
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
    fn parse_unary_expression() {
        insta::assert_debug_snapshot!(parse_expression("+1"));
        insta::assert_debug_snapshot!(parse_expression("-1"));
        insta::assert_debug_snapshot!(parse_expression("x++"));
        insta::assert_debug_snapshot!(parse_expression("x--"));
        insta::assert_debug_snapshot!(parse_expression("!true"));
    }

    #[test]
    fn parse_exponent_expression() {
        insta::assert_debug_snapshot!(parse_expression("-1 ^ x++"));
        insta::assert_debug_snapshot!(parse_expression("(-1) ^ (x++)"));
    }

    #[test]
    fn parse_binary_expression() {
        insta::assert_debug_snapshot!(parse_expression("1 * 2"));
        insta::assert_debug_snapshot!(parse_expression("1 / 3"));
        insta::assert_debug_snapshot!(parse_expression("1 % 4"));
        insta::assert_debug_snapshot!(parse_expression("1 * 2 + 3 / 4 - 5"));
    }

    #[test]
    fn parse_comparison_expression() {
        insta::assert_debug_snapshot!(parse_expression("a == b"));
        insta::assert_debug_snapshot!(parse_expression("a != b"));
        insta::assert_debug_snapshot!(parse_expression("a > b"));
        insta::assert_debug_snapshot!(parse_expression("a >= b"));
        insta::assert_debug_snapshot!(parse_expression("a < b"));
        insta::assert_debug_snapshot!(parse_expression("a <= b"));
        insta::assert_debug_snapshot!(parse_expression("a <= b == c"));
    }

    #[test]
    fn parse_logical_expression() {
        insta::assert_debug_snapshot!(parse_expression("a && b"));
        insta::assert_debug_snapshot!(parse_expression("a && b && c"));
        insta::assert_debug_snapshot!(parse_expression("a || b"));
        insta::assert_debug_snapshot!(parse_expression("a || b || c"));
        insta::assert_debug_snapshot!(parse_expression("a && b || c"));
        insta::assert_debug_snapshot!(parse_expression("a || b && c"));
    }

    #[test]
    fn parse_assignment_expression() {
        insta::assert_debug_snapshot!(parse_expression("x = a && b"));
    }
}
