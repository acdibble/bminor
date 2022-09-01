#[derive(PartialEq, Eq, Debug)]
enum TokenKind {
    Bang,
    Comma,
    Colon,
    Equal,
    Semicolon,
    Plus,
    Minus,
    Slash,
    Star,
    Exponent,
    Modulo,
    Less,
    Greater,

    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,

    PlusPlus,
    MinusMinus,
    EqualEqual,
    BangEqual,
    GreaterEqual,
    LessEqual,
    And,
    Or,

    // literals
    Identifier,
    CharLiteral,
    IntegerLiteral,
    StringLiteral,

    // keywords
    Array,
    Boolean,
    Char,
    Else,
    False,
    For,
    Function,
    If,
    Integer,
    Map,
    Print,
    Return,
    String,
    True,
    Void,
    While,

    Error,
    EOF,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
    line: usize,
}

pub struct Lexer<'a> {
    token_start: usize,
    current_index: usize,
    chars: std::iter::Peekable<core::str::CharIndices<'a>>,
    line: usize,
    source: &'a str,
    done: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.char_indices().peekable(),
            source: input,
            line: 0,
            token_start: 0,
            current_index: 0,
            done: false,
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let (index, ch) = self.chars.next()?;

        self.current_index = index;

        Some((index, ch))
    }

    fn consume(&mut self, ch: char) -> bool {
        match self.peek_char() {
            Some(other) if other == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn advance_while(&mut self, fun: fn(char) -> bool) {
        loop {
            match self.peek_char() {
                Some(ch) if fun(ch) => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek().copied()
    }

    fn peek_char(&mut self) -> Option<char> {
        self.peek().map(|(_, ch)| ch)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek_char() {
                Some(ch @ (' ' | '\r' | '\t' | '\n')) => {
                    if ch == '\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn lexeme(&mut self) -> &'a str {
        let end = self
            .peek()
            .map(|(index, _)| index)
            .unwrap_or(self.source.len());

        &self.source[self.token_start..end]
    }

    fn create_token(&mut self, kind: TokenKind) -> Option<Token<'a>> {
        Some(Token {
            kind,
            line: self.line,
            lexeme: self.lexeme(),
        })
    }

    fn create_error(&self, message: &'static str) -> Option<Token<'static>> {
        Some(Token {
            kind: TokenKind::Error,
            line: self.line,
            lexeme: message,
        })
    }

    fn identifier(&mut self) -> Option<Token<'a>> {
        self.advance_while(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'));

        if self.current_index - self.token_start > 256 {
            return self.create_error("identifier too long");
        }

        let kind = match self.lexeme() {
            "array" => TokenKind::Array,
            "boolean" => TokenKind::Boolean,
            "char" => TokenKind::Char,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "function" => TokenKind::Function,
            "if" => TokenKind::If,
            "integer" => TokenKind::Integer,
            "map" => TokenKind::Map,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "string" => TokenKind::String,
            "true" => TokenKind::True,
            "void" => TokenKind::Void,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier,
        };

        self.create_token(kind)
    }

    fn char_literal(&mut self) -> Option<Token<'a>> {
        self.consume('\\');

        self.advance()?;

        if self.consume('\'') {
            self.create_token(TokenKind::CharLiteral)
        } else {
            self.create_error("unterminated char literal")
        }
    }

    fn integer(&mut self) -> Option<Token<'a>> {
        self.advance_while(|ch| matches!(ch, '0'..='9'));
        self.create_token(TokenKind::IntegerLiteral)
    }

    fn string(&mut self) -> Option<Token<'a>> {
        loop {
            if self.consume('\\') {
                self.advance();
            }

            match self.peek_char() {
                Some('"') | None => break,
                _ => {
                    self.advance();
                }
            }
        }

        if self.consume('"') {
            self.create_token(TokenKind::StringLiteral)
        } else {
            self.create_error("unterminated string")
        }
    }

    fn consume_comment(&mut self, single_line: bool) -> Option<()> {
        self.advance()?;

        loop {
            match self.advance() {
                Some((_, ch)) if single_line && ch == '\n' => {
                    self.line += 1;
                    break;
                }
                Some((_, ch)) if ch == '*' => {
                    if self.consume('/') {
                        break;
                    }
                }
                None => break,
                _ => (),
            }
        }

        Some(())
    }

    fn parse_token(&mut self) -> Option<Token<'a>> {
        self.skip_whitespace();

        let (index, ch) = self.advance()?;
        self.token_start = index;

        // () [] f() ++ -- - ! ^ * / % + - < <= > >= == != && || =
        match ch {
            ',' => self.create_token(TokenKind::Comma),
            ':' => self.create_token(TokenKind::Colon),
            ';' => self.create_token(TokenKind::Semicolon),
            '%' => self.create_token(TokenKind::Modulo),
            '*' => self.create_token(TokenKind::Star),
            '^' => self.create_token(TokenKind::Exponent),
            '{' => self.create_token(TokenKind::LeftBrace),
            '}' => self.create_token(TokenKind::RightBrace),
            '[' => self.create_token(TokenKind::LeftBracket),
            ']' => self.create_token(TokenKind::RightBracket),
            '(' => self.create_token(TokenKind::LeftParen),
            ')' => self.create_token(TokenKind::RightParen),
            '&' => {
                if self.consume('&') {
                    self.create_token(TokenKind::And)
                } else {
                    self.create_error("invalid character")
                }
            }
            '|' => {
                if self.consume('|') {
                    self.create_token(TokenKind::Or)
                } else {
                    self.create_error("invalid character")
                }
            }
            '+' => {
                if self.consume('+') {
                    self.create_token(TokenKind::PlusPlus)
                } else {
                    self.create_token(TokenKind::Plus)
                }
            }
            '-' => {
                if self.consume('-') {
                    self.create_token(TokenKind::MinusMinus)
                } else {
                    self.create_token(TokenKind::Minus)
                }
            }
            '/' => match self.peek_char() {
                Some(next @ ('*' | '/')) => {
                    self.consume_comment(next == '/');
                    self.parse_token()
                }
                _ => self.create_token(TokenKind::Slash),
            },
            '=' => {
                if self.consume('=') {
                    self.create_token(TokenKind::EqualEqual)
                } else {
                    self.create_token(TokenKind::Equal)
                }
            }
            '!' => {
                if self.consume('=') {
                    self.create_token(TokenKind::BangEqual)
                } else {
                    self.create_token(TokenKind::Bang)
                }
            }
            '>' => {
                if self.consume('=') {
                    self.create_token(TokenKind::GreaterEqual)
                } else {
                    self.create_token(TokenKind::Greater)
                }
            }
            '<' => {
                if self.consume('=') {
                    self.create_token(TokenKind::LessEqual)
                } else {
                    self.create_token(TokenKind::Less)
                }
            }
            '\'' => self.char_literal(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            '0'..='9' => self.integer(),
            '"' => self.string(),
            _ => unreachable!("{}", self.lexeme()),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        match self.parse_token() {
            None => {
                self.done = true;
                self.create_token(TokenKind::EOF).map(|mut eof| {
                    eof.lexeme = "";
                    eof
                })
            }
            Some(
                token @ Token {
                    kind: TokenKind::Error,
                    ..
                },
            ) => {
                self.done = true;
                Some(token)
            }
            opt => opt,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use insta;
    #[test]
    fn test_char_literal() {
        let tokens: Vec<_> = Lexer::new("ch: char = 'a';").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("ch: char = '\n';").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("ch: char = 'aa';").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_number_literal() {
        let tokens: Vec<_> = Lexer::new("num: integer = 123;").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("num: integer = -123;").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("num: integer = +123;").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_boolean_literal() {
        let tokens: Vec<_> = Lexer::new("bool: boolean = true;").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("bool: boolean = false;").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_string_literal() {
        let tokens: Vec<_> = Lexer::new("str: string = \"true\";").into_iter().collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("str: string = \"with quote \\\" here\";")
            .into_iter()
            .collect();

        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("str: string = \"unterminated;")
            .into_iter()
            .collect();

        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_array() {
        let tokens: Vec<_> = Lexer::new("a: array [5] integer;").into_iter().collect();
        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> = Lexer::new("a: array [5] integer = {1,2,3,4,5};")
            .into_iter()
            .collect();
        insta::assert_debug_snapshot!(tokens);

        let tokens: Vec<_> =
            Lexer::new("months: array [3] string = {\"January\",\"February\",\"March\"};")
                .into_iter()
                .collect();
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_map() {
        let tokens: Vec<_> =
            Lexer::new("m: map string integer = { \"hello\" : 5, \"goodbye\" : 10 };")
                .into_iter()
                .collect();
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_comments() {
        let tokens: Vec<_> = Lexer::new(
            "/* A C-style comment */
a=5; // A C++ style comment",
        )
        .into_iter()
        .collect();
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_keywords() {
        let tokens: Vec<_> = Lexer::new(
            "array boolean char else false for function
if integer map print return string true void while",
        )
        .into_iter()
        .collect();
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_operators() {
        let tokens: Vec<_> = Lexer::new("() [] f() ++ -- - ! ^ * / % + - < <= > >= == != && || =")
            .into_iter()
            .collect();
        insta::assert_debug_snapshot!(tokens);
    }
}
