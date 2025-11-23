use crate::language::{
    span::Span,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

pub fn lex(source: &str) -> Result<Vec<Token>, Vec<LexError>> {
    let lexer = Lexer::new(source);
    lexer.run()
}

struct Lexer<'a> {
    src: &'a str,
    chars: std::str::Chars<'a>,
    current: Option<char>,
    offset: usize,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        let mut chars = src.chars();
        let current = chars.next();
        Self {
            src,
            chars,
            current,
            offset: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn run(mut self) -> Result<Vec<Token>, Vec<LexError>> {
        while let Some(ch) = self.current {
            match ch {
                '/' if self.peek() == Some('/') => self.eat_line_comment(),
                '/' if self.peek() == Some('*') => self.eat_block_comment(),
                ch if ch.is_whitespace() => {
                    self.bump();
                }
                ch if ch.is_ascii_alphabetic() || ch == '_' => self.lex_identifier(),
                ch if ch.is_ascii_digit() => self.lex_number(),
                '"' => self.lex_string(),
                '`' => self.lex_template_string(),
                '\'' => self.lex_rune(),
                _ => self.lex_symbol(),
            }
        }
        self.push_token(TokenKind::Eof, self.offset, self.offset);

        if self.errors.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errors)
        }
    }

    fn bump(&mut self) -> Option<char> {
        if let Some(ch) = self.current {
            self.offset += ch.len_utf8();
        }
        self.current = self.chars.next();
        self.current
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn push_token(&mut self, kind: TokenKind, start: usize, end: usize) {
        self.tokens.push(Token {
            kind,
            span: Span::new(start, end),
        });
    }

    fn error(&mut self, start: usize, end: usize, message: impl Into<String>) {
        self.errors.push(LexError {
            message: message.into(),
            span: Span::new(start, end),
        });
    }

    fn eat_line_comment(&mut self) {
        self.bump();
        self.bump();
        while let Some(ch) = self.current {
            if ch == '\n' {
                break;
            }
            self.bump();
        }
    }

    fn eat_block_comment(&mut self) {
        self.bump();
        self.bump();
        while let Some(ch) = self.current {
            if ch == '*' && self.peek() == Some('/') {
                self.bump();
                self.bump();
                return;
            }
            self.bump();
        }
        self.error(self.offset, self.offset, "Unterminated block comment");
    }

    fn lex_identifier(&mut self) {
        let start = self.offset;
        while let Some(ch) = self.current {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.bump();
            } else {
                break;
            }
        }

        let end = self.offset;
        let slice = &self.src[start..end];
        let kind = match slice {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "const" => TokenKind::Const,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "interface" => TokenKind::Interface,
            "impl" => TokenKind::Impl,
            "import" => TokenKind::Import,
            "using" => TokenKind::Using,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "match" => TokenKind::Match,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "defer" => TokenKind::Defer,
            "spawn" => TokenKind::Spawn,
            "try" => TokenKind::Try,
            "move" => TokenKind::Move,
            "pub" => TokenKind::Pub,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "as" => TokenKind::As,
            "module" => TokenKind::ModuleKw,
            "library" => TokenKind::LibraryKw,
            "test" => TokenKind::TestKw,
            _ => TokenKind::Identifier(slice.to_string()),
        };
        self.push_token(kind, start, end);
    }

    fn lex_number(&mut self) {
        let start = self.offset;
        while let Some(ch) = self.current {
            if ch.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }

        let mut is_float = false;
        if self.current == Some('.') {
            if let Some(next) = self.peek() {
                if next.is_ascii_digit() {
                    is_float = true;
                    self.bump(); // consume '.'
                    while let Some(ch) = self.current {
                        if ch.is_ascii_digit() {
                            self.bump();
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        if matches!(self.current, Some('e') | Some('E')) {
            is_float = true;
            self.bump();
            if matches!(self.current, Some('+') | Some('-')) {
                self.bump();
            }
            while let Some(ch) = self.current {
                if ch.is_ascii_digit() {
                    self.bump();
                } else {
                    break;
                }
            }
        }

        let end = self.offset;
        let text = &self.src[start..end];

        if is_float {
            match text.parse::<f64>() {
                Ok(value) => self.push_token(TokenKind::Float(value), start, end),
                Err(_) => self.error(start, end, "Invalid float literal"),
            }
        } else {
            match text.parse::<i128>() {
                Ok(value) => self.push_token(TokenKind::Integer(value), start, end),
                Err(_) => self.error(start, end, "Invalid integer literal"),
            }
        }
    }

    fn lex_string(&mut self) {
        let start = self.offset;
        self.bump();
        let mut value = String::new();
        while let Some(ch) = self.current {
            match ch {
                '"' => {
                    self.bump();
                    let end = self.offset;
                    self.push_token(TokenKind::String(value), start, end);
                    return;
                }
                '\\' => {
                    self.bump();
                    if let Some(escaped) = self.current {
                        value.push(match escaped {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\\' => '\\',
                            '"' => '"',
                            other => other,
                        });
                        self.bump();
                    } else {
                        break;
                    }
                }
                _ => {
                    value.push(ch);
                    self.bump();
                }
            }
        }
        self.error(start, self.offset, "Unterminated string literal");
    }

    fn lex_template_string(&mut self) {
        let start = self.offset;
        self.bump();
        let mut value = String::new();
        while let Some(ch) = self.current {
            match ch {
                '`' => {
                    self.bump();
                    let end = self.offset;
                    self.push_token(TokenKind::TemplateString(value), start, end);
                    return;
                }
                '\\' => {
                    self.bump();
                    if let Some(escaped) = self.current {
                        value.push(match escaped {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\\' => '\\',
                            '`' => '`',
                            '"' => '"',
                            other => other,
                        });
                        self.bump();
                    } else {
                        break;
                    }
                }
                _ => {
                    value.push(ch);
                    self.bump();
                }
            }
        }
        self.error(start, self.offset, "Unterminated format string literal");
    }

    fn lex_rune(&mut self) {
        let start = self.offset;
        self.bump(); // '
        let Some(ch) = self.current else {
            self.error(start, self.offset, "Unterminated rune literal");
            return;
        };
        let value = if ch == '\\' {
            self.bump();
            match self.current {
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some('\'') => '\'',
                Some(other) => other,
                None => {
                    self.error(start, self.offset, "Invalid escape in rune literal");
                    return;
                }
            }
        } else {
            ch
        };
        self.bump();
        if self.current != Some('\'') {
            self.error(start, self.offset, "Unterminated rune literal");
            return;
        }
        self.bump();
        let end = self.offset;
        self.push_token(TokenKind::Rune(value), start, end);
    }

    fn lex_symbol(&mut self) {
        let start = self.offset;
        let ch = self.current;
        match ch {
            Some('(') => self.single(TokenKind::LParen),
            Some(')') => self.single(TokenKind::RParen),
            Some('{') => self.single(TokenKind::LBrace),
            Some('}') => self.single(TokenKind::RBrace),
            Some('[') => self.single(TokenKind::LBracket),
            Some(']') => self.single(TokenKind::RBracket),
            Some(',') => self.single(TokenKind::Comma),
            Some('.') => {
                self.bump();
                if self.current == Some('.') {
                    self.bump();
                    if self.current == Some('=') {
                        self.bump();
                        self.push_token(TokenKind::DotDotEq, start, self.offset);
                    } else {
                        self.push_token(TokenKind::DotDot, start, self.offset);
                    }
                } else {
                    self.push_token(TokenKind::Dot, start, self.offset);
                }
            }
            Some(':') => {
                self.bump();
                if self.current == Some(':') {
                    self.bump();
                    // Treat '::' the same as ':' to keep enum qualification simple
                    self.push_token(TokenKind::Colon, start, self.offset);
                } else {
                    self.push_token(TokenKind::Colon, start, self.offset);
                }
            }
            Some(';') => self.single(TokenKind::Semi),
            Some('%') => self.single(TokenKind::Percent),
            Some('?') => self.single(TokenKind::Question),
            Some('@') => self.single(TokenKind::At),
            Some('#') => self.single(TokenKind::Hash),
            Some('+') => self.single(TokenKind::Plus),
            Some('*') => self.single(TokenKind::Star),
            Some('^') => self.single(TokenKind::Caret),
            Some('&') => {
                self.bump();
                if self.current == Some('&') {
                    self.bump();
                    self.push_token(TokenKind::AmpersandAmpersand, start, self.offset);
                } else {
                    self.push_token(TokenKind::Ampersand, start, self.offset);
                }
            }
            Some('|') => {
                self.bump();
                if self.current == Some('|') {
                    self.bump();
                    self.push_token(TokenKind::PipePipe, start, self.offset);
                } else {
                    self.push_token(TokenKind::Pipe, start, self.offset);
                }
            }
            Some('!') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    self.push_token(TokenKind::BangEq, start, self.offset);
                } else {
                    self.push_token(TokenKind::Bang, start, self.offset);
                }
            }
            Some('=') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    self.push_token(TokenKind::FatArrow, start, self.offset);
                } else if self.current == Some('=') {
                    self.bump();
                    self.push_token(TokenKind::EqEq, start, self.offset);
                } else {
                    self.push_token(TokenKind::Eq, start, self.offset);
                }
            }
            Some('<') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    self.push_token(TokenKind::LtEq, start, self.offset);
                } else {
                    self.push_token(TokenKind::Lt, start, self.offset);
                }
            }
            Some('>') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    self.push_token(TokenKind::GtEq, start, self.offset);
                } else {
                    self.push_token(TokenKind::Gt, start, self.offset);
                }
            }
            Some('-') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    self.push_token(TokenKind::Arrow, start, self.offset);
                } else {
                    self.push_token(TokenKind::Minus, start, self.offset);
                }
            }
            Some('/') => {
                self.bump();
                self.push_token(TokenKind::Slash, start, self.offset);
            }
            Some(ch) => {
                self.bump();
                self.error(start, self.offset, format!("Unexpected character '{}'", ch));
            }
            None => {}
        }
    }

    fn single(&mut self, kind: TokenKind) {
        let start = self.offset;
        self.bump();
        self.push_token(kind, start, self.offset);
    }
}
