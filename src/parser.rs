use miette::SourceSpan;
use nom::{
    IResult, Parser as NomParser,
    bytes::complete::{tag, take},
    character::complete::{alpha1, alphanumeric0, digit1, space0, space1},
    combinator::{map_res, recognize},
    sequence::{pair, preceded},
};
use std::ops::Range;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    FnMain,
    LetInt,
    Identifier(String),
    Equals,
    Integer(i32),
    Plus,
    Minus,
    Slash,
    Star,
    StdOut,
    SemiColon,
    LeftBracket,
    RightBracket,
    LeftCurlyBrace,
    RightCurlyBrace,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct LexToken {
    pub token: Token,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub label: String,
    pub span: SourceSpan,
    pub help: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let { name: String, value: Expr },
    Output(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i32),
    Identifier(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl Expr {
    fn as_source(&self) -> String {
        match self {
            Expr::Integer(value) => value.to_string(),
            Expr::Identifier(name) => name.clone(),
            Expr::Binary { left, op, right } => {
                format!("{} {} {}", left.as_source(), op.symbol(), right.as_source())
            }
        }
    }
}

impl BinaryOp {
    fn symbol(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
        }
    }
}
pub fn parse_left_curly_brace(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("{")(input)?;
    Ok((input, Token::LeftCurlyBrace))
}

pub fn parse_right_curly_brace(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("}")(input)?;
    Ok((input, Token::RightCurlyBrace))
}
pub fn parse_fn_main(input: &str) -> IResult<&str, Token> {
    let (input, _) = recognize(pair(tag("fn"), preceded(space1, tag("main")))).parse(input)?;
    let (input, _) = space0(input)?;
    Ok((input, Token::FnMain))
}

pub fn parse_let_int(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("let")(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag("int")(input)?;
    Ok((input, Token::LetInt))
}

pub fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (input, ident) = recognize(variable_name).parse(input)?;
    Ok((input, Token::Identifier(ident.to_string())))
}
fn variable_name(input: &str) -> IResult<&str, String> {
    let (input, (head, tail)) = pair(alpha1, alphanumeric0).parse(input)?;
    Ok((input, format!("{}{}", head, tail)))
}

pub fn parse_equals(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("=")(input)?;
    Ok((input, Token::Equals))
}
pub fn parse_integer(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, num) = map_res(recognize(digit1), |s: &str| s.parse::<i32>()).parse(input)?;
    Ok((input, Token::Integer(num)))
}

pub fn parse_plus(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("+")(input)?;
    Ok((input, Token::Plus))
}
pub fn parse_minus(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("-")(input)?;
    Ok((input, Token::Minus))
}
pub fn parse_slash(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("/")(input)?;
    Ok((input, Token::Slash))
}
pub fn parse_star(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("*")(input)?;
    Ok((input, Token::Star))
}

pub fn parse_std_out(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("out")(input)?;
    Ok((input, Token::StdOut))
}
pub fn parse_semi_colon(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = space0(input)?;
    Ok((input, Token::SemiColon))
}

pub fn parse_left_bracket(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("(")(input)?;
    Ok((input, Token::LeftBracket))
}

pub fn parse_right_bracket(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((input, Token::RightBracket))
}

pub fn parse_any(input: &str) -> IResult<&str, Token> {
    let (input, _) = nom::character::complete::anychar(input)?;
    Ok((input, Token::Unknown))
}

pub fn tokenize(input: &str) -> Vec<LexToken> {
    let mut tokens = Vec::new();
    let mut remaining_input = input;
    let mut offset = 0usize;

    while !remaining_input.is_empty() {
        let trimmed = remaining_input.trim_start();
        let skipped = remaining_input.len() - trimmed.len();
        offset += skipped;
        remaining_input = trimmed;

        if remaining_input.is_empty() {
            break;
        }

        let original = remaining_input;
        let result = parse_fn_main(remaining_input)
            .or_else(|_| parse_right_bracket(remaining_input))
            .or_else(|_| parse_left_bracket(remaining_input))
            .or_else(|_| parse_left_curly_brace(remaining_input))
            .or_else(|_| parse_let_int(remaining_input))
            .or_else(|_| parse_identifier(remaining_input))
            .or_else(|_| parse_equals(remaining_input))
            .or_else(|_| parse_integer(remaining_input))
            .or_else(|_| parse_plus(remaining_input))
            .or_else(|_| parse_minus(remaining_input))
            .or_else(|_| parse_slash(remaining_input))
            .or_else(|_| parse_star(remaining_input))
            .or_else(|_| parse_std_out(remaining_input))
            .or_else(|_| parse_semi_colon(remaining_input))
            .or_else(|_| parse_right_curly_brace(remaining_input))
            .or_else(|_| parse_any(remaining_input));

        match result {
            Ok((remaining, token)) => {
                let consumed = original.len() - remaining.len();
                let span = offset..offset + consumed;
                tokens.push(LexToken { token, span });
                remaining_input = remaining;
                offset += consumed;
            }
            Err(_) => {
                let (new_remaining_input, _) = take(1usize)
                    .parse(remaining_input)
                    .map_err::<nom::Err<(&str, nom::error::ErrorKind)>, _>(nom::Err::convert)
                    .unwrap_or((remaining_input, ""));
                if new_remaining_input != remaining_input {
                    let consumed = remaining_input.len() - new_remaining_input.len();
                    let span = offset..offset + consumed;
                    tokens.push(LexToken {
                        token: Token::Unknown,
                        span,
                    });
                    remaining_input = new_remaining_input;
                    offset += consumed;
                } else {
                    println!("Remaining input: {:?}", remaining_input);
                    panic!("Unexpected token: {:?}", remaining_input);
                }
            }
        }
    }

    tokens
}

pub fn parse(tokens: &[LexToken]) -> Result<Program, Vec<ParseError>> {
    let mut parser = AstParser::new(tokens);
    let program = parser.parse_program();
    if parser.errors.is_empty() {
        Ok(program)
    } else {
        Err(parser.errors)
    }
}

struct AstParser<'a> {
    tokens: &'a [LexToken],
    position: usize,
    last_span: Option<Range<usize>>,
    errors: Vec<ParseError>,
}

impl<'a> AstParser<'a> {
    fn new(tokens: &'a [LexToken]) -> Self {
        Self {
            tokens,
            position: 0,
            last_span: None,
            errors: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> Program {
        for (expected, message) in [
            (
                &Token::FnMain,
                "Expected 'fn main' at the start of the file",
            ),
            (&Token::LeftBracket, "Expected '(' after 'fn main'"),
            (&Token::RightBracket, "Expected ')' after 'fn main('"),
            (
                &Token::LeftCurlyBrace,
                "Expected '{' to start the main body",
            ),
        ] {
            if let Err(err) = self.consume(expected, message) {
                self.report_error(err);
                self.synchronize();
            }
        }

        let mut statements = Vec::new();
        while !self.check(&Token::RightCurlyBrace) && !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.report_error(err);
                    self.synchronize();
                }
            }
        }

        if let Err(err) = self.consume(
            &Token::RightCurlyBrace,
            "Expected '}' at the end of the main body",
        ) {
            self.report_error(err);
        }

        if !self.is_at_end() {
            let span = self
                .peek_lex()
                .map(|lex| to_source_span(&lex.span))
                .unwrap_or_else(|| self.eof_span());
            self.report_error(self.error("Unexpected tokens after end of program", span));
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek_token() {
            Some(Token::LetInt) => self.parse_let_statement(),
            Some(Token::Identifier(ident)) if ident == "out" => self.parse_output_statement(),
            Some(Token::StdOut) => self.parse_output_statement(),
            Some(Token::Identifier(ident)) if ident == "let" => {
                let span = self
                    .peek_lex()
                    .map(|lex| to_source_span(&lex.span))
                    .unwrap_or_else(|| self.eof_span());
                let mut err = self.error("Expected 'int' after 'let'", span);
                err.help = Some(let_syntax_help());
                Err(err)
            }
            Some(token) => {
                let span = self
                    .peek_lex()
                    .map(|lex| to_source_span(&lex.span))
                    .unwrap_or_else(|| self.eof_span());
                let mut err =
                    self.error(format!("Unexpected token {:?} in statement", token), span);
                if matches!(token, Token::Identifier(name) if name == "let") {
                    err.help = Some(let_syntax_help());
                }
                Err(err)
            }
            None => Err(self.error(
                "Unexpected end of input while reading statement",
                self.eof_span(),
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(&Token::LetInt, "Expected 'let int'")?;
        let name = match self.advance() {
            Some(lex) => match &lex.token {
                Token::Identifier(name) => name.clone(),
                _ => {
                    let mut err = self.error(
                        "Expected identifier after 'let int'",
                        to_source_span(&lex.span),
                    );
                    err.help = Some(let_syntax_help());
                    return Err(err);
                }
            },
            None => {
                let mut err = self.error("Expected identifier after 'let int'", self.eof_span());
                err.help = Some(let_syntax_help());
                return Err(err);
            }
        };
        self.consume_with_hint(
            &Token::Equals,
            "Expected '=' after identifier",
            None,
            Some(let_assignment_help(&name)),
        )?;
        let value = self.parse_expression()?;
        let value_preview = value.as_source();
        let hint = Some(self.span_after_last());
        if let Err(err) = self.consume_with_hint(
            &Token::SemiColon,
            "Expected ';' after expression",
            hint,
            Some(let_semicolon_help(&name, &value_preview)),
        ) {
            self.report_error(err);
        }
        Ok(Statement::Let { name, value })
    }

    fn parse_output_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek_token() {
            Some(Token::Identifier(ident)) if ident == "out" => {
                self.advance();
            }
            Some(Token::StdOut) => {
                self.advance();
            }
            _ => {
                let span = self
                    .peek_lex()
                    .map(|lex| to_source_span(&lex.span))
                    .unwrap_or_else(|| self.eof_span());
                return Err(self.error("Expected 'out' before output expression", span));
            }
        }
        self.consume(&Token::LeftBracket, "Expected '(' after 'out'")?;
        let expr = self.parse_expression()?;
        let expr_preview = expr.as_source();
        self.consume(&Token::RightBracket, "Expected ')' after expression")?;
        let hint = Some(self.span_after_last());
        if let Err(err) = self.consume_with_hint(
            &Token::SemiColon,
            "Expected ';' after output expression",
            hint,
            Some(format!("Try: out({});", expr_preview)),
        ) {
            self.report_error(err);
        }
        Ok(Statement::Output(expr))
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;

        loop {
            let op = match self.peek_token() {
                Some(Token::Plus) => BinaryOp::Add,
                Some(Token::Minus) => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;

        loop {
            let op = match self.peek_token() {
                Some(Token::Star) => BinaryOp::Mul,
                Some(Token::Slash) => BinaryOp::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        match self.advance() {
            Some(lex) => match &lex.token {
                Token::Integer(value) => Ok(Expr::Integer(*value)),
                Token::Identifier(name) => Ok(Expr::Identifier(name.clone())),
                Token::LeftBracket => {
                    let expr = self.parse_expression()?;
                    self.consume(&Token::RightBracket, "Expected ')' after expression")?;
                    Ok(expr)
                }
                token => Err(self.error(
                    format!("Unexpected token {:?} in expression", token),
                    to_source_span(&lex.span),
                )),
            },
            None => Err(self.error(
                "Unexpected end of input while reading expression",
                self.eof_span(),
            )),
        }
    }

    fn consume(&mut self, expected: &Token, message: &str) -> Result<(), ParseError> {
        self.consume_with_hint(expected, message, None, None)
    }

    fn consume_with_hint(
        &mut self,
        expected: &Token,
        message: &str,
        hint: Option<SourceSpan>,
        help: Option<String>,
    ) -> Result<(), ParseError> {
        match self.peek_lex() {
            Some(lex) if &lex.token == expected => {
                self.advance();
                Ok(())
            }
            Some(lex) => {
                let span = hint.unwrap_or_else(|| to_source_span(&lex.span));
                let mut err = self.error(format!("{}: found {:?}", message, lex.token), span);
                err.help = help;
                Err(err)
            }
            None => {
                let mut err = self.error(
                    format!("{}: reached end of input", message),
                    hint.unwrap_or_else(|| self.eof_span()),
                );
                err.help = help;
                Err(err)
            }
        }
    }

    fn peek_token(&self) -> Option<&'a Token> {
        self.peek_lex().map(|lex| &lex.token)
    }

    fn peek_lex(&self) -> Option<&'a LexToken> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&'a LexToken> {
        let token = self.tokens.get(self.position);
        if let Some(lex) = token {
            self.position += 1;
            self.last_span = Some(lex.span.clone());
        }
        token
    }

    fn check(&self, expected: &Token) -> bool {
        matches!(self.peek_token(), Some(token) if token == expected)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    fn eof_span(&self) -> SourceSpan {
        if let Some(span) = self
            .last_span
            .as_ref()
            .or_else(|| self.tokens.last().map(|t| &t.span))
        {
            (span.end, 0).into()
        } else {
            (0, 0).into()
        }
    }

    fn span_after_last(&self) -> SourceSpan {
        if let Some(span) = &self.last_span {
            (span.end, 0).into()
        } else {
            (0, 0).into()
        }
    }

    fn error(&self, message: impl Into<String>, span: SourceSpan) -> ParseError {
        let message = message.into();
        ParseError {
            label: message.clone(),
            message,
            span,
            help: None,
        }
    }

    fn report_error(&mut self, err: ParseError) {
        self.errors.push(err);
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if matches!(self.peek_token(), Some(Token::SemiColon)) {
                self.advance();
                break;
            }
            if matches!(self.peek_token(), Some(Token::RightCurlyBrace)) {
                break;
            }
            self.advance();
        }
    }
}

fn to_source_span(range: &Range<usize>) -> SourceSpan {
    (range.start, range.end.saturating_sub(range.start)).into()
}

fn let_syntax_help() -> String {
    "Syntax: let int <name> = <value>;".into()
}

fn let_assignment_help(name: &str) -> String {
    format!("Try: let int {} = <value>;", name)
}

fn let_semicolon_help(name: &str, value: &str) -> String {
    format!("Try: let int {} = {};", name, value)
}
