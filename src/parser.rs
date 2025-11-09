use nom::{
    IResult, Parser,
    bytes::complete::{tag, take},
    character::complete::{alpha1, alphanumeric0, digit1, space0, space1},
    combinator::{map_res, recognize},
    sequence::{pair, preceded},
};

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

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut remaining_input = input.trim_start();

    while !remaining_input.is_empty() {
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
                tokens.push(token);
                remaining_input = remaining.trim_start();
            }
            Err(_) => {
                let (new_remaining_input, _) = take(1usize)
                    .parse(remaining_input)
                    .map_err::<nom::Err<(&str, nom::error::ErrorKind)>, _>(nom::Err::convert)
                    .unwrap_or((remaining_input, ""));
                if new_remaining_input != remaining_input {
                    remaining_input = new_remaining_input.trim_start();
                } else {
                    println!("Remaining input: {:?}", remaining_input);
                    panic!("Unexpected token: {:?}", remaining_input);
                }
                tokens.push(Token::Unknown);
            }
        }
    }

    tokens
}

pub fn parse(tokens: &[Token]) -> Result<Program, String> {
    let mut parser = AstParser::new(tokens);
    parser.parse_program()
}

struct AstParser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> AstParser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        self.consume(
            &Token::FnMain,
            "Expected 'fn main' at the start of the file",
        )?;
        self.consume(&Token::LeftBracket, "Expected '(' after 'fn main'")?;
        self.consume(&Token::RightBracket, "Expected ')' after 'fn main('")?;
        self.consume(
            &Token::LeftCurlyBrace,
            "Expected '{' to start the main body",
        )?;

        let mut statements = Vec::new();
        while !self.check(&Token::RightCurlyBrace) {
            if self.is_at_end() {
                return Err("Expected '}' before end of file".into());
            }
            statements.push(self.parse_statement()?);
        }

        self.consume(
            &Token::RightCurlyBrace,
            "Expected '}' at the end of the main body",
        )?;

        if !self.is_at_end() {
            return Err("Unexpected tokens after end of program".into());
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::LetInt) => self.parse_let_statement(),
            Some(Token::Identifier(ident)) if ident == "out" => self.parse_output_statement(),
            Some(Token::StdOut) => self.parse_output_statement(),
            Some(token) => Err(format!("Unexpected token {:?} in statement", token)),
            None => Err("Unexpected end of input while reading statement".into()),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.consume(&Token::LetInt, "Expected 'let int'")?;
        let name = match self.advance() {
            Some(Token::Identifier(name)) => name.clone(),
            _ => return Err("Expected identifier after 'let int'".into()),
        };
        self.consume(&Token::Equals, "Expected '=' after identifier")?;
        let value = self.parse_expression()?;
        self.consume(&Token::SemiColon, "Expected ';' after expression")?;
        Ok(Statement::Let { name, value })
    }

    fn parse_output_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::Identifier(ident)) if ident == "out" => {
                self.advance();
            }
            Some(Token::StdOut) => {
                self.advance();
            }
            _ => return Err("Expected 'out' before output expression".into()),
        }
        self.consume(&Token::LeftBracket, "Expected '(' after 'out'")?;
        let expr = self.parse_expression()?;
        self.consume(&Token::RightBracket, "Expected ')' after expression")?;
        self.consume(&Token::SemiColon, "Expected ';' after output expression")?;
        Ok(Statement::Output(expr))
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;

        loop {
            let op = match self.peek() {
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

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_factor()?;

        loop {
            let op = match self.peek() {
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

    fn parse_factor(&mut self) -> Result<Expr, String> {
        match self.advance() {
            Some(Token::Integer(value)) => Ok(Expr::Integer(*value)),
            Some(Token::Identifier(name)) => Ok(Expr::Identifier(name.clone())),
            Some(Token::LeftBracket) => {
                let expr = self.parse_expression()?;
                self.consume(&Token::RightBracket, "Expected ')' after expression")?;
                Ok(expr)
            }
            Some(token) => Err(format!("Unexpected token {:?} in expression", token)),
            None => Err("Unexpected end of input while reading expression".into()),
        }
    }

    fn consume(&mut self, expected: &Token, message: &str) -> Result<(), String> {
        match self.peek() {
            Some(token) if token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("{}: found {:?}", message, token)),
            None => Err(format!("{}: reached end of input", message)),
        }
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.position);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    fn check(&self, expected: &Token) -> bool {
        matches!(self.peek(), Some(token) if token == expected)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }
}
