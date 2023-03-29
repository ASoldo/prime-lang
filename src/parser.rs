use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric0, digit1, space0, space1},
    combinator::{map_res, recognize},
    sequence::pair,
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum Token {
    LetInt,
    Identifier(String),
    Equals,
    Integer(i32),
    Plus,
    StdOut,
    SemiColon,
    LeftBracket,
    RightBracket,
    Unknown,
}

pub fn parse_let_int(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("let")(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag("int")(input)?;
    Ok((input, Token::LetInt))
}

pub fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (input, ident) = recognize(variable_name)(input)?;
    Ok((input, Token::Identifier(ident.to_string())))
}
fn variable_name(input: &str) -> IResult<&str, String> {
    let (input, (head, tail)) = pair(alpha1, alphanumeric0)(input)?;
    Ok((input, format!("{}{}", head, tail)))
}

pub fn parse_equals(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("=")(input)?;
    Ok((input, Token::Equals))
}
pub fn parse_integer(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, num) = map_res(recognize(digit1), |s: &str| s.parse::<i32>())(input)?;
    Ok((input, Token::Integer(num)))
}

pub fn parse_plus(input: &str) -> IResult<&str, Token> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("+")(input)?;
    Ok((input, Token::Plus))
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
        let result = parse_let_int(remaining_input)
            .or_else(|_| parse_identifier(remaining_input))
            .or_else(|_| parse_equals(remaining_input))
            .or_else(|_| parse_integer(remaining_input))
            .or_else(|_| parse_plus(remaining_input))
            .or_else(|_| parse_std_out(remaining_input))
            .or_else(|_| parse_semi_colon(remaining_input))
            .or_else(|_| parse_right_bracket(remaining_input))
            .or_else(|_| parse_left_bracket(remaining_input))
            .or_else(|_| parse_any(remaining_input));

        match result {
            Ok((remaining, token)) => {
                tokens.push(token);
                remaining_input = remaining.trim_start();
            }
            Err(_) => {
                let (new_remaining_input, _) = nom::bytes::complete::take(1usize)(remaining_input)
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
