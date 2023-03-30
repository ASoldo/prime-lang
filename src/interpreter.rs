use crate::parser::Token;
use std::collections::HashMap;

pub fn interpret(tokens: Vec<Token>) {
    println!("Starting interpretation.");
    let mut variables: HashMap<String, i32> = HashMap::new();
    let mut inside_main = false;
    let mut index = 0;
    while index < tokens.len() {
        match tokens[index] {
            Token::FnMain => {
                if let Token::LeftBracket = tokens[index + 1] {
                    if let Token::RightBracket = tokens[index + 2] {
                        if let Token::LeftCurlyBrace = tokens[index + 3] {
                            inside_main = true;
                            index += 4;
                        } else {
                            panic!("Expected a left curly brace after 'fn main' right bracket.");
                        }
                    } else {
                        panic!("Expected a right bracket after 'fn main' left bracket.");
                    }
                } else {
                    panic!("Expected a left bracket after 'fn main'.");
                }
            }
            Token::RightCurlyBrace => {
                inside_main = false;
                index += 1;
            }
            _ => {
                if inside_main {
                    // Place the existing code for handling other tokens here
                    match tokens[index] {
                        Token::LetInt => {
                            if let Token::Identifier(ref ident) = tokens[index + 1] {
                                if let Token::Equals = tokens[index + 2] {
                                    if let Token::Integer(value) = tokens[index + 3] {
                                        variables.insert(ident.clone(), value);
                                        index += 4;
                                        if let Token::SemiColon = tokens[index] {
                                            index += 1;
                                        } else {
                                            panic!("Expected a semicolon.");
                                        }
                                    } else {
                                        panic!("Expected an integer.");
                                    }
                                } else {
                                    panic!("Expected an equals sign.");
                                }
                            } else {
                                panic!("Expected an identifier.");
                            }
                        }
                        Token::Identifier(ref ident) if ident == "out" => {
                            if let Token::LeftBracket = tokens[index + 1] {
                                index += 2;
                                let mut expr_value = 0;
                                let mut operation = None;

                                while tokens[index] != Token::RightBracket {
                                    match tokens[index] {
                                        Token::Identifier(ref ident) => {
                                            if let Some(value) = variables.get(ident) {
                                                if let Some(op) = operation {
                                                    match op {
                                                        Token::Plus => expr_value += value,
                                                        Token::Minus => expr_value -= value,
                                                        Token::Slash => {
                                                            if *value == 0 {
                                                                panic!("Division by zero.");
                                                            } else {
                                                                expr_value /= value;
                                                            }
                                                        }
                                                        Token::Star => expr_value *= value,
                                                        _ => panic!("Unsupported operation."),
                                                        _ => panic!("Unsupported operation."),
                                                    }
                                                    operation = None;
                                                } else {
                                                    expr_value = *value;
                                                }
                                            } else {
                                                panic!("Undefined variable.");
                                            }
                                        }
                                        Token::Integer(value) => {
                                            if let Some(op) = operation {
                                                match op {
                                                    Token::Plus => expr_value += value,
                                                    Token::Minus => expr_value -= value,
                                                    Token::Slash => {
                                                        if value == 0 {
                                                            panic!("Division by zero.");
                                                        } else {
                                                            expr_value /= value;
                                                        }
                                                    }
                                                    Token::Star => expr_value *= value,

                                                    _ => panic!("Unsupported operation."),
                                                }
                                                operation = None;
                                            } else {
                                                expr_value = value;
                                            }
                                        }
                                        Token::Plus => {
                                            operation = Some(Token::Plus);
                                        }
                                        Token::Minus => {
                                            operation = Some(Token::Minus);
                                        }
                                        Token::Slash => {
                                            operation = Some(Token::Slash);
                                        }
                                        Token::Star => {
                                            operation = Some(Token::Star);
                                        }

                                        _ => panic!("Unexpected token: {:?}", tokens[index]),
                                    }
                                    index += 1;
                                }

                                index += 1; // Skip the right bracket
                                if let Token::SemiColon = tokens[index] {
                                    println!("{}", expr_value);
                                    index += 1;
                                } else {
                                    panic!("Expected a semicolon.");
                                }
                            } else {
                                panic!("Expected a left bracket.");
                            }
                        }
                        _ => {
                            panic!("Unexpected token: {:?}", tokens[index]);
                        }
                    }
                } else {
                    panic!("Code should be inside 'fn main' function.");
                }
            }
        }
        if !inside_main && index < tokens.len() {
            index += 1;
        }
    }
}
