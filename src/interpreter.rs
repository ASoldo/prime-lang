use crate::parser::Token;
use std::collections::HashMap;

pub fn interpret(tokens: Vec<Token>) {
    println!("Starting interpretation.");
    let mut variables: HashMap<String, i32> = HashMap::new();

    let mut index = 0;
    while index < tokens.len() {
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
    }
}
