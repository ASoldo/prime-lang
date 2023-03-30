use crate::parser::Token;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target},
    values::{BasicValueEnum, FunctionValue, IntValue},
    AddressSpace,
};
use std::collections::HashMap;

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    main_function: FunctionValue<'ctx>,
    variables: HashMap<String, IntValue<'ctx>>,
    printf_function: FunctionValue<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("prime");

        let init_config = InitializationConfig::default();
        Target::initialize_native(&init_config).unwrap();

        let main_fn_type = context.void_type().fn_type(&[], false);

        let main_function = module.add_function("main", main_fn_type, None);

        let printf_function_type = context.i32_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::from(0)).into()],
            true,
        );
        let printf_function = module.add_function("printf", printf_function_type, None);
        Self {
            context,
            builder,
            module,
            main_function,
            variables: HashMap::new(),
            printf_function,
        }
    }
    fn compile_expression(&mut self, tokens: &[Token], index: &mut usize) -> BasicValueEnum<'ctx> {
        let mut result = self.compile_term(tokens, index);

        while *index < tokens.len() {
            match tokens[*index] {
                Token::Plus => {
                    *index += 1;
                    let rhs = self.compile_term(tokens, index);
                    result = self
                        .builder
                        .build_int_add(result.into_int_value(), rhs.into_int_value(), "add")
                        .into();
                }
                Token::Minus => {
                    *index += 1;
                    let rhs = self.compile_term(tokens, index);
                    result = self
                        .builder
                        .build_int_sub(result.into_int_value(), rhs.into_int_value(), "sub")
                        .into();
                }
                Token::Slash => {
                    *index += 1;
                    let rhs = self.compile_term(tokens, index);
                    result = self
                        .builder
                        .build_int_signed_div(result.into_int_value(), rhs.into_int_value(), "div")
                        .into();
                }
                Token::Star => {
                    *index += 1;
                    let rhs = self.compile_term(tokens, index);
                    result = self
                        .builder
                        .build_int_mul(result.into_int_value(), rhs.into_int_value(), "mul")
                        .into();
                }
                _ => break,
            }
        }

        result
    }
    fn compile_term(&mut self, tokens: &[Token], index: &mut usize) -> BasicValueEnum<'ctx> {
        match &tokens[*index] {
            Token::Integer(value) => {
                *index += 1;
                self.context
                    .i32_type()
                    .const_int(*value as u64, false)
                    .into()
            }
            Token::Identifier(ident) => {
                *index += 1;
                if let Some(value) = self.variables.get(ident) {
                    value.clone().into()
                } else {
                    panic!("Undefined variable: {}", ident);
                }
            }
            _ => panic!("Expected an integer or variable reference."),
        }
    }

    pub fn compile(&mut self, tokens: &[Token]) {
        let entry_block = self.context.append_basic_block(self.main_function, "entry");
        self.builder.position_at_end(entry_block);

        let mut inside_main = false;
        let mut index = 0;
        while index < tokens.len() {
            match tokens[index] {
                Token::FnMain => {
                    if let Token::LeftBracket = tokens[index + 1] {
                        inside_main = true;
                        index += 2;
                    } else {
                        panic!("Expected a left bracket after 'fn main'.");
                    }
                }
                Token::RightBracket if inside_main => {
                    inside_main = false;
                    index += 1;
                }
                Token::LeftCurlyBrace => {
                    inside_main = true;
                    index += 1;
                }
                Token::RightCurlyBrace => {
                    inside_main = false;
                    index += 1;
                }
                _ => {
                    if inside_main {
                        match tokens[index] {
                            Token::LetInt => {
                                if let Token::Identifier(ref ident) = tokens[index + 1] {
                                    if let Token::Equals = tokens[index + 2] {
                                        index += 3;
                                        let value = self.compile_expression(tokens, &mut index);
                                        self.variables
                                            .insert(ident.clone(), value.into_int_value());

                                        if let Token::SemiColon = tokens[index] {
                                            index += 1;
                                        } else {
                                            panic!("Expected a semicolon.");
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
                                    let output = self.compile_expression(tokens, &mut index);
                                    if let Token::RightBracket = tokens[index] {
                                        index += 1;
                                    } else {
                                        panic!("Expected a right bracket.");
                                    }
                                    if let Token::SemiColon = tokens[index] {
                                        index += 1;
                                    } else {
                                        panic!("Expected a semicolon.");
                                    }

                                    let string_format_ptr = self
                                        .builder
                                        .build_global_string_ptr("%d\n", "fmt")
                                        .as_pointer_value();

                                    // Call the print_function with the compiled expression
                                    self.builder.build_call(
                                        self.printf_function,
                                        &[string_format_ptr.into(), output.into()],
                                        "printf_call",
                                    );
                                } else {
                                    panic!("Expected a left bracket.");
                                }
                            }
                            Token::Unknown => {
                                panic!("Unexpected token: {:?}", tokens[index]);
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
        }

        self.builder.build_return(None);
    }

    pub fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }

    pub fn write_ir_to_file(&self, file_name: &str) -> Result<(), String> {
        match self.module.print_to_file(file_name) {
            Ok(_) => Ok(()),
            Err(e) => Err(e.to_string()),
        }
    }
}
