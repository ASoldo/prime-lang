use crate::parser::Token;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{FileType, InitializationConfig, Target},
    values::{BasicValueEnum, FunctionValue, IntValue},
};

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    function: FunctionValue<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("prime");

        let init_config = InitializationConfig::default();
        Target::initialize_native(&init_config).unwrap();

        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);

        let function = module.add_function("add", fn_type, None);

        Self {
            context,
            builder,
            module,
            function,
        }
    }

    fn compile_addition(&self, lhs: i32, rhs: i32) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let lhs_val = i32_type.const_int(lhs as u64, false);
        let rhs_val = i32_type.const_int(rhs as u64, false);

        self.builder.build_int_add(lhs_val, rhs_val, "add")
    }

    pub fn compile(&self, tokens: &[Token]) {
        let entry_block = self.context.append_basic_block(self.function, "entry");
        self.builder.position_at_end(entry_block);
        let mut lhs = 0;
        let mut rhs = 0;
        for token in tokens {
            match token {
                Token::Plus => {
                    if let (Token::Integer(l), Token::Integer(r)) = (&tokens[0], &tokens[2]) {
                        lhs = *l;
                        rhs = *r;
                    }
                }
                _ => (),
            }
        }

        let sum = self.compile_addition(lhs, rhs);
        self.builder.build_return(Some(&sum));
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
