use crate::parser::{BinaryOp, Expr, Program, Statement};
use llvm_sys::{
    LLVMLinkage,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildCall2,
        LLVMBuildGlobalString, LLVMBuildMul, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSub,
        LLVMConstInt, LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext,
        LLVMDisposeBuilder, LLVMDisposeMessage, LLVMDisposeModule, LLVMFunctionType,
        LLVMInt8TypeInContext, LLVMInt32TypeInContext, LLVMModuleCreateWithNameInContext,
        LLVMPointerType, LLVMPositionBuilderAtEnd, LLVMPrintModuleToFile, LLVMPrintModuleToString,
        LLVMSetLinkage, LLVMVoidTypeInContext,
    },
    prelude::*,
};
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    ptr,
};

pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    i32_type: LLVMTypeRef,
    printf_type: LLVMTypeRef,
    printf: LLVMValueRef,
    main_function: LLVMValueRef,
    variables: HashMap<String, LLVMValueRef>,
}

impl Compiler {
    pub fn new() -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(context);
            let i32_type = LLVMInt32TypeInContext(context);

            let mut compiler = Self {
                context,
                module: ptr::null_mut(),
                builder,
                i32_type,
                printf_type: ptr::null_mut(),
                printf: ptr::null_mut(),
                main_function: ptr::null_mut(),
                variables: HashMap::new(),
            };

            compiler.reset_module();
            compiler
        }
    }

    fn reset_module(&mut self) {
        unsafe {
            if !self.module.is_null() {
                LLVMDisposeModule(self.module);
            }

            let module_name = CString::new("prime").unwrap();
            self.module = LLVMModuleCreateWithNameInContext(module_name.as_ptr(), self.context);

            let void_type = LLVMVoidTypeInContext(self.context);
            let main_fn_type = LLVMFunctionType(void_type, ptr::null_mut(), 0, 0);
            let main_name = CString::new("main").unwrap();
            self.main_function = LLVMAddFunction(self.module, main_name.as_ptr(), main_fn_type);

            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut printf_params = [i8_ptr];
            self.printf_type = LLVMFunctionType(
                self.i32_type,
                printf_params.as_mut_ptr(),
                printf_params.len() as u32,
                1,
            );
            let printf_name = CString::new("printf").unwrap();
            self.printf = LLVMAddFunction(self.module, printf_name.as_ptr(), self.printf_type);
            LLVMSetLinkage(self.printf, LLVMLinkage::LLVMExternalLinkage);
        }
    }

    pub fn compile(&mut self, program: &Program) {
        self.reset_module();
        self.variables.clear();

        unsafe {
            let entry_name = CString::new("entry").unwrap();
            let entry_block = LLVMAppendBasicBlockInContext(
                self.context,
                self.main_function,
                entry_name.as_ptr(),
            );
            LLVMPositionBuilderAtEnd(self.builder, entry_block);
        }

        for statement in &program.statements {
            match statement {
                Statement::Let { name, value } => {
                    let compiled = self.compile_expression(value);
                    self.variables.insert(name.clone(), compiled);
                }
                Statement::Output(expr) => {
                    let compiled = self.compile_expression(expr);
                    unsafe {
                        let fmt_literal = CString::new("%d\n").unwrap();
                        let fmt_name = CString::new("fmt").unwrap();
                        let format_ptr = LLVMBuildGlobalString(
                            self.builder,
                            fmt_literal.as_ptr(),
                            fmt_name.as_ptr(),
                        );
                        let call_name = CString::new("printf_call").unwrap();
                        let mut args = [format_ptr, compiled];
                        LLVMBuildCall2(
                            self.builder,
                            self.printf_type,
                            self.printf,
                            args.as_mut_ptr(),
                            args.len() as u32,
                            call_name.as_ptr(),
                        );
                    }
                }
            }
        }

        unsafe {
            LLVMBuildRetVoid(self.builder);
        }
    }

    fn compile_expression(&mut self, expr: &Expr) -> LLVMValueRef {
        match expr {
            Expr::Integer(value) => unsafe { LLVMConstInt(self.i32_type, *value as u64, 0) },
            Expr::Identifier(name) => self.variables.get(name).copied().unwrap_or_else(|| {
                panic!("Undefined variable: {}", name);
            }),
            Expr::Binary { left, op, right } => {
                let lhs = self.compile_expression(left);
                let rhs = self.compile_expression(right);
                unsafe {
                    match op {
                        BinaryOp::Add => {
                            let tmp = CString::new("addtmp").unwrap();
                            LLVMBuildAdd(self.builder, lhs, rhs, tmp.as_ptr())
                        }
                        BinaryOp::Sub => {
                            let tmp = CString::new("subtmp").unwrap();
                            LLVMBuildSub(self.builder, lhs, rhs, tmp.as_ptr())
                        }
                        BinaryOp::Mul => {
                            let tmp = CString::new("multmp").unwrap();
                            LLVMBuildMul(self.builder, lhs, rhs, tmp.as_ptr())
                        }
                        BinaryOp::Div => {
                            let tmp = CString::new("divtmp").unwrap();
                            LLVMBuildSDiv(self.builder, lhs, rhs, tmp.as_ptr())
                        }
                    }
                }
            }
        }
    }

    pub fn print_ir(&self) {
        unsafe {
            let ir_string = LLVMPrintModuleToString(self.module);
            if ir_string.is_null() {
                return;
            }
            let message = CStr::from_ptr(ir_string).to_string_lossy().into_owned();
            LLVMDisposeMessage(ir_string);
            println!("{}", message);
        }
    }

    pub fn write_ir_to_file(&self, file_name: &str) -> Result<(), String> {
        unsafe {
            let c_file = CString::new(file_name)
                .map_err(|_| "File name contains a null byte".to_string())?;
            let mut error = ptr::null_mut();
            let result = LLVMPrintModuleToFile(self.module, c_file.as_ptr(), &mut error);
            if result == 0 {
                Ok(())
            } else {
                if !error.is_null() {
                    let message = CStr::from_ptr(error).to_string_lossy().into_owned();
                    LLVMDisposeMessage(error);
                    Err(message)
                } else {
                    Err("Failed to write LLVM IR".into())
                }
            }
        }
    }
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe {
            if !self.builder.is_null() {
                LLVMDisposeBuilder(self.builder);
            }
            if !self.module.is_null() {
                LLVMDisposeModule(self.module);
            }
            if !self.context.is_null() {
                LLVMContextDispose(self.context);
            }
        }
    }
}
