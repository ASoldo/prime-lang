use crate::language::ast::*;
use llvm_sys::{
    LLVMLinkage,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildCall2, LLVMBuildMul,
        LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSub, LLVMConstInt, LLVMContextCreate,
        LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeMessage,
        LLVMDisposeModule, LLVMFunctionType, LLVMInt8TypeInContext, LLVMInt32TypeInContext,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPositionBuilderAtEnd,
        LLVMPrintModuleToFile, LLVMPrintModuleToString, LLVMSetLinkage, LLVMVoidTypeInContext,
    },
    prelude::*,
};
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    path::Path,
    ptr,
};

pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    i32_type: LLVMTypeRef,
    printf_type: LLVMTypeRef,
    printf: LLVMValueRef,
    main_fn: LLVMValueRef,
    values: HashMap<String, LLVMValueRef>,
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
                main_fn: ptr::null_mut(),
                values: HashMap::new(),
            };
            compiler.reset_module();
            compiler
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        self.reset_module();
        self.values.clear();

        let main_fn = program
            .modules
            .iter()
            .flat_map(|module| module.items.iter())
            .find_map(|item| match item {
                Item::Function(func) if func.name == "main" => Some(func),
                _ => None,
            })
            .ok_or_else(|| "main function not found".to_string())?;

        let body = match &main_fn.body {
            FunctionBody::Block(block) => block,
            FunctionBody::Expr(_) => {
                return Err("main function must use block body for build".into());
            }
        };

        unsafe {
            let entry = CString::new("entry").unwrap();
            let block = LLVMAppendBasicBlockInContext(self.context, self.main_fn, entry.as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, block);
        }

        for stmt in &body.statements {
            self.emit_statement(stmt)?;
        }
        if let Some(tail) = &body.tail {
            self.emit_expression(tail)?;
        }

        unsafe {
            LLVMBuildRetVoid(self.builder);
        }

        Ok(())
    }

    pub fn write_ir_to<P: AsRef<Path>>(&self, path: P) -> Result<(), String> {
        let c_path = CString::new(
            path.as_ref()
                .to_str()
                .ok_or_else(|| "Invalid output path".to_string())?,
        )
        .map_err(|_| "Output path contains null byte".to_string())?;
        unsafe {
            let mut error = ptr::null_mut();
            if LLVMPrintModuleToFile(self.module, c_path.as_ptr(), &mut error) != 0 {
                if !error.is_null() {
                    let message = CStr::from_ptr(error).to_string_lossy().into_owned();
                    LLVMDisposeMessage(error);
                    return Err(message);
                }
                return Err("Failed to write LLVM IR".into());
            }
        }
        Ok(())
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

    fn emit_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Let(stmt) => {
                let value = if let Some(expr) = &stmt.value {
                    self.emit_expression(expr)?
                } else {
                    unsafe { LLVMConstInt(self.i32_type, 0, 0) }
                };
                self.values.insert(stmt.name.clone(), value);
            }
            Statement::Expr(expr_stmt) => {
                if let Expr::Call { callee, args, .. } = &expr_stmt.expr {
                    if let Expr::Identifier(ident) = callee.as_ref() {
                        if ident.name == "out" && args.len() == 1 {
                            let arg = self.emit_expression(&args[0])?;
                            self.emit_out(arg);
                            return Ok(());
                        }
                    }
                }
                return Err(
                    "Only out() calls are supported as standalone expressions in build mode".into(),
                );
            }
            _ => {
                return Err(
                    "Only let statements and out() calls are supported in build mode".into(),
                );
            }
        }
        Ok(())
    }

    fn emit_expression(&mut self, expr: &Expr) -> Result<LLVMValueRef, String> {
        match expr {
            Expr::Literal(Literal::Int(value, _)) => unsafe {
                Ok(LLVMConstInt(self.i32_type, *value as u64, 0))
            },
            Expr::Identifier(ident) => self
                .values
                .get(&ident.name)
                .copied()
                .ok_or_else(|| format!("Unknown variable {}", ident.name)),
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = self.emit_expression(left)?;
                let rhs = self.emit_expression(right)?;
                unsafe {
                    let name = CString::new("tmp").unwrap();
                    Ok(match op {
                        BinaryOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr()),
                        BinaryOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr()),
                        BinaryOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr()),
                        BinaryOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr()),
                        _ => {
                            return Err("Only +, -, *, / supported in build mode".into());
                        }
                    })
                }
            }
            _ => Err("Expression not supported in build mode".into()),
        }
    }

    fn emit_out(&mut self, value: LLVMValueRef) {
        unsafe {
            let fmt_literal = CString::new("%d\n").unwrap();
            let fmt_name = CString::new("fmt").unwrap();
            let fmt_ptr = llvm_sys::core::LLVMBuildGlobalString(
                self.builder,
                fmt_literal.as_ptr(),
                fmt_name.as_ptr(),
            );
            let call_name = CString::new("printf_call").unwrap();
            let mut args = [fmt_ptr, value];
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

    fn reset_module(&mut self) {
        unsafe {
            if !self.module.is_null() {
                LLVMDisposeModule(self.module);
            }
            let module_name = CString::new("prime").unwrap();
            self.module = LLVMModuleCreateWithNameInContext(module_name.as_ptr(), self.context);

            let void_type = LLVMVoidTypeInContext(self.context);
            let main_type = LLVMFunctionType(void_type, ptr::null_mut(), 0, 0);
            let main_name = CString::new("main").unwrap();
            self.main_fn = LLVMAddFunction(self.module, main_name.as_ptr(), main_type);

            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut params = [i8_ptr];
            self.printf_type =
                LLVMFunctionType(self.i32_type, params.as_mut_ptr(), params.len() as u32, 1);
            let printf_name = CString::new("printf").unwrap();
            self.printf = LLVMAddFunction(self.module, printf_name.as_ptr(), self.printf_type);
            LLVMSetLinkage(self.printf, LLVMLinkage::LLVMExternalLinkage);
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
