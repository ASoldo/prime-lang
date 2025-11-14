use crate::language::{ast::*, types::TypeExpr};
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
    scopes: Vec<HashMap<String, Value>>,
    structs: HashMap<String, StructDef>,
    functions: HashMap<String, FunctionDef>,
}

#[derive(Clone)]
enum Value {
    Int(LLVMValueRef),
    Str(LLVMValueRef),
    Struct(HashMap<String, Value>),
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
                scopes: Vec::new(),
                structs: HashMap::new(),
                functions: HashMap::new(),
            };
            compiler.reset_module();
            compiler
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        self.reset_module();
        self.scopes.clear();
        self.push_scope();
        self.structs.clear();
        self.functions.clear();
        for module in &program.modules {
            for item in &module.items {
                match item {
                    Item::Struct(def) => {
                        self.structs.insert(def.name.clone(), def.clone());
                    }
                    Item::Function(func) => {
                        self.functions.insert(func.name.clone(), func.clone());
                    }
                    _ => {}
                }
            }
        }

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

        let _ = self.execute_block_contents(body)?;
        self.pop_scope();

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

    fn emit_out_value(&mut self, value: Value) -> Result<(), String> {
        match value {
            Value::Int(v) => {
                self.emit_printf_call("%d\n", &mut [v]);
                Ok(())
            }
            Value::Str(ptr) => {
                self.emit_printf_call("%s\n", &mut [ptr]);
                Ok(())
            }
            Value::Struct(_) => Err("Cannot print struct value in build mode".into()),
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

    fn emit_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Let(stmt) => {
                let value = if let Some(expr) = &stmt.value {
                    self.emit_expression(expr)?
                } else {
                    Value::Int(unsafe { LLVMConstInt(self.i32_type, 0, 0) })
                };
                self.insert_var(&stmt.name, value);
            }
            Statement::Expr(expr_stmt) => self.eval_expression_statement(&expr_stmt.expr)?,
            Statement::Block(block) => {
                self.push_scope();
                let _ = self.execute_block_contents(block)?;
                self.pop_scope();
            }
            _ => {
                return Err(
                    "Only let statements, nested blocks, and function calls are supported in build mode"
                        .into(),
                );
            }
        }
        Ok(())
    }

    fn emit_expression(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(Literal::Int(value, _)) => unsafe {
                Ok(Value::Int(LLVMConstInt(self.i32_type, *value as u64, 0)))
            },
            Expr::Literal(Literal::Bool(value, _)) => unsafe {
                Ok(Value::Int(LLVMConstInt(
                    self.i32_type,
                    if *value { 1 } else { 0 },
                    0,
                )))
            },
            Expr::Literal(Literal::Float(value, _)) => unsafe {
                Ok(Value::Int(LLVMConstInt(
                    self.i32_type,
                    *value as i64 as u64,
                    0,
                )))
            },
            Expr::Literal(Literal::String(value, _)) => {
                let c_value = CString::new(value.as_str())
                    .map_err(|_| "String literal contains null byte".to_string())?;
                let name = CString::new("str_lit").unwrap();
                unsafe {
                    Ok(Value::Str(LLVMBuildGlobalString(
                        self.builder,
                        c_value.as_ptr(),
                        name.as_ptr(),
                    )))
                }
            }
            Expr::Literal(Literal::Rune(value, _)) => unsafe {
                Ok(Value::Int(LLVMConstInt(self.i32_type, *value as u64, 0)))
            },
            Expr::Identifier(ident) => self
                .get_var(&ident.name)
                .ok_or_else(|| format!("Unknown variable {}", ident.name)),
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = self.emit_expression(left)?;
                let rhs = self.emit_expression(right)?;
                let lhs = self.expect_int(lhs)?;
                let rhs = self.expect_int(rhs)?;
                unsafe {
                    let name = CString::new("tmp").unwrap();
                    Ok(Value::Int(match op {
                        BinaryOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr()),
                        BinaryOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr()),
                        BinaryOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr()),
                        BinaryOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr()),
                        _ => {
                            return Err("Only +, -, *, / supported in build mode".into());
                        }
                    }))
                }
            }
            Expr::StructLiteral { name, fields, .. } => self.build_struct_literal(name, fields),
            Expr::FieldAccess { base, field, .. } => {
                let base_value = self.emit_expression(base)?;
                match base_value {
                    Value::Struct(map) => map
                        .get(field)
                        .cloned()
                        .ok_or_else(|| format!("Field {} not found", field)),
                    Value::Int(_) => Err("Cannot access field on integer value".into()),
                    Value::Str(_) => Err("Cannot access field on string value".into()),
                }
            }
            _ => Err("Expression not supported in build mode".into()),
        }
    }

    fn emit_printf_call(&mut self, fmt: &str, args: &mut [LLVMValueRef]) {
        unsafe {
            let fmt_literal = CString::new(fmt).unwrap();
            let fmt_name = CString::new("fmt").unwrap();
            let fmt_ptr =
                LLVMBuildGlobalString(self.builder, fmt_literal.as_ptr(), fmt_name.as_ptr());
            let call_name = CString::new("printf_call").unwrap();
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(fmt_ptr);
            call_args.extend_from_slice(args);
            LLVMBuildCall2(
                self.builder,
                self.printf_type,
                self.printf,
                call_args.as_mut_ptr(),
                call_args.len() as u32,
                call_name.as_ptr(),
            );
        }
    }

    fn expect_int(&self, value: Value) -> Result<LLVMValueRef, String> {
        match value {
            Value::Int(v) => Ok(v),
            Value::Str(_) => Err("Expected integer value in build mode".into()),
            Value::Struct(_) => Err("Expected integer value in build mode".into()),
        }
    }

    fn build_struct_literal(
        &mut self,
        name: &str,
        fields: &StructLiteralKind,
    ) -> Result<Value, String> {
        match fields {
            StructLiteralKind::Named(named) => {
                let mut map = HashMap::new();
                for field in named {
                    let value = self.emit_expression(&field.value)?;
                    map.insert(field.name.clone(), value);
                }
                Ok(Value::Struct(map))
            }
            StructLiteralKind::Positional(values) => {
                let def = self
                    .structs
                    .get(name)
                    .cloned()
                    .ok_or_else(|| format!("Unknown struct {}", name))?;
                if def.fields.len() != values.len() {
                    return Err(format!(
                        "Struct `{}` expects {} fields, got {}",
                        name,
                        def.fields.len(),
                        values.len()
                    ));
                }
                let mut map = HashMap::new();
                for (field_def, expr) in def.fields.iter().zip(values.iter()) {
                    let key = field_def
                        .name
                        .clone()
                        .or_else(|| type_name_from_type_expr(&field_def.ty.ty))
                        .ok_or_else(|| {
                            "Unable to determine field name for embedded field".to_string()
                        })?;
                    let value = self.emit_expression(expr)?;
                    map.insert(key, value);
                }
                Ok(Value::Struct(map))
            }
        }
    }

    fn eval_expression_statement(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Call { callee, args, .. } => {
                if let Expr::Identifier(ident) = callee.as_ref() {
                    if ident.name == "out" {
                        if args.len() != 1 {
                            return Err("out() expects exactly one argument".into());
                        }
                        let value = self.emit_expression(&args[0])?;
                        self.emit_out_value(value)
                    } else {
                        let result = self.invoke_function(&ident.name, args)?;
                        if result.is_some() {
                            Err("Functions returning values are not supported in expression statements during build mode".into())
                        } else {
                            Ok(())
                        }
                    }
                } else {
                    Err("Only direct function calls are supported in build mode".into())
                }
            }
            _ => Err(
                "Only function calls are supported as standalone expressions in build mode".into(),
            ),
        }
    }

    fn invoke_function(&mut self, name: &str, args: &[Expr]) -> Result<Option<Value>, String> {
        let func = self
            .functions
            .get(name)
            .cloned()
            .ok_or_else(|| format!("Unknown function {}", name))?;
        if func.params.len() != args.len() {
            return Err(format!(
                "Function `{}` expects {} arguments, got {}",
                name,
                func.params.len(),
                args.len()
            ));
        }

        let mut evaluated_args = Vec::new();
        for expr in args {
            evaluated_args.push(self.emit_expression(expr)?);
        }

        self.push_scope();
        for (param, value) in func.params.iter().zip(evaluated_args.into_iter()) {
            self.insert_var(&param.name, value);
        }
        let result = match &func.body {
            FunctionBody::Block(block) => self.execute_block_contents(block)?,
            FunctionBody::Expr(expr) => Some(self.emit_expression(&expr.node)?),
        };
        self.pop_scope();

        if func.returns.is_empty() {
            Ok(None)
        } else if func.returns.len() == 1 {
            Ok(result)
        } else {
            Err("Functions returning multiple values are not supported in build mode".into())
        }
    }

    fn execute_block_contents(&mut self, block: &Block) -> Result<Option<Value>, String> {
        for statement in &block.statements {
            self.emit_statement(statement)?;
        }
        if let Some(tail) = &block.tail {
            self.emit_expression(tail).map(Some)
        } else {
            Ok(None)
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert_var(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value);
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
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

fn type_name_from_type_expr(expr: &TypeExpr) -> Option<String> {
    match expr {
        TypeExpr::Named(name, _) => Some(name.clone()),
        _ => None,
    }
}
