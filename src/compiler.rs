use crate::language::{ast::*, types::TypeExpr};
use llvm_sys::{
    LLVMLinkage,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildCall2, LLVMBuildGlobalString,
        LLVMBuildRetVoid, LLVMConstInt, LLVMContextCreate, LLVMContextDispose,
        LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeMessage, LLVMDisposeModule,
        LLVMFunctionType, LLVMInt8TypeInContext, LLVMInt32TypeInContext,
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
    scopes: Vec<HashMap<String, Value>>,
    structs: HashMap<String, StructDef>,
    functions: HashMap<String, FunctionDef>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    consts: Vec<ConstDef>,
}

#[derive(Clone)]
enum Value {
    Int(IntValue),
    Str(LLVMValueRef),
    Struct(HashMap<String, Value>),
    Enum(EnumValue),
    Tuple(Vec<Value>),
    Unit,
}

#[derive(Clone)]
struct EnumValue {
    enum_name: String,
    variant: String,
    values: Vec<Value>,
}

#[derive(Clone)]
struct IntValue {
    llvm: LLVMValueRef,
    constant: Option<i128>,
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    fields: usize,
}

impl IntValue {
    fn new(llvm: LLVMValueRef, constant: Option<i128>) -> Self {
        Self { llvm, constant }
    }

    fn llvm(&self) -> LLVMValueRef {
        self.llvm
    }

    fn constant(&self) -> Option<i128> {
        self.constant
    }
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
                enum_variants: HashMap::new(),
                consts: Vec::new(),
            };
            compiler.reset_module();
            compiler
        }
    }

    fn const_int_value(&self, value: i128) -> IntValue {
        unsafe {
            let llvm = LLVMConstInt(self.i32_type, value as u64, 0);
            IntValue::new(llvm, Some(value))
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        self.reset_module();
        self.scopes.clear();
        self.push_scope();
        self.structs.clear();
        self.functions.clear();
        self.enum_variants.clear();
        self.consts.clear();
        for module in &program.modules {
            for item in &module.items {
                match item {
                    Item::Struct(def) => {
                        self.structs.insert(def.name.clone(), def.clone());
                    }
                    Item::Enum(def) => {
                        for variant in &def.variants {
                            self.enum_variants.insert(
                                variant.name.clone(),
                                EnumVariantInfo {
                                    enum_name: def.name.clone(),
                                    fields: variant.fields.len(),
                                },
                            );
                        }
                    }
                    Item::Function(func) => {
                        self.functions.insert(func.name.clone(), func.clone());
                    }
                    Item::Const(const_def) => {
                        self.consts.push(const_def.clone());
                    }
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

        for const_def in self.consts.clone() {
            let value = self.emit_expression(&const_def.value)?;
            self.insert_var(&const_def.name, value);
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
        self.print_value(value)?;
        self.emit_printf_call("\n", &mut []);
        Ok(())
    }

    fn print_value(&mut self, value: Value) -> Result<(), String> {
        match value {
            Value::Int(int_value) => {
                self.emit_printf_call("%d", &mut [int_value.llvm()]);
                Ok(())
            }
            Value::Str(ptr) => {
                self.emit_printf_call("%s", &mut [ptr]);
                Ok(())
            }
            Value::Tuple(values) => {
                self.emit_printf_call("(", &mut []);
                for (idx, item) in values.into_iter().enumerate() {
                    if idx > 0 {
                        self.emit_printf_call(", ", &mut []);
                    }
                    self.print_value(item)?;
                }
                self.emit_printf_call(")", &mut []);
                Ok(())
            }
            Value::Enum(enum_value) => {
                if enum_value.values.is_empty() {
                    let label = format!("{}::{}", enum_value.enum_name, enum_value.variant);
                    self.emit_printf_call(&label, &mut []);
                    Ok(())
                } else {
                    let prefix = format!("{}::{}(", enum_value.enum_name, enum_value.variant);
                    self.emit_printf_call(&prefix, &mut []);
                    for (idx, item) in enum_value.values.into_iter().enumerate() {
                        if idx > 0 {
                            self.emit_printf_call(", ", &mut []);
                        }
                        self.print_value(item)?;
                    }
                    self.emit_printf_call(")", &mut []);
                    Ok(())
                }
            }
            Value::Struct(_) => Err("Cannot print struct value in build mode".into()),
            Value::Unit => {
                self.emit_printf_call("()", &mut []);
                Ok(())
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

    fn emit_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Let(stmt) => {
                let value = if let Some(expr) = &stmt.value {
                    self.emit_expression(expr)?
                } else {
                    Value::Int(self.const_int_value(0))
                };
                self.insert_var(&stmt.name, value);
            }
            Statement::Expr(expr_stmt) => self.eval_expression_statement(&expr_stmt.expr)?,
            Statement::Block(block) => {
                self.push_scope();
                let _ = self.execute_block_contents(block)?;
                self.pop_scope();
            }
            Statement::Defer(stmt) => self.eval_expression_statement(&stmt.expr)?,
            Statement::Assign(stmt) => {
                if let Expr::Identifier(ident) = &stmt.target {
                    let value = self.emit_expression(&stmt.value)?;
                    self.assign_var(&ident.name, value)?;
                } else {
                    return Err(
                        "Only assignments to identifiers are supported in build mode".into(),
                    );
                }
            }
            Statement::If(stmt) => {
                let condition = self.emit_expression(&stmt.condition)?;
                if self.value_to_bool(condition)? {
                    self.push_scope();
                    let _ = self.execute_block_contents(&stmt.then_branch)?;
                    self.pop_scope();
                } else if let Some(else_branch) = &stmt.else_branch {
                    self.push_scope();
                    let _ = self.execute_block_contents(else_branch)?;
                    self.pop_scope();
                }
            }
            Statement::While(stmt) => loop {
                let condition = self.emit_expression(&stmt.condition)?;
                if !self.value_to_bool(condition)? {
                    break;
                }
                self.push_scope();
                let _ = self.execute_block_contents(&stmt.body)?;
                self.pop_scope();
            },
            Statement::ForRange(stmt) => {
                let (start, end, inclusive) = self.evaluate_range(&stmt.range)?;
                let mut current = start;
                let limit = if inclusive { end + 1 } else { end };
                while current < limit {
                    self.push_scope();
                    self.insert_var(&stmt.binding, Value::Int(self.const_int_value(current)));
                    let _ = self.execute_block_contents(&stmt.body)?;
                    self.pop_scope();
                    current += 1;
                }
            }
            _ => {
                return Err("Statement not supported in build mode".into());
            }
        }
        Ok(())
    }

    fn emit_expression(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(Literal::Int(value, _)) => {
                Ok(Value::Int(self.const_int_value(*value as i128)))
            }
            Expr::Literal(Literal::Bool(value, _)) => {
                let int_value = if *value { 1 } else { 0 };
                Ok(Value::Int(self.const_int_value(int_value)))
            }
            Expr::Literal(Literal::Float(value, _)) => {
                Ok(Value::Int(self.const_int_value(*value as i128)))
            }
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
            Expr::Literal(Literal::Rune(value, _)) => {
                Ok(Value::Int(self.const_int_value(*value as i128)))
            }
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
                let result = match op {
                    BinaryOp::Add => lhs.constant().zip(rhs.constant()).map(|(a, b)| a + b),
                    BinaryOp::Sub => lhs.constant().zip(rhs.constant()).map(|(a, b)| a - b),
                    BinaryOp::Mul => lhs.constant().zip(rhs.constant()).map(|(a, b)| a * b),
                    BinaryOp::Div => lhs.constant().zip(rhs.constant()).map(|(a, b)| a / b),
                    BinaryOp::Rem => lhs.constant().zip(rhs.constant()).map(|(a, b)| a % b),
                    BinaryOp::Lt => lhs
                        .constant()
                        .zip(rhs.constant())
                        .map(|(a, b)| if a < b { 1 } else { 0 }),
                    BinaryOp::LtEq => lhs
                        .constant()
                        .zip(rhs.constant())
                        .map(|(a, b)| if a <= b { 1 } else { 0 }),
                    BinaryOp::Gt => lhs
                        .constant()
                        .zip(rhs.constant())
                        .map(|(a, b)| if a > b { 1 } else { 0 }),
                    BinaryOp::GtEq => lhs
                        .constant()
                        .zip(rhs.constant())
                        .map(|(a, b)| if a >= b { 1 } else { 0 }),
                    BinaryOp::Eq => lhs
                        .constant()
                        .zip(rhs.constant())
                        .map(|(a, b)| if a == b { 1 } else { 0 }),
                    BinaryOp::NotEq => lhs
                        .constant()
                        .zip(rhs.constant())
                        .map(|(a, b)| if a != b { 1 } else { 0 }),
                    _ => None,
                };
                if let Some(value) = result {
                    Ok(Value::Int(self.const_int_value(value)))
                } else {
                    Err("Operation not supported in build mode".into())
                }
            }
            Expr::StructLiteral { name, fields, .. } => self.build_struct_literal(name, fields),
            Expr::EnumLiteral {
                enum_name,
                variant,
                values,
                ..
            } => self.build_enum_literal(enum_name.as_deref(), variant, values),
            Expr::Match(match_expr) => self.emit_match_expression(match_expr),
            Expr::Tuple(values, _) => {
                let mut items = Vec::new();
                for value in values {
                    items.push(self.emit_expression(value)?);
                }
                Ok(Value::Tuple(items))
            }
            Expr::Block(block) => {
                self.push_scope();
                let result = self.execute_block_contents(block)?;
                self.pop_scope();
                Ok(result.unwrap_or(Value::Unit))
            }
            Expr::If(if_expr) => self.emit_if_expression(if_expr),
            Expr::FieldAccess { base, field, .. } => {
                let base_value = self.emit_expression(base)?;
                match base_value {
                    Value::Struct(map) => map
                        .get(field)
                        .cloned()
                        .ok_or_else(|| format!("Field {} not found", field)),
                    Value::Int(_) => Err("Cannot access field on integer value".into()),
                    Value::Str(_) => Err("Cannot access field on string value".into()),
                    Value::Enum(_) => Err("Cannot access field on enum value".into()),
                    Value::Tuple(_) => Err("Cannot access field on tuple value".into()),
                    Value::Unit => Err("Cannot access field on unit value".into()),
                }
            }
            Expr::Call { callee, args, .. } => self.emit_call_expression(callee, args),
            other => Err(format!(
                "Expression `{}` not supported in build mode",
                describe_expr(other)
            )),
        }
    }

    fn emit_call_expression(&mut self, callee: &Expr, args: &[Expr]) -> Result<Value, String> {
        if let Expr::Identifier(ident) = callee {
            if let Some(info) = self.enum_variants.get(&ident.name).cloned() {
                return self.build_enum_literal(Some(info.enum_name.as_str()), &ident.name, args);
            }
            if ident.name == "out" {
                return Err("out() cannot be used in expressions in build mode".into());
            }
            let result = self.invoke_function(&ident.name, args)?;
            result.ok_or_else(|| format!("Function `{}` does not return a value", ident.name))
        } else {
            Err("Only direct function calls are supported in build mode expressions".into())
        }
    }

    fn emit_if_expression(&mut self, if_expr: &IfExpr) -> Result<Value, String> {
        let condition = self.emit_expression(&if_expr.condition)?;
        if self.value_to_bool(condition)? {
            self.push_scope();
            let value = self.execute_block_contents(&if_expr.then_branch)?;
            self.pop_scope();
            Ok(value.unwrap_or(Value::Unit))
        } else if let Some(else_block) = &if_expr.else_branch {
            self.push_scope();
            let value = self.execute_block_contents(else_block)?;
            self.pop_scope();
            Ok(value.unwrap_or(Value::Unit))
        } else {
            Ok(Value::Unit)
        }
    }

    fn emit_match_expression(&mut self, match_expr: &MatchExpr) -> Result<Value, String> {
        let target = self.emit_expression(&match_expr.expr)?;
        for arm in &match_expr.arms {
            self.push_scope();
            if self.match_pattern(&target, &arm.pattern)? {
                let value = self.emit_expression(&arm.value)?;
                self.pop_scope();
                return Ok(value);
            }
            self.pop_scope();
        }
        Err("No match arm matched in build mode".into())
    }

    fn match_pattern(&mut self, value: &Value, pattern: &Pattern) -> Result<bool, String> {
        match pattern {
            Pattern::Wildcard(_) => Ok(true),
            Pattern::Identifier(name, _) => {
                self.insert_var(name, value.clone());
                Ok(true)
            }
            Pattern::Literal(lit) => self.match_literal(value.clone(), lit),
            Pattern::EnumVariant {
                enum_name,
                variant,
                bindings,
                ..
            } => {
                if let Value::Enum(enum_value) = value.clone() {
                    if enum_value.variant != *variant {
                        return Ok(false);
                    }
                    if enum_name
                        .as_ref()
                        .map(|name| enum_value.enum_name == *name)
                        .unwrap_or(true)
                    {
                        if bindings.len() != enum_value.values.len() {
                            return Err(format!(
                                "Variant `{}` expects {} bindings, got {}",
                                variant,
                                enum_value.values.len(),
                                bindings.len()
                            ));
                        }
                        for (binding, field_value) in bindings.iter().zip(enum_value.values.iter())
                        {
                            if !self.match_pattern(field_value, binding)? {
                                return Ok(false);
                            }
                        }
                        return Ok(true);
                    }
                }
                Ok(false)
            }
        }
    }

    fn match_literal(&self, value: Value, literal: &Literal) -> Result<bool, String> {
        match (literal, value) {
            (Literal::Int(expected, _), Value::Int(int_value)) => int_value
                .constant()
                .map(|val| val == *expected)
                .ok_or_else(|| "Non-constant integer pattern in build mode".into()),
            (Literal::Bool(expected, _), Value::Int(int_value)) => {
                let want = if *expected { 1 } else { 0 };
                int_value
                    .constant()
                    .map(|val| val == want)
                    .ok_or_else(|| "Non-constant boolean pattern in build mode".into())
            }
            (Literal::Rune(expected, _), Value::Int(int_value)) => {
                let want = *expected as i128;
                int_value
                    .constant()
                    .map(|val| val == want)
                    .ok_or_else(|| "Non-constant rune pattern in build mode".into())
            }
            _ => Err("Literal pattern not supported in build mode".into()),
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

    fn expect_int(&self, value: Value) -> Result<IntValue, String> {
        match value {
            Value::Int(v) => Ok(v),
            other => Err(format!(
                "Expected integer value in build mode, got {}",
                describe_value(&other)
            )),
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
                    let value = self.emit_expression(expr)?;
                    if field_def.embedded {
                        let embedded_struct = match value {
                            Value::Struct(inner) => inner,
                            _ => {
                                return Err(
                                    "Embedded field must be initialized with a struct value".into(),
                                );
                            }
                        };
                        for (key, val) in embedded_struct {
                            map.insert(key, val);
                        }
                    } else if let Some(key) = &field_def.name {
                        map.insert(key.clone(), value);
                    } else {
                        let fallback =
                            type_name_from_type_expr(&field_def.ty.ty).ok_or_else(|| {
                                "Unable to determine field name for struct field".to_string()
                            })?;
                        map.insert(fallback, value);
                    }
                }
                Ok(Value::Struct(map))
            }
        }
    }

    fn build_enum_literal(
        &mut self,
        enum_name: Option<&str>,
        variant: &str,
        values: &[Expr],
    ) -> Result<Value, String> {
        let info = self
            .enum_variants
            .get(variant)
            .cloned()
            .ok_or_else(|| format!("Unknown enum variant {}", variant))?;
        if let Some(name) = enum_name {
            if name != info.enum_name {
                return Err(format!(
                    "Variant `{}` does not belong to enum `{}`",
                    variant, name
                ));
            }
        }
        if info.fields != values.len() {
            return Err(format!(
                "Variant `{}` expects {} values, got {}",
                variant,
                info.fields,
                values.len()
            ));
        }
        let mut evaluated = Vec::new();
        for expr in values {
            evaluated.push(self.emit_expression(expr)?);
        }
        Ok(Value::Enum(EnumValue {
            enum_name: info.enum_name.clone(),
            variant: variant.to_string(),
            values: evaluated,
        }))
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
        } else {
            Ok(result)
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

    fn assign_var(&mut self, name: &str, value: Value) -> Result<(), String> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            }
        }
        Err(format!("Unknown variable {}", name))
    }

    fn value_to_bool(&self, value: Value) -> Result<bool, String> {
        let int_value = self.expect_int(value)?;
        let constant = int_value
            .constant()
            .ok_or_else(|| "Non-constant condition not supported in build mode".to_string())?;
        Ok(constant != 0)
    }

    fn evaluate_range(&mut self, range: &RangeExpr) -> Result<(i128, i128, bool), String> {
        let start_expr = self.emit_expression(&range.start)?;
        let start_value = self.expect_int(start_expr)?;
        let end_expr = self.emit_expression(&range.end)?;
        let end_value = self.expect_int(end_expr)?;
        let start_const = start_value
            .constant()
            .ok_or_else(|| "Range bounds must be constant in build mode".to_string())?;
        let end_const = end_value
            .constant()
            .ok_or_else(|| "Range bounds must be constant in build mode".to_string())?;
        Ok((start_const, end_const, range.inclusive))
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

fn describe_expr(expr: &Expr) -> &'static str {
    match expr {
        Expr::Identifier(_) => "identifier",
        Expr::Literal(_) => "literal",
        Expr::Binary { .. } => "binary",
        Expr::Unary { .. } => "unary",
        Expr::Call { .. } => "call",
        Expr::FieldAccess { .. } => "field access",
        Expr::StructLiteral { .. } => "struct literal",
        Expr::EnumLiteral { .. } => "enum literal",
        Expr::Block(_) => "block",
        Expr::If(_) => "if expression",
        Expr::Match(_) => "match expression",
        Expr::Tuple(_, _) => "tuple",
        Expr::ArrayLiteral(_, _) => "array literal",
        Expr::Range(_) => "range",
        Expr::Reference { .. } => "reference",
        Expr::Deref { .. } => "deref",
    }
}

fn describe_value(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "int",
        Value::Str(_) => "string",
        Value::Struct(_) => "struct",
        Value::Enum(_) => "enum",
        Value::Tuple(_) => "tuple",
        Value::Unit => "unit",
    }
}
