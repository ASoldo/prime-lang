use crate::language::{
    ast::*,
    types::{Mutability, TypeExpr},
};
use llvm_sys::{
    LLVMLinkage,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildCall2, LLVMBuildGlobalString,
        LLVMBuildRet, LLVMConstInt, LLVMConstReal, LLVMContextCreate, LLVMContextDispose,
        LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeMessage, LLVMDisposeModule,
        LLVMDoubleTypeInContext, LLVMFunctionType, LLVMInt8TypeInContext, LLVMInt32TypeInContext,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPositionBuilderAtEnd,
        LLVMPrintModuleToFile, LLVMSetLinkage,
    },
    prelude::*,
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    ffi::{CStr, CString},
    mem,
    path::Path,
    ptr,
    rc::Rc,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionKey {
    name: String,
    receiver: Option<String>,
    type_args: Option<Vec<String>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ImplKey {
    interface: String,
    type_args: Vec<String>,
    target: String,
}

pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    i32_type: LLVMTypeRef,
    f64_type: LLVMTypeRef,
    printf_type: LLVMTypeRef,
    printf: LLVMValueRef,
    main_fn: LLVMValueRef,
    scopes: Vec<HashMap<String, Binding>>,
    structs: HashMap<String, StructEntry>,
    functions: HashMap<FunctionKey, FunctionEntry>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    interfaces: HashMap<String, InterfaceEntry>,
    impls: HashSet<ImplKey>,
    consts: Vec<(String, ConstDef)>,
    active_mut_borrows: HashSet<String>,
    borrow_frames: Vec<Vec<String>>,
    defer_stack: Vec<Vec<Expr>>,
    deprecated_warnings: HashSet<String>,
    module_stack: Vec<String>,
}

#[derive(Clone)]
enum Value {
    Int(IntValue),
    Float(FloatValue),
    Bool(bool),
    Str(StringValue),
    Struct(StructValue),
    Enum(EnumValue),
    Tuple(Vec<Value>),
    Unit,
    Reference(ReferenceValue),
    Boxed(BoxValue),
    Slice(SliceValue),
    Map(MapValue),
    Moved,
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
struct FloatValue {
    llvm: LLVMValueRef,
    constant: Option<f64>,
}

#[derive(Clone)]
struct ReferenceValue {
    cell: Rc<RefCell<Value>>,
    mutable: bool,
    origin: Option<String>,
}

#[derive(Clone)]
struct StringValue {
    llvm: LLVMValueRef,
    text: Rc<String>,
}

#[derive(Clone)]
struct BoxValue {
    cell: Rc<RefCell<Value>>,
}

#[derive(Clone)]
struct SliceValue {
    items: Rc<RefCell<Vec<Value>>>,
}

#[derive(Clone)]
struct MapValue {
    entries: Rc<RefCell<BTreeMap<String, Value>>>,
}

#[derive(Clone)]
struct StructValue {
    name: String,
    fields: HashMap<String, Value>,
}

#[derive(Clone)]
struct Binding {
    cell: Rc<RefCell<Value>>,
    mutable: bool,
}

enum FlowSignal {
    Break,
    Continue,
    Return(Vec<Value>),
    Propagate(Value),
}

enum BlockEval {
    Value(Value),
    Flow(FlowSignal),
}

enum EvalOutcome<T> {
    Value(T),
    Flow(FlowSignal),
}

impl StringValue {
    fn new(llvm: LLVMValueRef, text: Rc<String>) -> Self {
        Self { llvm, text }
    }
}

impl BoxValue {
    fn new(value: Value) -> Self {
        Self {
            cell: Rc::new(RefCell::new(value)),
        }
    }

    fn replace(&self, value: Value) -> Value {
        std::mem::replace(&mut *self.cell.borrow_mut(), value)
    }
}

impl SliceValue {
    fn new() -> Self {
        Self {
            items: Rc::new(RefCell::new(Vec::new())),
        }
    }

    fn from_vec(items: Vec<Value>) -> Self {
        Self {
            items: Rc::new(RefCell::new(items)),
        }
    }

    fn push(&self, value: Value) {
        self.items.borrow_mut().push(value);
    }

    fn len(&self) -> usize {
        self.items.borrow().len()
    }

    fn get(&self, index: usize) -> Option<Value> {
        self.items.borrow().get(index).cloned()
    }
}

impl MapValue {
    fn new() -> Self {
        Self {
            entries: Rc::new(RefCell::new(BTreeMap::new())),
        }
    }

    fn from_entries(entries: Vec<(String, Value)>) -> Self {
        let map = Self::new();
        for (key, value) in entries {
            map.insert(key, value);
        }
        map
    }

    fn insert(&self, key: String, value: Value) {
        self.entries.borrow_mut().insert(key, value);
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.entries.borrow().get(key).cloned()
    }

    fn len(&self) -> usize {
        self.entries.borrow().len()
    }
}

impl StructValue {
    fn new(name: String, fields: HashMap<String, Value>) -> Self {
        Self { name, fields }
    }

    fn get(&self, field: &str) -> Option<Value> {
        self.fields.get(field).cloned()
    }

    fn into_fields(self) -> HashMap<String, Value> {
        self.fields
    }
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    fields: usize,
    module: String,
    visibility: Visibility,
}

#[derive(Clone)]
struct StructEntry {
    module: String,
    def: StructDef,
}

#[derive(Clone)]
struct InterfaceEntry {
    module: String,
    def: InterfaceDef,
}

#[derive(Clone)]
struct FunctionEntry {
    module: String,
    def: FunctionDef,
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

impl FloatValue {
    fn new(llvm: LLVMValueRef, constant: Option<f64>) -> Self {
        Self { llvm, constant }
    }

    fn llvm(&self) -> LLVMValueRef {
        self.llvm
    }

    fn constant(&self) -> Option<f64> {
        self.constant
    }
}

impl Compiler {
    pub fn new() -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(context);
            let i32_type = LLVMInt32TypeInContext(context);
            let f64_type = LLVMDoubleTypeInContext(context);

            let mut compiler = Self {
                context,
                module: ptr::null_mut(),
                builder,
                i32_type,
                f64_type,
                printf_type: ptr::null_mut(),
                printf: ptr::null_mut(),
                main_fn: ptr::null_mut(),
                scopes: Vec::new(),
                structs: HashMap::new(),
                functions: HashMap::new(),
                enum_variants: HashMap::new(),
                interfaces: HashMap::new(),
                impls: HashSet::new(),
                consts: Vec::new(),
                active_mut_borrows: HashSet::new(),
                borrow_frames: Vec::new(),
                defer_stack: Vec::new(),
                deprecated_warnings: HashSet::new(),
                module_stack: Vec::new(),
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

    fn const_float_value(&self, value: f64) -> FloatValue {
        unsafe {
            let llvm = LLVMConstReal(self.f64_type, value);
            FloatValue::new(llvm, Some(value))
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        self.reset_module();
        self.scopes.clear();
        self.active_mut_borrows.clear();
        self.borrow_frames.clear();
        self.defer_stack.clear();
        self.push_scope();
        self.structs.clear();
        self.functions.clear();
        self.enum_variants.clear();
        self.interfaces.clear();
        self.impls.clear();
        self.consts.clear();
        for module in &program.modules {
            for item in &module.items {
                match item {
                    Item::Struct(def) => {
                        self.structs.insert(
                            def.name.clone(),
                            StructEntry {
                                module: module.name.clone(),
                                def: def.clone(),
                            },
                        );
                    }
                    Item::Enum(def) => {
                        for variant in &def.variants {
                            self.enum_variants.insert(
                                variant.name.clone(),
                                EnumVariantInfo {
                                    enum_name: def.name.clone(),
                                    fields: variant.fields.len(),
                                    module: module.name.clone(),
                                    visibility: def.visibility,
                                },
                            );
                        }
                    }
                    Item::Interface(def) => {
                        self.interfaces.insert(
                            def.name.clone(),
                            InterfaceEntry {
                                module: module.name.clone(),
                                def: def.clone(),
                            },
                        );
                    }
                    Item::Impl(block) => {
                        self.register_impl_block(&module.name, block)?;
                    }
                    Item::Function(func) => {
                        self.register_function(func, &module.name)?;
                    }
                    Item::Const(const_def) => {
                        self.consts.push((module.name.clone(), const_def.clone()));
                    }
                }
            }
        }

        let (main_module, main_fn) = program
            .modules
            .iter()
            .flat_map(|module| {
                module
                    .items
                    .iter()
                    .map(move |item| (module.name.clone(), item))
            })
            .find_map(|(module_name, item)| match item {
                Item::Function(func) if func.name == "main" => Some((module_name, func)),
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

        for (module_name, const_def) in self.consts.clone() {
            self.module_stack.push(module_name.clone());
            let value = {
                let result = self.emit_expression(&const_def.value);
                self.module_stack.pop();
                match result? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => {
                        return Err(format!(
                            "Control flow {} not allowed in const initializer",
                            flow_name(&flow)
                        ));
                    }
                }
            };
            self.insert_var(&const_def.name, value, false)?;
        }

        self.module_stack.push(main_module.clone());
        let exec_result = self.execute_block_contents(body);
        self.module_stack.pop();
        match exec_result? {
            BlockEval::Value(_) => {}
            BlockEval::Flow(flow) => {
                return Err(format!(
                    "Control flow {} not allowed at top level",
                    flow_name(&flow)
                ));
            }
        }
        self.exit_scope()?;

        unsafe {
            let zero = LLVMConstInt(self.i32_type, 0, 0);
            LLVMBuildRet(self.builder, zero);
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
            Value::Float(float_value) => {
                self.emit_printf_call("%f", &mut [float_value.llvm()]);
                Ok(())
            }
            Value::Bool(flag) => {
                if flag {
                    self.emit_printf_call("true", &mut []);
                } else {
                    self.emit_printf_call("false", &mut []);
                }
                Ok(())
            }
            Value::Str(string) => {
                self.emit_printf_call("%s", &mut [string.llvm]);
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
            Value::Reference(reference) => {
                self.emit_printf_call("&", &mut []);
                let inner = reference.cell.borrow().clone();
                self.print_value(inner)
            }
            Value::Boxed(_) | Value::Slice(_) | Value::Map(_) => {
                Err("Cannot print heap value in build mode".into())
            }
            Value::Moved => Err("Cannot print moved value in build mode".into()),
        }
    }

    fn emit_statement(&mut self, statement: &Statement) -> Result<Option<FlowSignal>, String> {
        match statement {
            Statement::Let(stmt) => match &stmt.pattern {
                Pattern::Identifier(name, _) => {
                    let value = if let Some(expr) = &stmt.value {
                        match self.emit_expression(expr)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        }
                    } else {
                        Value::Unit
                    };
                    self.insert_var(name, value, stmt.mutability == Mutability::Mutable)?;
                }
                pattern => {
                    let expr = stmt.value.as_ref().ok_or_else(|| {
                        "Destructuring bindings require an initializer".to_string()
                    })?;
                    let value = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let allow_mut = stmt.mutability == Mutability::Mutable;
                    if !self.match_pattern(&value, pattern, allow_mut)? {
                        return Err("Pattern did not match value".into());
                    }
                }
            },
            Statement::Expr(expr_stmt) => {
                if let Some(flow) = self.eval_expression_statement(&expr_stmt.expr)? {
                    return Ok(Some(flow));
                }
            }
            Statement::Return(stmt) => {
                let mut values = Vec::new();
                for expr in &stmt.values {
                    match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => values.push(value),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    }
                }
                return Ok(Some(FlowSignal::Return(values)));
            }
            Statement::Break => return Ok(Some(FlowSignal::Break)),
            Statement::Continue => return Ok(Some(FlowSignal::Continue)),
            Statement::Block(block) => {
                self.push_scope();
                let result = self.execute_block_contents(block)?;
                self.exit_scope()?;
                if let BlockEval::Flow(flow) = result {
                    return Ok(Some(flow));
                }
            }
            Statement::Defer(stmt) => {
                if let Some(stack) = self.defer_stack.last_mut() {
                    stack.push(stmt.expr.clone());
                } else {
                    return Err("No scope available for defer".into());
                }
            }
            Statement::Assign(stmt) => match &stmt.target {
                Expr::Identifier(ident) => match self.emit_expression(&stmt.value)? {
                    EvalOutcome::Value(value) => {
                        self.assign_var(&ident.name, value)?;
                    }
                    EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                },
                Expr::Deref { expr, .. } => {
                    let target = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    match target {
                        Value::Reference(reference) => {
                            if !reference.mutable {
                                return Err("Cannot assign through immutable reference".into());
                            }
                            let value = match self.emit_expression(&stmt.value)? {
                                EvalOutcome::Value(value) => value,
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            };
                            *reference.cell.borrow_mut() = value;
                        }
                        _ => {
                            return Err(
                                "Cannot assign through non-reference value in build mode".into()
                            );
                        }
                    }
                }
                _ => {
                    return Err(
                        "Only assignments to identifiers or dereferences are supported in build mode"
                            .into(),
                    );
                }
            },
            Statement::While(stmt) => match &stmt.condition {
                WhileCondition::Expr(expr) => loop {
                    let condition = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    if !self.value_to_bool(condition)? {
                        break;
                    }
                    self.push_scope();
                    let result = self.execute_block_contents(&stmt.body)?;
                    self.exit_scope()?;
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                        BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => return Ok(Some(flow)),
                    }
                },
                WhileCondition::Let { pattern, value } => loop {
                    let candidate = match self.emit_expression(value)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    self.push_scope();
                    let matched = self.match_pattern(&candidate, pattern, false)?;
                    if !matched {
                        self.exit_scope()?;
                        break;
                    }
                    let result = self.execute_block_contents(&stmt.body)?;
                    self.exit_scope()?;
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                        BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => return Ok(Some(flow)),
                    }
                },
            },
            Statement::For(stmt) => match &stmt.target {
                ForTarget::Range(range_expr) => {
                    let (start, end, inclusive) = match self.evaluate_range(range_expr)? {
                        EvalOutcome::Value(values) => values,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let mut current = start;
                    let limit = if inclusive { end + 1 } else { end };
                    while current < limit {
                        self.push_scope();
                        self.insert_var(
                            &stmt.binding,
                            Value::Int(self.const_int_value(current)),
                            false,
                        )?;
                        let result = self.execute_block_contents(&stmt.body)?;
                        self.exit_scope()?;
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => {}
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
                        }
                        current += 1;
                    }
                }
                ForTarget::Collection(expr) => {
                    let iterable = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let elements = self.collect_iterable_values(iterable)?;
                    for element in elements {
                        self.push_scope();
                        self.insert_var(&stmt.binding, element, false)?;
                        let result = self.execute_block_contents(&stmt.body)?;
                        self.exit_scope()?;
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => continue,
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
                        }
                    }
                }
            },
        }
        Ok(None)
    }

    fn emit_expression(&mut self, expr: &Expr) -> Result<EvalOutcome<Value>, String> {
        match expr {
            Expr::Literal(Literal::Int(value, _)) => Ok(EvalOutcome::Value(Value::Int(
                self.const_int_value(*value as i128),
            ))),
            Expr::Literal(Literal::Bool(value, _)) => Ok(EvalOutcome::Value(Value::Bool(*value))),
            Expr::Literal(Literal::Float(value, _)) => Ok(EvalOutcome::Value(Value::Float(
                self.const_float_value(*value),
            ))),
            Expr::Literal(Literal::String(value, _)) => {
                let c_value = CString::new(value.as_str())
                    .map_err(|_| "String literal contains null byte".to_string())?;
                let name = CString::new("str_lit").unwrap();
                let text = Rc::new(value.clone());
                unsafe {
                    let ptr = LLVMBuildGlobalString(self.builder, c_value.as_ptr(), name.as_ptr());
                    Ok(EvalOutcome::Value(Value::Str(StringValue::new(ptr, text))))
                }
            }
            Expr::Literal(Literal::Rune(value, _)) => Ok(EvalOutcome::Value(Value::Int(
                self.const_int_value(*value as i128),
            ))),
            Expr::Try { block, .. } => {
                self.push_scope();
                let result = self.execute_block_contents(block)?;
                self.exit_scope()?;
                match result {
                    BlockEval::Value(value) => {
                        let wrapped = self.instantiate_enum_variant("Ok", vec![value])?;
                        Ok(EvalOutcome::Value(wrapped))
                    }
                    BlockEval::Flow(FlowSignal::Propagate(value)) => Ok(EvalOutcome::Value(value)),
                    BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                }
            }
            Expr::TryPropagate { expr, .. } => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => self.eval_try_operator(value),
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::Identifier(ident) => {
                let value = self
                    .get_var(&ident.name)
                    .ok_or_else(|| format!("Unknown variable {}", ident.name))?;
                if matches!(value, Value::Moved) {
                    return Err(format!("Value `{}` has been moved", ident.name));
                }
                Ok(EvalOutcome::Value(value))
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = match self.emit_expression(left)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let rhs = match self.emit_expression(right)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                self.eval_binary(*op, lhs, rhs).map(EvalOutcome::Value)
            }
            Expr::StructLiteral { name, fields, .. } => self.build_struct_literal(name, fields),
            Expr::MapLiteral { entries, .. } => self.emit_map_literal(entries),
            Expr::Match(match_expr) => self.emit_match_expression(match_expr),
            Expr::Tuple(values, _) => {
                let mut items = Vec::new();
                for value in values {
                    match self.emit_expression(value)? {
                        EvalOutcome::Value(value) => items.push(value),
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                Ok(EvalOutcome::Value(Value::Tuple(items)))
            }
            Expr::ArrayLiteral(values, _) => self.emit_array_literal(values),
            Expr::Block(block) => {
                self.push_scope();
                let result = self.execute_block_contents(block)?;
                self.exit_scope()?;
                match result {
                    BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                    BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                }
            }
            Expr::If(if_expr) => self.emit_if_expression(if_expr),
            Expr::Reference { mutable, expr, .. } => self.build_reference(expr, *mutable),
            Expr::Deref { expr, .. } => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => self.deref_value(value).map(EvalOutcome::Value),
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::Move { expr, .. } => self.emit_move_expression(expr),
            Expr::FieldAccess { base, field, .. } => {
                let base_value = match self.emit_expression(base)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let mut current = base_value;
                loop {
                    current = match current {
                        Value::Struct(instance) => {
                            let value = instance
                                .get(field)
                                .ok_or_else(|| format!("Field {} not found", field))?;
                            return Ok(EvalOutcome::Value(value));
                        }
                        Value::Reference(reference) => reference.cell.borrow().clone(),
                        Value::Int(_) => {
                            return Err("Cannot access field on integer value".into());
                        }
                        Value::Float(_) => {
                            return Err("Cannot access field on float value".into());
                        }
                        Value::Bool(_) => {
                            return Err("Cannot access field on bool value".into());
                        }
                        Value::Str(_) => {
                            return Err("Cannot access field on string value".into());
                        }
                        Value::Enum(_) => {
                            return Err("Cannot access field on enum value".into());
                        }
                        Value::Tuple(_) => {
                            return Err("Cannot access field on tuple value".into());
                        }
                        Value::Unit => {
                            return Err("Cannot access field on unit value".into());
                        }
                        Value::Boxed(_) | Value::Slice(_) | Value::Map(_) => {
                            return Err("Cannot access field on heap value in build mode".into());
                        }
                        Value::Moved => {
                            return Err("Cannot access field on moved value".into());
                        }
                    };
                }
            }
            Expr::Call {
                callee,
                type_args,
                args,
                ..
            } => self.emit_call_expression(callee, type_args, args),
            other => Err(format!(
                "Expression `{}` not supported in build mode",
                describe_expr(other)
            )),
        }
    }

    fn emit_call_expression(
        &mut self,
        callee: &Expr,
        type_args: &[TypeExpr],
        args: &[Expr],
    ) -> Result<EvalOutcome<Value>, String> {
        match callee {
            Expr::Identifier(ident) => {
                if let Some(info) = self.enum_variants.get(&ident.name).cloned() {
                    return self.build_enum_literal(
                        Some(info.enum_name.as_str()),
                        &ident.name,
                        args,
                    );
                }
                if ident.name == "out" {
                    return self.emit_out_call(args);
                }
                if let Some(result) = self.try_builtin_call(&ident.name, args) {
                    return result;
                }
                let results = self.invoke_function(&ident.name, type_args, args)?;
                let value = self.collapse_results(results);
                Ok(EvalOutcome::Value(value))
            }
            Expr::FieldAccess { base, field, .. } => {
                if let Expr::Identifier(module_ident) = base.as_ref() {
                    let qualified = format!("{}::{}", module_ident.name, field);
                    let key = FunctionKey {
                        name: qualified.clone(),
                        receiver: None,
                        type_args: None,
                    };
                    if self.functions.contains_key(&key) {
                        let results = self.invoke_function(&qualified, type_args, args)?;
                        let value = self.collapse_results(results);
                        return Ok(EvalOutcome::Value(value));
                    }
                }
                let mut method_args = Vec::with_capacity(args.len() + 1);
                method_args.push((**base).clone());
                method_args.extend(args.iter().cloned());
                if let Some(result) = self.try_builtin_call(field, &method_args) {
                    return result;
                }
                let results = self.invoke_function(field, type_args, &method_args)?;
                let value = self.collapse_results(results);
                Ok(EvalOutcome::Value(value))
            }
            _ => Err("Only direct function calls are supported in build mode expressions".into()),
        }
    }

    fn collapse_results(&self, mut values: Vec<Value>) -> Value {
        match values.len() {
            0 => Value::Unit,
            1 => values.pop().unwrap(),
            _ => Value::Tuple(values),
        }
    }

    fn emit_out_call(&mut self, args: &[Expr]) -> Result<EvalOutcome<Value>, String> {
        if args.len() != 1 {
            return Err("out() expects exactly one argument".into());
        }
        match self.emit_expression(&args[0])? {
            EvalOutcome::Value(value) => {
                self.emit_out_value(value)?;
                Ok(EvalOutcome::Value(Value::Unit))
            }
            EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
        }
    }

    fn try_builtin_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Option<Result<EvalOutcome<Value>, String>> {
        match name {
            "box_new" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_box_new(values)))
            }
            "box_get" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_box_get(values)))
            }
            "box_set" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_box_set(values)))
            }
            "box_take" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_box_take(values)))
            }
            "slice_new" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_slice_new(values)))
            }
            "slice_push" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_slice_push(values)))
            }
            "slice_len" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_slice_len(values)))
            }
            "slice_get" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_slice_get(values)))
            }
            "map_new" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_map_new(values)))
            }
            "map_insert" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_map_insert(values)))
            }
            "map_get" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_map_get(values)))
            }
            "len" => Some(self.invoke_builtin(args, |this, values| this.builtin_len(values))),
            "get" => Some(self.invoke_builtin(args, |this, values| this.builtin_get(values))),
            "push" => Some(self.invoke_builtin(args, |this, values| this.builtin_push(values))),
            "insert" => Some(self.invoke_builtin(args, |this, values| this.builtin_insert(values))),
            _ => None,
        }
    }

    fn emit_if_expression(&mut self, if_expr: &IfExpr) -> Result<EvalOutcome<Value>, String> {
        match &if_expr.condition {
            IfCondition::Expr(condition) => {
                let cond_value = match self.emit_expression(condition)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                if self.value_to_bool(cond_value)? {
                    self.push_scope();
                    let value = self.execute_block_contents(&if_expr.then_branch)?;
                    self.exit_scope()?;
                    match value {
                        BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                        BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                    }
                } else if let Some(else_branch) = &if_expr.else_branch {
                    match else_branch {
                        ElseBranch::Block(block) => {
                            self.push_scope();
                            let value = self.execute_block_contents(block)?;
                            self.exit_scope()?;
                            match value {
                                BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                                BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                            }
                        }
                        ElseBranch::ElseIf(nested) => self.emit_if_expression(nested),
                    }
                } else {
                    Ok(EvalOutcome::Value(Value::Unit))
                }
            }
            IfCondition::Let { pattern, value, .. } => {
                let scrutinee = match self.emit_expression(value)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                self.push_scope();
                let matches = self.match_pattern(&scrutinee, pattern, false)?;
                if matches {
                    let result = self.execute_block_contents(&if_expr.then_branch)?;
                    self.exit_scope()?;
                    match result {
                        BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                        BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                    }
                } else {
                    self.exit_scope()?;
                    if let Some(else_branch) = &if_expr.else_branch {
                        match else_branch {
                            ElseBranch::Block(block) => {
                                self.push_scope();
                                let value = self.execute_block_contents(block)?;
                                self.exit_scope()?;
                                match value {
                                    BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                                    BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                                }
                            }
                            ElseBranch::ElseIf(nested) => self.emit_if_expression(nested),
                        }
                    } else {
                        Ok(EvalOutcome::Value(Value::Unit))
                    }
                }
            }
        }
    }

    fn invoke_builtin<F>(&mut self, args: &[Expr], mut f: F) -> Result<EvalOutcome<Value>, String>
    where
        F: FnMut(&mut Self, Vec<Value>) -> Result<Value, String>,
    {
        let mut evaluated = Vec::with_capacity(args.len());
        for expr in args {
            match self.emit_expression(expr) {
                Ok(EvalOutcome::Value(value)) => evaluated.push(value),
                Ok(EvalOutcome::Flow(flow)) => return Ok(EvalOutcome::Flow(flow)),
                Err(err) => return Err(err),
            }
        }
        f(self, evaluated).map(EvalOutcome::Value)
    }

    fn build_reference(
        &mut self,
        expr: &Expr,
        mutable: bool,
    ) -> Result<EvalOutcome<Value>, String> {
        match expr {
            Expr::Identifier(ident) => {
                let (cell, binding_mut) = self
                    .get_binding(&ident.name)
                    .ok_or_else(|| format!("Unknown variable {}", ident.name))?;
                if mutable && !binding_mut {
                    return Err(format!(
                        "Variable `{}` is immutable and cannot be borrowed as mutable",
                        ident.name
                    ));
                }
                if matches!(*cell.borrow(), Value::Moved) {
                    return Err(format!("Value `{}` has been moved", ident.name));
                }
                Ok(EvalOutcome::Value(Value::Reference(ReferenceValue {
                    cell,
                    mutable,
                    origin: Some(ident.name.clone()),
                })))
            }
            _ => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => {
                    Ok(EvalOutcome::Value(Value::Reference(ReferenceValue {
                        cell: Rc::new(RefCell::new(value)),
                        mutable,
                        origin: None,
                    })))
                }
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
        }
    }

    fn emit_array_literal(&mut self, values: &[Expr]) -> Result<EvalOutcome<Value>, String> {
        let mut items = Vec::with_capacity(values.len());
        for expr in values {
            match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => items.push(value),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        Ok(EvalOutcome::Value(Value::Slice(SliceValue::from_vec(
            items,
        ))))
    }

    fn emit_map_literal(
        &mut self,
        entries: &[MapLiteralEntry],
    ) -> Result<EvalOutcome<Value>, String> {
        let mut pairs = Vec::new();
        for entry in entries {
            let key_value = match self.emit_expression(&entry.key)? {
                EvalOutcome::Value(value) => value,
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            let key = self.expect_string_value(key_value, "map literal key")?;
            let value = match self.emit_expression(&entry.value)? {
                EvalOutcome::Value(value) => value,
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            pairs.push((key, value));
        }
        Ok(EvalOutcome::Value(Value::Map(MapValue::from_entries(
            pairs,
        ))))
    }

    fn emit_move_expression(&mut self, expr: &Expr) -> Result<EvalOutcome<Value>, String> {
        match expr {
            Expr::Identifier(ident) => {
                let (cell, _) = self
                    .get_binding(&ident.name)
                    .ok_or_else(|| format!("Unknown variable {}", ident.name))?;
                if self.is_mut_borrowed(&ident.name) {
                    return Err(format!(
                        "Cannot move `{}` while it is mutably borrowed",
                        ident.name
                    ));
                }
                let mut slot = cell.borrow_mut();
                if matches!(*slot, Value::Moved) {
                    return Err(format!("Value `{}` has been moved", ident.name));
                }
                if !Self::is_heap_value(&slot) {
                    return Err(format!(
                        "`{}` cannot be moved; only boxes, slices, and maps support move semantics",
                        ident.name
                    ));
                }
                let moved = std::mem::replace(&mut *slot, Value::Moved);
                self.register_move(&ident.name);
                Ok(EvalOutcome::Value(moved))
            }
            _ => Err("move expressions require identifiers in build mode".into()),
        }
    }

    fn is_heap_value(value: &Value) -> bool {
        matches!(value, Value::Boxed(_) | Value::Slice(_) | Value::Map(_))
    }

    fn deref_value(&self, value: Value) -> Result<Value, String> {
        match value {
            Value::Reference(reference) => Ok(reference.cell.borrow().clone()),
            _ => Err("Cannot dereference non-reference value in build mode".into()),
        }
    }

    fn emit_match_expression(
        &mut self,
        match_expr: &MatchExpr,
    ) -> Result<EvalOutcome<Value>, String> {
        let target = match self.emit_expression(&match_expr.expr)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        for arm in &match_expr.arms {
            self.push_scope();
            if self.match_pattern(&target, &arm.pattern, false)? {
                let value = self.emit_expression(&arm.value)?;
                self.exit_scope()?;
                return Ok(value);
            }
            self.exit_scope()?;
        }
        Err("No match arm matched in build mode".into())
    }

    fn eval_try_operator(&self, value: Value) -> Result<EvalOutcome<Value>, String> {
        match value {
            Value::Enum(enum_value) => {
                if enum_value.enum_name != "Result" {
                    return Err("? operator expects Result value in build mode".into());
                }
                match enum_value.variant.as_str() {
                    "Ok" => {
                        let mut values = enum_value.values;
                        if values.is_empty() {
                            Ok(EvalOutcome::Value(Value::Unit))
                        } else if values.len() == 1 {
                            Ok(EvalOutcome::Value(values.remove(0)))
                        } else {
                            Ok(EvalOutcome::Value(Value::Tuple(values)))
                        }
                    }
                    "Err" => Ok(EvalOutcome::Flow(FlowSignal::Propagate(Value::Enum(
                        enum_value,
                    )))),
                    _ => Err("? operator expects Result value in build mode".into()),
                }
            }
            _ => Err("? operator expects Result value in build mode".into()),
        }
    }

    fn match_pattern(
        &mut self,
        value: &Value,
        pattern: &Pattern,
        mutable_bindings: bool,
    ) -> Result<bool, String> {
        match pattern {
            Pattern::Wildcard => Ok(true),
            Pattern::Identifier(name, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                self.insert_var(name, concrete, mutable_bindings)?;
                Ok(true)
            }
            Pattern::Literal(lit) => self.match_literal(value.clone(), lit),
            Pattern::EnumVariant {
                enum_name,
                variant,
                bindings,
                ..
            } => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Enum(enum_value) = concrete {
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
                            if !self.match_pattern(field_value, binding, mutable_bindings)? {
                                return Ok(false);
                            }
                        }
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Pattern::Tuple(patterns, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Tuple(values) = concrete {
                    if patterns.len() != values.len() {
                        return Ok(false);
                    }
                    for (pat, val) in patterns.iter().zip(values.iter()) {
                        if !self.match_pattern(val, pat, mutable_bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Map(entries, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Map(map) = concrete {
                    for entry in entries {
                        let Some(val) = map.get(&entry.key) else {
                            return Ok(false);
                        };
                        if !self.match_pattern(&val, &entry.pattern, mutable_bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Struct {
                struct_name,
                fields,
                has_spread: _,
                ..
            } => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Struct(instance) = concrete {
                    if let Some(expected) = struct_name {
                        if &instance.name != expected {
                            return Ok(false);
                        }
                    }
                    for field in fields {
                        let Some(field_value) = instance.get(&field.name) else {
                            return Ok(false);
                        };
                        if !self.match_pattern(&field_value, &field.pattern, mutable_bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Slice {
                prefix,
                rest,
                suffix,
                ..
            } => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                let elements = match concrete {
                    Value::Slice(slice) => slice.items.borrow().clone(),
                    _ => return Ok(false),
                };
                if rest.is_none() && elements.len() != prefix.len() + suffix.len() {
                    return Ok(false);
                }
                if prefix.len() + suffix.len() > elements.len() {
                    return Ok(false);
                }
                for (pat, val) in prefix.iter().zip(elements.iter()) {
                    if !self.match_pattern(val, pat, mutable_bindings)? {
                        return Ok(false);
                    }
                }
                for (pat, val) in suffix.iter().rev().zip(elements.iter().rev()) {
                    if !self.match_pattern(val, pat, mutable_bindings)? {
                        return Ok(false);
                    }
                }
                if let Some(rest_pattern) = rest {
                    let start = prefix.len();
                    let end = elements.len() - suffix.len();
                    let slice = SliceValue::from_vec(elements[start..end].to_vec());
                    if !self.match_pattern(&Value::Slice(slice), rest_pattern, mutable_bindings)? {
                        return Ok(false);
                    }
                } else if elements.len() != prefix.len() + suffix.len() {
                    return Ok(false);
                }
                Ok(true)
            }
        }
    }

    fn match_literal(&self, value: Value, literal: &Literal) -> Result<bool, String> {
        let concrete = match value {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other,
        };
        match (literal, concrete) {
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
            (Literal::Bool(expected, _), Value::Bool(actual)) => Ok(*expected == actual),
            (Literal::Float(expected, _), Value::Float(float_value)) => float_value
                .constant()
                .map(|val| val == *expected)
                .ok_or_else(|| "Non-constant float pattern in build mode".into()),
            (Literal::String(expected, _), Value::Str(inner)) => Ok(*inner.text == *expected),
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

    fn eval_binary(&mut self, op: BinaryOp, left: Value, right: Value) -> Result<Value, String> {
        let lhs = Self::deref_if_reference(left);
        let rhs = Self::deref_if_reference(right);
        match (lhs, rhs) {
            (Value::Bool(a), Value::Bool(b)) => self.eval_bool_binary(op, a, b),
            (Value::Int(a), Value::Int(b)) => self.eval_int_binary(op, a, b),
            (Value::Float(a), Value::Float(b)) => self.eval_float_binary(op, a, b),
            (Value::Int(a), Value::Float(b)) => {
                let converted = self.int_to_float(&a)?;
                self.eval_float_binary(op, converted, b)
            }
            (Value::Float(a), Value::Int(b)) => {
                let converted = self.int_to_float(&b)?;
                self.eval_float_binary(op, a, converted)
            }
            _ => Err("Operation not supported in build mode".into()),
        }
    }

    fn eval_bool_binary(&self, op: BinaryOp, lhs: bool, rhs: bool) -> Result<Value, String> {
        let value = match op {
            BinaryOp::And => Value::Bool(lhs && rhs),
            BinaryOp::Or => Value::Bool(lhs || rhs),
            BinaryOp::Eq => Value::Bool(lhs == rhs),
            BinaryOp::NotEq => Value::Bool(lhs != rhs),
            _ => return Err("Operation not supported in build mode".into()),
        };
        Ok(value)
    }

    fn eval_int_binary(
        &mut self,
        op: BinaryOp,
        lhs: IntValue,
        rhs: IntValue,
    ) -> Result<Value, String> {
        let result = match op {
            BinaryOp::Add => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Int(self.const_int_value(a + b))),
            BinaryOp::Sub => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Int(self.const_int_value(a - b))),
            BinaryOp::Mul => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Int(self.const_int_value(a * b))),
            BinaryOp::Div => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Int(self.const_int_value(a / b))),
            BinaryOp::Rem => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Int(self.const_int_value(a % b))),
            BinaryOp::Lt => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(a < b)),
            BinaryOp::LtEq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(a <= b)),
            BinaryOp::Gt => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(a > b)),
            BinaryOp::GtEq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(a >= b)),
            BinaryOp::Eq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(a == b)),
            BinaryOp::NotEq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(a != b)),
            _ => None,
        };
        result.ok_or_else(|| "Operation not supported in build mode".into())
    }

    fn eval_float_binary(
        &mut self,
        op: BinaryOp,
        lhs: FloatValue,
        rhs: FloatValue,
    ) -> Result<Value, String> {
        let values = lhs.constant().zip(rhs.constant());
        let result = match op {
            BinaryOp::Add => values.map(|(a, b)| Value::Float(self.const_float_value(a + b))),
            BinaryOp::Sub => values.map(|(a, b)| Value::Float(self.const_float_value(a - b))),
            BinaryOp::Mul => values.map(|(a, b)| Value::Float(self.const_float_value(a * b))),
            BinaryOp::Div => values.map(|(a, b)| Value::Float(self.const_float_value(a / b))),
            BinaryOp::Rem => values.map(|(a, b)| Value::Float(self.const_float_value(a % b))),
            BinaryOp::Lt => values.map(|(a, b)| Value::Bool(a < b)),
            BinaryOp::LtEq => values.map(|(a, b)| Value::Bool(a <= b)),
            BinaryOp::Gt => values.map(|(a, b)| Value::Bool(a > b)),
            BinaryOp::GtEq => values.map(|(a, b)| Value::Bool(a >= b)),
            BinaryOp::Eq => values.map(|(a, b)| Value::Bool(a == b)),
            BinaryOp::NotEq => values.map(|(a, b)| Value::Bool(a != b)),
            _ => None,
        };
        result.ok_or_else(|| "Operation not supported in build mode".into())
    }

    fn int_to_float(&self, value: &IntValue) -> Result<FloatValue, String> {
        value
            .constant()
            .map(|constant| self.const_float_value(constant as f64))
            .ok_or_else(|| "Operation not supported in build mode".into())
    }

    fn deref_if_reference(value: Value) -> Value {
        match value {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other,
        }
    }

    fn expect_int(&self, value: Value) -> Result<IntValue, String> {
        match value {
            Value::Int(v) => Ok(v),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.expect_int(inner)
            }
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
    ) -> Result<EvalOutcome<Value>, String> {
        let entry = self
            .structs
            .get(name)
            .cloned()
            .ok_or_else(|| format!("Unknown struct {}", name))?;
        self.ensure_item_visible(&entry.module, entry.def.visibility, name, "struct")?;
        let def = entry.def;
        match fields {
            StructLiteralKind::Named(named) => {
                let mut map = HashMap::new();
                for field in named {
                    match self.emit_expression(&field.value)? {
                        EvalOutcome::Value(value) => {
                            map.insert(field.name.clone(), value);
                        }
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                Ok(EvalOutcome::Value(Value::Struct(StructValue::new(
                    name.to_string(),
                    map,
                ))))
            }
            StructLiteralKind::Positional(values) => {
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
                    let value = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    if field_def.embedded {
                        let embedded_struct = match value {
                            Value::Struct(inner) => inner,
                            _ => {
                                return Err(
                                    "Embedded field must be initialized with a struct value".into(),
                                );
                            }
                        };
                        for (key, val) in embedded_struct.into_fields() {
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
                Ok(EvalOutcome::Value(Value::Struct(StructValue::new(
                    name.to_string(),
                    map,
                ))))
            }
        }
    }

    fn build_enum_literal(
        &mut self,
        enum_name: Option<&str>,
        variant: &str,
        values: &[Expr],
    ) -> Result<EvalOutcome<Value>, String> {
        let info = self
            .enum_variants
            .get(variant)
            .cloned()
            .ok_or_else(|| format!("Unknown enum variant {}", variant))?;
        self.ensure_item_visible(&info.module, info.visibility, &info.enum_name, "enum")?;
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
            match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => evaluated.push(value),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        Ok(EvalOutcome::Value(Value::Enum(EnumValue {
            enum_name: info.enum_name.clone(),
            variant: variant.to_string(),
            values: evaluated,
        })))
    }

    fn instantiate_enum_variant(&self, variant: &str, values: Vec<Value>) -> Result<Value, String> {
        let info = self
            .enum_variants
            .get(variant)
            .ok_or_else(|| format!("Unknown enum variant {}", variant))?;
        self.ensure_item_visible(&info.module, info.visibility, &info.enum_name, "enum")?;
        if info.fields != values.len() {
            return Err(format!(
                "Variant `{}` expects {} values, got {}",
                variant,
                info.fields,
                values.len()
            ));
        }
        Ok(Value::Enum(EnumValue {
            enum_name: info.enum_name.clone(),
            variant: variant.to_string(),
            values,
        }))
    }

    fn eval_expression_statement(&mut self, expr: &Expr) -> Result<Option<FlowSignal>, String> {
        match expr {
            Expr::Call {
                callee,
                type_args,
                args,
                ..
            } => match callee.as_ref() {
                Expr::Identifier(ident) => {
                    if ident.name == "out" {
                        match self.emit_out_call(args)? {
                            EvalOutcome::Value(_) => Ok(None),
                            EvalOutcome::Flow(flow) => Ok(Some(flow)),
                        }
                    } else if let Some(result) = self.try_builtin_call(&ident.name, args) {
                        match result? {
                            EvalOutcome::Value(_) => Ok(None),
                            EvalOutcome::Flow(flow) => Ok(Some(flow)),
                        }
                    } else {
                        let result = self.invoke_function(&ident.name, type_args, args)?;
                        if result.is_empty() {
                            Ok(None)
                        } else {
                            Err("Functions returning values are not supported in expression statements during build mode".into())
                        }
                    }
                }
                Expr::FieldAccess { base, field, .. } => {
                    if let Expr::Identifier(module_ident) = base.as_ref() {
                        let qualified = format!("{}::{}", module_ident.name, field);
                        let key = FunctionKey {
                            name: qualified.clone(),
                            receiver: None,
                            type_args: None,
                        };
                        if self.functions.contains_key(&key) {
                            let result = self.invoke_function(&qualified, type_args, args)?;
                            if result.is_empty() {
                                return Ok(None);
                            }
                            return Err(
                                "Functions returning values are not supported in expression statements during build mode"
                                    .into(),
                            );
                        }
                    }
                    let mut method_args = Vec::with_capacity(args.len() + 1);
                    method_args.push((**base).clone());
                    method_args.extend(args.iter().cloned());
                    if let Some(result) = self.try_builtin_call(field, &method_args) {
                        return match result? {
                            EvalOutcome::Value(_) => Ok(None),
                            EvalOutcome::Flow(flow) => Ok(Some(flow)),
                        };
                    }
                    let result = self.invoke_function(field, type_args, &method_args)?;
                    if result.is_empty() {
                        Ok(None)
                    } else {
                        Err("Functions returning values are not supported in expression statements during build mode".into())
                    }
                }
                _ => Err("Only direct function calls are supported in build mode".into()),
            },
            _ => match self.emit_expression(expr)? {
                EvalOutcome::Value(_) => Ok(None),
                EvalOutcome::Flow(flow) => Ok(Some(flow)),
            },
        }
    }

    fn invoke_function(
        &mut self,
        name: &str,
        type_args: &[TypeExpr],
        args: &[Expr],
    ) -> Result<Vec<Value>, String> {
        let mut evaluated_args = Vec::new();
        for expr in args {
            match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => evaluated_args.push(value),
                EvalOutcome::Flow(flow) => {
                    return Err(format!(
                        "Control flow {} cannot escape argument position in build mode",
                        flow_name(&flow)
                    ));
                }
            }
        }
        let receiver_candidate = self.receiver_name_from_values(&evaluated_args);
        let func_entry = self.resolve_function(
            name,
            receiver_candidate.as_deref(),
            type_args,
            &evaluated_args,
        )?;
        let func = func_entry.def.clone();
        if func.params.len() != evaluated_args.len() {
            return Err(format!(
                "Function `{}` expects {} arguments, got {}",
                name,
                func.params.len(),
                evaluated_args.len()
            ));
        }

        self.ensure_item_visible(&func_entry.module, func.visibility, &func.name, "function")?;
        self.ensure_interface_arguments(&func.params, &evaluated_args)?;
        self.module_stack.push(func_entry.module.clone());
        let result = (|| {
            self.push_scope();
            for (param, value) in func.params.iter().zip(evaluated_args.into_iter()) {
                self.insert_var(&param.name, value, param.mutability == Mutability::Mutable)?;
            }
            let result = match &func.body {
                FunctionBody::Block(block) => self.execute_block_contents(block)?,
                FunctionBody::Expr(expr) => match self.emit_expression(&expr.node)? {
                    EvalOutcome::Value(value) => BlockEval::Value(value),
                    EvalOutcome::Flow(flow) => BlockEval::Flow(flow),
                },
            };
            self.exit_scope()?;
            match result {
                BlockEval::Value(value) => {
                    if func.returns.len() <= 1 {
                        if func.returns.is_empty() {
                            Ok(Vec::new())
                        } else {
                            Ok(vec![value])
                        }
                    } else if let Value::Tuple(values) = value {
                        Ok(values)
                    } else {
                        Ok(vec![value])
                    }
                }
                BlockEval::Flow(FlowSignal::Return(values)) => Ok(values),
                BlockEval::Flow(flow @ FlowSignal::Break)
                | BlockEval::Flow(flow @ FlowSignal::Continue) => Err(format!(
                    "Control flow {} cannot escape function body in build mode",
                    flow_name(&flow)
                )),
                BlockEval::Flow(FlowSignal::Propagate(value)) => Ok(vec![value]),
            }
        })();
        self.module_stack.pop();
        result
    }

    fn register_function(&mut self, func: &FunctionDef, module: &str) -> Result<(), String> {
        let receiver = receiver_type_name(func, &self.structs);
        let key = FunctionKey {
            name: func.name.clone(),
            receiver: receiver.clone(),
            type_args: None,
        };
        if self.functions.contains_key(&key) {
            return Err(format!(
                "Duplicate function `{}` for receiver `{:?}`",
                func.name, receiver
            ));
        }
        self.functions.insert(
            key,
            FunctionEntry {
                module: module.to_string(),
                def: func.clone(),
            },
        );
        let qualified = format!("{}::{}", module, func.name);
        self.functions.insert(
            FunctionKey {
                name: qualified,
                receiver,
                type_args: None,
            },
            FunctionEntry {
                module: module.to_string(),
                def: func.clone(),
            },
        );
        Ok(())
    }

    fn register_impl_block(&mut self, module: &str, block: &ImplBlock) -> Result<(), String> {
        if !self.interfaces.contains_key(&block.interface) {
            return Err(format!("Unknown interface `{}`", block.interface));
        }
        if !self.structs.contains_key(&block.target) {
            return Err(format!("Unknown target type `{}`", block.target));
        }
        let iface = self
            .interfaces
            .get(&block.interface)
            .cloned()
            .map(|entry| entry.def)
            .unwrap();
        if iface.type_params.len() != block.type_args.len() {
            return Err(format!(
                "`{}` expects {} type arguments, got {}",
                block.interface,
                iface.type_params.len(),
                block.type_args.len()
            ));
        }
        let type_arg_names: Vec<String> = block
            .type_args
            .iter()
            .map(|ty| ty.canonical_name())
            .collect();
        let key = ImplKey {
            interface: block.interface.clone(),
            type_args: type_arg_names.clone(),
            target: block.target.clone(),
        };
        if self.impls.contains(&key) {
            return Err(format!(
                "`{}` already implemented for `{}`",
                block.interface, block.target
            ));
        }
        let mut provided = HashSet::new();
        for method in &block.methods {
            let mut method_def = method.clone();
            substitute_self_in_function(&mut method_def, &block.target);
            provided.insert(method_def.name.clone());
            if let Some(first) = method_def.params.first() {
                if let Some(name) = type_name_from_type_expr(&first.ty.ty) {
                    if name != block.target {
                        return Err(format!(
                            "First parameter of `{}` must be `{}`",
                            method_def.name, block.target
                        ));
                    }
                }
            }
            self.register_function(&method_def, module)?;
        }
        for sig in iface.methods {
            if !provided.contains(&sig.name) {
                return Err(format!(
                    "`{}` missing method `{}` required by interface `{}`",
                    block.target, sig.name, block.interface
                ));
            }
        }
        self.impls.insert(key);
        Ok(())
    }

    fn resolve_function(
        &mut self,
        name: &str,
        receiver_hint: Option<&str>,
        type_args: &[TypeExpr],
        args: &[Value],
    ) -> Result<FunctionEntry, String> {
        let type_arg_names = if type_args.is_empty() {
            None
        } else {
            Some(type_args.iter().map(|ty| ty.canonical_name()).collect())
        };
        let mut resolved_receiver = receiver_hint.map(|s| s.to_string());
        if resolved_receiver.is_none() {
            resolved_receiver = self.receiver_name_from_values(args);
        }
        if let Some(func) = self
            .functions
            .get(&FunctionKey {
                name: name.to_string(),
                receiver: resolved_receiver.clone(),
                type_args: type_arg_names.clone(),
            })
            .cloned()
        {
            return Ok(func);
        }
        if let Some(func) = self
            .functions
            .get(&FunctionKey {
                name: name.to_string(),
                receiver: None,
                type_args: type_arg_names.clone(),
            })
            .cloned()
        {
            return Ok(func);
        }
        let base = self
            .functions
            .get(&FunctionKey {
                name: name.to_string(),
                receiver: resolved_receiver.clone(),
                type_args: None,
            })
            .cloned()
            .or_else(|| {
                self.functions
                    .get(&FunctionKey {
                        name: name.to_string(),
                        receiver: None,
                        type_args: None,
                    })
                    .cloned()
            })
            .ok_or_else(|| format!("Unknown function {}", name))?;

        if base.def.type_params.is_empty() {
            if type_arg_names.is_some() {
                return Err(format!("`{}` is not generic", name));
            }
            return Ok(base);
        }

        if type_args.is_empty() {
            return Err(format!("`{}` requires type arguments", name));
        }
        if type_args.len() != base.def.type_params.len() {
            return Err(format!(
                "`{}` expects {} type arguments, got {}",
                name,
                base.def.type_params.len(),
                type_args.len()
            ));
        }

        let instantiated = self.instantiate_function(&base.def, type_args)?;
        let key = FunctionKey {
            name: name.to_string(),
            receiver: resolved_receiver,
            type_args: type_arg_names,
        };
        let entry = FunctionEntry {
            module: base.module.clone(),
            def: instantiated,
        };
        self.functions.insert(key.clone(), entry.clone());
        Ok(entry)
    }

    fn instantiate_function(
        &self,
        base: &FunctionDef,
        type_args: &[TypeExpr],
    ) -> Result<FunctionDef, String> {
        if type_args.len() != base.type_params.len() {
            return Err(format!(
                "`{}` expects {} type arguments, got {}",
                base.name,
                base.type_params.len(),
                type_args.len()
            ));
        }
        let mut map = HashMap::new();
        for (param, ty) in base.type_params.iter().zip(type_args.iter()) {
            map.insert(param.clone(), ty.clone());
        }
        let mut new_def = base.clone();
        new_def.type_params.clear();
        for param in &mut new_def.params {
            param.ty = param.ty.substitute(&map);
        }
        for ret in &mut new_def.returns {
            *ret = ret.substitute(&map);
        }
        Ok(new_def)
    }

    fn ensure_interface_arguments(
        &self,
        params: &[FunctionParam],
        args: &[Value],
    ) -> Result<(), String> {
        for (param, value) in params.iter().zip(args.iter()) {
            if let Some((interface, type_args)) = self.interface_name_from_type(&param.ty.ty) {
                self.ensure_interface_compat(&interface, &type_args, value)?;
            }
        }
        Ok(())
    }

    fn interface_name_from_type(&self, ty: &TypeExpr) -> Option<(String, Vec<TypeExpr>)> {
        match ty {
            TypeExpr::Named(name, args) => self
                .interfaces
                .contains_key(name)
                .then(|| (name.clone(), args.clone())),
            TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
                self.interface_name_from_type(ty)
            }
            _ => None,
        }
    }

    fn ensure_interface_compat(
        &self,
        interface: &str,
        type_args: &[TypeExpr],
        value: &Value,
    ) -> Result<(), String> {
        let entry = self
            .interfaces
            .get(interface)
            .ok_or_else(|| format!("Unknown interface {}", interface))?;
        self.ensure_item_visible(&entry.module, entry.def.visibility, interface, "interface")?;
        let struct_name = self.value_struct_name(value).ok_or_else(|| {
            format!(
                "Interface `{}` expects struct implementing it, found incompatible value",
                interface
            )
        })?;
        let key = ImplKey {
            interface: interface.to_string(),
            type_args: type_args.iter().map(|ty| ty.canonical_name()).collect(),
            target: struct_name.clone(),
        };
        if self.impls.contains(&key) {
            Ok(())
        } else {
            Err(format!(
                "`{}` does not implement interface `{}` with these type arguments",
                struct_name, interface
            ))
        }
    }

    fn current_module(&self) -> Option<&str> {
        self.module_stack.last().map(|s| s.as_str())
    }

    fn can_access(&self, owner: &str, visibility: Visibility) -> bool {
        matches!(visibility, Visibility::Public)
            || self
                .current_module()
                .map_or(true, |current| current == owner)
    }

    fn ensure_item_visible(
        &self,
        owner: &str,
        visibility: Visibility,
        name: &str,
        kind: &str,
    ) -> Result<(), String> {
        if self.can_access(owner, visibility) {
            Ok(())
        } else {
            Err(format!(
                "{kind} `{}` is private to module `{}`",
                name, owner
            ))
        }
    }

    fn receiver_name_from_values(&self, args: &[Value]) -> Option<String> {
        args.first().and_then(|value| self.value_struct_name(value))
    }

    fn value_struct_name(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow();
                self.value_struct_name(&inner)
            }
            Value::Boxed(inner) => {
                let value = inner.cell.borrow();
                self.value_struct_name(&value)
            }
            _ => None,
        }
    }
    fn begin_mut_borrow_in_scope(&mut self, name: &str, scope_index: usize) -> Result<(), String> {
        if self.active_mut_borrows.contains(name) {
            return Err(format!("`{}` is already mutably borrowed", name));
        }
        self.active_mut_borrows.insert(name.to_string());
        if let Some(frame) = self.borrow_frames.get_mut(scope_index) {
            frame.push(name.to_string());
        }
        Ok(())
    }

    fn end_mut_borrow(&mut self, name: &str) {
        self.active_mut_borrows.remove(name);
    }

    fn is_mut_borrowed(&self, name: &str) -> bool {
        self.active_mut_borrows.contains(name)
    }

    fn register_move(&mut self, name: &str) {
        self.end_mut_borrow(name);
    }

    fn track_reference_borrow_in_scope(
        &mut self,
        value: &Value,
        scope_index: usize,
    ) -> Result<(), String> {
        if let Value::Reference(reference) = value {
            if reference.mutable {
                if let Some(origin) = &reference.origin {
                    self.begin_mut_borrow_in_scope(origin, scope_index)?;
                }
            }
        }
        Ok(())
    }

    fn release_reference_borrow(&mut self, value: &Value) {
        if let Value::Reference(reference) = value {
            if reference.mutable {
                if let Some(origin) = &reference.origin {
                    self.end_mut_borrow(origin);
                }
            }
        }
    }

    fn warn_deprecated(&mut self, name: &str) {
        if self.deprecated_warnings.insert(name.to_string()) {
            eprintln!(
                "warning: `{}` is deprecated; prefer slice/map literals or direct methods",
                name
            );
        }
    }

    fn expect_box_value(&self, value: Value, ctx: &str) -> Result<BoxValue, String> {
        match value {
            Value::Boxed(b) => Ok(b),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.expect_box_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects Box value")),
        }
    }

    fn expect_slice_value(&self, value: Value, ctx: &str) -> Result<SliceValue, String> {
        match value {
            Value::Slice(slice) => Ok(slice),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.expect_slice_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects slice value")),
        }
    }

    fn expect_map_value(&self, value: Value, ctx: &str) -> Result<MapValue, String> {
        match value {
            Value::Map(map) => Ok(map),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.expect_map_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects map value")),
        }
    }

    fn expect_string_value(&self, value: Value, ctx: &str) -> Result<String, String> {
        match value {
            Value::Str(s) => Ok((*s.text).clone()),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.expect_string_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects string value")),
        }
    }

    fn make_string_value(&self, text: &str) -> Result<Value, String> {
        let c_value = CString::new(text.as_bytes())
            .map_err(|_| "string value cannot contain interior null bytes".to_string())?;
        let name = CString::new("map_key").unwrap();
        let text_rc = Rc::new(text.to_string());
        unsafe {
            let ptr = LLVMBuildGlobalString(self.builder, c_value.as_ptr(), name.as_ptr());
            Ok(Value::Str(StringValue::new(ptr, text_rc)))
        }
    }

    fn builtin_box_new(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_new expects 1 argument".into());
        }
        let value = args.pop().unwrap();
        Ok(Value::Boxed(BoxValue::new(value)))
    }

    fn builtin_box_get(&self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_get expects 1 argument".into());
        }
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_get")?;
        Ok(boxed.cell.borrow().clone())
    }

    fn builtin_box_set(&self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("box_set expects 2 arguments".into());
        }
        let value = args.pop().unwrap();
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_set")?;
        boxed.replace(value);
        Ok(Value::Unit)
    }

    fn builtin_box_take(&self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_take expects 1 argument".into());
        }
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_take")?;
        Ok(boxed.replace(Value::Unit))
    }

    fn builtin_slice_new(&mut self, args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_new");
        if !args.is_empty() {
            return Err("slice_new expects 0 arguments".into());
        }
        Ok(Value::Slice(SliceValue::new()))
    }

    fn builtin_slice_push(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_push");
        if args.len() != 2 {
            return Err("slice_push expects 2 arguments".into());
        }
        let value = args.pop().unwrap();
        let slice = self.expect_slice_value(args.pop().unwrap(), "slice_push")?;
        slice.push(value);
        Ok(Value::Unit)
    }

    fn builtin_slice_len(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_len");
        if args.len() != 1 {
            return Err("slice_len expects 1 argument".into());
        }
        let slice = self.expect_slice_value(args.pop().unwrap(), "slice_len")?;
        let len = slice.len() as i128;
        Ok(Value::Int(self.const_int_value(len)))
    }

    fn builtin_slice_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_get");
        if args.len() != 2 {
            return Err("slice_get expects 2 arguments".into());
        }
        let index_value = args.pop().unwrap();
        let slice = self.expect_slice_value(args.pop().unwrap(), "slice_get")?;
        let int_value = self.expect_int(index_value)?;
        let idx = int_value
            .constant()
            .ok_or_else(|| "slice_get index must be constant in build mode".to_string())?;
        if idx < 0 {
            return self.instantiate_enum_variant("None", Vec::new());
        }
        let value = slice.get(idx as usize);
        if value.is_none() {
            return self.instantiate_enum_variant("None", Vec::new());
        }
        let value = value.unwrap();
        self.instantiate_enum_variant("Some", vec![value])
    }

    fn builtin_map_new(&mut self, args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_new");
        if !args.is_empty() {
            return Err("map_new expects 0 arguments".into());
        }
        Ok(Value::Map(MapValue::new()))
    }

    fn builtin_map_insert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_insert");
        if args.len() != 3 {
            return Err("map_insert expects 3 arguments".into());
        }
        let value = args.pop().unwrap();
        let key = self.expect_string_value(args.pop().unwrap(), "map_insert")?;
        let map = self.expect_map_value(args.pop().unwrap(), "map_insert")?;
        map.insert(key, value);
        Ok(Value::Unit)
    }

    fn builtin_map_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_get");
        if args.len() != 2 {
            return Err("map_get expects 2 arguments".into());
        }
        let key = self.expect_string_value(args.pop().unwrap(), "map_get")?;
        let map = self.expect_map_value(args.pop().unwrap(), "map_get")?;
        match map.get(&key) {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_len(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("len expects 1 argument".into());
        }
        let receiver = args.pop().unwrap();
        match receiver {
            Value::Slice(slice) => Ok(Value::Int(self.const_int_value(slice.len() as i128))),
            Value::Map(map) => Ok(Value::Int(self.const_int_value(map.len() as i128))),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.builtin_len(vec![inner])
            }
            _ => Err("len expects slice or map".into()),
        }
    }

    fn builtin_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("get expects receiver and argument".into());
        }
        let receiver = args.remove(0);
        match receiver {
            Value::Slice(slice) => {
                let int_value = self.expect_int(args.remove(0))?;
                let idx = int_value
                    .constant()
                    .ok_or_else(|| "get index must be constant in build mode".to_string())?;
                if idx < 0 {
                    return self.instantiate_enum_variant("None", Vec::new());
                }
                let value = slice.get(idx as usize);
                match value {
                    Some(value) => self.instantiate_enum_variant("Some", vec![value]),
                    None => self.instantiate_enum_variant("None", Vec::new()),
                }
            }
            Value::Map(map) => {
                let key = self.expect_string_value(args.remove(0), "get")?;
                match map.get(&key) {
                    Some(value) => self.instantiate_enum_variant("Some", vec![value]),
                    None => self.instantiate_enum_variant("None", Vec::new()),
                }
            }
            Value::Reference(reference) => {
                let mut forwarded = Vec::new();
                forwarded.push(reference.cell.borrow().clone());
                forwarded.extend(args);
                self.builtin_get(forwarded)
            }
            _ => Err("get expects slice or map".into()),
        }
    }

    fn builtin_push(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("push expects receiver and value".into());
        }
        let value = args.pop().unwrap();
        let slice = self.expect_slice_value(args.pop().unwrap(), "push")?;
        slice.push(value);
        Ok(Value::Unit)
    }

    fn builtin_insert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 3 {
            return Err("insert expects receiver, key, and value".into());
        }
        let value = args.pop().unwrap();
        let key = self.expect_string_value(args.pop().unwrap(), "insert")?;
        let map = self.expect_map_value(args.pop().unwrap(), "insert")?;
        map.insert(key, value);
        Ok(Value::Unit)
    }

    fn execute_block_contents(&mut self, block: &Block) -> Result<BlockEval, String> {
        for statement in &block.statements {
            if let Some(flow) = self.emit_statement(statement)? {
                return Ok(BlockEval::Flow(flow));
            }
        }
        if let Some(tail) = &block.tail {
            match self.emit_expression(tail)? {
                EvalOutcome::Value(value) => Ok(BlockEval::Value(value)),
                EvalOutcome::Flow(flow) => Ok(BlockEval::Flow(flow)),
            }
        } else {
            Ok(BlockEval::Value(Value::Unit))
        }
    }

    fn assign_var(&mut self, name: &str, value: Value) -> Result<(), String> {
        for index in (0..self.scopes.len()).rev() {
            if let Some(binding) = self.scopes[index].get(name) {
                if !binding.mutable {
                    return Err(format!("Variable {} is immutable", name));
                }
                let cell = binding.cell.clone();
                self.track_reference_borrow_in_scope(&value, index)?;
                {
                    let current = cell.borrow();
                    self.release_reference_borrow(&current);
                }
                *cell.borrow_mut() = value;
                return Ok(());
            }
        }
        Err(format!("Unknown variable {}", name))
    }

    fn value_to_bool(&self, value: Value) -> Result<bool, String> {
        let concrete = match value {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other,
        };
        match concrete {
            Value::Bool(flag) => Ok(flag),
            other => {
                let int_value = self.expect_int(other)?;
                let constant = int_value.constant().ok_or_else(|| {
                    "Non-constant condition not supported in build mode".to_string()
                })?;
                Ok(constant != 0)
            }
        }
    }

    fn evaluate_range(
        &mut self,
        range: &RangeExpr,
    ) -> Result<EvalOutcome<(i128, i128, bool)>, String> {
        let start_expr = match self.emit_expression(&range.start)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let start_value = self.expect_int(start_expr)?;
        let end_expr = match self.emit_expression(&range.end)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let end_value = self.expect_int(end_expr)?;
        let start_const = start_value
            .constant()
            .ok_or_else(|| "Range bounds must be constant in build mode".to_string())?;
        let end_const = end_value
            .constant()
            .ok_or_else(|| "Range bounds must be constant in build mode".to_string())?;
        Ok(EvalOutcome::Value((
            start_const,
            end_const,
            range.inclusive,
        )))
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.borrow_frames.push(Vec::new());
        self.defer_stack.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        if let Some(frame) = self.borrow_frames.pop() {
            for name in frame {
                self.active_mut_borrows.remove(&name);
            }
        }
        self.defer_stack.pop();
        if self.borrow_frames.is_empty() {
            self.borrow_frames.push(Vec::new());
        }
        if self.defer_stack.is_empty() {
            self.defer_stack.push(Vec::new());
        }
    }

    fn run_deferred(&mut self) -> Result<(), String> {
        if let Some(stack) = self.defer_stack.last_mut() {
            let mut pending = Vec::new();
            mem::swap(stack, &mut pending);
            while let Some(expr) = pending.pop() {
                match self.emit_expression(&expr)? {
                    EvalOutcome::Value(_) => {}
                    EvalOutcome::Flow(flow) => {
                        return Err(format!(
                            "Control flow {} not allowed in deferred expression",
                            flow_name(&flow)
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn exit_scope(&mut self) -> Result<(), String> {
        self.run_deferred()?;
        self.pop_scope();
        Ok(())
    }

    fn insert_var(&mut self, name: &str, value: Value, mutable: bool) -> Result<(), String> {
        let scope_index = self.scopes.len().saturating_sub(1);
        let cell = Rc::new(RefCell::new(value));
        {
            let stored = cell.borrow();
            self.track_reference_borrow_in_scope(&stored, scope_index)?;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), Binding { cell, mutable });
        }
        Ok(())
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some(binding.cell.borrow().clone());
            }
        }
        None
    }

    fn get_binding(&self, name: &str) -> Option<(Rc<RefCell<Value>>, bool)> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some((binding.cell.clone(), binding.mutable));
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

            let main_type = LLVMFunctionType(self.i32_type, ptr::null_mut(), 0, 0);
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

    fn collect_iterable_values(&self, value: Value) -> Result<Vec<Value>, String> {
        match value {
            Value::Slice(slice) => {
                let mut items = Vec::new();
                for idx in 0..slice.len() {
                    if let Some(item) = slice.get(idx) {
                        items.push(item);
                    }
                }
                Ok(items)
            }
            Value::Map(map) => {
                let mut items = Vec::new();
                for (key, value) in map.entries.borrow().iter() {
                    let key_value = self.make_string_value(key)?;
                    items.push(Value::Tuple(vec![key_value, value.clone()]));
                }
                Ok(items)
            }
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.collect_iterable_values(inner)
            }
            other => Err(format!(
                "`for ... in` only supports slices or maps (found {})",
                self.describe_value(&other)
            )),
        }
    }

    fn describe_value(&self, value: &Value) -> &'static str {
        match value {
            Value::Slice(_) => "slice",
            Value::Map(_) => "map",
            Value::Boxed(_) => "box",
            Value::Reference(_) => "reference",
            Value::Struct(_) => "struct",
            Value::Enum(_) => "enum",
            Value::Tuple(_) => "tuple",
            Value::Str(_) => "string",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Unit => "unit",
            Value::Moved => "moved value",
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

fn flow_name(flow: &FlowSignal) -> &'static str {
    match flow {
        FlowSignal::Break => "break",
        FlowSignal::Continue => "continue",
        FlowSignal::Return(_) => "return",
        FlowSignal::Propagate(_) => "error propagation",
    }
}

fn type_name_from_type_expr(expr: &TypeExpr) -> Option<String> {
    match expr {
        TypeExpr::Named(name, _) => Some(name.clone()),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
            type_name_from_type_expr(ty)
        }
        _ => None,
    }
}

fn receiver_type_name(def: &FunctionDef, structs: &HashMap<String, StructEntry>) -> Option<String> {
    def.params
        .first()
        .and_then(|param| type_name_from_type_expr(&param.ty.ty))
        .filter(|name| structs.contains_key(name))
}

fn substitute_self_in_function(def: &mut FunctionDef, target: &str) {
    let concrete = TypeExpr::Named(target.to_string(), Vec::new());
    for param in &mut def.params {
        param.ty = param.ty.replace_self(&concrete);
    }
    for ret in &mut def.returns {
        *ret = ret.replace_self(&concrete);
    }
}

fn describe_expr(expr: &Expr) -> &'static str {
    match expr {
        Expr::Identifier(_) => "identifier",
        Expr::Literal(_) => "literal",
        Expr::Try { .. } => "try expression",
        Expr::TryPropagate { .. } => "error propagation",
        Expr::Binary { .. } => "binary",
        Expr::Unary { .. } => "unary",
        Expr::Call { .. } => "call",
        Expr::FieldAccess { .. } => "field access",
        Expr::StructLiteral { .. } => "struct literal",
        Expr::MapLiteral { .. } => "map literal",
        Expr::Block(_) => "block",
        Expr::If(_) => "if expression",
        Expr::Match(_) => "match expression",
        Expr::Tuple(_, _) => "tuple",
        Expr::ArrayLiteral(_, _) => "array literal",
        Expr::Move { .. } => "move expression",
        Expr::Range(_) => "range",
        Expr::Reference { .. } => "reference",
        Expr::Deref { .. } => "deref",
    }
}

fn describe_value(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::Bool(_) => "bool",
        Value::Str(_) => "string",
        Value::Struct(_) => "struct",
        Value::Enum(_) => "enum",
        Value::Tuple(_) => "tuple",
        Value::Unit => "unit",
        Value::Reference(_) => "reference",
        Value::Boxed(_) => "box",
        Value::Slice(_) => "slice",
        Value::Map(_) => "map",
        Value::Moved => "moved",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::{ast::Program, parser::parse_module};
    use std::path::PathBuf;

    fn compile_source(source: &str) -> Result<(), String> {
        let module =
            parse_module("tests::build", PathBuf::from("test.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        let mut compiler = Compiler::new();
        compiler.compile_program(&program)
    }

    #[test]
    fn compiler_releases_borrows_across_control_flow() {
        let source = r#"
module tests::build;

fn release_after_if() {
  let mut int32 value = 0;
  if true {
    let &mut int32 alias = &mut value;
    *alias = 1;
  } else {
    let &mut int32 alias = &mut value;
    *alias = 2;
  }
  let &mut int32 after_if = &mut value;
  *after_if = 3;
}

fn release_after_match() {
  let mut int32 value = 0;
  match true {
    true => {
      let &mut int32 alias = &mut value;
      *alias = 4;
    },
    false => {
      let &mut int32 alias = &mut value;
      *alias = 5;
    },
  }
  let &mut int32 after_match = &mut value;
  *after_match = 6;
}

fn release_after_while() {
  let mut int32 value = 0;
  let mut int32 idx = 0;
  while idx < 1 {
    let &mut int32 alias = &mut value;
    *alias = idx;
    idx = idx + 1;
  }
  let &mut int32 after = &mut value;
  *after = 7;
}

fn release_after_while_let() {
  let mut int32 value = 0;
  let mut int32 idx = 0;
  while let true = idx == 0 {
    let &mut int32 alias = &mut value;
    *alias = idx;
    idx = idx + 1;
  }
  let &mut int32 after = &mut value;
  *after = 8;
}

fn release_after_for_range() {
  let mut int32 value = 0;
  for count in 0..1 {
    let &mut int32 alias = &mut value;
    *alias = count;
  }
  let &mut int32 after = &mut value;
  *after = 9;
}

fn release_after_for_collection() {
  let []int32 items = [1, 2];
  let mut int32 value = 0;
  for entry in items {
    let &mut int32 alias = &mut value;
    *alias = entry;
  }
  let &mut int32 after = &mut value;
  *after = 10;
}

fn release_in_nested_block() {
  let mut int32 value = 0;
  {
    let &mut int32 alias = &mut value;
    *alias = 11;
  }
  let &mut int32 after = &mut value;
  *after = 12;
}

fn release_after_early_return(flag: bool) -> int32 {
  let mut int32 value = 0;
  if flag {
    let &mut int32 alias = &mut value;
    *alias = 13;
    return value;
  }
  let &mut int32 final_ref = &mut value;
  *final_ref = 14;
  value
}

fn main() {
  release_after_if();
  release_after_match();
  release_after_while();
  release_after_while_let();
  release_after_for_range();
  release_after_for_collection();
  release_in_nested_block();
  let _ = release_after_early_return(true);
  let _ = release_after_early_return(false);
}
"#;
        compile_source(source).expect("borrow-aware control flow should compile");
    }

    #[test]
    fn compiler_reports_live_alias() {
        let source = r#"
module tests::build;

fn main() {
  let mut int32 value = 0;
  let &mut int32 alias = &mut value;
  let &mut int32 second = &mut value;
  *second = 1;
}
"#;
        let err = compile_source(source).expect_err("expected borrow error");
        assert!(
            err.contains("already mutably borrowed"),
            "unexpected error message: {err}"
        );
    }

    #[test]
    fn compiler_supports_mutable_destructuring() {
        let source = r#"
module tests::build_patterns;

struct Telemetry {
  hp: int32;
  mp: int32;
  notes: []string;
}

fn main() {
  let mut (left, right) = (10, 5);
  left = left + right;

  let mut #{ "hp": hp_score, "mp": mp_score } = #{
    "hp": 80,
    "mp": 40,
  };
  hp_score = hp_score + mp_score;

  let mut Telemetry{ hp, mp, .. } = Telemetry{
    hp: 70,
    mp: 35,
    notes: ["alpha"],
  };
  hp = hp + mp;

  let mut [first, ..rest] = ["steady", "ready"];
  first = "launch";
  rest = rest;
}
"#;
        compile_source(source).expect("mutable destructuring should compile");
    }
}
