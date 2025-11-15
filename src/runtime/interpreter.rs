use crate::language::{
    ast::*,
    types::{Mutability, TypeAnnotation},
};
use crate::project::Package;
use crate::runtime::{
    environment::Environment,
    error::{RuntimeError, RuntimeResult},
    value::{EnumValue, RangeValue, ReferenceValue, StructInstance, Value},
};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

pub struct Interpreter {
    package: Package,
    env: Environment,
    structs: HashMap<String, StructDef>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    functions: HashMap<String, FunctionInfo>,
    consts: Vec<ConstDef>,
}

#[derive(Clone)]
struct FunctionInfo {
    module: String,
    def: FunctionDef,
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    fields: Vec<TypeAnnotation>,
}

enum FlowSignal {
    Break,
    Continue,
    Return(Vec<Value>),
}

enum BlockEval {
    Value(Value),
    Flow(FlowSignal),
}

impl Interpreter {
    pub fn new(package: Package) -> Self {
        Self {
            package,
            env: Environment::new(),
            structs: HashMap::new(),
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            consts: Vec::new(),
        }
    }

    pub fn run(&mut self) -> RuntimeResult<()> {
        self.bootstrap()?;
        let _ = self.call_function("main", Vec::new())?;
        Ok(())
    }

    fn bootstrap(&mut self) -> RuntimeResult<()> {
        for module in self.package.program.modules.clone() {
            for item in &module.items {
                match item {
                    Item::Struct(def) => {
                        if self.structs.contains_key(&def.name) {
                            return Err(RuntimeError::Panic {
                                message: format!("Duplicate struct `{}`", def.name),
                            });
                        }
                        self.structs.insert(def.name.clone(), def.clone());
                    }
                    Item::Enum(def) => {
                        if self
                            .enum_variants
                            .values()
                            .any(|info| info.enum_name == def.name)
                        {
                            return Err(RuntimeError::Panic {
                                message: format!("Duplicate enum `{}`", def.name),
                            });
                        }
                        for variant in &def.variants {
                            self.enum_variants.insert(
                                variant.name.clone(),
                                EnumVariantInfo {
                                    enum_name: def.name.clone(),
                                    fields: variant.fields.clone(),
                                },
                            );
                        }
                    }
                    Item::Function(def) => {
                        self.register_function(&module.name, def.clone())?;
                    }
                    Item::Const(const_def) => {
                        self.consts.push(const_def.clone());
                    }
                }
            }
        }

        for const_def in self.consts.clone() {
            let value = self.eval_expression(&const_def.value)?;
            self.env
                .declare(&const_def.name, value, false)
                .map_err(|err| err)?;
        }
        Ok(())
    }

    fn register_function(&mut self, module: &str, def: FunctionDef) -> RuntimeResult<()> {
        if self.functions.contains_key(&def.name) {
            return Err(RuntimeError::Panic {
                message: format!("Duplicate function `{}`", def.name),
            });
        }
        self.functions.insert(
            def.name.clone(),
            FunctionInfo {
                module: module.to_string(),
                def: def.clone(),
            },
        );
        let qualified = format!("{}::{}", module, def.name);
        self.functions.insert(
            qualified,
            FunctionInfo {
                module: module.to_string(),
                def,
            },
        );
        Ok(())
    }

    fn call_function(&mut self, name: &str, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        if name == "out" {
            return self.call_out(args);
        }
        let info =
            self.functions
                .get(name)
                .cloned()
                .ok_or_else(|| RuntimeError::UnknownSymbol {
                    name: name.to_string(),
                })?;
        if info.def.params.len() != args.len() {
            return Err(RuntimeError::ArityMismatch {
                name: name.to_string(),
                expected: info.def.params.len(),
                received: args.len(),
            });
        }

        self.env.push_scope();
        for (param, value) in info.def.params.iter().zip(args.into_iter()) {
            self.env
                .declare(&param.name, value, param.mutability == Mutability::Mutable)?;
        }

        let result = match &info.def.body {
            FunctionBody::Block(block) => self.eval_block(block)?,
            FunctionBody::Expr(expr) => BlockEval::Value(self.eval_expression(&expr.node)?),
        };

        let values = match result {
            BlockEval::Value(value) => {
                if info.def.returns.len() <= 1 {
                    if info.def.returns.is_empty() {
                        Vec::new()
                    } else {
                        vec![value]
                    }
                } else if let Value::Tuple(values) = value {
                    values
                } else {
                    vec![value]
                }
            }
            BlockEval::Flow(FlowSignal::Return(values)) => values,
            BlockEval::Flow(FlowSignal::Break) => {
                return Err(RuntimeError::Panic {
                    message: "break outside loop".into(),
                });
            }
            BlockEval::Flow(FlowSignal::Continue) => {
                return Err(RuntimeError::Panic {
                    message: "continue outside loop".into(),
                });
            }
        };

        self.execute_deferred()?;
        self.env.pop_scope();
        Ok(values)
    }

    fn call_out(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        if args.len() != 1 {
            return Err(RuntimeError::ArityMismatch {
                name: "out".into(),
                expected: 1,
                received: args.len(),
            });
        }
        println!("{}", args[0]);
        Ok(Vec::new())
    }

    fn eval_statement(&mut self, statement: &Statement) -> RuntimeResult<Option<FlowSignal>> {
        match statement {
            Statement::Let(stmt) => {
                let value = match &stmt.value {
                    Some(expr) => self.eval_expression(expr)?,
                    None => Value::Unit,
                };
                self.env
                    .declare(&stmt.name, value, stmt.mutability == Mutability::Mutable)?;
                Ok(None)
            }
            Statement::Assign(stmt) => {
                match &stmt.target {
                    Expr::Identifier(ident) => {
                        let value = self.eval_expression(&stmt.value)?;
                        self.env.assign(&ident.name, value)?;
                    }
                    Expr::Deref { expr, .. } => {
                        let target = self.eval_expression(expr)?;
                        match target {
                            Value::Reference(reference) => {
                                if !reference.mutable {
                                    return Err(RuntimeError::Panic {
                                        message: "Cannot assign through immutable reference".into(),
                                    });
                                }
                                let value = self.eval_expression(&stmt.value)?;
                                *reference.cell.borrow_mut() = value;
                            }
                            _ => {
                                return Err(RuntimeError::TypeMismatch {
                                    message: "Cannot assign through non-reference value".into(),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(RuntimeError::Unsupported {
                            message: "Only identifier or dereference assignments are supported"
                                .into(),
                        });
                    }
                }
                Ok(None)
            }
            Statement::Expr(stmt) => {
                self.eval_expression(&stmt.expr)?;
                Ok(None)
            }
            Statement::Return(stmt) => {
                let mut values = Vec::new();
                for expr in &stmt.values {
                    values.push(self.eval_expression(expr)?);
                }
                Ok(Some(FlowSignal::Return(values)))
            }
            Statement::While(stmt) => {
                loop {
                    let condition = self.eval_expression(&stmt.condition)?;
                    if !condition.as_bool() {
                        break;
                    }
                    let result = self.eval_block(&stmt.body)?;
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                    }
                }
                Ok(None)
            }
            Statement::ForRange(stmt) => {
                let range = self.eval_range_expr(&stmt.range)?;
                let end = if range.inclusive {
                    range.end + 1
                } else {
                    range.end
                };
                for i in range.start..end {
                    self.env.push_scope();
                    self.env.declare(&stmt.binding, Value::Int(i), false)?;
                    let result = self.eval_block(&stmt.body)?;
                    self.execute_deferred()?;
                    self.env.pop_scope();
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                    }
                }
                Ok(None)
            }
            Statement::Break(_) => Ok(Some(FlowSignal::Break)),
            Statement::Continue(_) => Ok(Some(FlowSignal::Continue)),
            Statement::Defer(stmt) => {
                self.env.defer(stmt.expr.clone());
                Ok(None)
            }
            Statement::Block(block) => match self.eval_block(block)? {
                BlockEval::Value(_) => Ok(None),
                BlockEval::Flow(flow) => Ok(Some(flow)),
            },
            Statement::Match(_) => Err(RuntimeError::Unsupported {
                message: "Match statements are not supported; use match expressions".into(),
            }),
            Statement::If(stmt) => {
                let condition = self.eval_expression(&stmt.condition)?;
                if condition.as_bool() {
                    let result = self.eval_block(&stmt.then_branch)?;
                    match result {
                        BlockEval::Value(_) => Ok(None),
                        BlockEval::Flow(flow) => Ok(Some(flow)),
                    }
                } else if let Some(else_block) = &stmt.else_branch {
                    let result = self.eval_block(else_block)?;
                    match result {
                        BlockEval::Value(_) => Ok(None),
                        BlockEval::Flow(flow) => Ok(Some(flow)),
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn eval_block(&mut self, block: &Block) -> RuntimeResult<BlockEval> {
        self.env.push_scope();
        for statement in &block.statements {
            if let Some(flow) = self.eval_statement(statement)? {
                self.execute_deferred()?;
                self.env.pop_scope();
                return Ok(BlockEval::Flow(flow));
            }
        }
        let value = if let Some(tail) = &block.tail {
            self.eval_expression(tail)?
        } else {
            Value::Unit
        };
        self.execute_deferred()?;
        self.env.pop_scope();
        Ok(BlockEval::Value(value))
    }

    fn execute_deferred(&mut self) -> RuntimeResult<()> {
        let mut deferred = self.env.drain_deferred();
        while let Some(expr) = deferred.pop() {
            self.eval_expression(&expr)?;
        }
        Ok(())
    }

    fn eval_arguments(&mut self, args: &[Expr]) -> RuntimeResult<Vec<Value>> {
        args.iter().map(|expr| self.eval_expression(expr)).collect()
    }

    fn eval_expression(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        match expr {
            Expr::Identifier(ident) => {
                self.env
                    .get(&ident.name)
                    .ok_or_else(|| RuntimeError::UnknownSymbol {
                        name: ident.name.clone(),
                    })
            }
            Expr::Literal(lit) => Ok(match lit {
                Literal::Int(value, _) => Value::Int(*value),
                Literal::Float(value, _) => Value::Float(*value),
                Literal::Bool(value, _) => Value::Bool(*value),
                Literal::String(value, _) => Value::String(value.clone()),
                Literal::Rune(value, _) => Value::Int(*value as i128),
            }),
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = self.eval_expression(left)?;
                let rhs = self.eval_expression(right)?;
                self.eval_binary(*op, lhs, rhs)
            }
            Expr::Unary { op, expr, .. } => {
                let value = self.eval_expression(expr)?;
                match op {
                    UnaryOp::Neg => match value {
                        Value::Int(v) => Ok(Value::Int(-v)),
                        Value::Float(v) => Ok(Value::Float(-v)),
                        _ => Err(RuntimeError::TypeMismatch {
                            message: "Unary - expects numeric value".into(),
                        }),
                    },
                    UnaryOp::Not => Ok(Value::Bool(!value.as_bool())),
                    UnaryOp::Addr => Ok(value),
                    UnaryOp::Deref => Ok(value),
                }
            }
            Expr::Call { callee, args, .. } => match callee.as_ref() {
                Expr::Identifier(ident) => {
                    let arg_values = self.eval_arguments(args)?;
                    if let Some(variant) = self.enum_variants.get(&ident.name) {
                        let enum_name = variant.enum_name.clone();
                        let variant_name = ident.name.clone();
                        return self.instantiate_enum(&enum_name, &variant_name, arg_values);
                    }
                    let results = self.call_function(&ident.name, arg_values)?;
                    Ok(match results.len() {
                        0 => Value::Unit,
                        1 => results.into_iter().next().unwrap(),
                        _ => Value::Tuple(results),
                    })
                }
                Expr::FieldAccess { base, field, .. } => {
                    if let Expr::Identifier(module_ident) = base.as_ref() {
                        let qualified = format!("{}::{}", module_ident.name, field);
                        if self.functions.contains_key(&qualified) {
                            let arg_values = self.eval_arguments(args)?;
                            let results = self.call_function(&qualified, arg_values)?;
                            return Ok(match results.len() {
                                0 => Value::Unit,
                                1 => results.into_iter().next().unwrap(),
                                _ => Value::Tuple(results),
                            });
                        }
                    }
                    let receiver = self.eval_expression(base)?;
                    let mut method_args = Vec::with_capacity(args.len() + 1);
                    method_args.push(receiver);
                    for expr in args {
                        method_args.push(self.eval_expression(expr)?);
                    }
                    let results = self.call_function(field, method_args)?;
                    Ok(match results.len() {
                        0 => Value::Unit,
                        1 => results.into_iter().next().unwrap(),
                        _ => Value::Tuple(results),
                    })
                }
                _ => Err(RuntimeError::Unsupported {
                    message: "Unsupported call target".into(),
                }),
            },
            Expr::FieldAccess { base, field, .. } => {
                let value = self.eval_expression(base)?;
                match value {
                    Value::Struct(instance) => {
                        instance
                            .get_field(field)
                            .ok_or_else(|| RuntimeError::UnknownSymbol {
                                name: field.clone(),
                            })
                    }
                    _ => Err(RuntimeError::TypeMismatch {
                        message: "Field access requires struct value".into(),
                    }),
                }
            }
            Expr::StructLiteral { name, fields, .. } => self.instantiate_struct(name, fields),
            Expr::Match(expr) => self.eval_match(expr),
            Expr::Block(block) => match self.eval_block(block)? {
                BlockEval::Value(value) => Ok(value),
                BlockEval::Flow(flow) => Err(RuntimeError::Panic {
                    message: format!(
                        "Control flow {} not allowed in expression",
                        flow_name(&flow)
                    ),
                }),
            },
            Expr::If(if_expr) => {
                let condition = self.eval_expression(&if_expr.condition)?;
                if condition.as_bool() {
                    match self.eval_block(&if_expr.then_branch)? {
                        BlockEval::Value(value) => Ok(value),
                        BlockEval::Flow(flow) => Err(RuntimeError::Panic {
                            message: format!(
                                "Control flow {} not allowed in expression",
                                flow_name(&flow)
                            ),
                        }),
                    }
                } else if let Some(else_block) = &if_expr.else_branch {
                    match self.eval_block(else_block)? {
                        BlockEval::Value(value) => Ok(value),
                        BlockEval::Flow(flow) => Err(RuntimeError::Panic {
                            message: format!(
                                "Control flow {} not allowed in expression",
                                flow_name(&flow)
                            ),
                        }),
                    }
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::Tuple(values, _) => {
                let evaluated = values
                    .iter()
                    .map(|expr| self.eval_expression(expr))
                    .collect::<RuntimeResult<Vec<_>>>()?;
                Ok(Value::Tuple(evaluated))
            }
            Expr::Range(range) => {
                let range_value = self.eval_range_expr(range)?;
                Ok(Value::Range(range_value))
            }
            Expr::Reference { mutable, expr, .. } => self.build_reference(expr, *mutable),
            Expr::Deref { expr, .. } => {
                let value = self.eval_expression(expr)?;
                match value {
                    Value::Reference(reference) => Ok(reference.cell.borrow().clone()),
                    _ => Err(RuntimeError::TypeMismatch {
                        message: "Cannot dereference non-reference value".into(),
                    }),
                }
            }
            Expr::EnumLiteral {
                enum_name,
                variant,
                values,
                ..
            } => {
                let args = values
                    .iter()
                    .map(|expr| self.eval_expression(expr))
                    .collect::<RuntimeResult<Vec<_>>>()?;
                let enum_name = if let Some(name) = enum_name {
                    name.clone()
                } else if let Some(info) = self.enum_variants.get(variant) {
                    info.enum_name.clone()
                } else {
                    variant.clone()
                };
                self.instantiate_enum(&enum_name, variant, args)
            }
            Expr::ArrayLiteral(_, _) => Err(RuntimeError::Unsupported {
                message: "Array literals not yet supported".into(),
            }),
        }
    }

    fn build_reference(&mut self, expr: &Expr, mutable: bool) -> RuntimeResult<Value> {
        match expr {
            Expr::Identifier(ident) => {
                let (cell, binding_mut) =
                    self.env
                        .get_cell(&ident.name)
                        .ok_or_else(|| RuntimeError::UnknownSymbol {
                            name: ident.name.clone(),
                        })?;
                if mutable && !binding_mut {
                    return Err(RuntimeError::ImmutableBinding {
                        name: ident.name.clone(),
                    });
                }
                Ok(Value::Reference(ReferenceValue { cell, mutable }))
            }
            _ => {
                let value = self.eval_expression(expr)?;
                Ok(Value::Reference(ReferenceValue {
                    cell: Rc::new(RefCell::new(value)),
                    mutable,
                }))
            }
        }
    }

    fn eval_match(&mut self, expr: &MatchExpr) -> RuntimeResult<Value> {
        let target = self.eval_expression(&expr.expr)?;
        for arm in &expr.arms {
            self.env.push_scope();
            if self.match_pattern(&arm.pattern, &target)? {
                let value = self.eval_expression(&arm.value)?;
                self.execute_deferred()?;
                self.env.pop_scope();
                return Ok(value);
            }
            self.env.pop_scope();
        }
        Err(RuntimeError::MatchError {
            message: "No match arm matched value".into(),
        })
    }

    fn match_pattern(&mut self, pattern: &Pattern, value: &Value) -> RuntimeResult<bool> {
        match pattern {
            Pattern::Wildcard(_) => Ok(true),
            Pattern::Identifier(name, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                self.env.declare(name, concrete, false)?;
                Ok(true)
            }
            Pattern::Literal(lit) => {
                let lit_value = match lit {
                    Literal::Int(v, _) => Value::Int(*v),
                    Literal::Float(v, _) => Value::Float(*v),
                    Literal::Bool(v, _) => Value::Bool(*v),
                    Literal::String(v, _) => Value::String(v.clone()),
                    Literal::Rune(v, _) => Value::Int(*v as i128),
                };
                let target = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                Ok(match (target, lit_value) {
                    (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
                    (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
                    (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
                    (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
                    _ => false,
                })
            }
            Pattern::EnumVariant {
                enum_name,
                variant,
                bindings,
                ..
            } => {
                let target = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Enum(enum_value) = target {
                    if &enum_value.variant == variant
                        && enum_name
                            .as_ref()
                            .map(|name| &enum_value.enum_name == name)
                            .unwrap_or(true)
                    {
                        for (binding, val) in bindings.iter().zip(enum_value.values.iter()) {
                            if !self.match_pattern(binding, val)? {
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

    fn instantiate_struct(
        &mut self,
        name: &str,
        fields: &StructLiteralKind,
    ) -> RuntimeResult<Value> {
        let def = self
            .structs
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::UnknownSymbol {
                name: name.to_string(),
            })?;
        let mut field_map = BTreeMap::new();
        let mut embedded = Vec::new();

        match fields {
            StructLiteralKind::Named(named_fields) => {
                for field in named_fields {
                    let value = self.eval_expression(&field.value)?;
                    field_map.insert(field.name.clone(), value);
                }
            }
            StructLiteralKind::Positional(values) => {
                if values.len() != def.fields.len() {
                    return Err(RuntimeError::TypeMismatch {
                        message: format!(
                            "Struct `{}` expects {} fields but got {}",
                            name,
                            def.fields.len(),
                            values.len()
                        ),
                    });
                }
                for (field_def, expr) in def.fields.iter().zip(values.iter()) {
                    let is_embedded = field_def.embedded;
                    let field_name = field_def.name.clone();
                    let value = self.eval_expression(expr)?;
                    if is_embedded {
                        embedded.push(value);
                    } else if let Some(field_name) = field_name {
                        field_map.insert(field_name, value);
                    }
                }
            }
        }

        Ok(Value::Struct(StructInstance {
            name: name.to_string(),
            fields: field_map,
            embedded,
        }))
    }

    fn instantiate_enum(
        &mut self,
        enum_name: &str,
        variant: &str,
        values: Vec<Value>,
    ) -> RuntimeResult<Value> {
        let info = self
            .enum_variants
            .get(variant)
            .ok_or_else(|| RuntimeError::UnknownSymbol {
                name: variant.to_string(),
            })?;
        if info.fields.len() != values.len() {
            return Err(RuntimeError::ArityMismatch {
                name: variant.to_string(),
                expected: info.fields.len(),
                received: values.len(),
            });
        }
        Ok(Value::Enum(EnumValue {
            enum_name: enum_name.to_string(),
            variant: variant.to_string(),
            values,
        }))
    }

    fn eval_binary(&self, op: BinaryOp, left: Value, right: Value) -> RuntimeResult<Value> {
        use BinaryOp::*;
        match op {
            Add | Sub | Mul | Div | Rem => self.eval_numeric(op, left, right),
            And => Ok(Value::Bool(left.as_bool() && right.as_bool())),
            Or => Ok(Value::Bool(left.as_bool() || right.as_bool())),
            BitAnd | BitOr | BitXor => self.eval_bitwise(op, left, right),
            Eq => Ok(Value::Bool(self.values_equal(&left, &right)?)),
            NotEq => Ok(Value::Bool(!self.values_equal(&left, &right)?)),
            Lt => self.eval_compare(left, right, |a, b| a < b),
            LtEq => self.eval_compare(left, right, |a, b| a <= b),
            Gt => self.eval_compare(left, right, |a, b| a > b),
            GtEq => self.eval_compare(left, right, |a, b| a >= b),
        }
    }

    fn eval_numeric(&self, op: BinaryOp, left: Value, right: Value) -> RuntimeResult<Value> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => match op {
                BinaryOp::Add => Ok(Value::Int(a + b)),
                BinaryOp::Sub => Ok(Value::Int(a - b)),
                BinaryOp::Mul => Ok(Value::Int(a * b)),
                BinaryOp::Div => Ok(Value::Int(a / b)),
                BinaryOp::Rem => Ok(Value::Int(a % b)),
                _ => unreachable!(),
            },
            (Value::Float(a), Value::Float(b)) => match op {
                BinaryOp::Add => Ok(Value::Float(a + b)),
                BinaryOp::Sub => Ok(Value::Float(a - b)),
                BinaryOp::Mul => Ok(Value::Float(a * b)),
                BinaryOp::Div => Ok(Value::Float(a / b)),
                BinaryOp::Rem => Ok(Value::Float(a % b)),
                _ => unreachable!(),
            },
            (Value::Int(a), Value::Float(b)) => {
                self.eval_numeric(op, Value::Float(a as f64), Value::Float(b))
            }
            (Value::Float(a), Value::Int(b)) => {
                self.eval_numeric(op, Value::Float(a), Value::Float(b as f64))
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: "Numeric operation expects numbers".into(),
            }),
        }
    }

    fn eval_bitwise(&self, op: BinaryOp, left: Value, right: Value) -> RuntimeResult<Value> {
        let (lhs, rhs) = match (left, right) {
            (Value::Int(a), Value::Int(b)) => (a, b),
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "Bitwise operations require integers".into(),
                });
            }
        };
        let result = match op {
            BinaryOp::BitAnd => lhs & rhs,
            BinaryOp::BitOr => lhs | rhs,
            BinaryOp::BitXor => lhs ^ rhs,
            _ => unreachable!(),
        };
        Ok(Value::Int(result))
    }

    fn eval_compare<F>(&self, left: Value, right: Value, cmp: F) -> RuntimeResult<Value>
    where
        F: Fn(f64, f64) -> bool,
    {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(cmp(a as f64, b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(cmp(a, b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(cmp(a as f64, b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(cmp(a, b as f64))),
            _ => Err(RuntimeError::TypeMismatch {
                message: "Comparison expects numeric values".into(),
            }),
        }
    }

    fn values_equal(&self, left: &Value, right: &Value) -> RuntimeResult<bool> {
        let left_val = match left {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other.clone(),
        };
        let right_val = match right {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other.clone(),
        };
        match (left_val, right_val) {
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::Int(a), Value::Int(b)) => Ok(a == b),
            (Value::Float(a), Value::Float(b)) => Ok(a == b),
            (Value::String(a), Value::String(b)) => Ok(a == b),
            _ => Err(RuntimeError::TypeMismatch {
                message: "Equality comparison expects similar types".into(),
            }),
        }
    }

    fn eval_range_expr(&mut self, range: &RangeExpr) -> RuntimeResult<RangeValue> {
        let start = self.eval_expression(&range.start)?;
        let end = self.eval_expression(&range.end)?;
        let start_int = match start {
            Value::Int(v) => v,
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "Range start must be integer".into(),
                });
            }
        };
        let end_int = match end {
            Value::Int(v) => v,
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "Range end must be integer".into(),
                });
            }
        };
        Ok(RangeValue {
            start: start_int,
            end: end_int,
            inclusive: range.inclusive,
            span: range.span,
        })
    }
}

fn flow_name(flow: &FlowSignal) -> &'static str {
    match flow {
        FlowSignal::Break => "break",
        FlowSignal::Continue => "continue",
        FlowSignal::Return(_) => "return",
    }
}
