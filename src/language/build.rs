use crate::language::ast::{
    AssignStmt, BinaryOp, Block, Expr, FormatSegment, FormatStringLiteral, Identifier, IfCondition,
    IfExpr, Literal, MatchExpr, Pattern, RangeExpr, Statement, StructLiteralField,
    StructLiteralKind, UnaryOp,
};
use crate::language::span::Span;
use crate::language::types::{Mutability, TypeExpr};
use crate::runtime::value::{FormatRuntimeSegmentGeneric, FormatTemplateValueGeneric};
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::collections::HashSet;
use std::sync::{Arc, Condvar, Mutex};

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum BuildValue {
    Unit,
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Struct {
        name: String,
        fields: BTreeMap<String, BuildValue>,
    },
    Enum {
        enum_name: String,
        variant: String,
        values: Vec<BuildValue>,
        variant_index: u32,
    },
    ChannelSender(BuildChannelSender),
    ChannelReceiver(BuildChannelReceiver),
    Tuple(Vec<BuildValue>),
    Range {
        start: i128,
        end: i128,
        inclusive: bool,
    },
    Boxed(Box<BuildValue>),
    Slice(Vec<BuildValue>),
    Map(BTreeMap<String, BuildValue>),
    FormatTemplate(FormatTemplateValueGeneric<BuildValue>),
    DeferredCall {
        name: String,
        type_args: Vec<TypeExpr>,
        args: Vec<BuildValue>,
    },
    Reference(BuildReference),
    Moved,
}

#[derive(Clone, Debug)]
pub struct BuildReference {
    pub cell: Arc<Mutex<BuildValue>>,
    pub mutable: bool,
}

#[allow(dead_code)]
impl BuildValue {
    pub fn kind(&self) -> &'static str {
        match self {
            BuildValue::Unit => "unit",
            BuildValue::Int(_) => "int",
            BuildValue::Float(_) => "float",
            BuildValue::Bool(_) => "bool",
            BuildValue::String(_) => "string",
            BuildValue::Struct { .. } => "struct",
            BuildValue::Enum { .. } => "enum",
            BuildValue::Tuple(_) => "tuple",
            BuildValue::Range { .. } => "range",
            BuildValue::Boxed(_) => "box",
            BuildValue::Slice(_) => "slice",
            BuildValue::Map(_) => "map",
            BuildValue::FormatTemplate(_) => "format string",
            BuildValue::ChannelSender(_) => "channel sender",
            BuildValue::ChannelReceiver(_) => "channel receiver",
            BuildValue::DeferredCall { .. } => "function call",
            BuildValue::Reference(_) => "reference",
            BuildValue::Moved => "moved value",
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildBinding {
    pub cell: Arc<Mutex<BuildValue>>,
    pub mutable: bool,
    pub borrowed_mut: bool,
    pub borrowed_shared: usize,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildScope {
    pub bindings: HashMap<String, BuildBinding>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildSnapshot {
    pub scopes: Vec<BuildScope>,
    pub enum_variants: HashMap<String, BuildEnumVariant>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildEnumVariant {
    pub enum_name: String,
    pub variant_index: u32,
    pub fields: usize,
}

#[allow(dead_code)]
pub type BuildFormatSegment = FormatRuntimeSegmentGeneric<BuildValue>;
#[allow(dead_code)]
pub type BuildFormatTemplate = FormatTemplateValueGeneric<BuildValue>;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildInterpreter {
    scopes: Vec<BuildScope>,
    effects: Vec<BuildEffect>,
    enum_variants: HashMap<String, BuildEnumVariant>,
    next_channel_id: u64,
    active_mut_borrows: HashSet<String>,
    borrow_frames: Vec<Vec<BorrowMark>>,
}

#[derive(Clone, Debug)]
struct BorrowMark {
    name: String,
    kind: BorrowKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum BorrowKind {
    Mutable,
    Shared,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildEvaluation {
    pub value: BuildValue,
    pub effects: Vec<BuildEffect>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum BuildEffect {
    Out(Vec<BuildValue>),
    ChannelCreate { id: u64 },
    ChannelSend { id: u64, value: BuildValue },
    ChannelClose { id: u64 },
}

#[derive(Clone, Debug)]
pub struct BuildChannelSender {
    pub id: u64,
    inner: Arc<(Mutex<BuildChannelState>, Condvar)>,
}

#[derive(Clone, Debug)]
pub struct BuildChannelReceiver {
    pub id: u64,
    inner: Arc<(Mutex<BuildChannelState>, Condvar)>,
}

#[derive(Debug)]
struct BuildChannelState {
    queue: VecDeque<BuildValue>,
    closed: bool,
}

impl BuildChannelSender {
    fn new(id: u64, inner: Arc<(Mutex<BuildChannelState>, Condvar)>) -> Self {
        Self { id, inner }
    }

    fn send(&self, value: BuildValue) -> Result<(), String> {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        if guard.closed {
            return Err("channel closed".into());
        }
        guard.queue.push_back(value);
        cv.notify_one();
        Ok(())
    }

    fn close(&self) {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        guard.closed = true;
        cv.notify_all();
    }
}

impl BuildChannelReceiver {
    fn new(id: u64, inner: Arc<(Mutex<BuildChannelState>, Condvar)>) -> Self {
        Self { id, inner }
    }

    fn recv(&self) -> Option<BuildValue> {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        loop {
            if let Some(v) = guard.queue.pop_front() {
                return Some(v);
            }
            if guard.closed {
                return None;
            }
            guard = cv.wait(guard).unwrap();
        }
    }

    fn close(&self) {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        guard.closed = true;
        cv.notify_all();
    }
}

impl BuildInterpreter {
    pub fn new(snapshot: BuildSnapshot) -> Self {
        let scope_frames = snapshot.scopes.len().max(1);
        Self {
            scopes: snapshot.scopes,
            effects: Vec::new(),
            enum_variants: snapshot.enum_variants,
            next_channel_id: 0,
            active_mut_borrows: HashSet::new(),
            borrow_frames: vec![Vec::new(); scope_frames],
        }
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<BuildValue, String> {
        self.eval_expr_mut(expr)
    }

    pub fn eval_with_effects(&self, expr: &Expr) -> Result<BuildEvaluation, String> {
        let mut runner = self.clone();
        let value = runner.eval_expr_mut(expr)?;
        Ok(BuildEvaluation {
            value,
            effects: runner.effects,
        })
    }

    fn load_identifier(&mut self, name: &str) -> Result<BuildValue, String> {
        let binding = self
            .find_binding_mut(name)
            .ok_or_else(|| format!("Unknown identifier `{}` in build spawn", name))?;
        let guard = binding.cell.lock().unwrap();
        if let BuildValue::Moved = *guard {
            return Err(format!("Identifier `{}` has been moved", name));
        }
        Ok(guard.clone())
    }

    fn eval_expr_mut(&mut self, expr: &Expr) -> Result<BuildValue, String> {
        match expr {
            Expr::Literal(lit) => self.eval_literal(lit),
            Expr::FormatString(literal) => self.eval_format_string(literal),
            Expr::Identifier(ident) => self.load_identifier(&ident.name),
            Expr::Tuple(items, _) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval_expr_mut(item)?);
                }
                Ok(BuildValue::Tuple(values))
            }
            Expr::ArrayLiteral(items, _) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval_expr_mut(item)?);
                }
                Ok(BuildValue::Slice(values))
            }
            Expr::MapLiteral { entries, .. } => {
                let mut map = BTreeMap::new();
                for entry in entries {
                    let key_value = self.eval_expr_mut(&entry.key)?;
                    let key_str = match key_value {
                        BuildValue::String(text) => text,
                        BuildValue::Int(v) => v.to_string(),
                        other => {
                            return Err(format!(
                                "map literal keys must be string or int for build spawn, found {}",
                                other.kind()
                            ))
                        }
                    };
                    let value = self.eval_expr_mut(&entry.value)?;
                    map.insert(key_str, value);
                }
                Ok(BuildValue::Map(map))
            }
            Expr::Binary { op, left, right, .. } => {
                let l = self.eval_expr_mut(left)?;
                let r = self.eval_expr_mut(right)?;
                self.eval_binary(*op, l, r)
            }
            Expr::Unary { op, expr, .. } => {
                let value = self.eval_expr_mut(expr)?;
                self.eval_unary(*op, value)
            }
            Expr::If(if_expr) => self.eval_if(if_expr),
            Expr::Range(range) => self.eval_range(range),
            Expr::StructLiteral { name, fields, .. } => {
                self.eval_struct_literal(name, fields)
            }
            Expr::EnumLiteral { enum_name, variant, values, .. } => {
                self.eval_enum_literal(enum_name.as_ref(), variant, values)
            }
            Expr::Block(block) => match self.eval_block(block)? {
                Flow::Value(v) => Ok(v),
                Flow::Return(v) => Ok(v),
                Flow::Break | Flow::Continue => {
                    Err("break/continue not allowed in expression context for build spawn".into())
                }
            },
            Expr::Match(match_expr) => self.eval_match(match_expr),
            Expr::Call { callee, args, type_args, .. } => self.eval_call(callee, type_args, args),
            Expr::FieldAccess { base, field, .. } => self.eval_field_access(base, field),
            Expr::Reference { expr, mutable, .. } => {
                match expr.as_ref() {
                    Expr::Identifier(ident) => {
                        let (cell, mutable_flag, borrowed_mut, borrowed_shared) = {
                            let binding = self
                                .find_binding_mut(&ident.name)
                                .ok_or_else(|| {
                                    format!("Unknown identifier `{}` for reference", ident.name)
                                })?;
                            (
                                binding.cell.clone(),
                                binding.mutable,
                                binding.borrowed_mut,
                                binding.borrowed_shared,
                            )
                        };
                        if *mutable && !mutable_flag {
                            return Err(format!("Identifier `{}` is immutable", ident.name));
                        }
                        if *mutable && borrowed_mut {
                            return Err(format!(
                                "Identifier `{}` is already mutably borrowed",
                                ident.name
                            ));
                        }
                        if *mutable && borrowed_shared > 0 {
                            return Err(format!(
                                "Identifier `{}` has active shared borrows",
                                ident.name
                            ));
                        }
                        if *mutable {
                            self.begin_mut_borrow(&ident.name)?;
                            if let Some(binding) = self.find_binding_mut(&ident.name) {
                                binding.borrowed_mut = true;
                            }
                        } else {
                            self.begin_shared_borrow(&ident.name)?;
                        }
                        Ok(BuildValue::Reference(BuildReference {
                            cell,
                            mutable: *mutable,
                        }))
                    }
                    _ => {
                        let value = self.eval_expr_mut(expr)?;
                        Ok(BuildValue::Reference(BuildReference {
                            cell: Arc::new(Mutex::new(value)),
                            mutable: *mutable,
                        }))
                    }
                }
            }
            Expr::Deref { expr, .. } => {
                match self.eval_expr_mut(expr)? {
                    BuildValue::Reference(reference) => Ok(reference.cell.lock().unwrap().clone()),
                    other => Err(format!("deref not supported for {}", other.kind())),
                }
            }
            Expr::Try { block, .. } => self.eval_try(block),
            Expr::TryPropagate { expr, .. } => self.eval_try_propagate(expr),
            Expr::Move { expr, .. } => self.eval_move(expr),
            Expr::Spawn { .. } => Err("nested spawn not supported in build interpreter".into()),
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Result<BuildValue, String> {
        match lit {
            Literal::Int(v, _) => Ok(BuildValue::Int(*v)),
            Literal::Float(v, _) => Ok(BuildValue::Float(*v)),
            Literal::Bool(v, _) => Ok(BuildValue::Bool(*v)),
            Literal::String(v, _) => Ok(BuildValue::String(v.clone())),
            Literal::Rune(_, _) => Err("rune literals not yet supported in build spawn".into()),
        }
    }

    fn eval_format_string(
        &mut self,
        literal: &FormatStringLiteral,
    ) -> Result<BuildValue, String> {
        let mut segments = Vec::with_capacity(literal.segments.len());
        let mut implicit = 0usize;
        for segment in &literal.segments {
            let converted = match segment {
                FormatSegment::Literal(text) => FormatRuntimeSegmentGeneric::Literal(text.clone()),
                FormatSegment::Implicit(_) => {
                    implicit += 1;
                    FormatRuntimeSegmentGeneric::Implicit
                }
                FormatSegment::Expr { expr, .. } => {
                    FormatRuntimeSegmentGeneric::Named(self.eval_expr_mut(expr)?)
                }
            };
            segments.push(converted);
        }
        Ok(BuildValue::FormatTemplate(BuildFormatTemplate {
            segments,
            implicit_placeholders: implicit,
        }))
    }

    fn eval_block(&mut self, block: &Block) -> Result<Flow, String> {
        self.push_scope();
        let mut last_value = BuildValue::Unit;
        for stmt in &block.statements {
            let flow = self.eval_statement(stmt)?;
            match flow {
                Flow::Value(value) => last_value = value,
                Flow::Return(_) | Flow::Break | Flow::Continue => {
                    self.pop_scope();
                    return Ok(flow);
                }
            };
        }
        let result = if let Some(tail) = &block.tail {
            self.eval_expr_mut(tail)?
        } else {
            last_value
        };
        self.pop_scope();
        Ok(Flow::Value(result))
    }

    fn eval_statement(&mut self, stmt: &Statement) -> Result<Flow, String> {
        match stmt {
            Statement::Let(let_stmt) => {
                let value = if let Some(expr) = &let_stmt.value {
                    self.eval_expr_mut(expr)?
                } else {
                    BuildValue::Unit
                };
                self.bind_pattern(&let_stmt.pattern, value, let_stmt.mutability)?;
                Ok(Flow::Value(BuildValue::Unit))
            }
            Statement::Assign(assign) => {
                self.eval_assign(assign)?;
                Ok(Flow::Value(BuildValue::Unit))
            }
            Statement::Expr(expr_stmt) => Ok(Flow::Value(self.eval_expr_mut(&expr_stmt.expr)?)),
            Statement::Block(block) => self.eval_block(block),
            Statement::Return(ret) => {
                let value = if ret.values.is_empty() {
                    BuildValue::Unit
                } else if ret.values.len() == 1 {
                    self.eval_expr_mut(&ret.values[0])?
                } else {
                    let mut values = Vec::with_capacity(ret.values.len());
                    for expr in &ret.values {
                        values.push(self.eval_expr_mut(expr)?);
                    }
                    BuildValue::Tuple(values)
                };
                Ok(Flow::Return(value))
            }
            Statement::While(while_stmt) => self.eval_while(while_stmt),
            Statement::Loop(loop_stmt) => self.eval_loop(loop_stmt),
            Statement::For(for_stmt) => self.eval_for(for_stmt),
            Statement::Defer(_) => Err("defer not supported in build spawn interpreter yet".into()),
            Statement::Break => Ok(Flow::Break),
            Statement::Continue => Ok(Flow::Continue),
        }
    }

    fn eval_while(&mut self, stmt: &crate::language::ast::WhileStmt) -> Result<Flow, String> {
        loop {
            let cond = match &stmt.condition {
                crate::language::ast::WhileCondition::Expr(expr) => self.eval_expr_mut(expr)?,
                crate::language::ast::WhileCondition::Let { pattern, value } => {
                    let scrutinee = self.eval_expr_mut(value)?;
                    if self.try_bind_pattern(pattern, scrutinee, Mutability::Immutable)? {
                        BuildValue::Bool(true)
                    } else {
                        BuildValue::Bool(false)
                    }
                }
            };
            if !self.expect_bool(cond)? {
                break;
            }
            match self.eval_block(&stmt.body)? {
                Flow::Value(_) => {}
                Flow::Break => break,
                Flow::Continue => continue,
                Flow::Return(v) => return Ok(Flow::Return(v)),
            }
        }
        Ok(Flow::Value(BuildValue::Unit))
    }

    fn eval_loop(&mut self, stmt: &crate::language::ast::LoopStmt) -> Result<Flow, String> {
        loop {
            match self.eval_block(&stmt.body)? {
                Flow::Value(_) => {}
                Flow::Break => break,
                Flow::Continue => continue,
                Flow::Return(v) => return Ok(Flow::Return(v)),
            }
        }
        Ok(Flow::Value(BuildValue::Unit))
    }

    fn eval_for(&mut self, stmt: &crate::language::ast::ForStmt) -> Result<Flow, String> {
        let iterable = match &stmt.target {
            crate::language::ast::ForTarget::Range(range) => self.eval_range(range)?,
            crate::language::ast::ForTarget::Collection(expr) => self.eval_expr_mut(expr)?,
        };
        match iterable {
            BuildValue::Range { start, end, inclusive } => {
                let end_bound = if inclusive { end + 1 } else { end };
                for idx in start..end_bound {
                    self.push_scope();
                    self.bind_pattern(
                        &Pattern::Identifier(stmt.binding.clone(), Span::new(0, 0)),
                        BuildValue::Int(idx),
                        Mutability::Mutable,
                    )?;
                    let flow = self.eval_block(&stmt.body)?;
                    match flow {
                        Flow::Value(_) => {
                            self.pop_scope();
                        }
                        Flow::Break => {
                            self.pop_scope();
                            break;
                        }
                        Flow::Continue => {
                            self.pop_scope();
                            continue;
                        }
                        Flow::Return(v) => {
                            self.pop_scope();
                            return Ok(Flow::Return(v));
                        }
                    }
                }
                Ok(Flow::Value(BuildValue::Unit))
            }
            BuildValue::Slice(items) => {
                for item in items {
                    self.push_scope();
                    self.bind_pattern(
                        &Pattern::Identifier(stmt.binding.clone(), Span::new(0, 0)),
                        item,
                        Mutability::Mutable,
                    )?;
                    let flow = self.eval_block(&stmt.body)?;
                    match flow {
                        Flow::Value(_) => {
                            self.pop_scope();
                        }
                        Flow::Break => {
                            self.pop_scope();
                            break;
                        }
                        Flow::Continue => {
                            self.pop_scope();
                            continue;
                        }
                        Flow::Return(v) => {
                            self.pop_scope();
                            return Ok(Flow::Return(v));
                        }
                    }
                }
                Ok(Flow::Value(BuildValue::Unit))
            }
            other => Err(format!(
                "for loops in build spawn require range or slice, found {}",
                other.kind()
            )),
        }
    }

    fn eval_assign(&mut self, assign: &AssignStmt) -> Result<(), String> {
        match &assign.target {
            Expr::Identifier(Identifier { name, .. }) => {
                let value = self.eval_expr_mut(&assign.value)?;
                let binding = self.find_binding_mut(name).ok_or_else(|| {
                    format!("Unknown identifier `{}` in assignment", name)
                })?;
                if !binding.mutable {
                    return Err(format!("Identifier `{}` is immutable", name));
                }
                *binding.cell.lock().unwrap() = value;
                Ok(())
            }
            _ => Err("only simple identifier assignments supported in build spawn".into()),
        }
    }

    fn eval_if(&mut self, if_expr: &IfExpr) -> Result<BuildValue, String> {
        let cond = match &if_expr.condition {
            IfCondition::Expr(expr) => self.eval_expr_mut(expr)?,
            IfCondition::Let { pattern, value } => {
                let scrutinee = self.eval_expr_mut(value)?;
                if self.try_bind_pattern(pattern, scrutinee.clone(), Mutability::Immutable)? {
                    BuildValue::Bool(true)
                } else {
                    BuildValue::Bool(false)
                }
            }
        };
        if self.expect_bool(cond)? {
            match self.eval_block(&if_expr.then_branch)? {
                Flow::Value(v) | Flow::Return(v) => Ok(v),
                Flow::Break | Flow::Continue => Err(
                    "break/continue not allowed in expression context for build spawn".into(),
                ),
            }
        } else if let Some(else_branch) = &if_expr.else_branch {
            match else_branch {
                crate::language::ast::ElseBranch::Block(block) => match self.eval_block(block)? {
                    Flow::Value(v) | Flow::Return(v) => Ok(v),
                    Flow::Break | Flow::Continue => Err(
                        "break/continue not allowed in expression context for build spawn".into(),
                    ),
                },
                crate::language::ast::ElseBranch::ElseIf(nested) => self.eval_if(nested),
            }
        } else {
            Ok(BuildValue::Unit)
        }
    }

    fn eval_range(&mut self, range: &RangeExpr) -> Result<BuildValue, String> {
        let start_val = self.eval_expr_mut(&range.start)?;
        let start = self.expect_int(start_val)?;
        let end_val = self.eval_expr_mut(&range.end)?;
        let end = self.expect_int(end_val)?;
        Ok(BuildValue::Range {
            start,
            end,
            inclusive: range.inclusive,
        })
    }

    fn eval_struct_literal(
        &mut self,
        name: &str,
        fields: &StructLiteralKind,
    ) -> Result<BuildValue, String> {
        match fields {
            StructLiteralKind::Named(named) => {
                let mut map = BTreeMap::new();
                for StructLiteralField { name: field_name, value } in named {
                    map.insert(field_name.clone(), self.eval_expr_mut(value)?);
                }
                Ok(BuildValue::Struct {
                    name: name.to_string(),
                    fields: map,
                })
            }
            StructLiteralKind::Positional(_) => {
                Err("positional struct literals not supported in build spawn".into())
            }
        }
    }

    fn eval_field_access(&mut self, base: &Expr, field: &str) -> Result<BuildValue, String> {
        let value = self.eval_expr_mut(base)?;
        match value {
            BuildValue::Struct { mut fields, .. } => fields
                .remove(field)
                .ok_or_else(|| format!("field `{}` not found in struct", field)),
            BuildValue::Map(mut entries) => entries
                .remove(field)
                .ok_or_else(|| format!("key `{}` not found in map", field)),
            BuildValue::Tuple(items) => {
                if let Ok(idx) = field.parse::<usize>() {
                    items
                        .get(idx)
                        .cloned()
                        .ok_or_else(|| format!("tuple index {} out of bounds", idx))
                } else {
                    Err("tuple field access expects numeric index".into())
                }
            }
            other => Err(format!(
                "field access not supported for {} in build spawn",
                other.kind()
            )),
        }
    }

    fn eval_enum_literal(
        &mut self,
        enum_name: Option<&String>,
        variant: &str,
        values: &[Expr],
    ) -> Result<BuildValue, String> {
        let mut converted = Vec::with_capacity(values.len());
        for value in values {
            converted.push(self.eval_expr_mut(value)?);
        }
        let meta = self.enum_variants.get(variant);
        let (enum_name, variant_index) = match (enum_name, meta) {
            (Some(name), Some(info)) if &info.enum_name == name => (name.clone(), info.variant_index),
            (Some(name), _) => (name.clone(), 0),
            (None, Some(info)) => (info.enum_name.clone(), info.variant_index),
            (None, None) => ("".into(), 0),
        };
        Ok(BuildValue::Enum {
            enum_name,
            variant: variant.to_string(),
            values: converted,
            variant_index,
        })
    }

    fn eval_call(
        &mut self,
        callee: &Expr,
        type_args: &[TypeExpr],
        args: &[Expr],
    ) -> Result<BuildValue, String> {
        match callee {
            Expr::Identifier(Identifier { name, .. }) if name == "out" => {
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    values.push(self.eval_expr_mut(arg)?);
                }
                self.effects.push(BuildEffect::Out(values));
                Ok(BuildValue::Unit)
            }
            Expr::Identifier(Identifier { name, .. }) => {
                self.eval_builtin_or_deferred(name, None, type_args, args)
            }
            Expr::FieldAccess { base, field, .. } => {
                // Treat method calls as deferred functions with receiver as first arg.
                let mut converted = Vec::with_capacity(args.len() + 1);
                converted.push(self.eval_expr_mut(base)?);
                for arg in args {
                    converted.push(self.eval_expr_mut(arg)?);
                }
                Ok(BuildValue::DeferredCall {
                    name: field.to_string(),
                    type_args: type_args.to_vec(),
                    args: converted,
                })
            }
            _ => Err("only identifier and method calls supported in build spawn interpreter".into()),
        }
    }

    fn eval_match(&mut self, match_expr: &MatchExpr) -> Result<BuildValue, String> {
        let scrutinee = self.eval_expr_mut(&match_expr.expr)?;
        for arm in &match_expr.arms {
            if self.try_bind_pattern(&arm.pattern, scrutinee.clone(), Mutability::Immutable)? {
                if let Some(guard) = &arm.guard {
                    let guard_val = self.eval_expr_mut(guard)?;
                    if !self.expect_bool(guard_val)? {
                        continue;
                    }
                }
                match self.eval_expr_mut(&arm.value) {
                    Ok(v) => return Ok(v),
                    Err(err) => return Err(err),
                }
            }
        }
        Err("match expression had no matching arm in build spawn".into())
    }

    fn eval_move(&mut self, expr: &Expr) -> Result<BuildValue, String> {
        match expr {
            Expr::Identifier(ident) => {
                let binding = self
                    .find_binding_mut(&ident.name)
                    .ok_or_else(|| format!("Unknown identifier `{}` for move", ident.name))?;
                let mut cell = binding.cell.lock().unwrap();
                if let BuildValue::Moved = *cell {
                    return Err(format!("Identifier `{}` already moved", ident.name));
                }
                let value = std::mem::replace(&mut *cell, BuildValue::Moved);
                Ok(value)
            }
            _ => self.eval_expr_mut(expr),
        }
    }

    fn eval_try(&mut self, block: &Block) -> Result<BuildValue, String> {
        match self.eval_block(block)? {
            Flow::Value(v) => Ok(v),
            Flow::Return(v) => Ok(v),
            Flow::Break | Flow::Continue => Err("break/continue not allowed in try expression".into()),
        }
    }

    fn eval_try_propagate(&mut self, expr: &Expr) -> Result<BuildValue, String> {
        let value = self.eval_expr_mut(expr)?;
        match value {
            BuildValue::Enum { enum_name, variant, mut values, .. } if enum_name == "Result" && variant == "Ok" => {
                if values.len() == 1 {
                    Ok(values.pop().unwrap())
                } else {
                    Ok(BuildValue::Tuple(values))
                }
            }
            BuildValue::Enum { enum_name, variant, values, .. } if enum_name == "Result" && variant == "Err" => {
                Err(format!("try propagation yielded error {:?}", values))
            }
            other => Err(format!("? operator expects Result, found {}", other.kind())),
        }
    }

    fn eval_binary(&self, op: BinaryOp, left: BuildValue, right: BuildValue) -> Result<BuildValue, String> {
        match (left, right) {
            (BuildValue::Int(l), BuildValue::Int(r)) => {
                let value = match op {
                    BinaryOp::Add => BuildValue::Int(l + r),
                    BinaryOp::Sub => BuildValue::Int(l - r),
                    BinaryOp::Mul => BuildValue::Int(l * r),
                    BinaryOp::Div => BuildValue::Int(l / r),
                    BinaryOp::Rem => BuildValue::Int(l % r),
                    BinaryOp::Eq => BuildValue::Bool(l == r),
                    BinaryOp::NotEq => BuildValue::Bool(l != r),
                    BinaryOp::Lt => BuildValue::Bool(l < r),
                    BinaryOp::LtEq => BuildValue::Bool(l <= r),
                    BinaryOp::Gt => BuildValue::Bool(l > r),
                    BinaryOp::GtEq => BuildValue::Bool(l >= r),
                    _ => return Err("unsupported integer binary operation in build spawn".into()),
                };
                Ok(value)
            }
            (BuildValue::Float(l), BuildValue::Float(r)) => {
                let value = match op {
                    BinaryOp::Add => BuildValue::Float(l + r),
                    BinaryOp::Sub => BuildValue::Float(l - r),
                    BinaryOp::Mul => BuildValue::Float(l * r),
                    BinaryOp::Div => BuildValue::Float(l / r),
                    BinaryOp::Rem => BuildValue::Float(l % r),
                    BinaryOp::Eq => BuildValue::Bool((l - r).abs() < f64::EPSILON),
                    BinaryOp::NotEq => BuildValue::Bool((l - r).abs() >= f64::EPSILON),
                    BinaryOp::Lt => BuildValue::Bool(l < r),
                    BinaryOp::LtEq => BuildValue::Bool(l <= r),
                    BinaryOp::Gt => BuildValue::Bool(l > r),
                    BinaryOp::GtEq => BuildValue::Bool(l >= r),
                    _ => return Err("unsupported float binary operation in build spawn".into()),
                };
                Ok(value)
            }
            (BuildValue::Bool(l), BuildValue::Bool(r)) => {
                let value = match op {
                    BinaryOp::And => BuildValue::Bool(l && r),
                    BinaryOp::Or => BuildValue::Bool(l || r),
                    BinaryOp::Eq => BuildValue::Bool(l == r),
                    BinaryOp::NotEq => BuildValue::Bool(l != r),
                    _ => return Err("unsupported boolean binary operation in build spawn".into()),
                };
                Ok(value)
            }
            (BuildValue::String(l), BuildValue::String(r)) => {
                let value = match op {
                    BinaryOp::Add => BuildValue::String(format!("{l}{r}")),
                    BinaryOp::Eq => BuildValue::Bool(l == r),
                    BinaryOp::NotEq => BuildValue::Bool(l != r),
                    _ => {
                        return Err("unsupported string binary operation in build spawn".into());
                    }
                };
                Ok(value)
            }
            (l, r) => Err(format!(
                "Binary op {:?} not supported for {} and {}",
                op,
                l.kind(),
                r.kind()
            )),
        }
    }

    fn eval_unary(&self, op: UnaryOp, value: BuildValue) -> Result<BuildValue, String> {
        match (op, value) {
            (UnaryOp::Neg, BuildValue::Int(v)) => Ok(BuildValue::Int(-v)),
            (UnaryOp::Neg, BuildValue::Float(v)) => Ok(BuildValue::Float(-v)),
            (UnaryOp::Not, BuildValue::Bool(v)) => Ok(BuildValue::Bool(!v)),
            (_, other) => Err(format!("Unary op {:?} not supported for {}", op, other.kind())),
        }
    }

    fn find_binding_mut(&mut self, name: &str) -> Option<&mut BuildBinding> {
        for (_idx, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(binding) = scope.bindings.get_mut(name) {
                return Some(binding);
            }
        }
        None
    }

    fn begin_mut_borrow(&mut self, name: &str) -> Result<(), String> {
        if self.active_mut_borrows.contains(name) {
            return Err(format!("`{}` is already mutably borrowed", name));
        }
        if let Some(binding) = self.find_binding_mut(name) {
            if !binding.mutable {
                return Err(format!("Identifier `{}` is immutable", name));
            }
            if binding.borrowed_shared > 0 {
                return Err(format!(
                    "Identifier `{}` has active shared borrows",
                    name
                ));
            }
            binding.borrowed_mut = true;
        }
        self.active_mut_borrows.insert(name.to_string());
        if let Some(frame) = self.borrow_frames.last_mut() {
            frame.push(BorrowMark {
                name: name.to_string(),
                kind: BorrowKind::Mutable,
            });
        }
        Ok(())
    }

    fn begin_shared_borrow(&mut self, name: &str) -> Result<(), String> {
        if self.active_mut_borrows.contains(name) {
            return Err(format!("`{}` has an active mutable borrow", name));
        }
        if let Some(binding) = self.find_binding_mut(name) {
            binding.borrowed_shared = binding.borrowed_shared.saturating_add(1);
        }
        if let Some(frame) = self.borrow_frames.last_mut() {
            frame.push(BorrowMark {
                name: name.to_string(),
                kind: BorrowKind::Shared,
            });
        }
        Ok(())
    }

    fn push_scope(&mut self) {
        self.scopes.push(BuildScope {
            bindings: HashMap::new(),
        });
        self.borrow_frames.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        if let Some(frame) = self.borrow_frames.pop() {
            for mark in frame {
                match mark.kind {
                    BorrowKind::Mutable => {
                        self.active_mut_borrows.remove(&mark.name);
                        if let Some(binding) = self.find_binding_mut(&mark.name) {
                            binding.borrowed_mut = false;
                        }
                    }
                    BorrowKind::Shared => {
                        if let Some(binding) = self.find_binding_mut(&mark.name) {
                            binding.borrowed_shared = binding.borrowed_shared.saturating_sub(1);
                        }
                    }
                }
            }
        }
        if self.borrow_frames.is_empty() {
            self.borrow_frames.push(Vec::new());
        }
    }

    fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        value: BuildValue,
        mutability: Mutability,
    ) -> Result<(), String> {
        let bound = self.try_bind_pattern(pattern, value, mutability)?;
        if bound {
            Ok(())
        } else {
            Err("pattern did not match in build spawn".into())
        }
    }

    fn try_bind_pattern(
        &mut self,
        pattern: &Pattern,
        value: BuildValue,
        mutability: Mutability,
    ) -> Result<bool, String> {
        match pattern {
            Pattern::Identifier(name, _) => {
                let mutable = mutability == Mutability::Mutable;
                if let Some(scope) = self.scopes.last_mut() {
                    scope.bindings.insert(
                        name.clone(),
                        BuildBinding {
                            cell: Arc::new(Mutex::new(value)),
                            mutable,
                            borrowed_mut: false,
                            borrowed_shared: 0,
                        },
                    );
                    if mutable {
                        self.begin_mut_borrow(name)?;
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Wildcard => Ok(true),
            Pattern::Literal(lit) => self.match_literal(lit, &value),
            Pattern::Tuple(items, _) => {
                if let BuildValue::Tuple(values) = value {
                    if items.len() != values.len() {
                        return Ok(false);
                    }
                    for (pat, val) in items.iter().zip(values.into_iter()) {
                        if !self.try_bind_pattern(pat, val, mutability)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::EnumVariant {
                enum_name,
                variant,
                bindings,
            } => {
                if let BuildValue::Enum {
                    enum_name: value_enum,
                    variant: value_variant,
                    values,
                    ..
                } = value
                {
                    if let Some(expected) = enum_name {
                        if &value_enum != expected {
                            return Ok(false);
                        }
                    }
                    if &value_variant != variant {
                        return Ok(false);
                    }
                    if bindings.len() != values.len() {
                        return Ok(false);
                    }
                    for (pat, val) in bindings.iter().zip(values.into_iter()) {
                        if !self.try_bind_pattern(pat, val, mutability)? {
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
                has_spread,
                ..
            } => {
                if let BuildValue::Struct {
                    name,
                    fields: mut values,
                } = value
                {
                    if let Some(expected) = struct_name {
                        if &name != expected {
                            return Ok(false);
                        }
                    }
                    if !*has_spread && fields.len() != values.len() {
                        return Ok(false);
                    }
                    for field in fields {
                        if let Some(value) = values.remove(&field.name) {
                            if !self.try_bind_pattern(&field.pattern, value, mutability)? {
                                return Ok(false);
                            }
                        } else {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Map(entries, _) => {
                if let BuildValue::Map(mut map) = value {
                    for entry in entries {
                        if let Some(val) = map.remove(&entry.key) {
                            if !self.try_bind_pattern(&entry.pattern, val, mutability)? {
                                return Ok(false);
                            }
                        } else {
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
                if let BuildValue::Slice(values) = value {
                    if values.len() < prefix.len() + suffix.len() {
                        return Ok(false);
                    }
                    let mut idx = 0;
                    for pat in prefix {
                        if !self.try_bind_pattern(pat, values[idx].clone(), mutability)? {
                            return Ok(false);
                        }
                        idx += 1;
                    }
                    let mut end_idx = values.len();
                    for pat in suffix.iter().rev() {
                        end_idx -= 1;
                        if !self.try_bind_pattern(pat, values[end_idx].clone(), mutability)? {
                            return Ok(false);
                        }
                    }
                    if let Some(rest_pat) = rest {
                        let slice = values[idx..end_idx].to_vec();
                        if !self.try_bind_pattern(
                            rest_pat,
                            BuildValue::Slice(slice),
                            mutability,
                        )? {
                            return Ok(false);
                        }
                    } else if idx != end_idx {
                        return Ok(false);
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    fn expect_bool(&self, value: BuildValue) -> Result<bool, String> {
        match value {
            BuildValue::Bool(v) => Ok(v),
            other => Err(format!("expected bool, found {}", other.kind())),
        }
    }

    fn expect_int(&self, value: BuildValue) -> Result<i128, String> {
        match value {
            BuildValue::Int(v) => Ok(v),
            other => Err(format!("expected int, found {}", other.kind())),
        }
    }

    fn match_literal(&self, lit: &Literal, value: &BuildValue) -> Result<bool, String> {
        Ok(match (lit, value) {
            (Literal::Int(l, _), BuildValue::Int(v)) => l == v,
            (Literal::Float(l, _), BuildValue::Float(v)) => (*l - v).abs() < f64::EPSILON,
            (Literal::Bool(l, _), BuildValue::Bool(v)) => l == v,
            (Literal::String(l, _), BuildValue::String(v)) => l == v,
            (Literal::Rune(_, _), _) => false,
            _ => false,
        })
    }

    fn eval_builtin_or_deferred(
        &mut self,
        name: &str,
        receiver: Option<BuildValue>,
        type_args: &[TypeExpr],
        args: &[Expr],
    ) -> Result<BuildValue, String> {
        match name {
            "len" => {
                if args.len() != 1 {
                    return Err("len expects 1 argument".into());
                }
                match self.eval_expr_mut(&args[0])? {
                    BuildValue::Slice(items) => Ok(BuildValue::Int(items.len() as i128)),
                    BuildValue::Map(entries) => Ok(BuildValue::Int(entries.len() as i128)),
                    BuildValue::String(text) => Ok(BuildValue::Int(text.len() as i128)),
                    BuildValue::FormatTemplate(_) => Ok(BuildValue::Int(0)),
                    other => Err(format!("len not supported for {}", other.kind())),
                }
            }
            "min" | "max" => {
                if args.len() != 2 {
                    return Err(format!("{name} expects 2 arguments"));
                }
                let left = self.eval_expr_mut(&args[0])?;
                let right = self.eval_expr_mut(&args[1])?;
                match (left, right, name) {
                    (BuildValue::Int(l), BuildValue::Int(r), "min") => Ok(BuildValue::Int(l.min(r))),
                    (BuildValue::Int(l), BuildValue::Int(r), "max") => Ok(BuildValue::Int(l.max(r))),
                    (BuildValue::Float(l), BuildValue::Float(r), "min") => Ok(BuildValue::Float(l.min(r))),
                    (BuildValue::Float(l), BuildValue::Float(r), "max") => Ok(BuildValue::Float(l.max(r))),
                    (l, r, _) => Err(format!("{name} not supported for {} and {}", l.kind(), r.kind())),
                }
            }
            "abs" => {
                if args.len() != 1 {
                    return Err("abs expects 1 argument".into());
                }
                match self.eval_expr_mut(&args[0])? {
                    BuildValue::Int(v) => Ok(BuildValue::Int(v.abs())),
                    BuildValue::Float(v) => Ok(BuildValue::Float(v.abs())),
                    other => Err(format!("abs expects int or float, found {}", other.kind())),
                }
            }
            "channel" => {
                if !args.is_empty() {
                    return Err("channel expects no arguments".into());
                }
                let id = self.next_channel_id;
                self.next_channel_id += 1;
                self.effects.push(BuildEffect::ChannelCreate { id });
                let shared = Arc::new((
                    Mutex::new(BuildChannelState {
                        queue: VecDeque::new(),
                        closed: false,
                    }),
                    Condvar::new(),
                ));
                Ok(BuildValue::Tuple(vec![
                    BuildValue::ChannelSender(BuildChannelSender::new(id, shared.clone())),
                    BuildValue::ChannelReceiver(BuildChannelReceiver::new(id, shared)),
                ]))
            }
            "send" => {
                if args.len() != 2 {
                    return Err("send expects 2 arguments".into());
                }
                let sender = match self.eval_expr_mut(&args[0])? {
                    BuildValue::ChannelSender(tx) => tx,
                    other => return Err(format!("send expects channel sender, found {}", other.kind())),
                };
                let value = self.eval_expr_mut(&args[1])?;
                match sender.send(value.clone()) {
                    Ok(()) => {
                        self.effects.push(BuildEffect::ChannelSend { id: sender.id, value });
                        Ok(self.wrap_enum("Ok", vec![BuildValue::Unit]))
                    }
                    Err(msg) => Ok(self.wrap_enum("Err", vec![BuildValue::String(msg)])),
                }
            }
            "recv" => {
                if args.len() != 1 {
                    return Err("recv expects 1 argument".into());
                }
                let receiver = match self.eval_expr_mut(&args[0])? {
                    BuildValue::ChannelReceiver(rx) => rx,
                    other => {
                        return Err(format!("recv expects channel receiver, found {}", other.kind()))
                    }
                };
                match receiver.recv() {
                    Some(value) => Ok(self.wrap_enum("Some", vec![value])),
                    None => Ok(self.wrap_enum("None", vec![])),
                }
            }
            "close" => {
                if args.len() != 1 {
                    return Err("close expects 1 argument".into());
                }
                match self.eval_expr_mut(&args[0])? {
                    BuildValue::ChannelSender(tx) => {
                        tx.close();
                        self.effects.push(BuildEffect::ChannelClose { id: tx.id });
                    }
                    BuildValue::ChannelReceiver(rx) => {
                        rx.close();
                        self.effects.push(BuildEffect::ChannelClose { id: rx.id });
                    }
                    other => return Err(format!("close expects channel endpoint, found {}", other.kind())),
                }
                Ok(BuildValue::Unit)
            }
            name if self.is_user_function(name) => {
                Err(format!("call to `{}` not supported in build spawn interpreter yet", name))
            }
            other => {
                let mut converted = Vec::with_capacity(args.len());
                if let Some(recv) = receiver {
                    converted.push(recv);
                }
                for arg in args {
                    converted.push(self.eval_expr_mut(arg)?);
                }
                Ok(BuildValue::DeferredCall {
                    name: other.to_string(),
                    type_args: type_args.to_vec(),
                    args: converted,
                })
            }
        }
    }

    fn is_user_function(&self, _name: &str) -> bool {
        true
    }

    fn wrap_enum(&self, variant: &str, values: Vec<BuildValue>) -> BuildValue {
        if let Some(info) = self.enum_variants.get(variant) {
            BuildValue::Enum {
                enum_name: info.enum_name.clone(),
                variant: variant.to_string(),
                values,
                variant_index: info.variant_index,
            }
        } else {
            BuildValue::Enum {
                enum_name: "".into(),
                variant: variant.to_string(),
                values,
                variant_index: 0,
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum Flow {
    Value(BuildValue),
    Return(BuildValue),
    Break,
    Continue,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::ast::{LetStmt, Block, MatchArmExpr, MatchExpr};
    use crate::language::span::Span;

    fn empty_snapshot() -> BuildSnapshot {
        BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
        }
    }

    fn span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn eval_block_with_let_and_tail() {
        let s = span();
        let block = Block {
            statements: vec![Statement::Let(LetStmt {
                pattern: Pattern::Identifier("x".into(), s),
                ty: None,
                value: Some(Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Literal(Literal::Int(1, s))),
                    right: Box::new(Expr::Literal(Literal::Int(2, s))),
                    span: s,
                }),
                mutability: Mutability::Immutable,
                span: s,
            })],
            tail: Some(Box::new(Expr::Identifier(Identifier {
                name: "x".into(),
                span: s,
            }))),
            span: s,
        };

        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("block evaluates");
        match result {
            BuildValue::Int(v) => assert_eq!(v, 3),
            other => panic!("unexpected result: {:?}", other),
        }
    }

    #[test]
    fn eval_assignment_updates_mut_binding() {
        let s = span();
        let mut scope = BuildScope {
            bindings: HashMap::new(),
        };
        scope.bindings.insert(
            "y".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Int(4))),
                mutable: true,
                borrowed_mut: false,
                borrowed_shared: 0,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
        };
        let block = Block {
            statements: vec![Statement::Assign(AssignStmt {
                target: Expr::Identifier(Identifier {
                    name: "y".into(),
                    span: s,
                }),
                value: Expr::Literal(Literal::Int(5, s)),
            })],
            tail: Some(Box::new(Expr::Identifier(Identifier {
                name: "y".into(),
                span: s,
            }))),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let result = interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("block evaluates");
        match result {
            BuildValue::Int(v) => assert_eq!(v, 5),
            other => panic!("unexpected result: {:?}", other),
        }
    }

    #[test]
    fn eval_out_records_effect() {
        let s = span();
        let expr = Expr::Call {
            callee: Box::new(Expr::Identifier(Identifier {
                name: "out".into(),
                span: s,
            })),
            type_args: Vec::new(),
            args: vec![Expr::Literal(Literal::String("hi".into(), s))],
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter
            .eval_with_effects(&expr)
            .expect("out call evaluates");
        assert!(matches!(result.value, BuildValue::Unit));
        assert_eq!(result.effects.len(), 1);
        if let Some(BuildEffect::Out(values)) = result.effects.get(0) {
            assert_eq!(values.len(), 1);
            assert!(matches!(values[0], BuildValue::String(ref v) if v == "hi"));
        } else {
            panic!("unexpected effect");
        }
    }

    #[test]
    fn while_loop_updates_binding() {
        let s = span();
        let block = Block {
            statements: vec![
                Statement::Let(crate::language::ast::LetStmt {
                    pattern: Pattern::Identifier("n".into(), s),
                    ty: None,
                    value: Some(Expr::Literal(Literal::Int(0, s))),
                    mutability: Mutability::Mutable,
                    span: s,
                }),
                Statement::While(crate::language::ast::WhileStmt {
                    condition: crate::language::ast::WhileCondition::Expr(Expr::Binary {
                        op: BinaryOp::Lt,
                        left: Box::new(Expr::Identifier(Identifier {
                            name: "n".into(),
                            span: s,
                        })),
                        right: Box::new(Expr::Literal(Literal::Int(3, s))),
                        span: s,
                    }),
                    body: Block {
                        statements: vec![Statement::Assign(AssignStmt {
                            target: Expr::Identifier(Identifier {
                                name: "n".into(),
                                span: s,
                            }),
                            value: Expr::Binary {
                                op: BinaryOp::Add,
                                left: Box::new(Expr::Identifier(Identifier {
                                    name: "n".into(),
                                    span: s,
                                })),
                                right: Box::new(Expr::Literal(Literal::Int(1, s))),
                                span: s,
                            },
                        })],
                        tail: None,
                        span: s,
                    },
                }),
            ],
            tail: Some(Box::new(Expr::Identifier(Identifier {
                name: "n".into(),
                span: s,
            }))),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("while loop evaluates");
        assert!(matches!(result, BuildValue::Int(3)));
    }

    #[test]
    fn for_range_accumulates() {
        let s = span();
        let block = Block {
            statements: vec![
                Statement::Let(crate::language::ast::LetStmt {
                    pattern: Pattern::Identifier("sum".into(), s),
                    ty: None,
                    value: Some(Expr::Literal(Literal::Int(0, s))),
                    mutability: Mutability::Mutable,
                    span: s,
                }),
                Statement::For(crate::language::ast::ForStmt {
                    binding: "i".into(),
                    target: crate::language::ast::ForTarget::Range(crate::language::ast::RangeExpr {
                        start: Box::new(Expr::Literal(Literal::Int(0, s))),
                        end: Box::new(Expr::Literal(Literal::Int(3, s))),
                        inclusive: false,
                        span: s,
                    }),
                    body: Block {
                        statements: vec![Statement::Assign(AssignStmt {
                            target: Expr::Identifier(Identifier {
                                name: "sum".into(),
                                span: s,
                            }),
                            value: Expr::Binary {
                                op: BinaryOp::Add,
                                left: Box::new(Expr::Identifier(Identifier {
                                    name: "sum".into(),
                                    span: s,
                                })),
                                right: Box::new(Expr::Identifier(Identifier {
                                    name: "i".into(),
                                    span: s,
                                })),
                                span: s,
                            },
                        })],
                        tail: None,
                        span: s,
                    },
                    span: s,
                }),
            ],
            tail: Some(Box::new(Expr::Identifier(Identifier {
                name: "sum".into(),
                span: s,
            }))),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("for loop evaluates");
        assert!(matches!(result, BuildValue::Int(3)));
    }

    #[test]
    fn struct_pattern_binds_fields() {
        let s = span();
        let block = Block {
            statements: vec![Statement::Let(crate::language::ast::LetStmt {
                pattern: Pattern::Struct {
                    struct_name: Some("Point".into()),
                    fields: vec![
                        crate::language::ast::StructPatternField {
                            name: "x".into(),
                            pattern: Pattern::Identifier("a".into(), s),
                        },
                        crate::language::ast::StructPatternField {
                            name: "y".into(),
                            pattern: Pattern::Identifier("b".into(), s),
                        },
                    ],
                    has_spread: false,
                    span: s,
                },
                ty: None,
                value: Some(Expr::StructLiteral {
                    name: "Point".into(),
                    fields: StructLiteralKind::Named(vec![
                        StructLiteralField {
                            name: "x".into(),
                            value: Expr::Literal(Literal::Int(1, s)),
                        },
                        StructLiteralField {
                            name: "y".into(),
                            value: Expr::Literal(Literal::Int(2, s)),
                        },
                    ]),
                    span: s,
                }),
                mutability: Mutability::Immutable,
                span: s,
            })],
            tail: Some(Box::new(Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Identifier(Identifier {
                    name: "a".into(),
                    span: s,
                })),
                right: Box::new(Expr::Identifier(Identifier {
                    name: "b".into(),
                    span: s,
                })),
                span: s,
            })),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("struct pattern evaluates");
        assert!(matches!(result, BuildValue::Int(3)));
    }

    #[test]
    fn slice_pattern_rest_captures() {
        let s = span();
        let block = Block {
            statements: vec![Statement::Let(crate::language::ast::LetStmt {
                pattern: Pattern::Slice {
                    prefix: vec![Pattern::Identifier("first".into(), s)],
                    rest: Some(Box::new(Pattern::Identifier("rest".into(), s))),
                    suffix: vec![Pattern::Identifier("last".into(), s)],
                    span: s,
                },
                ty: None,
                value: Some(Expr::ArrayLiteral(
                    vec![
                        Expr::Literal(Literal::Int(1, s)),
                        Expr::Literal(Literal::Int(2, s)),
                        Expr::Literal(Literal::Int(3, s)),
                    ],
                    s,
                )),
                mutability: Mutability::Immutable,
                span: s,
            })],
            tail: Some(Box::new(Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Identifier(Identifier {
                    name: "first".into(),
                    span: s,
                })),
                right: Box::new(Expr::Identifier(Identifier {
                    name: "last".into(),
                    span: s,
                })),
                span: s,
            })),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("slice pattern evaluates");
        assert!(matches!(result, BuildValue::Int(4)));
    }

    #[test]
    fn builtin_len_and_min_work() {
        let s = span();
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Call {
                callee: Box::new(Expr::Identifier(Identifier {
                    name: "len".into(),
                    span: s,
                })),
                type_args: Vec::new(),
                args: vec![Expr::ArrayLiteral(
                    vec![
                        Expr::Literal(Literal::Int(1, s)),
                        Expr::Literal(Literal::Int(2, s)),
                    ],
                    s,
                )],
                span: s,
            }),
            right: Box::new(Expr::Call {
                callee: Box::new(Expr::Identifier(Identifier {
                    name: "min".into(),
                    span: s,
                })),
                type_args: Vec::new(),
                args: vec![
                    Expr::Literal(Literal::Int(5, s)),
                    Expr::Literal(Literal::Int(3, s)),
                ],
                span: s,
            }),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(empty_snapshot());
        let result = interpreter.eval_expr(&expr).expect("builtins evaluate");
        assert!(matches!(result, BuildValue::Int(5)));
    }

    #[test]
    fn channel_send_recv_roundtrip() {
        let s = span();
        let mut snapshot = empty_snapshot();
        snapshot.enum_variants.insert(
            "Ok".into(),
            BuildEnumVariant {
                enum_name: "Result".into(),
                variant_index: 0,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "Err".into(),
            BuildEnumVariant {
                enum_name: "Result".into(),
                variant_index: 1,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "Some".into(),
            BuildEnumVariant {
                enum_name: "Option".into(),
                variant_index: 0,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "None".into(),
            BuildEnumVariant {
                enum_name: "Option".into(),
                variant_index: 1,
                fields: 0,
            },
        );
        let block = Block {
            statements: vec![Statement::Let(crate::language::ast::LetStmt {
                pattern: Pattern::Tuple(vec![Pattern::Identifier("tx".into(), s), Pattern::Identifier("rx".into(), s)], s),
                ty: None,
                value: Some(Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "channel".into(),
                        span: s,
                    })),
                    type_args: Vec::new(),
                    args: Vec::new(),
                    span: s,
                }),
                mutability: Mutability::Immutable,
                span: s,
            }),
            Statement::Expr(crate::language::ast::ExprStmt {
                expr: Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "send".into(),
                        span: s,
                    })),
                    type_args: Vec::new(),
                    args: vec![
                        Expr::Identifier(Identifier {
                            name: "tx".into(),
                            span: s,
                        }),
                        Expr::Literal(Literal::Int(42, s)),
                    ],
                    span: s,
                },
            })],
            tail: Some(Box::new(Expr::Match(MatchExpr {
                expr: Box::new(Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "recv".into(),
                        span: s,
                    })),
                    type_args: Vec::new(),
                    args: vec![Expr::Identifier(Identifier {
                        name: "rx".into(),
                        span: s,
                    })],
                    span: s,
                }),
                arms: vec![
                    MatchArmExpr {
                        pattern: Pattern::EnumVariant {
                            enum_name: Some("Option".into()),
                            variant: "Some".into(),
                            bindings: vec![Pattern::Identifier("v".into(), s)],
                        },
                        guard: None,
                        value: Expr::Identifier(Identifier {
                            name: "v".into(),
                            span: s,
                        }),
                    },
                    MatchArmExpr {
                        pattern: Pattern::EnumVariant {
                            enum_name: Some("Option".into()),
                            variant: "None".into(),
                            bindings: vec![],
                        },
                        guard: None,
                        value: Expr::Literal(Literal::Int(-1, s)),
                    },
                ],
                span: s,
            }))),
            span: s,
        };

        let result = BuildInterpreter::new(snapshot)
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("channel roundtrip");
        assert!(matches!(result, BuildValue::Int(42)));
    }
    #[test]
    fn match_enum_variant_extracts_value() {
        let s = span();
        let mut snapshot = empty_snapshot();
        snapshot.enum_variants.insert(
            "Ok".into(),
            BuildEnumVariant {
                enum_name: "Result".into(),
                variant_index: 0,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "Err".into(),
            BuildEnumVariant {
                enum_name: "Result".into(),
                variant_index: 1,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "Some".into(),
            BuildEnumVariant {
                enum_name: "Option".into(),
                variant_index: 0,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "None".into(),
            BuildEnumVariant {
                enum_name: "Option".into(),
                variant_index: 1,
                fields: 0,
            },
        );
        let expr = Expr::Match(MatchExpr {
            expr: Box::new(Expr::EnumLiteral {
                enum_name: Some("Option".into()),
                variant: "Some".into(),
                values: vec![Expr::Literal(Literal::Int(10, s))],
                span: s,
            }),
            arms: vec![
                MatchArmExpr {
                    pattern: Pattern::EnumVariant {
                        enum_name: Some("Option".into()),
                        variant: "Some".into(),
                        bindings: vec![Pattern::Identifier("x".into(), s)],
                    },
                    guard: None,
                    value: Expr::Identifier(Identifier {
                        name: "x".into(),
                        span: s,
                    }),
                },
                MatchArmExpr {
                    pattern: Pattern::EnumVariant {
                        enum_name: Some("Option".into()),
                        variant: "None".into(),
                        bindings: vec![],
                    },
                    guard: None,
                    value: Expr::Literal(Literal::Int(0, s)),
                },
            ],
            span: s,
        });
        let mut interpreter = BuildInterpreter::new(snapshot);
        let result = interpreter.eval_expr(&expr).expect("match evaluates");
        assert!(matches!(result, BuildValue::Int(10)));
    }

    #[test]
    fn move_consumes_identifier() {
        let s = span();
        let mut scope = BuildScope {
            bindings: HashMap::new(),
        };
        scope.bindings.insert(
            "x".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Int(1))),
                mutable: true,
                borrowed_mut: false,
                borrowed_shared: 0,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let first = interpreter
            .eval_expr(&Expr::Move {
                expr: Box::new(Expr::Identifier(Identifier { name: "x".into(), span: s })),
                span: s,
            })
            .expect("first move should succeed");
        assert!(matches!(first, BuildValue::Int(1)));
        let second = interpreter.eval_expr(&Expr::Identifier(Identifier { name: "x".into(), span: s }));
        assert!(second.is_err(), "moved identifier should error");
    }

    #[test]
    fn mutable_borrow_blocks_second() {
        let s = span();
        let mut scope = BuildScope {
            bindings: HashMap::new(),
        };
        scope.bindings.insert(
            "y".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Int(5))),
                mutable: true,
                borrowed_mut: false,
                borrowed_shared: 0,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let first = interpreter.eval_expr(&Expr::Reference {
            mutable: true,
            expr: Box::new(Expr::Identifier(Identifier { name: "y".into(), span: s })),
            span: s,
        });
        assert!(first.is_ok());
        let second = interpreter.eval_expr(&Expr::Reference {
            mutable: true,
            expr: Box::new(Expr::Identifier(Identifier { name: "y".into(), span: s })),
            span: s,
        });
        assert!(second.is_err(), "second mutable borrow should fail");
    }

    #[test]
    fn try_propagate_handles_result() {
        let s = span();
        let mut snapshot = empty_snapshot();
        snapshot.enum_variants.insert(
            "Ok".into(),
            BuildEnumVariant {
                enum_name: "Result".into(),
                variant_index: 0,
                fields: 1,
            },
        );
        snapshot.enum_variants.insert(
            "Err".into(),
            BuildEnumVariant {
                enum_name: "Result".into(),
                variant_index: 1,
                fields: 1,
            },
        );
        let ok_expr = Expr::TryPropagate {
            expr: Box::new(Expr::EnumLiteral {
                enum_name: Some("Result".into()),
                variant: "Ok".into(),
                values: vec![Expr::Literal(Literal::Int(9, s))],
                span: s,
            }),
            span: s,
        };
        let mut interpreter = BuildInterpreter::new(snapshot.clone());
        let ok_value = interpreter.eval_expr(&ok_expr).expect("Ok should propagate");
        assert!(matches!(ok_value, BuildValue::Int(9)));

        let err_expr = Expr::TryPropagate {
            expr: Box::new(Expr::EnumLiteral {
                enum_name: Some("Result".into()),
                variant: "Err".into(),
                values: vec![Expr::Literal(Literal::String("oops".into(), s))],
                span: s,
            }),
            span: s,
        };
        let err_result = BuildInterpreter::new(snapshot).eval_expr(&err_expr);
        assert!(err_result.is_err(), "Err should propagate as error");
    }
}
