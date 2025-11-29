use crate::language::ast::{
    AssignStmt, BinaryOp, Block, CaptureMode, CapturedVar, ClosureBody, Expr, FormatSegment,
    FormatStringLiteral, FunctionDef, FunctionParam, Identifier, IfCondition, IfExpr, Literal,
    MatchExpr, Pattern, RangeExpr, Statement, StructLiteralField, StructLiteralKind, UnaryOp,
};
use crate::language::span::Span;
use crate::language::types::{Mutability, TypeAnnotation, TypeExpr};
use crate::runtime::platform::std_disabled_message;
use crate::runtime::value::{FormatRuntimeSegmentGeneric, FormatTemplateValueGeneric};
use std::collections::HashSet;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::thread;
use std::time::Duration;

fn closure_body_span(body: &ClosureBody) -> Span {
    match body {
        ClosureBody::Block(block) => block.span,
        ClosureBody::Expr(expr) => expr.span,
    }
}

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
    Iterator(BuildIterator),
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
    JoinHandle(BuildJoinHandle),
    Task(Box<BuildTask>),
    Reference(BuildReference),
    Closure(BuildClosure),
    Moved,
}

#[derive(Clone, Debug)]
pub struct BuildDropRecord {
    pub binding: String,
    pub type_name: String,
}

#[derive(Clone, Debug)]
pub enum BuildCleanup {
    Defer(Expr),
    Drop(BuildDropRecord),
}

#[derive(Clone, Debug)]
pub struct BuildReference {
    pub cell: Arc<Mutex<BuildValue>>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct BuildIterator {
    pub items: Arc<Mutex<Vec<BuildValue>>>,
    pub index: Arc<Mutex<usize>>,
}

impl BuildIterator {
    pub fn from_items(items: Vec<BuildValue>) -> Self {
        Self {
            items: Arc::new(Mutex::new(items)),
            index: Arc::new(Mutex::new(0)),
        }
    }

    pub fn next(&self) -> Option<BuildValue> {
        let mut idx_guard = self.index.lock().unwrap();
        let items_guard = self.items.lock().unwrap();
        if *idx_guard >= items_guard.len() {
            return None;
        }
        let value = items_guard.get(*idx_guard).cloned();
        *idx_guard += 1;
        value
    }
}

#[derive(Clone, Debug)]
pub struct BuildCaptured {
    pub name: String,
    pub mutable: bool,
    pub mode: CaptureMode,
    pub value: BuildValue,
    pub ty: Option<TypeExpr>,
    pub origin: Span,
}

#[derive(Clone, Debug)]
pub struct BuildClosure {
    pub params: Vec<FunctionParam>,
    pub body: ClosureBody,
    pub ret: Option<TypeAnnotation>,
    pub captures: Vec<BuildCaptured>,
    #[allow(dead_code)]
    pub origin: Span,
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
            BuildValue::Iterator(_) => "iterator",
            BuildValue::DeferredCall { .. } => "function call",
            BuildValue::JoinHandle(_) => "join handle",
            BuildValue::Task(_) => "task",
            BuildValue::Reference(_) => "reference",
            BuildValue::Closure(_) => "closure",
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
    pub borrowed_shared_names: HashSet<String>,
    pub origin: Span,
    pub last_move: Option<Span>,
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
    pub functions: HashMap<BuildFunctionKey, BuildFunction>,
    pub struct_fields: HashMap<String, Vec<String>>,
    pub next_channel_id: u64,
    pub cleanup_stack: Vec<Vec<BuildCleanup>>,
    pub clock_ms: i128,
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
pub struct BuildFunction {
    pub def: FunctionDef,
    pub receiver: Option<String>,
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct BuildFunctionKey {
    pub name: String,
    pub receiver: Option<String>,
}

#[derive(Clone, Debug)]
pub enum BuildJoinState {
    Pending(Arc<Mutex<Option<thread::JoinHandle<Result<BuildEvaluation, String>>>>>),
    Ready(Box<BuildEvaluation>),
}

#[derive(Clone, Debug)]
pub struct BuildJoinHandle {
    pub(crate) state: BuildJoinState,
}

#[derive(Clone, Debug)]
pub struct BuildTask {
    pub result: BuildValue,
}

impl BuildJoinHandle {
    fn new(handle: thread::JoinHandle<Result<BuildEvaluation, String>>) -> Self {
        Self {
            state: BuildJoinState::Pending(Arc::new(Mutex::new(Some(handle)))),
        }
    }

    pub fn ready(evaluation: BuildEvaluation) -> Self {
        Self {
            state: BuildJoinState::Ready(Box::new(evaluation)),
        }
    }

    pub fn into_outcome(
        self,
    ) -> Result<BuildJoinOutcome, String> {
        match self.state {
            BuildJoinState::Pending(handle) => {
                let thread = handle
                    .lock()
                    .unwrap()
                    .take()
                    .ok_or_else(|| "join handle already taken".to_string())?;
                Ok(BuildJoinOutcome::Thread(thread))
            }
            BuildJoinState::Ready(eval) => Ok(BuildJoinOutcome::Ready(*eval)),
        }
    }
}

#[derive(Debug)]
pub enum BuildJoinOutcome {
    Thread(thread::JoinHandle<Result<BuildEvaluation, String>>),
    Ready(BuildEvaluation),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct BuildInterpreter {
    scopes: Vec<BuildScope>,
    effects: Vec<BuildEffect>,
    enum_variants: HashMap<String, BuildEnumVariant>,
    functions: HashMap<BuildFunctionKey, BuildFunction>,
    struct_fields: HashMap<String, Vec<String>>,
    next_channel_id: u64,
    drop_impls: HashMap<String, BuildFunctionKey>,
    active_mut_borrows: HashMap<String, Vec<BorrowMark>>,
    borrow_frames: Vec<Vec<BorrowMark>>,
    captured_borrows: HashSet<String>,
    cleanup_stack: Vec<Vec<BuildCleanup>>,
    suppress_drop_schedule: bool,
    clock_ms: i128,
}

#[derive(Clone, Debug)]
struct BorrowMark {
    name: String,
    kind: BorrowKind,
    borrower: Option<String>,
    span: Span,
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
    FsExists { path: String, exists: bool },
    FsRead { path: String, result: Result<String, String> },
    FsWrite {
        path: String,
        contents: String,
        result: Result<(), String>,
    },
    NowMs { value: i128 },
    SleepMs { millis: i128 },
}

#[derive(Clone, Debug)]
pub struct BuildChannelSender {
    pub id: u64,
    pub(crate) inner: Arc<(Mutex<BuildChannelState>, Condvar)>,
}

#[derive(Clone, Debug)]
pub struct BuildChannelReceiver {
    pub id: u64,
    pub(crate) inner: Arc<(Mutex<BuildChannelState>, Condvar)>,
}

#[derive(Debug)]
pub(crate) struct BuildChannelState {
    pub queue: VecDeque<BuildValue>,
    pub closed: bool,
}

fn require_std_builtins(name: &str) -> Result<(), String> {
    if cfg!(feature = "std-builtins") {
        Ok(())
    } else {
        Err(std_disabled_message(name))
    }
}

fn build_values_equal(left: &BuildValue, right: &BuildValue) -> bool {
    match (left, right) {
        (BuildValue::Int(a), BuildValue::Int(b)) => a == b,
        (BuildValue::Float(a), BuildValue::Float(b)) => (*a - *b).abs() < f64::EPSILON,
        (BuildValue::Bool(a), BuildValue::Bool(b)) => a == b,
        (BuildValue::String(a), BuildValue::String(b)) => a == b,
        (BuildValue::Unit, BuildValue::Unit) => true,
        (
            BuildValue::Enum {
                enum_name: a_name,
                variant: a_variant,
                values: a_values,
                ..
            },
            BuildValue::Enum {
                enum_name: b_name,
                variant: b_variant,
                values: b_values,
                ..
            },
        ) => {
            a_name == b_name
                && a_variant == b_variant
                && a_values.len() == b_values.len()
                && a_values
                    .iter()
                    .zip(b_values.iter())
                    .all(|(x, y)| build_values_equal(x, y))
        }
        (BuildValue::Tuple(a), BuildValue::Tuple(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| build_values_equal(x, y))
        }
        _ => false,
    }
}

impl BuildChannelSender {
    pub(crate) fn new(id: u64, inner: Arc<(Mutex<BuildChannelState>, Condvar)>) -> Self {
        Self { id, inner }
    }

    pub(crate) fn snapshot(&self) -> (VecDeque<BuildValue>, bool) {
        let (lock, _) = &*self.inner;
        let guard = lock.lock().unwrap();
        (guard.queue.clone(), guard.closed)
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
    pub(crate) fn new(id: u64, inner: Arc<(Mutex<BuildChannelState>, Condvar)>) -> Self {
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

    fn recv_timeout(&self, millis: i64) -> Option<BuildValue> {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        let duration = if millis < 0 {
            Duration::from_millis(0)
        } else {
            Duration::from_millis(millis as u64)
        };
        loop {
            if let Some(v) = guard.queue.pop_front() {
                return Some(v);
            }
            if guard.closed {
                return None;
            }
            let (g, timeout) = cv.wait_timeout(guard, duration).unwrap();
            guard = g;
            if timeout.timed_out() {
                return None;
            }
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
        let mut drop_impls = HashMap::new();
        for (key, func) in &snapshot.functions {
            if func.def.name == "drop" {
                if let Some(receiver) = key.receiver.clone() {
                    drop_impls.insert(receiver, key.clone());
                }
            }
        }
        Self {
            scopes: snapshot.scopes,
            effects: Vec::new(),
            enum_variants: snapshot.enum_variants,
            functions: snapshot.functions,
            struct_fields: snapshot.struct_fields,
            next_channel_id: snapshot.next_channel_id,
            drop_impls,
            active_mut_borrows: HashMap::new(),
            borrow_frames: vec![Vec::new(); scope_frames],
            captured_borrows: HashSet::new(),
            cleanup_stack: if snapshot.cleanup_stack.is_empty() {
                vec![Vec::new(); scope_frames]
            } else {
                snapshot.cleanup_stack
            },
            suppress_drop_schedule: false,
            clock_ms: snapshot.clock_ms,
        }
    }

    fn clone_for_spawn(&self) -> Self {
        let mut clone = self.clone();
        clone.effects = Vec::new();
        clone.active_mut_borrows = HashMap::new();
        clone.borrow_frames = vec![Vec::new(); clone.scopes.len()];
        clone.cleanup_stack = self.cleanup_stack.clone();
        clone.captured_borrows = self.captured_borrows.clone();
        clone.suppress_drop_schedule = false;
        clone.clock_ms = self.clock_ms;
        clone
    }

    fn first_mut_borrow(&self, name: &str) -> Option<BorrowMark> {
        self.active_mut_borrows
            .get(name)
            .and_then(|records| records.first())
            .cloned()
    }

    fn first_shared_borrow(&self, name: &str) -> Option<BorrowMark> {
        for frame in self.borrow_frames.iter().rev() {
            if let Some(mark) = frame
                .iter()
                .find(|mark| mark.name == name && mark.kind == BorrowKind::Shared)
            {
                return Some(mark.clone());
            }
        }
        None
    }

    fn format_borrow_conflict(&self, name: &str, origin: Span, action: &str) -> String {
        let mut message = format!("`{}` {}", name, action);
        if let Some(first) = self.first_mut_borrow(name) {
            let borrower = first
                .borrower
                .as_deref()
                .map(str::to_string)
                .unwrap_or_else(|| name.to_string());
            message.push_str(&format!(
                "\n  first mutable borrow by `{}` at {}..{}",
                borrower, first.span.start, first.span.end
            ));
        }
        message.push_str(&format!(
            "\n  binding declared at {}..{}",
            origin.start, origin.end
        ));
        message.push_str("\n  help: end the borrow or clone before retrying");
        message
    }

    fn format_shared_borrow_conflict(&self, name: &str, origin: Span, action: &str) -> String {
        let mut message = format!("`{}` {}", name, action);
        if let Some(first) = self.first_shared_borrow(name) {
            let borrower = first
                .borrower
                .as_deref()
                .map(str::to_string)
                .unwrap_or_else(|| name.to_string());
            message.push_str(&format!(
                "\n  first shared borrow by `{}` at {}..{}",
                borrower, first.span.start, first.span.end
            ));
        }
        message.push_str(&format!(
            "\n  binding declared at {}..{}",
            origin.start, origin.end
        ));
        message.push_str("\n  help: wait for borrows to end or create a copy before moving");
        message
    }

    fn format_moved_use(&self, name: &str, last_move: Option<Span>, action: &str) -> String {
        let mut message = format!("`{}` {}", name, action);
        if let Some(span) = last_move {
            message.push_str(&format!("\n  move occurred at {}..{}", span.start, span.end));
        }
        message.push_str("\n  help: borrow a reference instead of moving if reuse is needed");
        message
    }

    // Used heavily in tests; keep available but silence dead-code warning in normal builds.
    #[cfg_attr(not(test), allow(dead_code))]
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
        let (cell, borrowed_mut, origin, last_move) = {
            let binding = self
                .find_binding_mut(name)
                .ok_or_else(|| format!("Unknown identifier `{}` in build spawn", name))?;
            (
                binding.cell.clone(),
                binding.borrowed_mut,
                binding.origin,
                binding.last_move,
            )
        };
        if borrowed_mut {
            return Err(self.format_borrow_conflict(
                name,
                origin,
                "is mutably borrowed and cannot be used here",
            ));
        }
        let guard = cell.lock().unwrap();
        if let BuildValue::Moved = *guard {
            return Err(self.format_moved_use(
                name,
                last_move,
                "was moved and can no longer be used here",
            ));
        }
        Ok(guard.clone())
    }

    fn take_binding_value(&mut self, name: &str, move_span: Span) -> Result<BuildValue, String> {
        let (cell, borrowed_mut, borrowed_shared, origin, last_move) = {
            let binding = self
                .find_binding_mut(name)
                .ok_or_else(|| format!("Unknown identifier `{}` for move", name))?;
            (
                binding.cell.clone(),
                binding.borrowed_mut,
                binding.borrowed_shared,
                binding.origin,
                binding.last_move,
            )
        };
        if borrowed_mut || borrowed_shared > 0 {
            if borrowed_mut {
                return Err(self.format_borrow_conflict(
                    name,
                    origin,
                    "cannot be moved because it is mutably borrowed",
                ));
            }
            return Err(self.format_shared_borrow_conflict(
                name,
                origin,
                "cannot be moved because it has active shared borrows",
            ));
        }
        let mut cell = cell.lock().unwrap();
        if let BuildValue::Moved = *cell {
            return Err(self.format_moved_use(
                name,
                last_move,
                "was moved and cannot be moved again",
            ));
        }
        let value = std::mem::replace(&mut *cell, BuildValue::Moved);
        if let Some(binding) = self.find_binding_mut(name) {
            binding.last_move = Some(move_span);
        }
        Ok(value)
    }

    fn capture_reference_by_name(
        &mut self,
        name: &str,
        mutable: bool,
        span: Span,
    ) -> Result<BuildReference, String> {
        let (cell, mutable_flag, borrowed_mut, borrowed_shared, origin_span) = {
            let binding = self
                .find_binding_mut(name)
                .ok_or_else(|| format!("Unknown identifier `{}` for reference", name))?;
            (
                binding.cell.clone(),
                binding.mutable,
                binding.borrowed_mut,
                binding.borrowed_shared,
                binding.origin,
            )
        };
        if mutable && !mutable_flag {
            return Err(format!("Identifier `{}` is immutable", name));
        }
        if mutable && borrowed_mut {
            return Err(self.format_borrow_conflict(
                name,
                origin_span,
                "is already mutably borrowed",
            ));
        }
        if mutable && borrowed_shared > 0 {
            return Err(self.format_shared_borrow_conflict(
                name,
                origin_span,
                "cannot be mutably borrowed while shared borrows are active",
            ));
        }
        if mutable {
            self.begin_mut_borrow(name, span, Some(name.to_string()))?;
            if let Some(binding) = self.find_binding_mut(name) {
                binding.borrowed_mut = true;
            }
        } else {
            self.begin_shared_borrow(name, name, span)?;
        }
        self.captured_borrows.insert(name.to_string());
        Ok(BuildReference { cell, mutable })
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
                            ));
                        }
                    };
                    let value = self.eval_expr_mut(&entry.value)?;
                    map.insert(key_str, value);
                }
                Ok(BuildValue::Map(map))
            }
            Expr::Binary {
                op, left, right, ..
            } => {
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
            Expr::Index { base, index, .. } => {
                let base_value = self.eval_expr_mut(base)?;
                let index_value = self.eval_expr_mut(index)?;
                self.eval_index(base_value, index_value)
            }
            Expr::StructLiteral { name, fields, .. } => self.eval_struct_literal(name, fields),
            Expr::EnumLiteral {
                enum_name,
                variant,
                values,
                ..
            } => self.eval_enum_literal(enum_name.as_ref(), variant, values),
            Expr::Block(block) => match self.eval_block(block)? {
                Flow::Value(v) => Ok(v),
                Flow::Return(v) => Ok(v),
                Flow::Break | Flow::Continue => {
                    Err("break/continue not allowed in expression context for build spawn".into())
                }
            },
            Expr::Match(match_expr) => self.eval_match(match_expr),
            Expr::Call {
                callee,
                args,
                type_args,
                ..
            } => self.eval_call(callee, type_args, args),
            Expr::FieldAccess { base, field, .. } => self.eval_field_access(base, field),
            Expr::Closure {
                params,
                body,
                ret,
                captures,
                ..
            } => self.eval_closure_literal(params, body, ret, captures),
            Expr::Reference { expr, mutable, .. } => match expr.as_ref() {
                Expr::Identifier(ident) => {
                    let (cell, mutable_flag, borrowed_mut, borrowed_shared, origin_span) = {
                        let binding = self.find_binding_mut(&ident.name).ok_or_else(|| {
                            format!("Unknown identifier `{}` for reference", ident.name)
                        })?;
                        (
                            binding.cell.clone(),
                            binding.mutable,
                            binding.borrowed_mut,
                            binding.borrowed_shared,
                            binding.origin,
                        )
                    };
                    if *mutable && !mutable_flag {
                        return Err(format!("Identifier `{}` is immutable", ident.name));
                    }
                    if *mutable && borrowed_mut {
                        return Err(self.format_borrow_conflict(
                            &ident.name,
                            origin_span,
                            "is already mutably borrowed",
                        ));
                    }
                    if *mutable && borrowed_shared > 0 {
                        return Err(self.format_shared_borrow_conflict(
                            &ident.name,
                            origin_span,
                            "cannot be mutably borrowed while shared borrows are active",
                        ));
                    }
                    if *mutable {
                        self.begin_mut_borrow(&ident.name, ident.span, None)?;
                        if let Some(binding) = self.find_binding_mut(&ident.name) {
                            binding.borrowed_mut = true;
                        }
                    } else {
                        self.begin_shared_borrow(&ident.name, &ident.name, ident.span)?;
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
            },
            Expr::Deref { expr, .. } => match self.eval_expr_mut(expr)? {
                BuildValue::Reference(reference) => Ok(reference.cell.lock().unwrap().clone()),
                other => Err(format!("deref not supported for {}", other.kind())),
            },
            Expr::Try { block, .. } => self.eval_try(block),
            Expr::TryPropagate { expr, .. } => self.eval_try_propagate(expr),
            Expr::Move { expr, .. } => self.eval_move(expr),
            Expr::Async { block, .. } => {
                // Execute synchronously for now; async scheduler will make this cooperative.
                match self.eval_block(block)? {
                    Flow::Value(value) => Ok(BuildValue::Task(Box::new(BuildTask { result: value }))),
                    Flow::Return(value) => Ok(BuildValue::Task(Box::new(BuildTask { result: value }))),
                    Flow::Break | Flow::Continue => {
                        Err("control flow not allowed in async expression".into())
                    }
                }
            }
            Expr::Await { expr, .. } => match self.eval_expr_mut(expr)? {
                BuildValue::Task(task) => Ok(task.result.clone()),
                other => Err(format!("`await` expects a Task, found {}", other.kind())),
            },
            Expr::Spawn { expr, .. } => {
                require_std_builtins("spawn")?;
                let child = self.clone_for_spawn();
                let expr_clone = expr.clone();
                let handle = thread::spawn(move || {
                    let interp = child;
                    interp.eval_with_effects(&expr_clone)
                });
                Ok(BuildValue::JoinHandle(BuildJoinHandle::new(handle)))
            }
            Expr::MacroCall { name, .. } => Err(format!(
                "macro `{}` cannot be evaluated in build mode before expansion",
                name.name
            )),
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Result<BuildValue, String> {
        match lit {
            Literal::Int(v, _) => Ok(BuildValue::Int(*v)),
            Literal::Float(v, _) => Ok(BuildValue::Float(*v)),
            Literal::Bool(v, _) => Ok(BuildValue::Bool(*v)),
            Literal::String(v, _) => Ok(BuildValue::String(v.clone())),
            Literal::Rune(v, _) => Ok(BuildValue::Int(*v as i128)),
        }
    }

    fn eval_format_string(&mut self, literal: &FormatStringLiteral) -> Result<BuildValue, String> {
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
                    self.pop_scope()?;
                    return Ok(flow);
                }
            };
        }
        let result = if let Some(tail) = &block.tail {
            self.eval_expr_mut(tail)?
        } else {
            last_value
        };
        self.pop_scope()?;
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
            Statement::MacroSemi(expr) => {
                let _ = self.eval_expr_mut(&expr.node)?;
                Ok(Flow::Value(BuildValue::Unit))
            }
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
            Statement::Defer(defer_stmt) => {
                if let Some(frame) = self.cleanup_stack.last_mut() {
                    frame.push(BuildCleanup::Defer(defer_stmt.expr.clone()));
                }
                Ok(Flow::Value(BuildValue::Unit))
            }
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
            BuildValue::Range {
                start,
                end,
                inclusive,
            } => {
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
                            self.pop_scope()?;
                        }
                        Flow::Break => {
                            self.pop_scope()?;
                            break;
                        }
                        Flow::Continue => {
                            self.pop_scope()?;
                            continue;
                        }
                        Flow::Return(v) => {
                            self.pop_scope()?;
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
                            self.pop_scope()?;
                        }
                        Flow::Break => {
                            self.pop_scope()?;
                            break;
                        }
                        Flow::Continue => {
                            self.pop_scope()?;
                            continue;
                        }
                        Flow::Return(v) => {
                            self.pop_scope()?;
                            return Ok(Flow::Return(v));
                        }
                    }
                }
                Ok(Flow::Value(BuildValue::Unit))
            }
            BuildValue::Map(entries) => {
                for (key, value) in entries {
                    self.push_scope();
                    self.bind_pattern(
                        &Pattern::Identifier(stmt.binding.clone(), Span::new(0, 0)),
                        BuildValue::Tuple(vec![BuildValue::String(key), value]),
                        Mutability::Mutable,
                    )?;
                    let flow = self.eval_block(&stmt.body)?;
                    match flow {
                        Flow::Value(_) => {
                            self.pop_scope()?;
                        }
                        Flow::Break => {
                            self.pop_scope()?;
                            break;
                        }
                        Flow::Continue => {
                            self.pop_scope()?;
                            continue;
                        }
                        Flow::Return(v) => {
                            self.pop_scope()?;
                            return Ok(Flow::Return(v));
                        }
                    }
                }
                Ok(Flow::Value(BuildValue::Unit))
            }
            BuildValue::Iterator(iter) => {
                while let Some(item) = iter.next() {
                    self.push_scope();
                    self.bind_pattern(
                        &Pattern::Identifier(stmt.binding.clone(), Span::new(0, 0)),
                        item,
                        Mutability::Mutable,
                    )?;
                    let flow = self.eval_block(&stmt.body)?;
                    match flow {
                        Flow::Value(_) => {
                            self.pop_scope()?;
                        }
                        Flow::Break => {
                            self.pop_scope()?;
                            break;
                        }
                        Flow::Continue => {
                            self.pop_scope()?;
                            continue;
                        }
                        Flow::Return(v) => {
                            self.pop_scope()?;
                            return Ok(Flow::Return(v));
                        }
                    }
                }
                Ok(Flow::Value(BuildValue::Unit))
            }
            other => Err(format!(
                "for loops in build spawn require range, slice, map, or iterator, found {}",
                other.kind()
            )),
        }
    }

    fn eval_assign(&mut self, assign: &AssignStmt) -> Result<(), String> {
        match &assign.target {
            Expr::Identifier(Identifier { name, .. }) => {
                let value = self.eval_expr_mut(&assign.value)?;
                let (cell, mutable_flag, borrowed_mut, borrowed_shared, origin_span) = {
                    let binding = self
                        .find_binding_mut(name)
                        .ok_or_else(|| format!("Unknown identifier `{}` in assignment", name))?;
                    (
                        binding.cell.clone(),
                        binding.mutable,
                        binding.borrowed_mut,
                        binding.borrowed_shared,
                        binding.origin,
                    )
                };
                if borrowed_mut {
                    return Err(self.format_borrow_conflict(
                        name,
                        origin_span,
                        "is mutably borrowed and cannot be assigned",
                    ));
                }
                if borrowed_shared > 0 {
                    return Err(self.format_shared_borrow_conflict(
                        name,
                        origin_span,
                        "cannot be assigned while shared borrows are active",
                    ));
                }
                if !mutable_flag {
                    return Err(format!("Identifier `{}` is immutable", name));
                }
                *cell.lock().unwrap() = value;
                Ok(())
            }
            Expr::Deref { expr, .. } => {
                let value = self.eval_expr_mut(&assign.value)?;
                match self.eval_expr_mut(expr)? {
                    BuildValue::Reference(reference) => {
                        if !reference.mutable {
                            return Err("Cannot assign through immutable reference".into());
                        }
                        *reference.cell.lock().unwrap() = value;
                        Ok(())
                    }
                    other => Err(format!(
                        "Cannot assign through non-reference value `{}`",
                        other.kind()
                    )),
                }
            }
            Expr::Index { base, index, .. } => {
                let value = self.eval_expr_mut(&assign.value)?;
                let index_value = self.eval_expr_mut(index)?;
                match base.as_ref() {
                    Expr::Identifier(Identifier { name, .. }) => {
                        let (cell, mutable_flag, borrowed_mut, borrowed_shared, origin_span) = {
                            let binding = self.find_binding_mut(name).ok_or_else(|| {
                                format!("Unknown identifier `{}` in assignment", name)
                            })?;
                            (
                                binding.cell.clone(),
                                binding.mutable,
                                binding.borrowed_mut,
                                binding.borrowed_shared,
                                binding.origin,
                            )
                        };
                        if !mutable_flag {
                            return Err(format!("Identifier `{}` is immutable", name));
                        }
                        if borrowed_mut {
                            return Err(self.format_borrow_conflict(
                                name,
                                origin_span,
                                "is mutably borrowed and cannot be assigned",
                            ));
                        }
                        if borrowed_shared > 0 {
                            return Err(self.format_shared_borrow_conflict(
                                name,
                                origin_span,
                                "cannot be assigned while shared borrows are active",
                            ));
                        }
                        let mut guard = cell.lock().unwrap();
                        self.assign_index_into_value(&mut *guard, index_value, value)
                    }
                    Expr::Deref { expr, .. } => match self.eval_expr_mut(expr)? {
                        BuildValue::Reference(reference) => {
                            if !reference.mutable {
                                return Err("Cannot assign through immutable reference".into());
                            }
                            let mut guard = reference.cell.lock().unwrap();
                            self.assign_index_into_value(&mut *guard, index_value, value)
                        }
                        other => Err(format!(
                            "Cannot assign through non-reference value `{}`",
                            other.kind()
                        )),
                    },
                    _ => Err("assignment target not supported in build spawn".into()),
                }
            }
            Expr::FieldAccess { base, field, .. } => {
                let value = self.eval_expr_mut(&assign.value)?;
                match base.as_ref() {
                    Expr::Identifier(Identifier { name, .. }) => {
                        let (cell, mutable_flag, borrowed_mut, borrowed_shared, origin_span) = {
                            let binding = self.find_binding_mut(name).ok_or_else(|| {
                                format!("Unknown identifier `{}` in assignment", name)
                            })?;
                            (
                                binding.cell.clone(),
                                binding.mutable,
                                binding.borrowed_mut,
                                binding.borrowed_shared,
                                binding.origin,
                            )
                        };
                        if !mutable_flag {
                            return Err(format!("Identifier `{}` is immutable", name));
                        }
                        if borrowed_mut {
                            return Err(self.format_borrow_conflict(
                                name,
                                origin_span,
                                "is mutably borrowed and cannot be assigned",
                            ));
                        }
                        if borrowed_shared > 0 {
                            return Err(self.format_shared_borrow_conflict(
                                name,
                                origin_span,
                                "cannot be assigned while shared borrows are active",
                            ));
                        }
                        let mut guard = cell.lock().unwrap();
                        match &mut *guard {
                            BuildValue::Struct { fields, .. } => {
                                if !fields.contains_key(field) {
                                    return Err(format!(
                                        "field `{}` not found in struct `{}`",
                                        field, name
                                    ));
                                }
                                fields.insert(field.clone(), value);
                                Ok(())
                            }
                            BuildValue::Map(entries) => {
                                entries.insert(field.clone(), value);
                                Ok(())
                            }
                            BuildValue::Tuple(items) => {
                                let idx = field.parse::<usize>().map_err(|_| {
                                    "tuple field access expects numeric index".to_string()
                                })?;
                                if let Some(slot) = items.get_mut(idx) {
                                    *slot = value;
                                    Ok(())
                                } else {
                                    Err(format!("tuple index {} out of bounds", idx))
                                }
                            }
                            other => Err(format!("Cannot assign field on {}", other.kind())),
                        }
                    }
                    _ => match self.eval_expr_mut(base)? {
                        BuildValue::Reference(reference) if reference.mutable => {
                            let mut guard = reference.cell.lock().unwrap();
                            match &mut *guard {
                                BuildValue::Struct { fields, .. } => {
                                    fields.insert(field.clone(), value);
                                    Ok(())
                                }
                                BuildValue::Map(entries) => {
                                    entries.insert(field.clone(), value);
                                    Ok(())
                                }
                                BuildValue::Tuple(items) => {
                                    let idx = field.parse::<usize>().map_err(|_| {
                                        "tuple field access expects numeric index".to_string()
                                    })?;
                                    if let Some(slot) = items.get_mut(idx) {
                                        *slot = value;
                                        Ok(())
                                    } else {
                                        Err(format!("tuple index {} out of bounds", idx))
                                    }
                                }
                                other => Err(format!("Cannot assign field on {}", other.kind())),
                            }
                        }
                        BuildValue::Reference(reference) => Err(format!(
                            "Cannot assign field through immutable reference to {}",
                            reference.cell.lock().unwrap().kind()
                        )),
                        BuildValue::Struct { .. } | BuildValue::Map(_) => {
                            Err("Cannot assign field on temporary value".into())
                        }
                        other => Err(format!("Cannot assign field on {}", other.kind())),
                    },
                }
            }
            _ => Err("assignment target not supported in build spawn".into()),
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
                Flow::Break | Flow::Continue => {
                    Err("break/continue not allowed in expression context for build spawn".into())
                }
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
                for StructLiteralField {
                    name: field_name,
                    value,
                } in named
                {
                    map.insert(field_name.clone(), self.eval_expr_mut(value)?);
                }
                Ok(BuildValue::Struct {
                    name: name.to_string(),
                    fields: map,
                })
            }
            StructLiteralKind::Positional(values) => {
                let order =
                    self.struct_fields.get(name).cloned().ok_or_else(|| {
                        format!("Unknown struct `{}` for positional literal", name)
                    })?;
                if order.len() != values.len() {
                    return Err(format!(
                        "Struct `{}` expects {} fields, got {}",
                        name,
                        order.len(),
                        values.len()
                    ));
                }
                let mut map = BTreeMap::new();
                for (field_name, value_expr) in order.into_iter().zip(values.iter()) {
                    map.insert(field_name, self.eval_expr_mut(value_expr)?);
                }
                Ok(BuildValue::Struct {
                    name: name.to_string(),
                    fields: map,
                })
            }
        }
    }

    fn eval_field_access(&mut self, base: &Expr, field: &str) -> Result<BuildValue, String> {
        let value = self.eval_expr_mut(base)?;
        match value {
            BuildValue::Struct { mut fields, .. } => fields
                .remove(field)
                .ok_or_else(|| format!("field `{}` not found in struct", field)),
            BuildValue::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                match inner {
                    BuildValue::Struct { mut fields, .. } => fields
                        .remove(field)
                        .ok_or_else(|| format!("field `{}` not found in struct", field)),
                    _ => Err(format!(
                        "field access not supported for {} in build spawn",
                        inner.kind()
                    )),
                }
            }
            BuildValue::Boxed(inner) => {
                match *inner {
                    BuildValue::Struct { mut fields, .. } => fields
                        .remove(field)
                        .ok_or_else(|| format!("field `{}` not found in struct", field)),
                    _ => Err(format!(
                        "field access not supported for {} in build spawn",
                        inner.kind()
                    )),
                }
            }
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
            (Some(name), Some(info)) if &info.enum_name == name => {
                (name.clone(), info.variant_index)
            }
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
                if let Ok(value) = self.load_identifier(name) {
                    if let Some(closure) = self.extract_closure(value.clone()) {
                        let mut evaluated_args = Vec::with_capacity(args.len());
                        for arg in args {
                            evaluated_args.push(self.eval_expr_mut(arg)?);
                        }
                        return self.call_closure_value(closure, evaluated_args);
                    }
                }
                self.eval_builtin_or_deferred(name, None, type_args, args)
            }
            Expr::FieldAccess { base, field, .. } => {
                let receiver = self.eval_expr_mut(base)?;
                self.eval_builtin_or_deferred(field, Some(receiver), type_args, args)
            }
            _ => {
                let callee_value = self.eval_expr_mut(callee)?;
                if let Some(closure) = self.extract_closure(callee_value) {
                    let mut evaluated_args = Vec::with_capacity(args.len());
                    for arg in args {
                        evaluated_args.push(self.eval_expr_mut(arg)?);
                    }
                    return self.call_closure_value(closure, evaluated_args);
                }
                Err("only identifier and method calls supported in build spawn interpreter".into())
            }
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

    fn extract_closure(&self, value: BuildValue) -> Option<BuildClosure> {
        match value {
            BuildValue::Closure(closure) => Some(closure),
            BuildValue::Reference(reference) => {
                let guard = reference.cell.lock().ok()?;
                self.extract_closure(guard.clone())
            }
            _ => None,
        }
    }

    fn call_closure_value(
        &mut self,
        closure: BuildClosure,
        args: Vec<BuildValue>,
    ) -> Result<BuildValue, String> {
        if closure.params.len() != args.len() {
            return Err(format!(
                "closure expects {} arguments, got {}",
                closure.params.len(),
                args.len()
            ));
        }
        self.push_scope();
        let result = (|| {
            for captured in closure.captures {
                let pattern = Pattern::Identifier(captured.name.clone(), Span::new(0, 0));
                self.bind_pattern(
                    &pattern,
                    captured.value.clone(),
                    if captured.mutable {
                        Mutability::Mutable
                    } else {
                        Mutability::Immutable
                    },
                )?;
            }
            for (param, value) in closure.params.iter().zip(args.into_iter()) {
                let pattern = Pattern::Identifier(param.name.clone(), param.span);
                self.bind_pattern(&pattern, value, param.mutability)?;
            }
            let result = match &closure.body {
                ClosureBody::Block(block) => self.eval_block(block)?,
                ClosureBody::Expr(expr) => Flow::Value(self.eval_expr_mut(expr.node.as_ref())?),
            };
            match result {
                Flow::Value(v) | Flow::Return(v) => Ok(v),
                Flow::Break | Flow::Continue => {
                    Err("Control flow cannot escape closure body in build mode".into())
                }
            }
        })();
        self.pop_scope()?;
        result
    }

    fn eval_move(&mut self, expr: &Expr) -> Result<BuildValue, String> {
        match expr {
            Expr::Identifier(ident) => self.take_binding_value(&ident.name, ident.span),
            _ => self.eval_expr_mut(expr),
        }
    }

    fn eval_closure_literal(
        &mut self,
        params: &[FunctionParam],
        body: &ClosureBody,
        ret: &Option<TypeAnnotation>,
        captures: &Arc<RwLock<Vec<CapturedVar>>>,
    ) -> Result<BuildValue, String> {
        let mut captured_values = Vec::new();
        for captured in captures.read().unwrap().iter() {
            let value = match captured.mode {
                CaptureMode::Move => self.take_binding_value(&captured.name, captured.span)?,
                CaptureMode::Reference { mutable } => {
                    BuildValue::Reference(
                        self.capture_reference_by_name(&captured.name, mutable, captured.span)?,
                    )
                }
            };
            captured_values.push(BuildCaptured {
                name: captured.name.clone(),
                mutable: captured.mutable,
                mode: captured.mode.clone(),
                value,
                ty: captured.ty.clone(),
                origin: captured.span,
            });
        }
        Ok(BuildValue::Closure(BuildClosure {
            params: params.to_vec(),
            body: body.clone(),
            ret: ret.clone(),
            captures: captured_values,
            origin: ret
                .as_ref()
                .map(|ann| ann.span)
                .unwrap_or_else(|| closure_body_span(body)),
        }))
    }

    fn eval_try(&mut self, block: &Block) -> Result<BuildValue, String> {
        match self.eval_block(block)? {
            Flow::Value(v) => Ok(v),
            Flow::Return(v) => Ok(v),
            Flow::Break | Flow::Continue => {
                Err("break/continue not allowed in try expression".into())
            }
        }
    }

    fn eval_try_propagate(&mut self, expr: &Expr) -> Result<BuildValue, String> {
        let value = self.eval_expr_mut(expr)?;
        match value {
            BuildValue::Enum {
                enum_name,
                variant,
                mut values,
                ..
            } if enum_name == "Result" && variant == "Ok" => {
                if values.len() == 1 {
                    Ok(values.pop().unwrap())
                } else {
                    Ok(BuildValue::Tuple(values))
                }
            }
            BuildValue::Enum {
                enum_name,
                variant,
                values,
                ..
            } if enum_name == "Result" && variant == "Err" => {
                Err(format!("try propagation yielded error {:?}", values))
            }
            other => Err(format!("? operator expects Result, found {}", other.kind())),
        }
    }

    fn eval_binary(
        &self,
        op: BinaryOp,
        left: BuildValue,
        right: BuildValue,
    ) -> Result<BuildValue, String> {
        match (left, right) {
            (BuildValue::Int(l), BuildValue::Int(r)) => {
                let value = match op {
                    BinaryOp::Add => BuildValue::Int(l + r),
                    BinaryOp::Sub => BuildValue::Int(l - r),
                    BinaryOp::Mul => BuildValue::Int(l * r),
                    BinaryOp::Div => BuildValue::Int(l / r),
                    BinaryOp::Rem => BuildValue::Int(l % r),
                    BinaryOp::BitAnd => BuildValue::Int(l & r),
                    BinaryOp::BitOr => BuildValue::Int(l | r),
                    BinaryOp::BitXor => BuildValue::Int(l ^ r),
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
            (BuildValue::Int(l), BuildValue::Float(r)) => {
                self.eval_binary(op, BuildValue::Float(l as f64), BuildValue::Float(r))
            }
            (BuildValue::Float(l), BuildValue::Int(r)) => {
                self.eval_binary(op, BuildValue::Float(l), BuildValue::Float(r as f64))
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
            (l, r) => {
                if op == BinaryOp::Eq || op == BinaryOp::NotEq {
                    let eq = self.values_equal(&l, &r)?;
                    return Ok(BuildValue::Bool(if op == BinaryOp::Eq { eq } else { !eq }));
                }
                Err(format!(
                    "Binary op {:?} not supported for {} and {}",
                    op,
                    l.kind(),
                    r.kind()
                ))
            }
        }
    }

    fn eval_unary(&self, op: UnaryOp, value: BuildValue) -> Result<BuildValue, String> {
        match (op, value) {
            (UnaryOp::Neg, BuildValue::Int(v)) => Ok(BuildValue::Int(-v)),
            (UnaryOp::Neg, BuildValue::Float(v)) => Ok(BuildValue::Float(-v)),
            (UnaryOp::Not, BuildValue::Bool(v)) => Ok(BuildValue::Bool(!v)),
            (_, other) => Err(format!(
                "Unary op {:?} not supported for {}",
                op,
                other.kind()
            )),
        }
    }

    fn values_equal(&self, left: &BuildValue, right: &BuildValue) -> Result<bool, String> {
        Ok(match (left, right) {
            (BuildValue::Unit, BuildValue::Unit) => true,
            (BuildValue::Bool(l), BuildValue::Bool(r)) => l == r,
            (BuildValue::Int(l), BuildValue::Int(r)) => l == r,
            (BuildValue::Float(l), BuildValue::Float(r)) => (*l - r).abs() < f64::EPSILON,
            (BuildValue::String(l), BuildValue::String(r)) => l == r,
            (
                BuildValue::Range {
                    start: ls,
                    end: le,
                    inclusive: li,
                },
                BuildValue::Range {
                    start: rs,
                    end: re,
                    inclusive: ri,
                },
            ) => ls == rs && le == re && li == ri,
            (BuildValue::Tuple(l), BuildValue::Tuple(r)) => {
                l.len() == r.len()
                    && l.iter()
                        .zip(r.iter())
                        .all(|(a, b)| self.values_equal(a, b).unwrap_or(false))
            }
            (BuildValue::Slice(l), BuildValue::Slice(r)) => {
                l.len() == r.len()
                    && l.iter()
                        .zip(r.iter())
                        .all(|(a, b)| self.values_equal(a, b).unwrap_or(false))
            }
            (
                BuildValue::Struct {
                    name: ln,
                    fields: lf,
                },
                BuildValue::Struct {
                    name: rn,
                    fields: rf,
                },
            ) => {
                ln == rn
                    && lf.len() == rf.len()
                    && lf.iter().all(|(k, v)| {
                        rf.get(k)
                            .map(|other| self.values_equal(v, other).unwrap_or(false))
                            .unwrap_or(false)
                    })
            }
            (BuildValue::Map(lm), BuildValue::Map(rm)) => {
                lm.len() == rm.len()
                    && lm.iter().all(|(k, v)| {
                        rm.get(k)
                            .map(|other| self.values_equal(v, other).unwrap_or(false))
                            .unwrap_or(false)
                    })
            }
            (
                BuildValue::Enum {
                    enum_name: le,
                    variant: lv,
                    values: lv_vals,
                    variant_index: l_idx,
                },
                BuildValue::Enum {
                    enum_name: re,
                    variant: rv,
                    values: rv_vals,
                    variant_index: r_idx,
                },
            ) => {
                le == re
                    && lv == rv
                    && l_idx == r_idx
                    && lv_vals.len() == rv_vals.len()
                    && lv_vals
                        .iter()
                        .zip(rv_vals.iter())
                        .all(|(a, b)| self.values_equal(a, b).unwrap_or(false))
            }
            (BuildValue::Reference(lref), BuildValue::Reference(rref)) => {
                let l = lref.cell.lock().unwrap().clone();
                let r = rref.cell.lock().unwrap().clone();
                self.values_equal(&l, &r)?
            }
            _ => false,
        })
    }

    fn find_binding_mut(&mut self, name: &str) -> Option<&mut BuildBinding> {
        for (_idx, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(binding) = scope.bindings.get_mut(name) {
                return Some(binding);
            }
        }
        None
    }

    fn begin_mut_borrow(
        &mut self,
        name: &str,
        span: Span,
        borrower: Option<String>,
    ) -> Result<(), String> {
        let origin_span = self
            .find_binding_mut(name)
            .map(|binding| binding.origin)
            .unwrap_or_else(|| Span::new(0, 0));
        if self.active_mut_borrows.get(name).is_some() {
            return Err(self.format_borrow_conflict(
                name,
                origin_span,
                "is already mutably borrowed",
            ));
        }
        if let Some(binding) = self.find_binding_mut(name) {
            if !binding.mutable {
                return Err(format!("Identifier `{}` is immutable", name));
            }
            if binding.borrowed_shared > 0 {
                return Err(self.format_shared_borrow_conflict(
                    name,
                    origin_span,
                    "cannot be mutably borrowed while shared borrows are active",
                ));
            }
            binding.borrowed_mut = true;
        }
        self.active_mut_borrows
            .entry(name.to_string())
            .or_default()
            .push(BorrowMark {
                name: name.to_string(),
                kind: BorrowKind::Mutable,
                borrower: borrower.clone(),
                span,
            });
        if let Some(frame) = self.borrow_frames.last_mut() {
            frame.push(BorrowMark {
                name: name.to_string(),
                kind: BorrowKind::Mutable,
                borrower,
                span,
            });
        }
        Ok(())
    }

    fn begin_shared_borrow(&mut self, name: &str, borrower: &str, span: Span) -> Result<(), String> {
        if self.active_mut_borrows.get(name).is_some() {
            let origin_span = self
                .find_binding_mut(name)
                .map(|binding| binding.origin)
                .unwrap_or_else(|| Span::new(0, 0));
            return Err(self.format_borrow_conflict(
                name,
                origin_span,
                "cannot be shared because it is mutably borrowed",
            ));
        }
        if let Some(binding) = self.find_binding_mut(name) {
            binding.borrowed_shared = binding.borrowed_shared.saturating_add(1);
            binding.borrowed_shared_names.insert(borrower.to_string());
        }
        if let Some(frame) = self.borrow_frames.last_mut() {
            frame.push(BorrowMark {
                name: name.to_string(),
                kind: BorrowKind::Shared,
                borrower: Some(borrower.to_string()),
                span,
            });
        }
        Ok(())
    }

    fn push_scope(&mut self) {
        self.scopes.push(BuildScope {
            bindings: HashMap::new(),
        });
        self.borrow_frames.push(Vec::new());
        self.cleanup_stack.push(Vec::new());
    }

    fn run_cleanups(&mut self) -> Result<(), String> {
        if let Some(cleanups) = self.cleanup_stack.last_mut() {
            let mut pending = Vec::new();
            while let Some(action) = cleanups.pop() {
                pending.push(action);
            }
            for action in pending {
                match action {
                    BuildCleanup::Defer(expr) => {
                        self.eval_expr_mut(&expr)?;
                    }
                    BuildCleanup::Drop(record) => self.run_drop(record)?,
                }
            }
        }
        Ok(())
    }

    fn run_drop(&mut self, record: BuildDropRecord) -> Result<(), String> {
        let Some(_key) = self.drop_impls.get(&record.type_name).cloned() else {
            return Ok(());
        };
        let Some(binding) = self.find_binding_mut(&record.binding) else {
            return Ok(());
        };
        let guard = binding.cell.lock().unwrap();
        if let BuildValue::Moved = *guard {
            return Ok(());
        }
        let receiver_value = guard.clone();
        let reference = BuildValue::Reference(BuildReference {
            cell: binding.cell.clone(),
            mutable: true,
        });
        drop(guard);
        let prev = self.suppress_drop_schedule;
        self.suppress_drop_schedule = true;
        let _ = self.call_user_function("drop", &Some(receiver_value), vec![reference], &[])?;
        self.suppress_drop_schedule = prev;
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<(), String> {
        self.run_cleanups()?;
        self.scopes.pop();
        if let Some(frame) = self.borrow_frames.pop() {
            for mark in frame {
                if self.captured_borrows.contains(&mark.name) {
                    continue;
                }
                match mark.kind {
                    BorrowKind::Mutable => {
                        if let Some(records) = self.active_mut_borrows.get_mut(&mark.name) {
                            if records.len() > 1 {
                                records.pop();
                            } else {
                                self.active_mut_borrows.remove(&mark.name);
                            }
                        }
                        if let Some(binding) = self.find_binding_mut(&mark.name) {
                            binding.borrowed_mut = false;
                        }
                    }
                    BorrowKind::Shared => {
                        if let Some(binding) = self.find_binding_mut(&mark.name) {
                            binding.borrowed_shared = binding.borrowed_shared.saturating_sub(1);
                            if let Some(borrower) = mark.borrower.as_ref() {
                                binding.borrowed_shared_names.remove(borrower);
                            }
                        }
                    }
                }
            }
        }
        self.cleanup_stack.pop();
        if self.borrow_frames.is_empty() {
            self.borrow_frames.push(Vec::new());
        }
        if self.cleanup_stack.is_empty() {
            self.cleanup_stack.push(Vec::new());
        }
        Ok(())
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
            Pattern::Identifier(name, span) => {
                let mutable = mutability == Mutability::Mutable;
                if let Some(scope) = self.scopes.last_mut() {
                    if mutable && self.active_mut_borrows.contains_key(name) {
                        return Err(format!("`{}` is already mutably borrowed", name));
                    }
                    let value_for_drop = value.clone();
                    scope.bindings.insert(
                        name.clone(),
                        BuildBinding {
                            cell: Arc::new(Mutex::new(value)),
                            mutable,
                            borrowed_mut: false,
                            borrowed_shared: 0,
                            borrowed_shared_names: HashSet::new(),
                            origin: *span,
                            last_move: None,
                        },
                    );
                    if !self.suppress_drop_schedule {
                        self.schedule_drop_for_value(name, &value_for_drop);
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
                        if !self.try_bind_pattern(rest_pat, BuildValue::Slice(slice), mutability)? {
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

    fn schedule_drop_for_value(&mut self, name: &str, value: &BuildValue) {
        if let Some(type_name) = self.drop_type_for_value(value) {
            if self.drop_impls.contains_key(&type_name) {
                if let Some(frame) = self.cleanup_stack.last_mut() {
                    frame.push(BuildCleanup::Drop(BuildDropRecord {
                        binding: name.to_string(),
                        type_name,
                    }));
                }
            }
        }
    }

    fn drop_type_for_value(&self, value: &BuildValue) -> Option<String> {
        match value {
            BuildValue::Struct { name, .. } => Some(name.clone()),
            BuildValue::Enum { enum_name, .. } => Some(enum_name.clone()),
            _ => None,
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

    fn eval_index(&self, base: BuildValue, index: BuildValue) -> Result<BuildValue, String> {
        match base {
            BuildValue::Slice(items) => {
                let idx = self.expect_int(index)?;
                if idx < 0 {
                    return Ok(self.wrap_enum("None", Vec::new()));
                }
                if let Some(value) = items.get(idx as usize) {
                    Ok(self.wrap_enum("Some", vec![value.clone()]))
                } else {
                    Ok(self.wrap_enum("None", Vec::new()))
                }
            }
            BuildValue::Map(entries) => {
                let key = match index {
                    BuildValue::String(s) => s,
                    other => {
                        return Err(format!("map index expects string, found {}", other.kind()));
                    }
                };
                if let Some(value) = entries.get(&key) {
                    Ok(self.wrap_enum("Some", vec![value.clone()]))
                } else {
                    Ok(self.wrap_enum("None", Vec::new()))
                }
            }
            BuildValue::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.eval_index(inner, index)
            }
            other => Err(format!("{} cannot be indexed", other.kind())),
        }
    }

    fn expect_string_value(&self, value: BuildValue, context: &str) -> Result<String, String> {
        match value {
            BuildValue::String(s) => Ok(s),
            BuildValue::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.expect_string_value(inner, context)
            }
            other => Err(format!("{context} expects string, found {}", other.kind())),
        }
    }

    fn iter_items_from_value(&self, value: BuildValue) -> Result<Vec<BuildValue>, String> {
        match value {
            BuildValue::Slice(items) => Ok(items),
            BuildValue::Map(entries) => {
                let mut collected = Vec::new();
                for (key, value) in entries {
                    collected.push(BuildValue::Tuple(vec![BuildValue::String(key), value]));
                }
                Ok(collected)
            }
            BuildValue::Iterator(iter) => {
                let start = *iter.index.lock().unwrap();
                let guard = iter.items.lock().unwrap();
                Ok(guard.iter().skip(start).cloned().collect())
            }
            BuildValue::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.iter_items_from_value(inner)
            }
            other => Err(format!("iter not supported for {}", other.kind())),
        }
    }

    fn assign_index_into_value(
        &self,
        target: &mut BuildValue,
        index: BuildValue,
        value: BuildValue,
    ) -> Result<(), String> {
        match target {
            BuildValue::Slice(items) => {
                let idx = self.expect_int(index)?;
                if idx < 0 {
                    return Err("slice index cannot be negative".into());
                }
                if let Some(slot) = items.get_mut(idx as usize) {
                    *slot = value;
                    Ok(())
                } else {
                    Err(format!("slice index {} out of bounds", idx))
                }
            }
            BuildValue::Map(entries) => {
                let key = match index {
                    BuildValue::String(s) => s,
                    other => {
                        return Err(format!("map index expects string, found {}", other.kind()));
                    }
                };
                entries.insert(key, value);
                Ok(())
            }
            other => Err(format!("{} cannot be indexed", other.kind())),
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
        if self.is_user_function(name, &receiver) {
            let mut evaluated = Vec::with_capacity(args.len() + receiver.is_some() as usize);
            let receiver_value = receiver;
            if let Some(recv) = receiver_value.clone() {
                evaluated.push(recv);
            }
            for arg in args {
                evaluated.push(self.eval_expr_mut(arg)?);
            }
            return self.call_user_function(name, &receiver_value, evaluated, type_args);
        }
        match name {
            "iter" => {
                if let Some(recv) = receiver {
                    if !args.is_empty() {
                        return Err("iter expects no arguments after receiver".into());
                    }
                    let items = self.iter_items_from_value(recv)?;
                    return Ok(BuildValue::Iterator(BuildIterator::from_items(items)));
                }
                if args.len() != 1 {
                    return Err("iter expects 1 argument".into());
                }
                let target = self.eval_expr_mut(&args[0])?;
                let items = self.iter_items_from_value(target)?;
                Ok(BuildValue::Iterator(BuildIterator::from_items(items)))
            }
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
            "next" => {
                let iter_value = if let Some(recv) = receiver {
                    recv
                } else {
                    if args.len() != 1 {
                        return Err("next expects 1 argument".into());
                    }
                    self.eval_expr_mut(&args[0])?
                };
                match iter_value {
                    BuildValue::Iterator(iter) => match iter.next() {
                        Some(value) => Ok(self.wrap_enum("Some", vec![value])),
                        None => Ok(self.wrap_enum("None", Vec::new())),
                    },
                    other => Err(format!("next expects iterator, found {}", other.kind())),
                }
            }
            "min" | "max" => {
                if args.len() != 2 {
                    return Err(format!("{name} expects 2 arguments"));
                }
                let left = self.eval_expr_mut(&args[0])?;
                let right = self.eval_expr_mut(&args[1])?;
                match (left, right, name) {
                    (BuildValue::Int(l), BuildValue::Int(r), "min") => {
                        Ok(BuildValue::Int(l.min(r)))
                    }
                    (BuildValue::Int(l), BuildValue::Int(r), "max") => {
                        Ok(BuildValue::Int(l.max(r)))
                    }
                    (BuildValue::Float(l), BuildValue::Float(r), "min") => {
                        Ok(BuildValue::Float(l.min(r)))
                    }
                    (BuildValue::Float(l), BuildValue::Float(r), "max") => {
                        Ok(BuildValue::Float(l.max(r)))
                    }
                    (l, r, _) => Err(format!(
                        "{name} not supported for {} and {}",
                        l.kind(),
                        r.kind()
                    )),
                }
            }
            "map_keys" => {
                let map_value = if let Some(recv) = receiver.clone() {
                    if !args.is_empty() {
                        return Err("map_keys expects 0 arguments after receiver".into());
                    }
                    recv
                } else {
                    if args.len() != 1 {
                        return Err("map_keys expects 1 argument".into());
                    }
                    self.eval_expr_mut(&args[0])?
                };
                let map = match map_value {
                    BuildValue::Map(entries) => entries,
                    BuildValue::Reference(reference) => {
                        let inner = reference.cell.lock().unwrap().clone();
                        match inner {
                            BuildValue::Map(entries) => entries,
                            other => {
                                return Err(format!(
                                    "map_keys expects map, found {}",
                                    other.kind()
                                ));
                            }
                        }
                    }
                    other => {
                        return Err(format!("map_keys expects map, found {}", other.kind()));
                    }
                };
                let mut keys = Vec::new();
                for key in map.keys() {
                    keys.push(BuildValue::String(key.clone()));
                }
                Ok(BuildValue::Slice(keys))
            }
            "map_values" => {
                let map_value = if let Some(recv) = receiver.clone() {
                    if !args.is_empty() {
                        return Err("map_values expects 0 arguments after receiver".into());
                    }
                    recv
                } else {
                    if args.len() != 1 {
                        return Err("map_values expects 1 argument".into());
                    }
                    self.eval_expr_mut(&args[0])?
                };
                let map = match map_value {
                    BuildValue::Map(entries) => entries,
                    BuildValue::Reference(reference) => {
                        let inner = reference.cell.lock().unwrap().clone();
                        match inner {
                            BuildValue::Map(entries) => entries,
                            other => {
                                return Err(format!(
                                    "map_values expects map, found {}",
                                    other.kind()
                                ));
                            }
                        }
                    }
                    other => {
                        return Err(format!("map_values expects map, found {}", other.kind()));
                    }
                };
                let mut values = Vec::new();
                for value in map.values() {
                    values.push(value.clone());
                }
                Ok(BuildValue::Slice(values))
            }
            "fs_exists" => {
                require_std_builtins("fs_exists")?;
                if args.len() != 1 {
                    return Err("fs_exists expects 1 argument".into());
                }
                let path_value = self.eval_expr_mut(&args[0])?;
                let path = self.expect_string_value(path_value, "fs_exists")?;
                let exists = std::path::Path::new(&path).exists();
                self.effects
                    .push(BuildEffect::FsExists { path: path.clone(), exists });
                Ok(BuildValue::Bool(exists))
            }
            "fs_read" => {
                require_std_builtins("fs_read")?;
                if args.len() != 1 {
                    return Err("fs_read expects 1 argument".into());
                }
                let path_value = self.eval_expr_mut(&args[0])?;
                let path = self.expect_string_value(path_value, "fs_read")?;
                let result = std::fs::read_to_string(&path).map_err(|err| err.to_string());
                self.effects.push(BuildEffect::FsRead {
                    path: path.clone(),
                    result: result.clone(),
                });
                match result {
                    Ok(text) => Ok(self.wrap_enum("Ok", vec![BuildValue::String(text)])),
                    Err(msg) => Ok(self.wrap_enum("Err", vec![BuildValue::String(msg)])),
                }
            }
            "fs_write" => {
                require_std_builtins("fs_write")?;
                if args.len() != 2 {
                    return Err("fs_write expects 2 arguments".into());
                }
                let path_value = self.eval_expr_mut(&args[0])?;
                let path = self.expect_string_value(path_value, "fs_write")?;
                let contents_value = self.eval_expr_mut(&args[1])?;
                let contents = self.expect_string_value(contents_value, "fs_write")?;
                let result = std::fs::write(&path, contents.clone()).map_err(|err| err.to_string());
                self.effects.push(BuildEffect::FsWrite {
                    path: path.clone(),
                    contents,
                    result: result.clone(),
                });
                match result {
                    Ok(()) => Ok(self.wrap_enum("Ok", vec![BuildValue::Unit])),
                    Err(msg) => Ok(self.wrap_enum("Err", vec![BuildValue::String(msg)])),
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
            "now_ms" => {
                require_std_builtins("now_ms")?;
                if !args.is_empty() {
                    return Err("now_ms expects no arguments".into());
                }
                let value = self.clock_ms;
                self.clock_ms = self.clock_ms.saturating_add(1);
                self.effects.push(BuildEffect::NowMs { value });
                Ok(BuildValue::Int(value))
            }
            "cast" => {
                if type_args.len() != 1 {
                    return Err("cast expects exactly one type argument".into());
                }
                if args.len() != 1 {
                    return Err(format!("cast expects 1 argument, got {}", args.len()));
                }
                let target = &type_args[0];
                let target_name = match target {
                    TypeExpr::Named(name, _) => name.as_str(),
                    _ => return Err("cast only supports numeric target types".into()),
                };
                let value = self.eval_expr_mut(&args[0])?;
                let casted = match (target_name, value) {
                    ("float32" | "float64", BuildValue::Int(i)) => BuildValue::Float(i as f64),
                    ("float32" | "float64", BuildValue::Float(f)) => BuildValue::Float(f),
                    (name, BuildValue::Float(f))
                        if name.starts_with("int") || name.starts_with("uint") =>
                    {
                        BuildValue::Int(f as i128)
                    }
                    (name, BuildValue::Int(i))
                        if name.starts_with("int") || name.starts_with("uint") =>
                    {
                        BuildValue::Int(i)
                    }
                    (_, other) => {
                        return Err(format!(
                            "cast only supports numeric values (found {})",
                            other.kind()
                        ));
                    }
                };
                Ok(casted)
            }
            "channel" => {
                require_std_builtins("channel")?;
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
                require_std_builtins("send")?;
                if args.len() != 2 {
                    return Err("send expects 2 arguments".into());
                }
                let sender = match self.eval_expr_mut(&args[0])? {
                    BuildValue::ChannelSender(tx) => tx,
                    other => {
                        return Err(format!(
                            "send expects channel sender, found {}",
                            other.kind()
                        ));
                    }
                };
                let value = self.eval_expr_mut(&args[1])?;
                match sender.send(value.clone()) {
                    Ok(()) => {
                        self.effects.push(BuildEffect::ChannelSend {
                            id: sender.id,
                            value,
                        });
                        Ok(self.wrap_enum("Ok", vec![BuildValue::Unit]))
                    }
                    Err(msg) => Ok(self.wrap_enum("Err", vec![BuildValue::String(msg)])),
                }
            }
            "recv" => {
                require_std_builtins("recv")?;
                if args.len() != 1 {
                    return Err("recv expects 1 argument".into());
                }
                let receiver = match self.eval_expr_mut(&args[0])? {
                    BuildValue::ChannelReceiver(rx) => rx,
                    other => {
                        return Err(format!(
                            "recv expects channel receiver, found {}",
                            other.kind()
                        ));
                    }
                };
                match receiver.recv() {
                    Some(value) => Ok(self.wrap_enum("Some", vec![value])),
                    None => Ok(self.wrap_enum("None", vec![])),
                }
            }
            "recv_timeout" => {
                require_std_builtins("recv_timeout")?;
                if args.len() != 2 {
                    return Err("recv_timeout expects 2 arguments".into());
                }
                let receiver = match self.eval_expr_mut(&args[0])? {
                    BuildValue::ChannelReceiver(rx) => rx,
                    other => {
                        return Err(format!(
                            "recv_timeout expects channel receiver, found {}",
                            other.kind()
                        ));
                    }
                };
                let millis = match self.eval_expr_mut(&args[1])? {
                    BuildValue::Int(v) => v,
                    other => {
                        return Err(format!(
                            "recv_timeout expects integer millis, found {}",
                            other.kind()
                        ));
                    }
                };
                let millis_i64: i64 = millis
                    .try_into()
                    .unwrap_or_else(|_| if millis.is_negative() { 0 } else { i64::MAX });
                match receiver.recv_timeout(millis_i64) {
                    Some(value) => Ok(self.wrap_enum("Some", vec![value])),
                    None => Ok(self.wrap_enum("None", vec![])),
                }
            }
            "close" => {
                require_std_builtins("close")?;
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
                    other => {
                        return Err(format!(
                            "close expects channel endpoint, found {}",
                            other.kind()
                        ));
                    }
                }
                Ok(BuildValue::Unit)
            }
            "join" => {
                require_std_builtins("join")?;
                if args.len() != 1 {
                    return Err("join expects 1 argument".into());
                }
                let handle = match self.eval_expr_mut(&args[0])? {
                    BuildValue::JoinHandle(handle) => handle,
                    other => {
                        return Err(format!("join expects join handle, found {}", other.kind()));
                    }
                };
                let evaluation = match handle.into_outcome()? {
                    BuildJoinOutcome::Thread(thread) => {
                        thread
                            .join()
                            .map_err(|_| "join handle panicked".to_string())??
                    }
                    BuildJoinOutcome::Ready(eval) => eval,
                };
                self.effects.extend(evaluation.effects);
                Ok(evaluation.value)
            }
            "assert_eq" => {
                if args.len() != 2 {
                    return Err("assert_eq expects 2 arguments".into());
                }
                let left = self.eval_expr_mut(&args[0])?;
                let right = self.eval_expr_mut(&args[1])?;
                if !build_values_equal(&left, &right) {
                    return Err("assert_eq failed".into());
                }
                Ok(BuildValue::Unit)
            }
            "panic" => {
                if args.len() != 1 {
                    return Err("panic expects 1 argument".into());
                }
                let msg = match self.eval_expr_mut(&args[0])? {
                    BuildValue::String(s) => s,
                    other => return Err(format!("panic expects string, found {}", other.kind())),
                };
                Err(msg)
            }
            "sleep" => {
                require_std_builtins("sleep")?;
                if args.len() != 1 {
                    return Err("sleep expects 1 argument".into());
                }
                let millis = match self.eval_expr_mut(&args[0])? {
                    BuildValue::Int(v) => v,
                    other => {
                        return Err(format!(
                            "sleep expects integer millis, found {}",
                            other.kind()
                        ));
                    }
                };
                if millis > 0 {
                    self.clock_ms = self.clock_ms.saturating_add(millis);
                }
                self.effects.push(BuildEffect::SleepMs { millis });
                Ok(BuildValue::Unit)
            }
            "sleep_ms" => {
                require_std_builtins("sleep_ms")?;
                if args.len() != 1 {
                    return Err("sleep_ms expects 1 argument".into());
                }
                self.eval_builtin_or_deferred("sleep", receiver, type_args, args)
            }
            "sleep_task" => Err(
                "`sleep_task` is async-only and not supported in build snapshots yet".into(),
            ),
            "recv_task" => Err(
                "`recv_task` is async-only and not supported in build snapshots yet".into(),
            ),
            "delay_ms" | "pin_mode" | "digital_write" => Err(
                "embedded-only built-ins are unavailable in build snapshots; build for the ESP32 target to run them"
                    .into(),
            ),
            "get" => {
                if args.len() != 2 {
                    return Err("get expects 2 arguments".into());
                }
                let mut evaluated = Vec::with_capacity(2);
                if let Some(recv) = receiver {
                    evaluated.push(recv);
                } else {
                    evaluated.push(self.eval_expr_mut(&args[0])?);
                }
                if evaluated.len() == 1 {
                    evaluated.push(self.eval_expr_mut(&args[1])?);
                } else {
                    evaluated.push(self.eval_expr_mut(&args[0])?);
                }
                match (&evaluated[0], &evaluated[1]) {
                    (BuildValue::Slice(items), BuildValue::Int(idx)) => {
                        if *idx < 0 {
                            return Ok(self.wrap_enum("None", vec![]));
                        }
                        let idx_usize = *idx as usize;
                        match items.get(idx_usize) {
                            Some(value) => Ok(self.wrap_enum("Some", vec![value.clone()])),
                            None => Ok(self.wrap_enum("None", vec![])),
                        }
                    }
                    (BuildValue::Map(entries), BuildValue::String(key)) => match entries.get(key) {
                        Some(value) => Ok(self.wrap_enum("Some", vec![value.clone()])),
                        None => Ok(self.wrap_enum("None", vec![])),
                    },
                    (BuildValue::Reference(reference), _) => {
                        let inner = reference.cell.lock().unwrap().clone();
                        self.eval_builtin_or_deferred(name, Some(inner), type_args, &args[1..])
                    }
                    (receiver, _) => Err(format!(
                        "get expects slice or map, found {}",
                        receiver.kind()
                    )),
                }
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

    fn receiver_key(receiver: &Option<BuildValue>) -> Option<String> {
        match receiver {
            Some(BuildValue::Struct { name, .. }) => Some(name.clone()),
            Some(BuildValue::Enum { enum_name, .. }) => Some(enum_name.clone()),
            _ => None,
        }
    }

    fn is_user_function(&self, name: &str, receiver: &Option<BuildValue>) -> bool {
        let key = BuildFunctionKey {
            name: name.to_string(),
            receiver: Self::receiver_key(receiver),
        };
        self.functions.contains_key(&key)
    }

    fn call_user_function(
        &mut self,
        name: &str,
        receiver: &Option<BuildValue>,
        args: Vec<BuildValue>,
        _type_args: &[TypeExpr],
    ) -> Result<BuildValue, String> {
        let receiver_key = Self::receiver_key(receiver);
        let key = BuildFunctionKey {
            name: name.to_string(),
            receiver: receiver_key,
        };
        let func = self
            .functions
            .get(&key)
            .cloned()
            .ok_or_else(|| format!("Unknown function `{}`", name))?;
        if func.def.params.len() != args.len() {
            return Err(format!(
                "Function `{}` expects {} arguments, got {}",
                name,
                func.def.params.len(),
                args.len()
            ));
        }
        self.push_scope();
        for (param, value) in func.def.params.iter().zip(args.into_iter()) {
            let pattern = Pattern::Identifier(param.name.clone(), param.span);
            self.bind_pattern(&pattern, value, param.mutability)?;
        }
        let result = match &func.def.body {
            crate::language::ast::FunctionBody::Block(block) => match self.eval_block(block)? {
                Flow::Value(v) | Flow::Return(v) => Ok(v),
                Flow::Break | Flow::Continue => {
                    Err("Control flow cannot escape function body in build mode".into())
                }
            },
            crate::language::ast::FunctionBody::Expr(expr) => self.eval_expr_mut(&expr.node),
        };
        self.pop_scope()?;
        result
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
    use crate::language::ast::{
        Block, DeferStmt, ExprStmt, FunctionBody, LetStmt, MatchArmExpr, MatchExpr, Visibility,
    };
    use crate::language::span::{Span, Spanned};
    use std::sync::mpsc;
    use std::thread;
    use std::time::{Duration, Instant};

    fn empty_snapshot() -> BuildSnapshot {
        BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        }
    }

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn drop_function(type_name: &str, tag: i128) -> (BuildFunctionKey, BuildFunction) {
        let s = span();
        let param = FunctionParam {
            name: "self".into(),
            ty: Some(TypeAnnotation {
                ty: TypeExpr::Reference {
                    mutable: true,
                    ty: Box::new(TypeExpr::SelfType),
                },
                span: s,
            }),
            mutability: Mutability::Mutable,
            span: s,
        };
        let body = FunctionBody::Block(Box::new(Block {
            statements: vec![Statement::Expr(ExprStmt {
                expr: Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "out".into(),
                        span: s,
                    })),
                    type_args: Vec::new(),
                    args: vec![Expr::Literal(Literal::Int(tag, s))],
                    span: s,
                },
            })],
            tail: None,
            span: s,
        }));
        let def = FunctionDef {
            name: "drop".into(),
            name_span: s,
            type_params: Vec::new(),
            params: vec![param],
            returns: Vec::new(),
            body,
            span: s,
            visibility: Visibility::Private,
        };
        let key = BuildFunctionKey {
            name: "drop".into(),
            receiver: Some(type_name.to_string()),
        };
        let func = BuildFunction {
            def,
            receiver: Some(type_name.to_string()),
        };
        (key, func)
    }

    fn struct_literal(name: &str, span: Span) -> Expr {
        Expr::StructLiteral {
            name: name.into(),
            fields: StructLiteralKind::Named(Vec::new()),
            span,
        }
    }

    fn collect_out_tags(effects: &[BuildEffect]) -> Vec<i128> {
        effects
            .iter()
            .filter_map(|effect| {
                if let BuildEffect::Out(values) = effect {
                    match values.as_slice() {
                        [BuildValue::Int(tag)] => Some(*tag),
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn build_channel_pair() -> (BuildChannelSender, BuildChannelReceiver) {
        let s = span();
        let expr = Expr::Call {
            callee: Box::new(Expr::Identifier(Identifier {
                name: "channel".into(),
                span: s,
            })),
            type_args: Vec::new(),
            args: Vec::new(),
            span: s,
        };
        match BuildInterpreter::new(empty_snapshot())
            .eval_expr(&expr)
            .expect("channel evaluates")
        {
            BuildValue::Tuple(mut items) if items.len() == 2 => {
                let rx = items.pop().unwrap();
                let tx = items.pop().unwrap();
                match (tx, rx) {
                    (BuildValue::ChannelSender(tx), BuildValue::ChannelReceiver(rx)) => (tx, rx),
                    other => panic!("unexpected channel pair: {:?}", other),
                }
            }
            other => panic!("unexpected channel value: {:?}", other),
        }
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
                borrowed_shared_names: HashSet::new(),
                origin: span(),
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
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
        let interpreter = BuildInterpreter::new(empty_snapshot());
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
                    target: crate::language::ast::ForTarget::Range(
                        crate::language::ast::RangeExpr {
                            start: Box::new(Expr::Literal(Literal::Int(0, s))),
                            end: Box::new(Expr::Literal(Literal::Int(3, s))),
                            inclusive: false,
                            span: s,
                        },
                    ),
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
            statements: vec![
                Statement::Let(crate::language::ast::LetStmt {
                    pattern: Pattern::Tuple(
                        vec![
                            Pattern::Identifier("tx".into(), s),
                            Pattern::Identifier("rx".into(), s),
                        ],
                        s,
                    ),
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
                }),
            ],
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
    fn channel_recv_blocks_until_send() {
        let (tx, rx) = build_channel_pair();
        let (ready_tx, ready_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            ready_tx.send(()).unwrap();
            let start = Instant::now();
            let value = rx.recv();
            (value, start.elapsed())
        });
        ready_rx.recv().unwrap();
        thread::sleep(Duration::from_millis(20));
        tx.send(BuildValue::Int(7)).expect("send succeeds");
        let (value, elapsed) = handle.join().expect("recv thread joins");
        match value {
            Some(BuildValue::Int(v)) => assert_eq!(v, 7),
            other => panic!("unexpected recv value: {:?}", other),
        }
        assert!(
            elapsed >= Duration::from_millis(10),
            "recv returned too early: {:?}",
            elapsed
        );
    }

    #[test]
    fn channel_close_unblocks_recv() {
        let (tx, rx) = build_channel_pair();
        let (ready_tx, ready_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            ready_tx.send(()).unwrap();
            rx.recv()
        });
        ready_rx.recv().unwrap();
        thread::sleep(Duration::from_millis(20));
        tx.close();
        let value = handle.join().expect("recv thread joins");
        assert!(value.is_none());
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
                borrowed_shared_names: HashSet::new(),
                origin: span(),
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let first = interpreter
            .eval_expr(&Expr::Move {
                expr: Box::new(Expr::Identifier(Identifier {
                    name: "x".into(),
                    span: s,
                })),
                span: s,
            })
            .expect("first move should succeed");
        assert!(matches!(first, BuildValue::Int(1)));
        let second = interpreter.eval_expr(&Expr::Identifier(Identifier {
            name: "x".into(),
            span: s,
        }));
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
                borrowed_shared_names: HashSet::new(),
                origin: s,
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let first = interpreter.eval_expr(&Expr::Reference {
            mutable: true,
            expr: Box::new(Expr::Identifier(Identifier {
                name: "y".into(),
                span: s,
            })),
            span: s,
        });
        assert!(first.is_ok());
        let second = interpreter.eval_expr(&Expr::Reference {
            mutable: true,
            expr: Box::new(Expr::Identifier(Identifier {
                name: "y".into(),
                span: s,
            })),
            span: s,
        });
        assert!(second.is_err(), "second mutable borrow should fail");
        // Reading while mutably borrowed should also fail.
        let third = interpreter.eval_expr(&Expr::Identifier(Identifier {
            name: "y".into(),
            span: s,
        }));
        assert!(third.is_err(), "read during mutable borrow should fail");
    }

    #[test]
    fn mut_borrow_conflict_reports_borrower_and_spans() {
        let origin = Span::new(2, 6);
        let borrow_span = Span::new(10, 14);
        let mut scope = BuildScope {
            bindings: HashMap::new(),
        };
        scope.bindings.insert(
            "item".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Int(1))),
                mutable: true,
                borrowed_mut: false,
                borrowed_shared: 0,
                borrowed_shared_names: HashSet::new(),
                origin,
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        interpreter
            .eval_expr(&Expr::Reference {
                mutable: true,
                expr: Box::new(Expr::Identifier(Identifier {
                    name: "item".into(),
                    span: borrow_span,
                })),
                span: borrow_span,
            })
            .expect("first borrow");
        let err = interpreter
            .eval_expr(&Expr::Identifier(Identifier {
                name: "item".into(),
                span: Span::new(20, 24),
            }))
            .expect_err("borrow conflict should error");
        assert!(
            err.contains("first mutable borrow"),
            "expected borrower information, got {err}"
        );
        assert!(
            err.contains("10..14"),
            "expected borrower span, got {err}"
        );
        assert!(
            err.contains("binding declared at 2..6"),
            "expected origin span, got {err}"
        );
    }

    #[test]
    fn moved_value_error_reports_move_span() {
        let origin = Span::new(30, 35);
        let move_span = Span::new(40, 45);
        let mut scope = BuildScope {
            bindings: HashMap::new(),
        };
        scope.bindings.insert(
            "slot".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Int(7))),
                mutable: true,
                borrowed_mut: false,
                borrowed_shared: 0,
                borrowed_shared_names: HashSet::new(),
                origin,
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        interpreter
            .eval_expr(&Expr::Move {
                expr: Box::new(Expr::Identifier(Identifier {
                    name: "slot".into(),
                    span: move_span,
                })),
                span: move_span,
            })
            .expect("move succeeds");
        let err = interpreter
            .eval_expr(&Expr::Identifier(Identifier {
                name: "slot".into(),
                span: Span::new(50, 52),
            }))
            .expect_err("moved use should error");
        assert!(
            err.contains("move occurred at 40..45"),
            "expected move span, got {err}"
        );
    }

    #[cfg(not(feature = "std-builtins"))]
    #[test]
    fn std_disabled_build_builtins_are_blocked() {
        let err = require_std_builtins("spawn").unwrap_err();
        assert!(
            err.contains("std-builtins feature disabled"),
            "expected std-disabled message, got {err}"
        );
    }

    #[test]
    fn reference_capture_keeps_borrow_active() {
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
                borrowed_shared_names: HashSet::new(),
                origin: span(),
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let captures = Arc::new(RwLock::new(vec![CapturedVar {
            name: "x".into(),
            mutable: true,
            ty: None,
            mode: CaptureMode::Reference { mutable: true },
            span: s,
        }]));
        let closure = interpreter
            .eval_expr(&Expr::Closure {
                params: vec![],
                body: ClosureBody::Expr(Spanned {
                    node: Box::new(Expr::Identifier(Identifier {
                        name: "x".into(),
                        span: s,
                    })),
                    span: s,
                }),
                ret: None,
                captures: captures.clone(),
                span: s,
            })
            .expect("closure builds");
        assert!(matches!(closure, BuildValue::Closure(_)));
        let second_borrow = interpreter.eval_expr(&Expr::Reference {
            mutable: true,
            expr: Box::new(Expr::Identifier(Identifier {
                name: "x".into(),
                span: s,
            })),
            span: s,
        });
        assert!(
            second_borrow.is_err(),
            "captured mutable borrow should block new borrow"
        );
    }

    #[test]
    fn defers_run_inside_closure() {
        let s = span();
        let scope = BuildScope {
            bindings: HashMap::new(),
        };
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let closure = interpreter
            .eval_expr(&Expr::Closure {
                params: vec![],
                body: ClosureBody::Block(Box::new(Block {
                    statements: vec![Statement::Defer(DeferStmt {
                        expr: Expr::Call {
                            callee: Box::new(Expr::Identifier(Identifier {
                                name: "out".into(),
                                span: s,
                            })),
                            type_args: vec![],
                            args: vec![Expr::Literal(Literal::Int(5, s))],
                            span: s,
                        },
                    })],
                    tail: Some(Box::new(Expr::Literal(Literal::Int(0, s)))),
                    span: s,
                })),
                ret: None,
                captures: Arc::new(RwLock::new(Vec::new())),
                span: s,
            })
            .expect("closure builds");
        let BuildValue::Closure(closure) = closure else {
            panic!("expected closure");
        };
        let result = interpreter
            .call_closure_value(closure.clone(), Vec::new())
            .expect("closure call succeeds");
        assert!(matches!(result, BuildValue::Int(0)));
        assert!(
            interpreter.effects.iter().any(|effect| matches!(
                effect,
                BuildEffect::Out(values) if matches!(values.as_slice(), [BuildValue::Int(5)])
            )),
            "defer should have recorded out effect"
        );
    }

    #[test]
    fn drops_run_lifo_with_defers() {
        let s = span();
        let mut functions = HashMap::new();
        let (a_key, a_fn) = drop_function("A", 1);
        functions.insert(a_key, a_fn);
        let (b_key, b_fn) = drop_function("B", 2);
        functions.insert(b_key, b_fn);
        let snapshot = BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
            functions,
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let block = Block {
            statements: vec![
                Statement::Let(LetStmt {
                    pattern: Pattern::Identifier("first".into(), s),
                    ty: None,
                    value: Some(struct_literal("A", s)),
                    mutability: Mutability::Immutable,
                    span: s,
                }),
                Statement::Defer(DeferStmt {
                    expr: Expr::Call {
                        callee: Box::new(Expr::Identifier(Identifier {
                            name: "out".into(),
                            span: s,
                        })),
                        type_args: Vec::new(),
                        args: vec![Expr::Literal(Literal::Int(99, s))],
                        span: s,
                    },
                }),
                Statement::Let(LetStmt {
                    pattern: Pattern::Identifier("second".into(), s),
                    ty: None,
                    value: Some(struct_literal("B", s)),
                    mutability: Mutability::Immutable,
                    span: s,
                }),
            ],
            tail: None,
            span: s,
        };
        interpreter.eval_block(&block).expect("block drops");
        assert_eq!(collect_out_tags(&interpreter.effects), vec![2, 99, 1]);
    }

    #[test]
    fn drop_runs_once_after_move() {
        let s = span();
        let mut functions = HashMap::new();
        let (key, func) = drop_function("Mover", 7);
        functions.insert(key, func);
        let snapshot = BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
            functions,
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let block = Block {
            statements: vec![
                Statement::Let(LetStmt {
                    pattern: Pattern::Identifier("a".into(), s),
                    ty: None,
                    value: Some(struct_literal("Mover", s)),
                    mutability: Mutability::Immutable,
                    span: s,
                }),
                Statement::Let(LetStmt {
                    pattern: Pattern::Identifier("b".into(), s),
                    ty: None,
                    value: Some(Expr::Move {
                        expr: Box::new(Expr::Identifier(Identifier {
                            name: "a".into(),
                            span: s,
                        })),
                        span: s,
                    }),
                    mutability: Mutability::Immutable,
                    span: s,
                }),
            ],
            tail: None,
            span: s,
        };
        interpreter.eval_block(&block).expect("move block");
        assert_eq!(collect_out_tags(&interpreter.effects), vec![7]);
    }

    #[test]
    fn drop_executes_on_break() {
        let s = span();
        let mut functions = HashMap::new();
        let (key, func) = drop_function("Loop", 5);
        functions.insert(key, func);
        let snapshot = BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
            functions,
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let loop_block = Block {
            statements: vec![
                Statement::Let(LetStmt {
                    pattern: Pattern::Identifier("item".into(), s),
                    ty: None,
                    value: Some(struct_literal("Loop", s)),
                    mutability: Mutability::Immutable,
                    span: s,
                }),
                Statement::Break,
            ],
            tail: None,
            span: s,
        };
        let block = Block {
            statements: vec![Statement::Loop(crate::language::ast::LoopStmt {
                body: loop_block,
                span: s,
            })],
            tail: None,
            span: s,
        };
        interpreter.eval_block(&block).expect("loop drops");
        assert_eq!(collect_out_tags(&interpreter.effects), vec![5]);
    }

    #[test]
    fn captured_values_drop_after_closure_call() {
        let s = span();
        let mut functions = HashMap::new();
        let (key, func) = drop_function("Cap", 3);
        functions.insert(key, func);
        let snapshot = BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
            functions,
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let captured = BuildCaptured {
            name: "cap".into(),
            mutable: false,
            mode: CaptureMode::Move,
            value: BuildValue::Struct {
                name: "Cap".into(),
                fields: BTreeMap::new(),
            },
            ty: None,
            origin: s,
        };
        let closure = BuildClosure {
            params: Vec::new(),
            body: ClosureBody::Block(Box::new(Block {
                statements: Vec::new(),
                tail: None,
                span: s,
            })),
            ret: None,
            captures: vec![captured],
            origin: s,
        };
        let result = interpreter
            .call_closure_value(closure, Vec::new())
            .expect("closure call");
        assert!(matches!(result, BuildValue::Unit));
        assert_eq!(collect_out_tags(&interpreter.effects), vec![3]);
    }

    #[test]
    fn drop_actions_replay_from_snapshots() {
        let mut bindings = HashMap::new();
        bindings.insert(
            "snap".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Struct {
                    name: "Snap".into(),
                    fields: BTreeMap::new(),
                })),
                mutable: false,
                borrowed_mut: false,
                borrowed_shared: 0,
                borrowed_shared_names: HashSet::new(),
                origin: span(),
                last_move: None,
            },
        );
        let (key, func) = drop_function("Snap", 11);
        let snapshot = BuildSnapshot {
            scopes: vec![BuildScope { bindings }],
            enum_variants: HashMap::new(),
            functions: HashMap::from([(key, func)]),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![vec![BuildCleanup::Drop(BuildDropRecord {
                binding: "snap".into(),
                type_name: "Snap".into(),
            })]],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        interpreter.pop_scope().expect("pop scope runs drops");
        assert_eq!(collect_out_tags(&interpreter.effects), vec![11]);
    }

    #[test]
    fn deref_assignment_updates_reference() {
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
                borrowed_shared_names: HashSet::new(),
                origin: span(),
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let block = Block {
            statements: vec![Statement::Assign(AssignStmt {
                target: Expr::Deref {
                    expr: Box::new(Expr::Reference {
                        mutable: true,
                        expr: Box::new(Expr::Identifier(Identifier {
                            name: "x".into(),
                            span: s,
                        })),
                        span: s,
                    }),
                    span: s,
                },
                value: Expr::Literal(Literal::Int(9, s)),
            })],
            tail: None,
            span: s,
        };
        interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("deref assignment succeeds");
        let result = interpreter
            .eval_expr(&Expr::Identifier(Identifier {
                name: "x".into(),
                span: s,
            }))
            .expect("load x");
        assert!(matches!(result, BuildValue::Int(9)));
    }

    #[test]
    fn field_assignment_updates_struct() {
        let s = span();
        let mut scope = BuildScope {
            bindings: HashMap::new(),
        };
        scope.bindings.insert(
            "p".into(),
            BuildBinding {
                cell: Arc::new(Mutex::new(BuildValue::Struct {
                    name: "Point".into(),
                    fields: [
                        ("x".into(), BuildValue::Int(1)),
                        ("y".into(), BuildValue::Int(2)),
                    ]
                    .into_iter()
                    .collect(),
                })),
                mutable: true,
                borrowed_mut: false,
                borrowed_shared: 0,
                borrowed_shared_names: HashSet::new(),
                origin: s,
                last_move: None,
            },
        );
        let snapshot = BuildSnapshot {
            scopes: vec![scope],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let mut interpreter = BuildInterpreter::new(snapshot);
        let block = Block {
            statements: vec![Statement::Assign(AssignStmt {
                target: Expr::FieldAccess {
                    base: Box::new(Expr::Identifier(Identifier {
                        name: "p".into(),
                        span: s,
                    })),
                    field: "x".into(),
                    span: s,
                },
                value: Expr::Literal(Literal::Int(5, s)),
            })],
            tail: None,
            span: s,
        };
        interpreter
            .eval_expr(&Expr::Block(Box::new(block)))
            .expect("field assignment");
        let updated = interpreter
            .eval_expr(&Expr::FieldAccess {
                base: Box::new(Expr::Identifier(Identifier {
                    name: "p".into(),
                    span: s,
                })),
                field: "x".into(),
                span: s,
            })
            .expect("read field");
        assert!(matches!(updated, BuildValue::Int(5)));
    }

    #[test]
    fn rune_literal_is_accepted() {
        let s = span();
        let result = BuildInterpreter::new(empty_snapshot())
            .eval_expr(&Expr::Literal(Literal::Rune('a', s)))
            .expect("rune literal");
        assert!(matches!(result, BuildValue::Int(v) if v == 'a' as i128));
    }

    #[test]
    fn get_fetches_slice_elements() {
        let s = span();
        let expr = Expr::Call {
            callee: Box::new(Expr::Identifier(Identifier {
                name: "get".into(),
                span: s,
            })),
            type_args: Vec::new(),
            args: vec![
                Expr::ArrayLiteral(
                    vec![
                        Expr::Literal(Literal::Int(1, s)),
                        Expr::Literal(Literal::Int(2, s)),
                    ],
                    s,
                ),
                Expr::Literal(Literal::Int(1, s)),
            ],
            span: s,
        };
        let mut snapshot = empty_snapshot();
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
        let result = BuildInterpreter::new(snapshot)
            .eval_expr(&expr)
            .expect("get evaluates");
        match result {
            BuildValue::Enum {
                variant, values, ..
            } if variant == "Some" => {
                assert!(matches!(values.as_slice(), [BuildValue::Int(2)]));
            }
            other => panic!("unexpected result: {:?}", other),
        }
    }

    #[test]
    fn bitwise_int_operations_work() {
        let s = span();
        let expr = Expr::Binary {
            op: BinaryOp::BitAnd,
            left: Box::new(Expr::Literal(Literal::Int(6, s))), // 110
            right: Box::new(Expr::Literal(Literal::Int(3, s))), // 011
            span: s,
        };
        let result = BuildInterpreter::new(empty_snapshot())
            .eval_expr(&expr)
            .expect("bitwise and");
        assert!(matches!(result, BuildValue::Int(v) if v == 2));
    }

    #[test]
    fn mixed_numeric_operations_promote_to_float() {
        let s = span();
        let expr = Expr::Binary {
            op: BinaryOp::Div,
            left: Box::new(Expr::Literal(Literal::Int(7, s))),
            right: Box::new(Expr::Literal(Literal::Float(2.0, s))),
            span: s,
        };
        let result = BuildInterpreter::new(empty_snapshot())
            .eval_expr(&expr)
            .expect("mixed numeric division");
        assert!(matches!(result, BuildValue::Float(v) if (v - 3.5).abs() < f64::EPSILON));
    }

    #[test]
    fn deep_struct_equality_matches() {
        let s = span();
        let expr = Expr::Binary {
            op: BinaryOp::Eq,
            left: Box::new(Expr::StructLiteral {
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
            right: Box::new(Expr::StructLiteral {
                name: "Point".into(),
                fields: StructLiteralKind::Named(vec![
                    StructLiteralField {
                        name: "y".into(),
                        value: Expr::Literal(Literal::Int(2, s)),
                    },
                    StructLiteralField {
                        name: "x".into(),
                        value: Expr::Literal(Literal::Int(1, s)),
                    },
                ]),
                span: s,
            }),
            span: s,
        };
        let result = BuildInterpreter::new(empty_snapshot())
            .eval_expr(&expr)
            .expect("struct equality");
        assert!(matches!(result, BuildValue::Bool(true)));
    }

    #[test]
    fn map_equality_matches_keys_and_values() {
        let s = span();
        let expr = Expr::Binary {
            op: BinaryOp::Eq,
            left: Box::new(Expr::MapLiteral {
                entries: vec![
                    crate::language::ast::MapLiteralEntry {
                        key: Expr::Literal(Literal::String("a".into(), s)),
                        value: Expr::Literal(Literal::Int(1, s)),
                    },
                    crate::language::ast::MapLiteralEntry {
                        key: Expr::Literal(Literal::String("b".into(), s)),
                        value: Expr::Literal(Literal::Int(2, s)),
                    },
                ],
                span: s,
            }),
            right: Box::new(Expr::MapLiteral {
                entries: vec![
                    crate::language::ast::MapLiteralEntry {
                        key: Expr::Literal(Literal::String("b".into(), s)),
                        value: Expr::Literal(Literal::Int(2, s)),
                    },
                    crate::language::ast::MapLiteralEntry {
                        key: Expr::Literal(Literal::String("a".into(), s)),
                        value: Expr::Literal(Literal::Int(1, s)),
                    },
                ],
                span: s,
            }),
            span: s,
        };
        let result = BuildInterpreter::new(empty_snapshot())
            .eval_expr(&expr)
            .expect("map equality");
        assert!(matches!(result, BuildValue::Bool(true)));
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
        let ok_value = interpreter
            .eval_expr(&ok_expr)
            .expect("Ok should propagate");
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
