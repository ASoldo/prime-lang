use crate::runtime::abi::{
    PrimeStatus, TYPE_BOOL, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_INT8, TYPE_INT16, TYPE_INT32,
    TYPE_INT64, TYPE_ISIZE, TYPE_RUNE, TYPE_STRING, TYPE_UINT8, TYPE_UINT16, TYPE_UINT32,
    TYPE_UINT64, TYPE_USIZE,
};
use crate::runtime::environment::{CleanupAction, DropRecord};
use crate::runtime::platform::platform;
use crate::target::{BuildTarget, embedded_target_hint};
use crate::{
    language::{
        ast::*,
        build::{
            BuildBinding, BuildCaptured, BuildChannelReceiver, BuildChannelSender,
            BuildChannelState, BuildCleanup, BuildClosure, BuildDropRecord, BuildEffect,
            BuildEnumVariant, BuildEvaluation, BuildFormatTemplate, BuildFunction,
            BuildFunctionKey, BuildInterpreter, BuildIterator, BuildJoinHandle, BuildJoinOutcome,
            BuildReference, BuildScope, BuildSnapshot, BuildTask, BuildValue,
        },
        runtime_abi::RuntimeAbi,
        span::Span,
        types::{Mutability, TypeAnnotation, TypeExpr},
    },
    runtime::value::{FormatRuntimeSegmentGeneric, FormatTemplateValueGeneric},
};
use llvm_sys::{
    LLVMLinkage, LLVMTypeKind,
    core::{
        LLVMAddCase, LLVMAddFunction, LLVMAddIncoming, LLVMAppendBasicBlockInContext,
        LLVMArrayType2, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildArrayMalloc,
        LLVMBuildBitCast, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildExtractValue,
        LLVMBuildFAdd, LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFRem, LLVMBuildFSub,
        LLVMBuildGlobalString, LLVMBuildICmp, LLVMBuildInBoundsGEP2, LLVMBuildIntCast,
        LLVMBuildLoad2, LLVMBuildMul, LLVMBuildNot, LLVMBuildOr, LLVMBuildPhi, LLVMBuildRet,
        LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSExt, LLVMBuildSIToFP, LLVMBuildSRem,
        LLVMBuildStore, LLVMBuildStructGEP2, LLVMBuildSub, LLVMBuildSwitch, LLVMConstInt,
        LLVMConstIntGetZExtValue, LLVMConstNull, LLVMConstPointerNull, LLVMConstReal,
        LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder,
        LLVMDisposeMessage, LLVMDisposeModule, LLVMDoubleTypeInContext, LLVMFloatTypeInContext,
        LLVMFunctionType, LLVMGetBasicBlockParent, LLVMGetElementType, LLVMGetFirstBasicBlock,
        LLVMGetFirstInstruction, LLVMGetGlobalParent, LLVMGetInsertBlock, LLVMGetIntTypeWidth,
        LLVMGetLastInstruction, LLVMGetModuleContext, LLVMGetParam, LLVMGetReturnType,
        LLVMGetTypeKind, LLVMInt8TypeInContext, LLVMInt32TypeInContext, LLVMIntTypeInContext,
        LLVMIsAConstantInt, LLVMIsAFunction, LLVMModuleCreateWithNameInContext, LLVMPointerType,
        LLVMPositionBuilder, LLVMPositionBuilderAtEnd, LLVMPositionBuilderBefore,
        LLVMPrintModuleToFile, LLVMSetLinkage, LLVMSetTarget, LLVMStructCreateNamed,
        LLVMStructSetBody, LLVMStructTypeInContext, LLVMTypeOf, LLVMVoidTypeInContext,
    },
    prelude::*,
};
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    env,
    ffi::{CStr, CString},
    fs,
    io::Read,
    mem,
    path::Path,
    ptr,
    rc::Rc,
    sync::{Arc, Condvar, Mutex, RwLock},
    thread,
};

mod builtins;
mod closures;
mod emit;
mod resolve;
mod runtime;
mod scopes;

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

#[derive(Clone, Debug)]
struct FnSignature {
    params: Vec<TypeExpr>,
    returns: Vec<TypeExpr>,
}

impl FnSignature {
    fn return_count(&self) -> usize {
        if self.returns.len() == 1 && matches!(self.returns.first(), Some(TypeExpr::Unit)) {
            0
        } else {
            self.returns.len()
        }
    }
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
    #[allow(dead_code)]
    target: BuildTarget,
    #[allow(dead_code)]
    runtime_abi: RuntimeAbi,
    scopes: Vec<HashMap<String, Binding>>,
    structs: HashMap<String, StructEntry>,
    functions: HashMap<FunctionKey, FunctionEntry>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    interfaces: HashMap<String, InterfaceEntry>,
    impls: HashSet<ImplKey>,
    consts: Vec<(String, ConstDef)>,
    drop_impls: HashMap<String, FunctionKey>,
    active_mut_borrows: HashSet<String>,
    borrow_frames: Vec<Vec<String>>,
    cleanup_stack: Vec<Vec<CleanupAction>>,
    deprecated_warnings: HashSet<String>,
    module_stack: Vec<String>,
    return_type_stack: Vec<Vec<TypeExpr>>,
    closure_counter: usize,
    closures: HashMap<usize, ClosureInfo>,
    closure_snapshots: HashMap<usize, ClosureSnapshot>,
    closure_value_type: LLVMTypeRef,
    closure_envs: Vec<(usize, LLVMValueRef)>,
    build_clock_ms: i128,
    pending_runtime: Option<LLVMValueRef>,
    force_runtime_handles: bool,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
enum Value {
    Int(IntValue),
    Float(FloatValue),
    Bool(BoolValue),
    Str(StringValue),
    Struct(StructValue),
    Enum(EnumValue),
    Tuple(Vec<Value>),
    Unit,
    Reference(ReferenceValue),
    Boxed(BoxValue),
    Slice(SliceValue),
    Map(MapValue),
    FormatTemplate(FormatTemplateValue),
    Sender(ChannelSender),
    Receiver(ChannelReceiver),
    Iterator(IteratorValue),
    JoinHandle(Box<JoinHandleValue>),
    Task(Box<TaskValue>),
    Pointer(PointerValue),
    Range(RangeValue),
    Closure(ClosureValue),
    Moved,
}

#[derive(Clone)]
struct ClosureValue {
    id: usize,
    env_ptr: LLVMValueRef,
    fn_ptr: LLVMValueRef,
}

#[derive(Clone)]
struct ClosureSnapshot {
    params: Vec<FunctionParam>,
    body: ClosureBody,
    ret: Option<TypeAnnotation>,
    captures: Vec<CapturedVar>,
    values: Vec<Value>,
}

#[derive(Clone)]
struct ClosureInfo {
    env_type: LLVMTypeRef,
    fn_type: LLVMTypeRef,
    function: LLVMValueRef,
    signature: FnSignature,
    capture_types: Vec<TypeExpr>,
    built: bool,
}

#[derive(Clone)]
struct EnumValue {
    enum_name: String,
    variant: String,
    values: Vec<Value>,
    variant_index: u32,
}

type FormatTemplateValue = FormatTemplateValueGeneric<EvaluatedValue>;
type FormatRuntimeSegment = FormatRuntimeSegmentGeneric<EvaluatedValue>;

#[derive(Clone)]
struct IntValue {
    llvm: LLVMValueRef,
    constant: Option<i128>,
}

#[derive(Clone)]
struct BoolValue {
    llvm: LLVMValueRef,
    constant: Option<bool>,
}

#[derive(Clone)]
struct FloatValue {
    llvm: LLVMValueRef,
    constant: Option<f64>,
}

#[derive(Clone)]
struct ReferenceValue {
    cell: Rc<Mutex<EvaluatedValue>>,
    mutable: bool,
    origin: Option<String>,
    handle: Option<LLVMValueRef>,
}

#[derive(Clone)]
struct PointerValue {
    cell: Rc<Mutex<EvaluatedValue>>,
    mutable: bool,
    handle: Option<LLVMValueRef>,
    origin: Option<String>,
}

#[derive(Clone)]
struct StringValue {
    llvm: LLVMValueRef,
    text: Arc<String>,
}

#[derive(Clone)]
struct RangeValue {
    start: IntValue,
    end: IntValue,
    inclusive: bool,
}

#[derive(Clone)]
struct BoxValue {
    handle: Option<LLVMValueRef>,
    cell: Rc<Mutex<Value>>,
    #[allow(dead_code)]
    origin: Option<String>,
}

#[derive(Clone)]
struct SliceValue {
    handle: Option<LLVMValueRef>,
    items: Rc<Mutex<Vec<Value>>>,
}

#[derive(Clone)]
struct MapValue {
    handle: Option<LLVMValueRef>,
    entries: Rc<Mutex<BTreeMap<String, Value>>>,
}

#[derive(Clone)]
struct IteratorValue {
    items: Rc<Mutex<Vec<Value>>>,
    index: Rc<Mutex<usize>>,
}

impl IteratorValue {
    fn from_items(items: Vec<Value>) -> Self {
        Self {
            items: Rc::new(Mutex::new(items)),
            index: Rc::new(Mutex::new(0)),
        }
    }

    fn next(&self) -> Option<Value> {
        let mut idx_guard = self.index.lock().unwrap();
        let guard = self.items.lock().unwrap();
        if *idx_guard >= guard.len() {
            return None;
        }
        let value = guard.get(*idx_guard).cloned();
        *idx_guard += 1;
        value
    }
}

#[derive(Clone)]
struct ChannelSender {
    inner: ChannelHandle,
    handle: Option<LLVMValueRef>,
    origin: Option<String>,
}

#[derive(Clone)]
struct ChannelReceiver {
    inner: ChannelHandle,
    handle: Option<LLVMValueRef>,
    origin: Option<String>,
}

enum JoinResult {
    Immediate(Option<Value>),
    BuildThread(Option<thread::JoinHandle<Result<BuildEvaluation, String>>>),
}

#[derive(Clone)]
struct JoinHandleValue {
    result: Rc<Mutex<JoinResult>>,
    handle: Option<LLVMValueRef>,
}

#[derive(Clone, Copy)]
enum TaskResultKind {
    Ready,
    RuntimeUnit,
    RuntimeOption,
    Deferred,
}

#[derive(Clone)]
struct TaskValue {
    state: Rc<(Mutex<Option<EvaluatedValue>>, Condvar)>,
    handle: Option<LLVMValueRef>,
    kind: TaskResultKind,
    pending: Option<Rc<Mutex<Option<DeferredTask>>>>,
}

#[derive(Clone)]
struct DeferredTask {
    block: Block,
    capture_names: Vec<String>,
}

struct ChannelState {
    queue: VecDeque<Value>,
    closed: bool,
}

type ChannelInner = (Mutex<ChannelState>, Condvar);
type ChannelHandle = Rc<ChannelInner>;
type ChannelHandles = HashMap<u64, ChannelHandle>;
type BuildChannelHandle = Arc<(Mutex<BuildChannelState>, Condvar)>;

impl ChannelSender {
    fn new(inner: ChannelHandle) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn new_with_state(inner: ChannelHandle) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        let state: ChannelHandle = Rc::new((
            Mutex::new(ChannelState {
                queue: VecDeque::new(),
                closed: false,
            }),
            Condvar::new(),
        ));
        Self {
            inner: state,
            handle: Some(handle),
            origin: None,
        }
    }

    fn send(&self, value: Value) -> Result<(), String> {
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

impl ChannelReceiver {
    fn new(inner: ChannelHandle) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn new_with_state(inner: ChannelHandle) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        let state: ChannelHandle = Rc::new((
            Mutex::new(ChannelState {
                queue: VecDeque::new(),
                closed: false,
            }),
            Condvar::new(),
        ));
        Self {
            inner: state,
            handle: Some(handle),
            origin: None,
        }
    }

    fn recv(&self) -> Option<Value> {
        let debug = env::var_os("PRIME_DEBUG_RECV").is_some();
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        loop {
            if let Some(v) = guard.queue.pop_front() {
                if debug {
                    eprintln!("[prime-debug] recv -> Some({})", describe_value(&v));
                }
                return Some(v);
            }
            if guard.closed {
                if debug {
                    eprintln!("[prime-debug] recv -> None (closed)");
                }
                return None;
            }
            guard = cv.wait(guard).unwrap();
        }
    }

    fn recv_timeout(&self, millis: i64) -> Option<Value> {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        let end = std::time::Instant::now()
            .checked_add(std::time::Duration::from_millis(millis.max(0) as u64));
        loop {
            if let Some(v) = guard.queue.pop_front() {
                return Some(v);
            }
            if guard.closed {
                return None;
            }
            if let Some(deadline) = end {
                let now = std::time::Instant::now();
                if now >= deadline {
                    return None;
                }
                let remaining = deadline - now;
                guard = cv.wait_timeout(guard, remaining).unwrap().0;
            } else {
                guard = cv.wait(guard).unwrap();
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

impl JoinHandleValue {
    fn new(value: Value) -> Self {
        Self {
            result: Rc::new(Mutex::new(JoinResult::Immediate(Some(value)))),
            handle: None,
        }
    }

    fn new_build(handle: thread::JoinHandle<Result<BuildEvaluation, String>>) -> Self {
        Self {
            result: Rc::new(Mutex::new(JoinResult::BuildThread(Some(handle)))),
            handle: None,
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            result: Rc::new(Mutex::new(JoinResult::Immediate(None))),
            handle: Some(handle),
        }
    }

    fn join_with(&self, compiler: &mut Compiler) -> Result<Value, String> {
        if let Some(handle) = self.handle {
            return Err(format!(
                "join handle {:?} cannot be joined during build evaluation",
                handle
            ));
        }
        let mut guard = self.result.lock().unwrap();
        match &mut *guard {
            JoinResult::Immediate(slot) => slot
                .take()
                .ok_or_else(|| "join handle already consumed".to_string()),
            JoinResult::BuildThread(handle_slot) => {
                let handle = handle_slot
                    .take()
                    .ok_or_else(|| "join handle already consumed".to_string())?;
                match handle.join() {
                    Ok(Ok(build)) => {
                        let mut channels = compiler.apply_build_effects(build.effects)?;
                        compiler.build_value_to_value(build.value, &mut channels)
                    }
                    Ok(Err(err)) => Err(err),
                    Err(_) => Err("spawned task panicked".into()),
                }
            }
        }
    }
}

impl TaskValue {
    fn ready(value: EvaluatedValue) -> Self {
        Self {
            state: Rc::new((Mutex::new(Some(value)), Condvar::new())),
            handle: None,
            kind: TaskResultKind::Ready,
            pending: None,
        }
    }

    fn deferred(block: Block, capture_names: Vec<String>) -> Self {
        Self {
            state: Rc::new((Mutex::new(None), Condvar::new())),
            handle: None,
            kind: TaskResultKind::Deferred,
            pending: Some(Rc::new(Mutex::new(Some(DeferredTask {
                block,
                capture_names,
            })))),
        }
    }

    fn with_handle(handle: LLVMValueRef, kind: TaskResultKind) -> Self {
        Self {
            state: Rc::new((Mutex::new(None), Condvar::new())),
            handle: Some(handle),
            kind,
            pending: None,
        }
    }

    #[allow(dead_code)]
    fn complete(&self, value: EvaluatedValue) {
        let (lock, cv) = &*self.state;
        if let Ok(mut guard) = lock.lock() {
            *guard = Some(value);
            cv.notify_all();
        }
    }

    fn take(&self, compiler: &mut Compiler) -> Result<EvaluatedValue, String> {
        match self.kind {
            TaskResultKind::Ready => {
                let (lock, cv) = &*self.state;
                let mut guard = lock.lock().map_err(|_| "task state poisoned".to_string())?;
                while guard.is_none() {
                    guard = cv
                        .wait(guard)
                        .map_err(|_| "task wait poisoned".to_string())?;
                }
                Ok(guard.take().unwrap())
            }
            TaskResultKind::RuntimeUnit => {
                let handle = self
                    .handle
                    .ok_or_else(|| "runtime task missing handle".to_string())?;
                compiler.await_runtime_task(handle, TaskResultKind::RuntimeUnit)
            }
            TaskResultKind::RuntimeOption => {
                let handle = self
                    .handle
                    .ok_or_else(|| "runtime task missing handle".to_string())?;
                compiler.await_runtime_task(handle, TaskResultKind::RuntimeOption)
            }
            TaskResultKind::Deferred => {
                let pending = self
                    .pending
                    .clone()
                    .ok_or_else(|| "deferred task missing body".to_string())?;
                let task = pending
                    .lock()
                    .map_err(|_| "deferred task poisoned".to_string())?
                    .take()
                    .ok_or_else(|| "deferred task already taken".to_string())?;
                let mut captures = Vec::new();
                for name in task.capture_names {
                    if let Some(value) = compiler.get_var(&name) {
                        captures.push((name, value));
                    }
                }
                compiler.eval_async_block(&task.block, &captures)
            }
        }
    }
}

#[derive(Clone)]
struct StructValue {
    name: String,
    fields: HashMap<String, Value>,
}

#[derive(Clone)]
struct Binding {
    cell: Rc<Mutex<EvaluatedValue>>,
    mutable: bool,
    slot: Option<LLVMValueRef>, // Optional alloca backing for mutable scalars (embedded loops).
}

#[derive(Clone, Copy)]
struct RuntimeValue {
    handle: LLVMValueRef,
}

#[derive(Clone)]
struct EvaluatedValue {
    value: Value,
    runtime: Option<RuntimeValue>,
}

impl EvaluatedValue {
    fn from_value(value: Value) -> Self {
        Self {
            value,
            runtime: None,
        }
    }

    fn runtime_handle(&self) -> Option<LLVMValueRef> {
        self.runtime.map(|rt| rt.handle)
    }

    fn into_value(self) -> Value {
        self.value
    }

    fn value(&self) -> &Value {
        &self.value
    }

    fn value_mut(&mut self) -> &mut Value {
        &mut self.value
    }

    fn set_runtime(&mut self, handle: LLVMValueRef) {
        self.runtime = Some(RuntimeValue { handle });
    }

    fn with_runtime(mut self, handle: LLVMValueRef) -> Self {
        self.set_runtime(handle);
        self
    }
}

impl From<Value> for EvaluatedValue {
    fn from(value: Value) -> Self {
        EvaluatedValue::from_value(value)
    }
}

enum FlowSignal {
    Break,
    Continue,
    Return(Vec<EvaluatedValue>),
    Propagate(EvaluatedValue),
}

enum BlockEval {
    Value(EvaluatedValue),
    Flow(FlowSignal),
}

enum EvalOutcome<T> {
    Value(T),
    Flow(FlowSignal),
}

impl StringValue {
    fn new(llvm: LLVMValueRef, text: Arc<String>) -> Self {
        Self { llvm, text }
    }
}

impl BoxValue {
    fn new(value: Value) -> Self {
        Self {
            handle: None,
            cell: Rc::new(Mutex::new(value)),
            origin: None,
        }
    }

    fn replace(&self, value: Value) -> Value {
        std::mem::replace(&mut *self.cell.lock().unwrap(), value)
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            handle: Some(handle),
            cell: Rc::new(Mutex::new(Value::Unit)),
            origin: None,
        }
    }
}

impl SliceValue {
    fn new() -> Self {
        Self {
            handle: None,
            items: Rc::new(Mutex::new(Vec::new())),
        }
    }

    fn from_vec(items: Vec<Value>) -> Self {
        Self {
            handle: None,
            items: Rc::new(Mutex::new(items)),
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            handle: Some(handle),
            items: Rc::new(Mutex::new(Vec::new())),
        }
    }

    fn push(&self, value: Value) {
        self.items.lock().unwrap().push(value);
    }

    fn len(&self) -> usize {
        self.items.lock().unwrap().len()
    }

    fn get(&self, index: usize) -> Option<Value> {
        self.items.lock().unwrap().get(index).cloned()
    }

    fn set(&self, index: usize, value: Value) -> bool {
        let mut guard = self.items.lock().unwrap();
        if let Some(slot) = guard.get_mut(index) {
            *slot = value;
            true
        } else {
            false
        }
    }

    fn remove(&self, index: usize) -> Option<Value> {
        let mut guard = self.items.lock().unwrap();
        if index >= guard.len() {
            return None;
        }
        Some(guard.remove(index))
    }
}

impl MapValue {
    fn new() -> Self {
        Self {
            handle: None,
            entries: Rc::new(Mutex::new(BTreeMap::new())),
        }
    }

    fn from_entries(entries: Vec<(String, Value)>) -> Self {
        let map = Self::new();
        for (key, value) in entries {
            map.insert(key, value);
        }
        map
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            handle: Some(handle),
            entries: Rc::new(Mutex::new(BTreeMap::new())),
        }
    }

    fn insert(&self, key: String, value: Value) {
        self.entries.lock().unwrap().insert(key, value);
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.entries.lock().unwrap().get(key).cloned()
    }

    fn len(&self) -> usize {
        self.entries.lock().unwrap().len()
    }

    fn remove(&self, key: &str) -> Option<Value> {
        self.entries.lock().unwrap().remove(key)
    }
}

fn expect_constant_int(int: &IntValue) -> Result<i128, String> {
    int.constant.ok_or_else(|| {
        "Non-constant integer cannot be captured for parallel build execution".into()
    })
}

fn expect_constant_float(float: &FloatValue) -> Result<f64, String> {
    float
        .constant
        .ok_or_else(|| "Non-constant float cannot be captured for parallel build execution".into())
}

fn expect_constant_bool(bool_val: &BoolValue) -> Result<bool, String> {
    bool_val.constant.ok_or_else(|| {
        "Non-constant boolean cannot be captured for parallel build execution".into()
    })
}

struct BuildCaptureContext {
    channels: HashMap<*const (), (u64, BuildChannelHandle)>,
    references: HashMap<*const (), BuildReference>,
    next_channel_id: u64,
    closure_snapshots: HashMap<usize, ClosureSnapshot>,
    join_handles: HashMap<*const (), BuildJoinHandle>,
    clock_ms: i128,
}

impl BuildCaptureContext {
    fn new() -> Self {
        Self {
            channels: HashMap::new(),
            references: HashMap::new(),
            next_channel_id: 0,
            closure_snapshots: HashMap::new(),
            join_handles: HashMap::new(),
            clock_ms: 0,
        }
    }

    fn channel_from_inner(
        &mut self,
        inner: &ChannelHandle,
    ) -> Result<(u64, BuildChannelHandle), String> {
        let key = Rc::as_ptr(inner) as *const ();
        if let Some((id, existing)) = self.channels.get(&key) {
            return Ok((*id, existing.clone()));
        }
        let (queue, closed) = {
            let (lock, _) = &**inner;
            let guard = lock
                .lock()
                .map_err(|_| "Channel poisoned while capturing build snapshot".to_string())?;
            (guard.queue.clone(), guard.closed)
        };
        let mut converted = VecDeque::new();
        for value in queue {
            converted.push_back(value_to_build_value_with_ctx(&value, self)?);
        }
        let state = BuildChannelState {
            queue: converted,
            closed,
        };
        let shared: BuildChannelHandle = Arc::new((Mutex::new(state), Condvar::new()));
        let id = self.next_channel_id;
        self.next_channel_id += 1;
        self.channels.insert(key, (id, shared.clone()));
        Ok((id, shared))
    }

    fn reference_from_cell(
        &mut self,
        cell: &Rc<Mutex<EvaluatedValue>>,
        mutable: bool,
    ) -> Result<BuildReference, String> {
        let key = Rc::as_ptr(cell) as *const ();
        if let Some(existing) = self.references.get(&key) {
            return Ok(existing.clone());
        }
        let guard = cell
            .lock()
            .map_err(|_| "Reference poisoned while capturing build snapshot".to_string())?;
        let inner = value_to_build_value_with_ctx(guard.value(), self)?;
        let build_ref = BuildReference {
            cell: Arc::new(Mutex::new(inner)),
            mutable,
        };
        self.references.insert(key, build_ref.clone());
        Ok(build_ref)
    }
}

fn closure_body_span(body: &ClosureBody) -> Span {
    match body {
        ClosureBody::Block(block) => block.span,
        ClosureBody::Expr(expr) => expr.span,
    }
}

fn format_template_to_build(
    template: &FormatTemplateValue,
    ctx: &mut BuildCaptureContext,
) -> Result<BuildFormatTemplate, String> {
    let mut segments = Vec::with_capacity(template.segments.len());
    for segment in &template.segments {
        let converted = match segment {
            FormatRuntimeSegment::Literal(text) => {
                FormatRuntimeSegmentGeneric::Literal(text.clone())
            }
            FormatRuntimeSegment::Named(value) => FormatRuntimeSegmentGeneric::Named(
                value_to_build_value_with_ctx(value.value(), ctx)?,
            ),
            FormatRuntimeSegment::Implicit => FormatRuntimeSegmentGeneric::Implicit,
        };
        segments.push(converted);
    }
    Ok(BuildFormatTemplate {
        segments,
        implicit_placeholders: template.implicit_placeholders,
    })
}

#[allow(dead_code)]
fn value_to_build_value(value: &Value) -> Result<BuildValue, String> {
    let mut ctx = BuildCaptureContext::new();
    value_to_build_value_with_ctx(value, &mut ctx)
}

fn value_to_build_value_with_ctx(
    value: &Value,
    ctx: &mut BuildCaptureContext,
) -> Result<BuildValue, String> {
    match value {
        Value::Unit => Ok(BuildValue::Unit),
        Value::Int(int) => Ok(BuildValue::Int(expect_constant_int(int)?)),
        Value::Float(float) => Ok(BuildValue::Float(expect_constant_float(float)?)),
        Value::Bool(flag) => Ok(BuildValue::Bool(expect_constant_bool(flag)?)),
        Value::Str(text) => Ok(BuildValue::String((*text.text).clone())),
        Value::Tuple(items) => {
            let mut converted = Vec::with_capacity(items.len());
            for item in items {
                converted.push(value_to_build_value_with_ctx(item, ctx)?);
            }
            Ok(BuildValue::Tuple(converted))
        }
        Value::Struct(instance) => {
            let mut fields = BTreeMap::new();
            for (name, field) in &instance.fields {
                fields.insert(name.clone(), value_to_build_value_with_ctx(field, ctx)?);
            }
            Ok(BuildValue::Struct {
                name: instance.name.clone(),
                fields,
            })
        }
        Value::Enum(value) => {
            let mut converted = Vec::with_capacity(value.values.len());
            for val in &value.values {
                converted.push(value_to_build_value_with_ctx(val, ctx)?);
            }
            Ok(BuildValue::Enum {
                enum_name: value.enum_name.clone(),
                variant: value.variant.clone(),
                values: converted,
                variant_index: value.variant_index,
            })
        }
        Value::Range(range) => Ok(BuildValue::Range {
            start: expect_constant_int(&range.start).unwrap_or(0),
            end: expect_constant_int(&range.end).unwrap_or(0),
            inclusive: range.inclusive,
        }),
        Value::Boxed(boxed) => {
            let guard = boxed
                .cell
                .lock()
                .map_err(|_| "Box value poisoned while capturing build snapshot")?;
            Ok(BuildValue::Boxed(Box::new(value_to_build_value_with_ctx(
                &guard, ctx,
            )?)))
        }
        Value::Slice(slice) => {
            let guard = slice
                .items
                .lock()
                .map_err(|_| "Slice value poisoned while capturing build snapshot")?;
            let mut converted = Vec::with_capacity(guard.len());
            for item in guard.iter() {
                converted.push(value_to_build_value_with_ctx(item, ctx)?);
            }
            Ok(BuildValue::Slice(converted))
        }
        Value::Map(map) => {
            let guard = map
                .entries
                .lock()
                .map_err(|_| "Map value poisoned while capturing build snapshot")?;
            let mut converted = BTreeMap::new();
            for (key, value) in guard.iter() {
                converted.insert(key.clone(), value_to_build_value_with_ctx(value, ctx)?);
            }
            Ok(BuildValue::Map(converted))
        }
        Value::Task(task) => {
            let (lock, _) = &*task.state;
            let guard = lock
                .lock()
                .map_err(|_| "task value poisoned while capturing build snapshot")?;
            if let Some(inner) = guard.as_ref() {
                let converted = value_to_build_value_with_ctx(inner.value(), ctx)?;
                Ok(BuildValue::Task(Box::new(BuildTask { result: converted })))
            } else {
                Err("task value not ready during build snapshot".into())
            }
        }
        Value::Iterator(iter) => {
            let mut converted = Vec::new();
            {
                let items_guard = iter
                    .items
                    .lock()
                    .map_err(|_| "Iterator value poisoned while capturing build snapshot")?;
                for value in items_guard.iter() {
                    converted.push(value_to_build_value_with_ctx(value, ctx)?);
                }
            }
            let index = *iter
                .index
                .lock()
                .map_err(|_| "Iterator index poisoned while capturing build snapshot")?;
            Ok(BuildValue::Iterator(BuildIterator {
                items: Arc::new(Mutex::new(converted)),
                index: Arc::new(Mutex::new(index)),
            }))
        }
        Value::FormatTemplate(template) => Ok(BuildValue::FormatTemplate(
            format_template_to_build(template, ctx)?,
        )),
        Value::Sender(sender) => {
            let (id, inner) = ctx.channel_from_inner(&sender.inner)?;
            Ok(BuildValue::ChannelSender(BuildChannelSender::new(
                id, inner,
            )))
        }
        Value::Receiver(receiver) => {
            let (id, inner) = ctx.channel_from_inner(&receiver.inner)?;
            Ok(BuildValue::ChannelReceiver(BuildChannelReceiver::new(
                id, inner,
            )))
        }
        Value::Reference(reference) => {
            let captured = ctx.reference_from_cell(&reference.cell, reference.mutable)?;
            Ok(BuildValue::Reference(captured))
        }
        Value::Pointer(pointer) => {
            let captured = ctx.reference_from_cell(&pointer.cell, pointer.mutable)?;
            Ok(BuildValue::Reference(captured))
        }
        Value::Closure(closure) => {
            let snapshot = ctx
                .closure_snapshots
                .get(&closure.id)
                .ok_or_else(|| "closure capture metadata missing for build snapshot".to_string())?
                .clone();
            let mut captures = Vec::with_capacity(snapshot.captures.len());
            for (captured, raw_value) in snapshot.captures.iter().zip(snapshot.values.iter()) {
                captures.push(BuildCaptured {
                    name: captured.name.clone(),
                    mutable: captured.mutable,
                    mode: captured.mode.clone(),
                    value: value_to_build_value_with_ctx(raw_value, ctx)?,
                    ty: captured.ty.clone(),
                    origin: captured.span,
                });
            }
            let origin_span = snapshot
                .ret
                .as_ref()
                .map(|ann| ann.span)
                .unwrap_or_else(|| closure_body_span(&snapshot.body));
            Ok(BuildValue::Closure(BuildClosure {
                params: snapshot.params.clone(),
                body: snapshot.body.clone(),
                ret: snapshot.ret.clone(),
                captures,
                origin: origin_span,
            }))
        }
        Value::JoinHandle(handle) => {
            let key = Rc::as_ptr(&handle.result) as *const ();
            if let Some(existing) = ctx.join_handles.get(&key) {
                return Ok(BuildValue::JoinHandle(existing.clone()));
            }
            let mut guard = handle.result.lock().unwrap();
            match &mut *guard {
                JoinResult::Immediate(Some(value)) => {
                    let build_val = value_to_build_value_with_ctx(value, ctx)?;
                    let eval = BuildEvaluation {
                        value: build_val,
                        effects: Vec::new(),
                    };
                    *guard = JoinResult::Immediate(None);
                    let captured = BuildJoinHandle::ready(eval);
                    ctx.join_handles.insert(key, captured.clone());
                    Ok(BuildValue::JoinHandle(captured))
                }
                JoinResult::BuildThread(slot) => {
                    if let Some(thread) = slot.take() {
                        match thread.join() {
                            Ok(result) => {
                                let eval = result?;
                                *guard = JoinResult::Immediate(None);
                                let captured = BuildJoinHandle::ready(eval.clone());
                                ctx.join_handles.insert(key, captured.clone());
                                Ok(BuildValue::JoinHandle(captured))
                            }
                            Err(_) => Err("spawned task panicked".into()),
                        }
                    } else {
                        Err("join handle already consumed".into())
                    }
                }
                JoinResult::Immediate(None) => Err("join handle already consumed".into()),
            }
        }
        Value::Moved => Err(format!(
            "{} cannot be captured for parallel build execution",
            describe_value(value)
        )),
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
    variant_index: u32,
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

impl BoolValue {
    fn new(llvm: LLVMValueRef, constant: Option<bool>) -> Self {
        Self { llvm, constant }
    }

    fn llvm(&self) -> LLVMValueRef {
        self.llvm
    }

    fn constant(&self) -> Option<bool> {
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
    fn log_rss(tag: &str, enabled: bool) {
        if !enabled {
            return;
        }
        if let Ok(mut status) = fs::File::open("/proc/self/status") {
            let mut buf = String::new();
            if status.read_to_string(&mut buf).is_ok() {
                if let Some(line) = buf.lines().find(|l| l.starts_with("VmRSS:")) {
                    let mut parts = line.split_whitespace();
                    // VmRSS: <kb> kB
                    let _label = parts.next();
                    let kb = parts.next().unwrap_or("?");
                    eprintln!("{tag} rss_kb={kb}");
                }
            }
        }
    }

    fn evaluated(&mut self, value: Value) -> EvaluatedValue {
        let mut evaluated = EvaluatedValue::from_value(value);
        if let Some(handle) = self.pending_runtime.take() {
            evaluated.set_runtime(handle);
        }
        evaluated
    }

    fn runtime_release(&mut self, handle: LLVMValueRef) {
        if handle.is_null() {
            return;
        }
        let _ = self.call_runtime(
            self.runtime_abi.prime_value_release,
            self.runtime_abi.prime_value_release_ty,
            &mut [handle],
            "value_release",
        );
    }

    fn runtime_close(&mut self, handle: LLVMValueRef) {
        if handle.is_null() {
            return;
        }
        let _ = self.call_runtime(
            self.runtime_abi.prime_close,
            self.runtime_abi.prime_close_ty,
            &mut [handle],
            "channel_close",
        );
    }

    fn build_channel_handles(&mut self) -> Option<(LLVMValueRef, LLVMValueRef)> {
        unsafe {
            if self.runtime_abi.handle_type.is_null() {
                return None;
            }
            let sender_slot = LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("chan_sender_out").unwrap().as_ptr(),
            );
            let receiver_slot = LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("chan_receiver_out").unwrap().as_ptr(),
            );
            let _ = self.call_runtime(
                self.runtime_abi.prime_channel_new,
                self.runtime_abi.prime_channel_new_ty,
                &mut [sender_slot, receiver_slot],
                "channel_new",
            );
            let sender = LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                sender_slot,
                CString::new("chan_sender").unwrap().as_ptr(),
            );
            let receiver = LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                receiver_slot,
                CString::new("chan_receiver").unwrap().as_ptr(),
            );
            Some((sender, receiver))
        }
    }

    fn seed_channel_queue(
        &mut self,
        sender_handle: LLVMValueRef,
        queued: &[Value],
    ) -> Result<(), String> {
        for value in queued {
            let handle = self.build_runtime_handle(value.clone())?;
            let _ = self.call_runtime(
                self.runtime_abi.prime_send,
                self.runtime_abi.prime_send_ty,
                &mut [sender_handle, handle],
                "channel_seed_send",
            );
            self.runtime_release(handle);
        }
        Ok(())
    }

    fn ensure_runtime_symbols(&mut self) {
        unsafe {
            let module = self.module;
            if module.is_null() {
                return;
            }
            let handle_ready = !self.runtime_abi.handle_type.is_null()
                && LLVMGetModuleContext(module) == self.context;
            let unit_parent = if self.runtime_abi.prime_unit_new.is_null() {
                ptr::null_mut()
            } else {
                LLVMGetGlobalParent(self.runtime_abi.prime_unit_new)
            };
            let needs_refresh = !handle_ready
                || unit_parent.is_null()
                || LLVMIsAFunction(self.runtime_abi.prime_unit_new).is_null()
                || unit_parent != module;
            if needs_refresh {
                self.runtime_abi =
                    RuntimeAbi::declare(self.context, module, self.target.pointer_width_bits());
            }
        }
    }

    fn runtime_handles_enabled(&self) -> bool {
        if self.runtime_abi.handle_type.is_null() {
            return false;
        }
        self.force_runtime_handles || env::var("PRIME_ENABLE_RT_HANDLES").is_ok()
    }

    fn ensure_async_supported(&self) -> Result<(), String> {
        match &self.target {
            BuildTarget::Host => Ok(()),
            BuildTarget::Triple(triple) => {
                let supported = self.target.is_esp32_xtensa()
                    || self.target.is_esp32_xtensa_espidf()
                    || self.target.is_esp32c3();
                if !supported {
                    Err(format!(
                        "async/await requires runtime handles; target `{}` is not supported (embedded targets: {})",
                        triple,
                        embedded_target_hint()
                    ))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn await_runtime_task(
        &mut self,
        task_handle: LLVMValueRef,
        kind: TaskResultKind,
    ) -> Result<EvaluatedValue, String> {
        if task_handle.is_null() {
            return Err("runtime task missing handle".into());
        }
        self.ensure_runtime_symbols();
        unsafe {
            if self.builder.is_null() || LLVMGetInsertBlock(self.builder).is_null() {
                return Err("await requires active IR builder".into());
            }
        }
        let slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("task_result").unwrap().as_ptr(),
            )
        };
        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let poll_block = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("task_poll").unwrap().as_ptr(),
            )
        };
        let ready_block = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("task_ready").unwrap().as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, poll_block) };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, poll_block) };
        let mut call_args = [task_handle, slot];
        let status = self.call_runtime(
            self.runtime_abi.prime_task_poll,
            self.runtime_abi.prime_task_poll_ty,
            &mut call_args,
            "task_poll",
        );
        let ok_const =
            unsafe { LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Ok as u64, 0) };
        let cond = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                status,
                ok_const,
                CString::new("task_ready_flag").unwrap().as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cond, ready_block, poll_block) };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, ready_block) };
        let loaded = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                slot,
                CString::new("task_result_loaded").unwrap().as_ptr(),
            )
        };
        let mut value = match kind {
            TaskResultKind::RuntimeUnit => self.evaluated(Value::Unit),
            TaskResultKind::RuntimeOption => self.evaluated(Value::Enum(EnumValue {
                enum_name: "Option".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            })),
            TaskResultKind::Ready => {
                return Err("ready task routed through runtime await".into());
            }
            TaskResultKind::Deferred => {
                return Err("deferred task routed through runtime await".into());
            }
        };
        value.set_runtime(loaded);
        Ok(value)
    }

    fn maybe_attach_runtime_handle(&mut self, value: &mut EvaluatedValue) -> Option<LLVMValueRef> {
        if value.runtime_handle().is_some() {
            return value.runtime_handle();
        }
        if !self.runtime_handles_enabled() {
            return None;
        }
        self.ensure_runtime_symbols();
        // Skip in compiler-mode to avoid unsafe builder states during multiple compilations.
        if self.runtime_abi.handle_type.is_null() {
            return None;
        }
        unsafe {
            if self.builder.is_null()
                || LLVMGetInsertBlock(self.builder).is_null()
                || self.runtime_abi.handle_type.is_null()
            {
                return None;
            }
        }
        self.build_runtime_handle_scoped(value.value().clone())
            .inspect(|&handle| value.set_runtime(handle))
    }

    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_target(BuildTarget::host())
    }

    pub fn with_target(target: BuildTarget) -> Self {
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
                target,
                runtime_abi: RuntimeAbi::empty(),
                scopes: Vec::new(),
                structs: HashMap::new(),
                functions: HashMap::new(),
                enum_variants: HashMap::new(),
                interfaces: HashMap::new(),
                impls: HashSet::new(),
                consts: Vec::new(),
                drop_impls: HashMap::new(),
                active_mut_borrows: HashSet::new(),
                borrow_frames: Vec::new(),
                cleanup_stack: Vec::new(),
                deprecated_warnings: HashSet::new(),
                module_stack: Vec::new(),
                return_type_stack: Vec::new(),
                closure_counter: 0,
                closures: HashMap::new(),
                closure_snapshots: HashMap::new(),
                closure_value_type: ptr::null_mut(),
                closure_envs: Vec::new(),
                build_clock_ms: 0,
                pending_runtime: None,
                force_runtime_handles: false,
            };
            compiler.reset_module();
            compiler
        }
    }

    fn apply_build_effects(&mut self, effects: Vec<BuildEffect>) -> Result<ChannelHandles, String> {
        let mut channel_handles: ChannelHandles = HashMap::new();
        for effect in effects {
            match effect {
                BuildEffect::Out(values) => {
                    if values.is_empty() {
                        continue;
                    }
                    let mut iter = values.into_iter();
                    let first = iter.next().unwrap();
                    let first_value = self.build_value_to_value(first, &mut channel_handles)?;
                    match first_value {
                        Value::FormatTemplate(template) => {
                            let provided: Result<Vec<EvaluatedValue>, String> = iter
                                .map(|v| {
                                    self.build_value_to_value(v, &mut channel_handles)
                                        .map(EvaluatedValue::from_value)
                                })
                                .collect();
                            let provided = provided?;
                            if template.implicit_placeholders != provided.len() {
                                return Err(format!(
                                    "Format string expects {} argument(s), got {}",
                                    template.implicit_placeholders,
                                    provided.len()
                                ));
                            }
                            self.emit_format_template(template, provided)?;
                        }
                        other => {
                            if iter.next().is_some() {
                                return Err(
                                    "out() with multiple arguments requires a format string literal"
                                        .into(),
                                );
                            }
                            self.emit_out_value(other.into())?;
                        }
                    }
                }
                BuildEffect::ChannelCreate { id } => {
                    channel_handles.insert(
                        id,
                        Rc::new((
                            Mutex::new(ChannelState {
                                queue: VecDeque::new(),
                                closed: false,
                            }),
                            Condvar::new(),
                        )),
                    );
                }
                BuildEffect::ChannelSend { id, value } => {
                    let inner = channel_handles
                        .get(&id)
                        .cloned()
                        .ok_or_else(|| "channel send without creation".to_string())?;
                    let built = self.build_value_to_value(value, &mut channel_handles)?;
                    let (lock, cv) = inner.as_ref();
                    let mut guard = lock.lock().unwrap();
                    if guard.closed {
                        continue;
                    }
                    guard.queue.push_back(built);
                    cv.notify_one();
                }
                BuildEffect::ChannelClose { id } => {
                    if let Some(inner) = channel_handles.get(&id) {
                        let (lock, cv) = inner.as_ref();
                        let mut guard = lock.lock().unwrap();
                        guard.closed = true;
                        cv.notify_all();
                    }
                }
                BuildEffect::FsExists { .. }
                | BuildEffect::FsRead { .. }
                | BuildEffect::FsWrite { .. } => {
                    // Recorded only for deterministic replays; nothing to apply in compiled artifacts.
                }
                BuildEffect::NowMs { value } => {
                    self.build_clock_ms = self.build_clock_ms.max(value.saturating_add(1));
                }
                BuildEffect::SleepMs { millis } => {
                    if millis > 0 {
                        self.build_clock_ms = self.build_clock_ms.saturating_add(millis);
                    }
                }
            }
        }
        Ok(channel_handles)
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

    fn const_bool_value(&self, value: bool) -> BoolValue {
        unsafe {
            let llvm = LLVMConstInt(self.runtime_abi.bool_type, value as u64, 0);
            BoolValue::new(llvm, Some(value))
        }
    }

    fn build_value_to_value(
        &mut self,
        value: BuildValue,
        channels: &mut ChannelHandles,
    ) -> Result<Value, String> {
        match value {
            BuildValue::Unit => Ok(Value::Unit),
            BuildValue::Int(v) => Ok(Value::Int(self.const_int_value(v))),
            BuildValue::Float(v) => Ok(Value::Float(self.const_float_value(v))),
            BuildValue::Bool(v) => Ok(Value::Bool(self.const_bool_value(v))),
            BuildValue::String(text) => self.build_string_constant(text),
            BuildValue::Tuple(items) => {
                let mut converted = Vec::with_capacity(items.len());
                for item in items {
                    converted.push(self.build_value_to_value(item, channels)?);
                }
                Ok(Value::Tuple(converted))
            }
            BuildValue::Struct { name, fields } => {
                let mut converted = HashMap::new();
                for (key, value) in fields {
                    converted.insert(key, self.build_value_to_value(value, channels)?);
                }
                Ok(Value::Struct(StructValue::new(name, converted)))
            }
            BuildValue::Enum {
                enum_name,
                variant,
                values,
                variant_index,
            } => {
                let mut converted = Vec::with_capacity(values.len());
                for value in values {
                    converted.push(self.build_value_to_value(value, channels)?);
                }
                Ok(Value::Enum(EnumValue {
                    enum_name,
                    variant,
                    values: converted,
                    variant_index,
                }))
            }
            BuildValue::Range {
                start,
                end,
                inclusive,
            } => Ok(Value::Range(RangeValue {
                start: self.const_int_value(start),
                end: self.const_int_value(end),
                inclusive,
            })),
            BuildValue::Task(task) => {
                let inner = self.build_value_to_value(task.result.clone(), channels)?;
                let task_value = TaskValue::ready(self.evaluated(inner));
                Ok(Value::Task(Box::new(task_value)))
            }
            BuildValue::Boxed(inner) => Ok(Value::Boxed(BoxValue::new(
                self.build_value_to_value(*inner, channels)?,
            ))),
            BuildValue::Slice(items) => {
                let mut converted = Vec::with_capacity(items.len());
                for item in items {
                    converted.push(self.build_value_to_value(item, channels)?);
                }
                Ok(Value::Slice(SliceValue::from_vec(converted)))
            }
            BuildValue::Map(entries) => {
                let mut converted = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    converted.push((key, self.build_value_to_value(value, channels)?));
                }
                Ok(Value::Map(MapValue::from_entries(converted)))
            }
            BuildValue::Iterator(iter) => {
                let mut converted = Vec::new();
                for value in iter.items.lock().unwrap().iter() {
                    converted.push(self.build_value_to_value(value.clone(), channels)?);
                }
                Ok(Value::Iterator(IteratorValue {
                    items: Rc::new(Mutex::new(converted)),
                    index: Rc::new(Mutex::new(*iter.index.lock().unwrap())),
                }))
            }
            BuildValue::JoinHandle(handle) => match handle.into_outcome()? {
                BuildJoinOutcome::Thread(thread) => Ok(Value::JoinHandle(Box::new(
                    JoinHandleValue::new_build(thread),
                ))),
                BuildJoinOutcome::Ready(eval) => {
                    let mut local_channels = self.apply_build_effects(eval.effects)?;
                    self.build_value_to_value(eval.value, &mut local_channels)
                }
            },
            BuildValue::Reference(reference) => {
                let inner = self.build_value_to_value(
                    reference
                        .cell
                        .lock()
                        .map_err(|_| "reference poisoned in build result")?
                        .clone(),
                    channels,
                )?;
                let cell = Rc::new(Mutex::new(EvaluatedValue::from_value(inner)));
                Ok(Value::Reference(ReferenceValue {
                    cell,
                    mutable: reference.mutable,
                    origin: None,
                    handle: None,
                }))
            }
            BuildValue::FormatTemplate(template) => {
                let mut segments = Vec::with_capacity(template.segments.len());
                for segment in template.segments {
                    let converted = match segment {
                        FormatRuntimeSegmentGeneric::Literal(text) => {
                            FormatRuntimeSegmentGeneric::Literal(text)
                        }
                        FormatRuntimeSegmentGeneric::Named(value) => {
                            let converted_value = self.build_value_to_value(value, channels)?;
                            FormatRuntimeSegmentGeneric::Named(EvaluatedValue::from_value(
                                converted_value,
                            ))
                        }
                        FormatRuntimeSegmentGeneric::Implicit => {
                            FormatRuntimeSegmentGeneric::Implicit
                        }
                    };
                    segments.push(converted);
                }
                Ok(Value::FormatTemplate(FormatTemplateValue {
                    segments,
                    implicit_placeholders: template.implicit_placeholders,
                }))
            }
            BuildValue::ChannelSender(sender) => {
                let inner = if let Some(existing) = channels.get(&sender.id).cloned() {
                    existing
                } else {
                    let (queue, closed) = sender.snapshot();
                    let shared = Rc::new((
                        Mutex::new(ChannelState {
                            queue: VecDeque::new(),
                            closed,
                        }),
                        Condvar::new(),
                    ));
                    channels.insert(sender.id, shared.clone());
                    {
                        let (lock, _) = &*shared;
                        let mut guard = lock.lock().unwrap();
                        for item in queue {
                            let built = self.build_value_to_value(item, channels)?;
                            guard.queue.push_back(built);
                        }
                    }
                    shared
                };
                Ok(Value::Sender(ChannelSender::new_with_state(inner)))
            }
            BuildValue::ChannelReceiver(receiver) => {
                let inner = if let Some(existing) = channels.get(&receiver.id).cloned() {
                    existing
                } else {
                    let (queue, closed) = {
                        let (lock, _) = &*receiver.inner;
                        let guard = lock.lock().unwrap();
                        (guard.queue.clone(), guard.closed)
                    };
                    let shared = Rc::new((
                        Mutex::new(ChannelState {
                            queue: VecDeque::new(),
                            closed,
                        }),
                        Condvar::new(),
                    ));
                    channels.insert(receiver.id, shared.clone());
                    {
                        let (lock, _) = &*shared;
                        let mut guard = lock.lock().unwrap();
                        for item in queue {
                            let built = self.build_value_to_value(item, channels)?;
                            guard.queue.push_back(built);
                        }
                    }
                    shared
                };
                Ok(Value::Receiver(ChannelReceiver::new_with_state(inner)))
            }
            BuildValue::DeferredCall {
                name,
                type_args,
                args,
            } => {
                let mut evaluated = Vec::with_capacity(args.len());
                for arg in args {
                    evaluated.push(self.build_value_to_value(arg, channels)?);
                }
                let mut evaluated_wrapped = Vec::with_capacity(evaluated.len());
                for value in evaluated {
                    evaluated_wrapped.push(EvaluatedValue::from_value(value));
                }
                let receiver_candidate = evaluated_wrapped
                    .first()
                    .and_then(|v| Self::value_struct_name(v.value()));
                let func_entry = self.resolve_function(
                    &name,
                    receiver_candidate.as_deref(),
                    &type_args,
                    &evaluated_wrapped
                        .iter()
                        .map(|v| v.value().clone())
                        .collect::<Vec<_>>(),
                )?;
                let results = self.run_function_with_values(func_entry, evaluated_wrapped)?;
                Ok(self.collapse_results(results).into_value())
            }
            BuildValue::Closure(closure) => self.build_closure_from_build(closure, channels),
            BuildValue::Moved => Err("moved value cannot be used in build spawn result".into()),
        }
    }

    fn build_closure_from_build(
        &mut self,
        closure: BuildClosure,
        channels: &mut ChannelHandles,
    ) -> Result<Value, String> {
        let mut key = self.closure_counter + self.closures.len() + 1;
        while self.closures.contains_key(&key) {
            key += 1;
        }

        let mut captured_vars = Vec::with_capacity(closure.captures.len());
        for captured in &closure.captures {
            let ty = captured.ty.clone().ok_or_else(|| {
                format!(
                    "Capture `{}` is missing a type; cannot reconstruct closure from build result",
                    captured.name
                )
            })?;
            captured_vars.push(CapturedVar {
                name: captured.name.clone(),
                mutable: captured.mutable,
                ty: Some(ty),
                mode: captured.mode.clone(),
                span: captured.origin,
            });
        }

        self.ensure_closure_info(
            key,
            &closure.params,
            &closure.body,
            &closure.ret,
            &captured_vars,
            None,
        )?;
        let info = self
            .closures
            .get(&key)
            .cloned()
            .ok_or_else(|| "closure metadata missing".to_string())?;

        let env_alloca = self.allocate_closure_env(info.env_type, key);
        for (idx, (captured, ty)) in closure
            .captures
            .iter()
            .zip(info.capture_types.iter())
            .enumerate()
        {
            let value = self.build_value_to_value(captured.value.clone(), channels)?;
            let llvm_value = self.value_to_llvm_for_type(value, ty)?;
            let field_ptr = unsafe {
                LLVMBuildStructGEP2(
                    self.builder,
                    info.env_type,
                    env_alloca,
                    idx as u32,
                    CString::new(format!("closure_env_store_{idx}"))
                        .unwrap()
                        .as_ptr(),
                )
            };
            unsafe {
                LLVMBuildStore(self.builder, llvm_value, field_ptr);
            }
        }

        Ok(Value::Closure(ClosureValue {
            id: key,
            env_ptr: env_alloca,
            fn_ptr: info.function,
        }))
    }

    fn build_string_constant(&mut self, text: String) -> Result<Value, String> {
        let c_value = CString::new(text.as_str())
            .map_err(|_| "String literal contains null byte".to_string())?;
        let name = CString::new("str_lit").unwrap();
        let rc = Arc::new(text);
        unsafe {
            let ptr = LLVMBuildGlobalString(self.builder, c_value.as_ptr(), name.as_ptr());
            Ok(Value::Str(StringValue::new(ptr, rc)))
        }
    }

    fn build_format_string_value(
        &mut self,
        literal: &FormatStringLiteral,
    ) -> Result<Value, String> {
        let mut segments = Vec::new();
        let mut implicit = 0usize;
        let mut has_placeholders = false;
        let mut literal_text = String::new();
        for segment in &literal.segments {
            match segment {
                FormatSegment::Literal(text) => {
                    literal_text.push_str(text);
                    segments.push(FormatRuntimeSegment::Literal(text.clone()));
                }
                FormatSegment::Expr { expr, .. } => {
                    has_placeholders = true;
                    let value = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(_) => {
                            return Err(
                                "control flow not allowed inside format placeholders".into()
                            );
                        }
                    };
                    segments.push(FormatRuntimeSegment::Named(value));
                }
                FormatSegment::Implicit(_) => {
                    has_placeholders = true;
                    implicit += 1;
                    segments.push(FormatRuntimeSegment::Implicit);
                }
            }
        }
        if !has_placeholders {
            return self.build_string_constant(literal_text);
        }
        Ok(Value::FormatTemplate(FormatTemplateValue {
            segments,
            implicit_placeholders: implicit,
        }))
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        let debug_mem = env::var_os("PRIME_DEBUG_MEM").is_some();
        if debug_mem {
            let module_count = program.modules.len();
            let item_count: usize = program.modules.iter().map(|m| m.items.len()).sum();
            eprintln!(
                "[prime-debug] compile_program: modules={}, total_items={}",
                module_count, item_count
            );
            Self::log_rss("[prime-debug] compile_program/start", true);
        }
        self.reset_module();
        self.force_runtime_handles = false;
        self.pending_runtime = None;
        self.scopes.clear();
        self.active_mut_borrows.clear();
        self.borrow_frames.clear();
        self.cleanup_stack.clear();
        self.return_type_stack.clear();
        self.drop_impls.clear();
        self.push_scope();
        self.push_return_types(&[]);
        self.structs.clear();
        self.functions.clear();
        self.enum_variants.clear();
        self.interfaces.clear();
        self.impls.clear();
        self.consts.clear();
        self.closure_snapshots.clear();
        // First collect types and interfaces so impls are validated without depending on module order.
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
                        for (idx, variant) in def.variants.iter().enumerate() {
                            self.enum_variants.insert(
                                variant.name.clone(),
                                EnumVariantInfo {
                                    enum_name: def.name.clone(),
                                    fields: variant.fields.len(),
                                    module: module.name.clone(),
                                    visibility: def.visibility,
                                    variant_index: idx as u32,
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
                    Item::Macro(_)
                    | Item::MacroInvocation(_)
                    | Item::Impl(_)
                    | Item::Function(_)
                    | Item::Const(_)
                    | Item::Comment { .. } => {}
                }
            }
        }
        if env::var_os("PRIME_DEBUG_TRACE").is_some() {
            eprintln!("[prime-debug] collected structs/enums/interfaces");
        }

        // With interfaces/types present, register impls, functions, and consts.
        for module in &program.modules {
            for item in &module.items {
                match item {
                    Item::Impl(block) => {
                        self.register_impl_block(&module.name, block)?;
                    }
                    Item::Function(func) => {
                        self.register_function(func, &module.name)?;
                    }
                    Item::Const(const_def) => {
                        self.consts.push((module.name.clone(), const_def.clone()));
                    }
                    Item::Struct(_)
                    | Item::Enum(_)
                    | Item::Interface(_)
                    | Item::Macro(_)
                    | Item::MacroInvocation(_)
                    | Item::Comment { .. } => {}
                }
            }
        }
        if env::var_os("PRIME_DEBUG_TRACE").is_some() {
            eprintln!("[prime-debug] registered impls/functions/consts");
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
        if env::var_os("PRIME_DEBUG_TRACE").is_some() {
            eprintln!("[prime-debug] entering main body {}", main_module);
        }
        let exec_result = self.execute_block_contents(body);
        if env::var_os("PRIME_DEBUG_TRACE").is_some() {
            eprintln!("[prime-debug] main body finished");
        }
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
        self.pop_return_types();
        self.release_closure_envs()?;

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

fn block_has_break_or_continue(block: &Block) -> bool {
    for stmt in &block.statements {
        match stmt {
            Statement::Break | Statement::Continue => return true,
            Statement::Block(inner) => {
                if block_has_break_or_continue(inner) {
                    return true;
                }
            }
            Statement::While(while_stmt) => {
                if block_has_break_or_continue(&while_stmt.body) {
                    return true;
                }
            }
            Statement::Loop(loop_stmt) => {
                if block_has_break_or_continue(&loop_stmt.body) {
                    return true;
                }
            }
            Statement::For(for_stmt) => {
                if block_has_break_or_continue(&for_stmt.body) {
                    return true;
                }
            }
            _ => {}
        }
    }
    if let Some(tail) = &block.tail {
        if let Expr::Block(tail_block) = &**tail {
            if block_has_break_or_continue(tail_block) {
                return true;
            }
        }
    }
    false
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
        .and_then(|param| param.ty.as_ref())
        .and_then(|ty| type_name_from_type_expr(&ty.ty))
        .filter(|name| structs.contains_key(name))
}

fn substitute_self_in_function(def: &mut FunctionDef, target: &str) {
    let concrete = TypeExpr::Named(target.to_string(), Vec::new());
    for param in &mut def.params {
        if let Some(ty) = param.ty.as_mut() {
            *ty = ty.replace_self(&concrete);
        }
    }
    for ret in &mut def.returns {
        *ret = ret.replace_self(&concrete);
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
        Value::Iterator(_) => "iterator",
        Value::FormatTemplate(_) => "format string",
        Value::Sender(_) => "channel sender",
        Value::Receiver(_) => "channel receiver",
        Value::Range(_) => "range",
        Value::Pointer(_) => "pointer",
        Value::JoinHandle(_) => "join handle",
        Value::Closure(_) => "closure",
        Value::Task(_) => "task",
        Value::Moved => "moved",
    }
}

#[cfg(test)]
mod tests;
