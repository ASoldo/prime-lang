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
        LLVMAddCase, LLVMAddFunction, LLVMAddIncoming, LLVMAppendBasicBlockInContext, LLVMArrayType2,
        LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildArrayMalloc, LLVMBuildBitCast, LLVMBuildBr,
        LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildExtractValue, LLVMBuildFAdd, LLVMBuildFCmp,
        LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFRem, LLVMBuildFSub, LLVMBuildGlobalString,
        LLVMBuildICmp, LLVMBuildInBoundsGEP2, LLVMBuildIntCast, LLVMBuildLoad2, LLVMBuildMul,
        LLVMBuildNot, LLVMBuildOr, LLVMBuildPhi, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv,
        LLVMBuildSExt, LLVMBuildSIToFP, LLVMBuildSRem, LLVMBuildStore, LLVMBuildStructGEP2,
        LLVMBuildSub, LLVMBuildSwitch, LLVMConstInt, LLVMConstIntGetZExtValue, LLVMConstNull,
        LLVMConstPointerNull, LLVMConstReal, LLVMContextCreate, LLVMContextDispose,
        LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeMessage, LLVMDisposeModule,
        LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMFunctionType, LLVMGetBasicBlockParent,
        LLVMGetElementType, LLVMGetFirstBasicBlock, LLVMGetFirstInstruction, LLVMGetGlobalParent,
        LLVMGetInsertBlock, LLVMGetIntTypeWidth, LLVMGetLastInstruction, LLVMGetModuleContext,
        LLVMGetParam, LLVMGetReturnType, LLVMGetTypeKind, LLVMInt8TypeInContext,
        LLVMInt32TypeInContext, LLVMIntTypeInContext, LLVMIsAConstantInt, LLVMIsAFunction,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPositionBuilder,
        LLVMPositionBuilderAtEnd, LLVMPositionBuilderBefore, LLVMPrintModuleToFile, LLVMSetLinkage,
        LLVMSetTarget, LLVMStructCreateNamed, LLVMStructSetBody, LLVMStructTypeInContext,
        LLVMTypeOf, LLVMVoidTypeInContext,
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
    sync::{Arc, Condvar, Mutex, RwLock},
    thread,
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
    cell: Arc<Mutex<EvaluatedValue>>,
    mutable: bool,
    origin: Option<String>,
    handle: Option<LLVMValueRef>,
}

#[derive(Clone)]
struct PointerValue {
    cell: Arc<Mutex<EvaluatedValue>>,
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
    cell: Arc<Mutex<Value>>,
    #[allow(dead_code)]
    origin: Option<String>,
}

#[derive(Clone)]
struct SliceValue {
    handle: Option<LLVMValueRef>,
    items: Arc<Mutex<Vec<Value>>>,
}

#[derive(Clone)]
struct MapValue {
    handle: Option<LLVMValueRef>,
    entries: Arc<Mutex<BTreeMap<String, Value>>>,
}

#[derive(Clone)]
struct IteratorValue {
    items: Arc<Mutex<Vec<Value>>>,
    index: Arc<Mutex<usize>>,
}

impl IteratorValue {
    fn from_items(items: Vec<Value>) -> Self {
        Self {
            items: Arc::new(Mutex::new(items)),
            index: Arc::new(Mutex::new(0)),
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
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
    handle: Option<LLVMValueRef>,
    origin: Option<String>,
}

#[derive(Clone)]
struct ChannelReceiver {
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
    handle: Option<LLVMValueRef>,
    origin: Option<String>,
}

enum JoinResult {
    Immediate(Option<Value>),
    BuildThread(Option<thread::JoinHandle<Result<BuildEvaluation, String>>>),
}

#[derive(Clone)]
struct JoinHandleValue {
    result: Arc<Mutex<JoinResult>>,
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
    state: Arc<(Mutex<Option<EvaluatedValue>>, Condvar)>,
    handle: Option<LLVMValueRef>,
    kind: TaskResultKind,
    pending: Option<Arc<Mutex<Option<DeferredTask>>>>,
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

impl ChannelSender {
    fn new(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn new_with_state(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        let state = Arc::new((
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
    fn new(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn new_with_state(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self {
            inner,
            handle: None,
            origin: None,
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        let state = Arc::new((
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
            result: Arc::new(Mutex::new(JoinResult::Immediate(Some(value)))),
            handle: None,
        }
    }

    fn new_build(handle: thread::JoinHandle<Result<BuildEvaluation, String>>) -> Self {
        Self {
            result: Arc::new(Mutex::new(JoinResult::BuildThread(Some(handle)))),
            handle: None,
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            result: Arc::new(Mutex::new(JoinResult::Immediate(None))),
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
            state: Arc::new((Mutex::new(Some(value)), Condvar::new())),
            handle: None,
            kind: TaskResultKind::Ready,
            pending: None,
        }
    }

    fn deferred(block: Block, capture_names: Vec<String>) -> Self {
        Self {
            state: Arc::new((Mutex::new(None), Condvar::new())),
            handle: None,
            kind: TaskResultKind::Deferred,
            pending: Some(Arc::new(Mutex::new(Some(DeferredTask {
                block,
                capture_names,
            })))),
        }
    }

    fn with_handle(handle: LLVMValueRef, kind: TaskResultKind) -> Self {
        Self {
            state: Arc::new((Mutex::new(None), Condvar::new())),
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
    cell: Arc<Mutex<EvaluatedValue>>,
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
            cell: Arc::new(Mutex::new(value)),
            origin: None,
        }
    }

    fn replace(&self, value: Value) -> Value {
        std::mem::replace(&mut *self.cell.lock().unwrap(), value)
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            handle: Some(handle),
            cell: Arc::new(Mutex::new(Value::Unit)),
            origin: None,
        }
    }
}

impl SliceValue {
    fn new() -> Self {
        Self {
            handle: None,
            items: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn from_vec(items: Vec<Value>) -> Self {
        Self {
            handle: None,
            items: Arc::new(Mutex::new(items)),
        }
    }

    fn with_handle(handle: LLVMValueRef) -> Self {
        Self {
            handle: Some(handle),
            items: Arc::new(Mutex::new(Vec::new())),
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
            entries: Arc::new(Mutex::new(BTreeMap::new())),
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
            entries: Arc::new(Mutex::new(BTreeMap::new())),
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
    channels: HashMap<*const (), (u64, Arc<(Mutex<BuildChannelState>, Condvar)>)>,
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
        inner: &Arc<(Mutex<ChannelState>, Condvar)>,
    ) -> Result<(u64, Arc<(Mutex<BuildChannelState>, Condvar)>), String> {
        let key = Arc::as_ptr(inner) as *const ();
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
        let shared = Arc::new((Mutex::new(state), Condvar::new()));
        let id = self.next_channel_id;
        self.next_channel_id += 1;
        self.channels.insert(key, (id, shared.clone()));
        Ok((id, shared))
    }

    fn reference_from_cell(
        &mut self,
        cell: &Arc<Mutex<EvaluatedValue>>,
        mutable: bool,
    ) -> Result<BuildReference, String> {
        let key = Arc::as_ptr(cell) as *const ();
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
            let key = Arc::as_ptr(&handle.result) as *const ();
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
                                let eval = result.map_err(|e| e)?;
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
            .map(|handle| {
                value.set_runtime(handle);
                handle
            })
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

    fn apply_build_effects(
        &mut self,
        effects: Vec<BuildEffect>,
    ) -> Result<HashMap<u64, Arc<(Mutex<ChannelState>, Condvar)>>, String> {
        let mut channel_handles: HashMap<u64, Arc<(Mutex<ChannelState>, Condvar)>> = HashMap::new();
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
                        Arc::new((
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
        channels: &mut HashMap<u64, Arc<(Mutex<ChannelState>, Condvar)>>,
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
                    items: Arc::new(Mutex::new(converted)),
                    index: Arc::new(Mutex::new(*iter.index.lock().unwrap())),
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
                let cell = Arc::new(Mutex::new(EvaluatedValue::from_value(inner)));
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
                    let shared = Arc::new((
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
                    let shared = Arc::new((
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
                    .and_then(|v| self.value_struct_name(v.value()));
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
        channels: &mut HashMap<u64, Arc<(Mutex<ChannelState>, Condvar)>>,
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
                    | Item::Const(_) => {}
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
                    | Item::MacroInvocation(_) => {}
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

    fn emit_out_value(&mut self, value: EvaluatedValue) -> Result<(), String> {
        self.print_value(value)?;
        if self.target.is_embedded() {
            // Terminate embedded prints with a newline.
            self.emit_embedded_newline();
        } else {
            self.emit_printf_call("\n", &mut []);
        }
        Ok(())
    }

    fn emit_format_template(
        &mut self,
        template: FormatTemplateValue,
        mut values: Vec<EvaluatedValue>,
    ) -> Result<(), String> {
        let mut args_iter = values.drain(..);
        for segment in template.segments {
            match segment {
                FormatRuntimeSegment::Literal(text) => {
                    self.emit_printf_call(&text, &mut []);
                }
                FormatRuntimeSegment::Named(value) => {
                    let mut wrapped = value;
                    wrapped.runtime = None;
                    self.maybe_attach_runtime_handle(&mut wrapped);
                    self.print_value(wrapped)?;
                }
                FormatRuntimeSegment::Implicit => {
                    if let Some(mut value) = args_iter.next() {
                        value.runtime = None;
                        self.maybe_attach_runtime_handle(&mut value);
                        self.print_value(value)?;
                    }
                }
            }
        }
        if self.target.is_embedded() {
            self.emit_embedded_newline();
        } else {
            self.emit_printf_call("\n", &mut []);
        }
        Ok(())
    }

    fn emit_embedded_newline(&mut self) {
        if !self.target.is_embedded() {
            return;
        }
        if let Ok((ptr, len)) = self.build_runtime_bytes("\n", "rt_print_nl") {
            let mut call_args = [ptr, len];
            let handle = self.call_runtime(
                self.runtime_abi.prime_string_new,
                self.runtime_abi.prime_string_new_ty,
                &mut call_args,
                "string_new_nl",
            );
            let mut print_args = [handle];
            self.call_runtime(
                self.runtime_abi.prime_print,
                self.runtime_abi.prime_print_ty,
                &mut print_args,
                "prime_print_nl",
            );
        }
    }

    fn emit_embedded_labeled_print(&mut self, label: &str, handle: LLVMValueRef) {
        if handle.is_null() || !self.target.is_embedded() {
            return;
        }
        if let Ok((ptr, len)) = self.build_runtime_bytes(label, "rt_label") {
            let mut call_args = [ptr, len];
            let label_handle = self.call_runtime(
                self.runtime_abi.prime_string_new,
                self.runtime_abi.prime_string_new_ty,
                &mut call_args,
                "string_new_label",
            );
            let mut print_args = [label_handle];
            self.call_runtime(
                self.runtime_abi.prime_print,
                self.runtime_abi.prime_print_ty,
                &mut print_args,
                "prime_print_label",
            );
        }
        let mut print_args = [handle];
        self.call_runtime(
            self.runtime_abi.prime_print,
            self.runtime_abi.prime_print_ty,
            &mut print_args,
            "prime_print_value",
        );
    }

    fn print_value(&mut self, value: EvaluatedValue) -> Result<(), String> {
        let mut value = value;
        let runtime_handle = if self.target.is_embedded() {
            value
                .runtime_handle()
                .or_else(|| self.maybe_attach_runtime_handle(&mut value))
        } else {
            value.runtime_handle()
        };
        let owned = value.clone().into_value();
        if self.target.is_embedded() {
            // Embedded runtime: render scalars into strings via runtime helpers.
            match &owned {
                Value::Reference(reference) => {
                    // Print the current pointed-to value.
                    let inner = reference.cell.lock().unwrap().clone();
                    return self.print_value(inner);
                }
                Value::Str(string) => {
                    let (ptr, len) = self.build_runtime_bytes(&string.text, "rt_print")?;
                    let mut call_args = [ptr, len];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_string_new,
                        self.runtime_abi.prime_string_new_ty,
                        &mut call_args,
                        "string_new",
                    );
                    let mut print_args = [handle];
                    self.call_runtime(
                        self.runtime_abi.prime_print,
                        self.runtime_abi.prime_print_ty,
                        &mut print_args,
                        "prime_print",
                    );
                    return Ok(());
                }
                Value::Int(int_value) => {
                    // Always use the current SSA value so mutable ints print correctly.
                    let arg = self.int_to_runtime_int(int_value, "print_int_cast")?;
                    let mut args = [arg];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_int_to_string,
                        self.runtime_abi.prime_int_to_string_ty,
                        &mut args,
                        "int_to_string",
                    );
                    // Optional diagnostic for tracking stale handles in embedded format prints.
                    if env::var_os("PRIME_DEBUG_EMBED_INT").is_some() {
                        self.emit_embedded_labeled_print("ssa=", handle);
                        if let Some(rt_handle) = runtime_handle
                            .or_else(|| self.maybe_attach_runtime_handle(&mut value))
                        {
                            let mut val_args = [rt_handle];
                            let rt_int = self.call_runtime(
                                self.runtime_abi.prime_value_as_int,
                                self.runtime_abi.prime_value_as_int_ty,
                                &mut val_args,
                                "debug_value_as_int",
                            );
                            let mut to_string_args = [rt_int];
                            let rt_string = self.call_runtime(
                                self.runtime_abi.prime_int_to_string,
                                self.runtime_abi.prime_int_to_string_ty,
                                &mut to_string_args,
                                "debug_int_to_string",
                            );
                            self.emit_embedded_labeled_print("rt=", rt_string);
                        }
                    }
                    let mut print_args = [handle];
                    self.call_runtime(
                        self.runtime_abi.prime_print,
                        self.runtime_abi.prime_print_ty,
                        &mut print_args,
                        "prime_print",
                    );
                    return Ok(());
                }
                Value::Bool(flag) => {
                    if let Some(constant) = flag.constant() {
                        let text = if constant { "true" } else { "false" };
                        let (ptr, len) = self.build_runtime_bytes(text, "rt_print_bool")?;
                        let mut call_args = [ptr, len];
                        let handle = self.call_runtime(
                            self.runtime_abi.prime_string_new,
                            self.runtime_abi.prime_string_new_ty,
                            &mut call_args,
                            "string_new",
                        );
                        let mut print_args = [handle];
                        self.call_runtime(
                            self.runtime_abi.prime_print,
                            self.runtime_abi.prime_print_ty,
                            &mut print_args,
                            "prime_print",
                        );
                        return Ok(());
                    }
                }
                _ => {}
            }
        }
        if let Some(handle) = runtime_handle
            .or_else(|| self.maybe_attach_runtime_handle(&mut value))
        {
            self.emit_runtime_print(handle);
            return Ok(());
        }
        match owned {
            Value::Int(int_value) => {
                let handle = self.build_runtime_handle(Value::Int(int_value.clone()))?;
                self.emit_runtime_print(handle);
                self.runtime_release(handle);
                Ok(())
            }
            Value::Float(float_value) => {
                self.emit_printf_call("%f", &mut [float_value.llvm()]);
                Ok(())
            }
            Value::Bool(flag) => {
                if let Some(constant) = flag.constant() {
                    if constant {
                        self.emit_printf_call("true", &mut []);
                    } else {
                        self.emit_printf_call("false", &mut []);
                    }
                    Ok(())
                } else if let Some(handle) = value.runtime_handle() {
                    self.emit_runtime_print(handle);
                    Ok(())
                } else if let Some(handle) = self.maybe_attach_runtime_handle(&mut value) {
                    self.emit_runtime_print(handle);
                    Ok(())
                } else {
                    self.emit_printf_call("<bool>", &mut []);
                    Ok(())
                }
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
                    self.print_value(item.into())?;
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
                        self.print_value(item.into())?;
                    }
                    self.emit_printf_call(")", &mut []);
                    Ok(())
                }
            }
            Value::Range(range) => {
                self.emit_printf_call("%d", &mut [range.start.llvm()]);
                self.emit_printf_call(if range.inclusive { "..=" } else { ".." }, &mut []);
                self.emit_printf_call("%d", &mut [range.end.llvm()]);
                Ok(())
            }
            Value::Pointer(pointer) => {
                if let Some(handle) = pointer.handle {
                    self.emit_runtime_print(handle);
                    Ok(())
                } else {
                    if pointer.mutable {
                        self.emit_printf_call("mut Pointer->", &mut []);
                    } else {
                        self.emit_printf_call("Pointer->", &mut []);
                    }
                    let inner = pointer.cell.lock().unwrap().clone();
                    self.print_value(inner)
                }
            }
            Value::Struct(_) => Err("Cannot print struct value in build mode".into()),
            Value::Unit => {
                self.emit_printf_call("()", &mut []);
                Ok(())
            }
            Value::Reference(reference) => {
                if let Some(handle) = reference.handle {
                    let mut args = [handle];
                    self.call_runtime(
                        self.runtime_abi.prime_print,
                        self.runtime_abi.prime_print_ty,
                        &mut args,
                        "print_ref_handle",
                    );
                    Ok(())
                } else {
                    self.emit_printf_call("&", &mut []);
                    let inner = reference.cell.lock().unwrap().clone();
                    self.print_value(inner)
                }
            }
            Value::Boxed(boxed) => {
                if let Some(handle) = boxed.handle {
                    self.emit_runtime_print(handle);
                    Ok(())
                } else {
                    let inner = boxed.cell.lock().unwrap().clone();
                    self.emit_printf_call("Box(", &mut []);
                    self.print_value(inner.into())?;
                    self.emit_printf_call(")", &mut []);
                    Ok(())
                }
            }
            Value::Slice(slice) => {
                if let Some(handle) = slice.handle {
                    self.emit_runtime_print(handle);
                    return Ok(());
                }
                self.emit_printf_call("[", &mut []);
                let values = slice.items.lock().unwrap().clone();
                for (idx, item) in values.into_iter().enumerate() {
                    if idx > 0 {
                        self.emit_printf_call(", ", &mut []);
                    }
                    self.print_value(item.into())?;
                }
                self.emit_printf_call("]", &mut []);
                Ok(())
            }
            Value::Map(map) => {
                if let Some(handle) = map.handle {
                    self.emit_runtime_print(handle);
                    return Ok(());
                }
                self.emit_printf_call("#{", &mut []);
                let guard = map.entries.lock().unwrap();
                let mut entries: Vec<_> = guard.iter().collect();
                entries.sort_by(|a, b| a.0.cmp(b.0));
                for (idx, (key, value)) in entries.into_iter().enumerate() {
                    if idx > 0 {
                        self.emit_printf_call(", ", &mut []);
                    }
                    let (ptr, len) = self.build_runtime_bytes(key, "map_print_key")?;
                    let mut args = [ptr, len];
                    self.emit_printf_call("%.*s: ", &mut args);
                    self.print_value(value.clone().into())?;
                }
                self.emit_printf_call("}", &mut []);
                Ok(())
            }
            Value::Iterator(_) => {
                self.emit_printf_call("<iter>", &mut []);
                Ok(())
            }
            Value::FormatTemplate(_) => Err("Format string must be printed via out()".into()),
            Value::Sender(_) | Value::Receiver(_) | Value::JoinHandle(_) => {
                let handle = self.build_runtime_handle(value.value().clone())?;
                self.emit_runtime_print(handle);
                self.runtime_release(handle);
                Ok(())
            }
            Value::Closure(_) => {
                let (ptr, len) = self.build_runtime_bytes("<closure>", "print_closure")?;
                let mut args = [ptr, len];
                self.emit_printf_call("%.*s", &mut args);
                Ok(())
            }
            Value::Task(_) => Err("Cannot print task value in build mode".into()),
            Value::Moved => Err("Cannot print moved value in build mode".into()),
        }
    }

    fn emit_runtime_print(&mut self, handle: LLVMValueRef) {
        let mut args = [handle];
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildCall2(
                self.builder,
                self.runtime_abi.prime_print_ty,
                self.runtime_abi.prime_print,
                args.as_mut_ptr(),
                args.len() as u32,
                name.as_ptr(),
            );
        }
    }

    fn build_runtime_handle(&mut self, value: Value) -> Result<LLVMValueRef, String> {
        match value {
            Value::Unit => Ok(self.call_runtime(
                self.runtime_abi.prime_unit_new,
                self.runtime_abi.prime_unit_new_ty,
                &mut [],
                "unit",
            )),
            Value::Int(int_value) => {
                let arg = if let Some(constant) = int_value.constant() {
                    unsafe { LLVMConstInt(self.runtime_abi.int_type, constant as u64, 1) }
                } else {
                    let current = unsafe { LLVMTypeOf(int_value.llvm()) };
                    if current == self.runtime_abi.int_type {
                        int_value.llvm()
                    } else {
                        unsafe {
                            LLVMBuildSExt(
                                self.builder,
                                int_value.llvm(),
                                self.runtime_abi.int_type,
                                CString::new("int_widen").unwrap().as_ptr(),
                            )
                        }
                    }
                };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_int_new,
                    self.runtime_abi.prime_int_new_ty,
                    &mut [arg],
                    "int_new",
                ))
            }
            Value::Float(float_value) => {
                let arg = if let Some(constant) = float_value.constant() {
                    unsafe { LLVMConstReal(self.runtime_abi.float_type, constant) }
                } else {
                    let current = unsafe { LLVMTypeOf(float_value.llvm()) };
                    if current == self.runtime_abi.float_type {
                        float_value.llvm()
                    } else {
                        unsafe {
                            LLVMBuildBitCast(
                                self.builder,
                                float_value.llvm(),
                                self.runtime_abi.float_type,
                                CString::new("float_cast").unwrap().as_ptr(),
                            )
                        }
                    }
                };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_float_new,
                    self.runtime_abi.prime_float_new_ty,
                    &mut [arg],
                    "float_new",
                ))
            }
            Value::Bool(flag) => {
                let arg = if let Some(constant) = flag.constant() {
                    unsafe { LLVMConstInt(self.runtime_abi.bool_type, constant as u64, 0) }
                } else {
                    self.bool_llvm_value(&flag)
                };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_bool_new,
                    self.runtime_abi.prime_bool_new_ty,
                    &mut [arg],
                    "bool_new",
                ))
            }
            Value::Str(text) => {
                let (ptr, len) = self.build_runtime_bytes(&text.text, "rt_str")?;
                Ok(self.call_runtime(
                    self.runtime_abi.prime_string_new,
                    self.runtime_abi.prime_string_new_ty,
                    &mut [ptr, len],
                    "string_new",
                ))
            }
            Value::Boxed(boxed) => {
                if let Some(handle) = boxed.handle {
                    return Ok(handle);
                }
                let inner = boxed.cell.lock().unwrap().clone();
                let inner_handle = self.build_runtime_handle(inner)?;
                let mut_flag = unsafe { LLVMConstInt(self.runtime_abi.bool_type, 1, 0) };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_reference_new,
                    self.runtime_abi.prime_reference_new_ty,
                    &mut [inner_handle, mut_flag],
                    "box_new",
                ))
            }
            Value::Tuple(values) => {
                let slice = self.call_runtime(
                    self.runtime_abi.prime_slice_new,
                    self.runtime_abi.prime_slice_new_ty,
                    &mut [],
                    "slice",
                );
                for v in values {
                    let handle = self.build_runtime_handle(v)?;
                    let _ = self.call_runtime(
                        self.runtime_abi.prime_slice_push,
                        self.runtime_abi.prime_slice_push_ty,
                        &mut [slice, handle],
                        "slice_push",
                    );
                }
                Ok(slice)
            }
            Value::Enum(enum_value) => {
                let mut elements = Vec::new();
                for v in enum_value.values {
                    elements.push(self.build_runtime_handle(v)?);
                }
                let arr_ptr = self.alloc_handle_array(&elements);
                let len_const =
                    unsafe { LLVMConstInt(self.runtime_abi.usize_type, elements.len() as u64, 0) };
                let tag = unsafe {
                    LLVMConstInt(
                        self.runtime_abi.int_type,
                        enum_value.variant_index as u64,
                        0,
                    )
                };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_enum_new,
                    self.runtime_abi.prime_enum_new_ty,
                    &mut [arr_ptr, len_const, tag],
                    "enum_new",
                ))
            }
            Value::Range(range) => {
                let start = range
                    .start
                    .constant()
                    .ok_or_else(|| "range start not constant".to_string())?;
                let end = range
                    .end
                    .constant()
                    .ok_or_else(|| "range end not constant".to_string())?;
                let rendered = if range.inclusive {
                    format!("{start}..={end}")
                } else {
                    format!("{start}..{end}")
                };
                let (ptr, len) = self.build_runtime_bytes(&rendered, "rt_range_str")?;
                Ok(self.call_runtime(
                    self.runtime_abi.prime_string_new,
                    self.runtime_abi.prime_string_new_ty,
                    &mut [ptr, len],
                    "range_str",
                ))
            }
            Value::Struct(struct_value) => {
                let (name_ptr, name_len) =
                    self.build_runtime_bytes(&struct_value.name, "rt_struct_name")?;
                let handle = self.call_runtime(
                    self.runtime_abi.prime_struct_new,
                    self.runtime_abi.prime_struct_new_ty,
                    &mut [name_ptr, name_len],
                    "struct_new",
                );
                let mut fields: Vec<_> = struct_value.fields.iter().collect();
                fields.sort_by(|a, b| a.0.cmp(b.0));
                for (field, val) in fields {
                    let (key_ptr, key_len) = self.build_runtime_bytes(field, "rt_struct_field")?;
                    let field_handle = self.build_runtime_handle(val.clone())?;
                    let _ = self.call_runtime(
                        self.runtime_abi.prime_struct_insert,
                        self.runtime_abi.prime_struct_insert_ty,
                        &mut [handle, key_ptr, key_len, field_handle],
                        "struct_insert",
                    );
                }
                Ok(handle)
            }
            Value::Sender(sender) => {
                let (queued, closed) = {
                    let (lock, _) = &*sender.inner;
                    let guard = lock.lock().unwrap();
                    (
                        guard.queue.iter().cloned().collect::<Vec<_>>(),
                        guard.closed,
                    )
                };
                let (send_handle, recv_handle) = self
                    .build_channel_handles()
                    .ok_or_else(|| "runtime channel handles unavailable".to_string())?;
                self.seed_channel_queue(send_handle, &queued)?;
                if closed {
                    self.runtime_close(send_handle);
                }
                self.runtime_release(recv_handle);
                Ok(send_handle)
            }
            Value::Receiver(receiver) => {
                let (queued, closed) = {
                    let (lock, _) = &*receiver.inner;
                    let guard = lock.lock().unwrap();
                    (
                        guard.queue.iter().cloned().collect::<Vec<_>>(),
                        guard.closed,
                    )
                };
                let (send_handle, recv_handle) = self
                    .build_channel_handles()
                    .ok_or_else(|| "runtime channel handles unavailable".to_string())?;
                self.seed_channel_queue(send_handle, &queued)?;
                if closed {
                    self.runtime_close(send_handle);
                }
                self.runtime_release(send_handle);
                Ok(recv_handle)
            }
            Value::JoinHandle(handle) => {
                if let Some(h) = handle.handle {
                    return Ok(h);
                }
                Err("join handle missing runtime backing".into())
            }
            Value::Pointer(ptr) => {
                let inner = ptr.cell.lock().unwrap().clone().into_value();
                let target = self.build_runtime_handle(inner)?;
                let mut_flag =
                    unsafe { LLVMConstInt(self.runtime_abi.bool_type, ptr.mutable as u64, 0) };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_reference_new,
                    self.runtime_abi.prime_reference_new_ty,
                    &mut [target, mut_flag],
                    "ptr_new",
                ))
            }
            Value::Slice(slice) => {
                if let Some(handle) = slice.handle {
                    return Ok(handle);
                }
                let handle = self.call_runtime(
                    self.runtime_abi.prime_slice_new,
                    self.runtime_abi.prime_slice_new_ty,
                    &mut [],
                    "slice",
                );
                for idx in 0..slice.len() {
                    if let Some(item) = slice.get(idx) {
                        let h = self.build_runtime_handle(item)?;
                        let _ = self.call_runtime(
                            self.runtime_abi.prime_slice_push,
                            self.runtime_abi.prime_slice_push_ty,
                            &mut [handle, h],
                            "slice_push",
                        );
                    }
                }
                Ok(handle)
            }
            Value::Map(map) => {
                if let Some(handle) = map.handle {
                    return Ok(handle);
                }
                let handle = self.call_runtime(
                    self.runtime_abi.prime_map_new,
                    self.runtime_abi.prime_map_new_ty,
                    &mut [],
                    "map_new",
                );
                for (k, v) in map.entries.lock().unwrap().iter() {
                    let (key_ptr, len) = self.build_runtime_bytes(k, "rt_map_key")?;
                    let val_handle = self.build_runtime_handle(v.clone())?;
                    let _ = self.call_runtime(
                        self.runtime_abi.prime_map_insert,
                        self.runtime_abi.prime_map_insert_ty,
                        &mut [handle, key_ptr, len, val_handle],
                        "map_insert",
                    );
                }
                Ok(handle)
            }
            Value::Reference(reference) => {
                if let Some(handle) = reference.handle {
                    let mut args = [handle];
                    let inner = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut args,
                        "ref_read_handle",
                    );
                    return Ok(inner);
                }
                let inner = reference.cell.lock().unwrap().clone().into_value();
                let target = self.build_runtime_handle(inner)?;
                let mut_flag = unsafe {
                    LLVMConstInt(self.runtime_abi.bool_type, reference.mutable as u64, 0)
                };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_reference_new,
                    self.runtime_abi.prime_reference_new_ty,
                    &mut [target, mut_flag],
                    "ref_new",
                ))
            }
            _ => Err("runtime handle generation not implemented for this value".into()),
        }
    }

    fn build_runtime_handle_scoped(&mut self, value: Value) -> Option<LLVMValueRef> {
        // Only lower safe, pure values for now.
        let allowed = matches!(
            value,
            Value::Unit
                | Value::Int(_)
                | Value::Float(_)
                | Value::Bool(_)
                | Value::Str(_)
                | Value::Tuple(_)
                | Value::Enum(_)
                | Value::Range(_)
                | Value::Boxed(_)
                | Value::Slice(_)
                | Value::Map(_)
                | Value::Reference(_)
                | Value::Sender(_)
                | Value::Receiver(_)
                | Value::Struct(_)
                | Value::Pointer(_)
        );
        if !allowed {
            return None;
        }
        unsafe {
            let current_block = LLVMGetInsertBlock(self.builder);
            if current_block.is_null() {
                return None;
            }
            let last_instr = LLVMGetLastInstruction(current_block);
            let result = self.build_runtime_handle(value).ok();
            // Ensure builder is still at a valid position; if not, reset to block end.
            let now_block = LLVMGetInsertBlock(self.builder);
            if now_block.is_null() || now_block != current_block {
                let end = LLVMGetLastInstruction(current_block);
                if end.is_null() {
                    LLVMPositionBuilderAtEnd(self.builder, current_block);
                } else {
                    // Position after the last instruction.
                    LLVMPositionBuilder(self.builder, current_block, end);
                }
            } else if last_instr == LLVMGetLastInstruction(current_block) {
                // No new instructions; keep position at end.
                LLVMPositionBuilderAtEnd(self.builder, current_block);
            }
            result
        }
    }

    fn call_runtime(
        &mut self,
        func: LLVMValueRef,
        func_type: LLVMTypeRef,
        args: &mut [LLVMValueRef],
        name: &str,
    ) -> LLVMValueRef {
        let is_void = unsafe {
            let ret = LLVMGetReturnType(func_type);
            LLVMGetTypeKind(ret) == LLVMTypeKind::LLVMVoidTypeKind
        };
        let name_c = if is_void {
            CString::new("").unwrap()
        } else {
            CString::new(name).unwrap()
        };
        let debug_handles = env::var("PRIME_DEBUG_RT_HANDLES").is_ok();
        assert!(!func.is_null(), "runtime function `{}` is null", name);
        assert!(!self.builder.is_null(), "LLVM builder is not initialized");
        unsafe {
            let block = LLVMGetInsertBlock(self.builder);
            assert!(
                !block.is_null(),
                "no insertion block for runtime call `{}`",
                name
            );
            if debug_handles {
                let parent_fn = LLVMGetBasicBlockParent(block);
                let func_mod = LLVMGetGlobalParent(func);
                let module_ctx = LLVMGetModuleContext(self.module);
                eprintln!(
                    "[rt_call] name={name} func_ptr={:?} func_mod={:?} current_mod={:?} block={:?} block_fn={:?} ctx={:?}",
                    func, func_mod, self.module, block, parent_fn, module_ctx
                );
            }
        }
        unsafe {
            let mut fn_type = func_type;
            if fn_type.is_null()
                && LLVMGetTypeKind(LLVMTypeOf(func)) == LLVMTypeKind::LLVMPointerTypeKind
            {
                fn_type = LLVMGetElementType(LLVMTypeOf(func));
            }
            assert!(
                !fn_type.is_null()
                    && LLVMGetTypeKind(fn_type) == LLVMTypeKind::LLVMFunctionTypeKind,
                "runtime function `{}` has invalid type for call",
                name
            );
            LLVMBuildCall2(
                self.builder,
                fn_type,
                func,
                args.as_mut_ptr(),
                args.len() as u32,
                name_c.as_ptr(),
            )
        }
    }

    fn build_runtime_bytes(
        &mut self,
        text: &str,
        symbol: &str,
    ) -> Result<(LLVMValueRef, LLVMValueRef), String> {
        let c_text = CString::new(text.as_bytes())
            .map_err(|_| "string contains interior null byte".to_string())?;
        let sym = CString::new(symbol).unwrap();
        let ptr = unsafe { LLVMBuildGlobalString(self.builder, c_text.as_ptr(), sym.as_ptr()) };
        let len = unsafe { LLVMConstInt(self.runtime_abi.usize_type, text.len() as u64, 0) };
        Ok((ptr, len))
    }

    fn null_handle_ptr(&self) -> LLVMValueRef {
        unsafe { LLVMConstNull(LLVMPointerType(self.runtime_abi.handle_type, 0)) }
    }

    fn render_runtime_value_bytes(&mut self, value: &Value) -> Result<Vec<u8>, String> {
        let rendered = match value {
            Value::Int(int_value) => int_value
                .constant()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "<int>".to_string()),
            Value::Float(float_value) => float_value
                .constant()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "<float>".to_string()),
            Value::Bool(flag) => flag
                .constant()
                .map(|b| b.to_string())
                .unwrap_or_else(|| "<bool>".to_string()),
            Value::Str(text) => text.text.to_string(),
            Value::Unit => "()".to_string(),
            Value::Tuple(values) => {
                let parts: Vec<String> = values
                    .iter()
                    .map(|v| {
                        self.render_runtime_value_bytes(v).and_then(|b| {
                            String::from_utf8(b).map_err(|_| "invalid utf8".to_string())
                        })
                    })
                    .collect::<Result<_, _>>()?;
                format!("({})", parts.join(", "))
            }
            Value::Enum(e) => {
                let parts: Vec<String> = e
                    .values
                    .iter()
                    .map(|v| {
                        self.render_runtime_value_bytes(v).and_then(|b| {
                            String::from_utf8(b).map_err(|_| "invalid utf8".to_string())
                        })
                    })
                    .collect::<Result<_, _>>()?;
                if parts.is_empty() {
                    format!("{}::{}", e.enum_name, e.variant)
                } else {
                    format!("{}::{}({})", e.enum_name, e.variant, parts.join(", "))
                }
            }
            _ => return Err("unsupported value in prompt formatting".into()),
        };
        Ok(rendered.into_bytes())
    }

    fn render_format_template_bytes(
        &mut self,
        template: FormatTemplateValue,
    ) -> Result<Vec<u8>, String> {
        let mut output = String::new();
        for segment in template.segments {
            match segment {
                FormatRuntimeSegment::Literal(text) => output.push_str(&text),
                FormatRuntimeSegment::Named(value) => {
                    let rendered = self.render_runtime_value_bytes(value.value())?;
                    output.push_str(&String::from_utf8(rendered).map_err(|_| "invalid utf8")?);
                }
                FormatRuntimeSegment::Implicit => output.push_str("{}"),
            }
        }
        Ok(output.into_bytes())
    }

    fn type_code_for(&self, ty: &TypeExpr) -> Result<u32, String> {
        match ty {
            TypeExpr::Named(name, _) => match name.as_str() {
                "string" => Ok(TYPE_STRING),
                "bool" => Ok(TYPE_BOOL),
                "int8" => Ok(TYPE_INT8),
                "int16" => Ok(TYPE_INT16),
                "int32" => Ok(TYPE_INT32),
                "int64" => Ok(TYPE_INT64),
                "isize" => Ok(TYPE_ISIZE),
                "uint8" => Ok(TYPE_UINT8),
                "uint16" => Ok(TYPE_UINT16),
                "uint32" => Ok(TYPE_UINT32),
                "uint64" => Ok(TYPE_UINT64),
                "usize" => Ok(TYPE_USIZE),
                "float32" => Ok(TYPE_FLOAT32),
                "float64" => Ok(TYPE_FLOAT64),
                "rune" => Ok(TYPE_RUNE),
                _ => Err(format!("`in` unsupported for type {}", name)),
            },
            _ => Err("`in` requires a concrete primitive type".into()),
        }
    }

    fn opaque_ptr_type(&self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(LLVMInt8TypeInContext(self.context), 0) }
    }

    fn llvm_type_for(&self, ty: &TypeExpr) -> Option<LLVMTypeRef> {
        match ty {
            TypeExpr::Named(name, _) => match name.as_str() {
                "int8" => Some(unsafe { LLVMIntTypeInContext(self.context, 8) }),
                "int16" => Some(unsafe { LLVMIntTypeInContext(self.context, 16) }),
                "int32" => Some(self.i32_type),
                "int64" => Some(unsafe { LLVMIntTypeInContext(self.context, 64) }),
                "isize" => Some(self.runtime_abi.usize_type),
                "uint8" => Some(unsafe { LLVMIntTypeInContext(self.context, 8) }),
                "uint16" => Some(unsafe { LLVMIntTypeInContext(self.context, 16) }),
                "uint32" => Some(self.i32_type),
                "uint64" => Some(unsafe { LLVMIntTypeInContext(self.context, 64) }),
                "usize" => Some(self.runtime_abi.usize_type),
                "bool" => Some(self.runtime_abi.bool_type),
                "float32" => Some(unsafe { LLVMFloatTypeInContext(self.context) }),
                "float64" => Some(self.f64_type),
                "rune" => Some(self.i32_type),
                "string" => Some(self.runtime_abi.string_data_type),
                "Map" | "Box" | "Sender" | "Receiver" | "JoinHandle" => {
                    Some(self.runtime_abi.handle_type)
                }
                _ => None,
            },
            TypeExpr::Reference { .. } | TypeExpr::Pointer { .. } => {
                Some(self.runtime_abi.handle_type)
            }
            TypeExpr::Slice(_) => Some(self.runtime_abi.handle_type),
            TypeExpr::Function { .. } => Some(self.closure_value_type),
            TypeExpr::Tuple(items) => self.tuple_llvm_type(items).ok(),
            TypeExpr::Unit => Some(unsafe { LLVMVoidTypeInContext(self.context) }),
            _ => None,
        }
    }

    fn expect_llvm_type(&self, ty: &TypeExpr, ctx: &str) -> Result<LLVMTypeRef, String> {
        self.llvm_type_for(ty)
            .ok_or_else(|| format!("Unsupported type `{}` for {ctx}", ty.canonical_name()))
    }

    fn tuple_llvm_type(&self, elems: &[TypeExpr]) -> Result<LLVMTypeRef, String> {
        let mut fields = Vec::with_capacity(elems.len());
        for ty in elems {
            fields.push(self.expect_llvm_type(ty, "tuple element")?);
        }
        Ok(unsafe {
            LLVMStructTypeInContext(self.context, fields.as_mut_ptr(), fields.len() as u32, 0)
        })
    }

    fn infer_closure_body_type(
        &self,
        body: &ClosureBody,
        bindings: &HashMap<String, TypeExpr>,
    ) -> Option<TypeExpr> {
        match body {
            ClosureBody::Block(block) => block
                .tail
                .as_ref()
                .and_then(|expr| self.infer_expr_type(expr, bindings)),
            ClosureBody::Expr(expr) => self.infer_expr_type(expr.node.as_ref(), bindings),
        }
    }

    fn infer_expr_type(
        &self,
        expr: &Expr,
        bindings: &HashMap<String, TypeExpr>,
    ) -> Option<TypeExpr> {
        match expr {
            Expr::Identifier(ident) => bindings.get(&ident.name).cloned(),
            Expr::Literal(lit) => match lit {
                Literal::Int(_, _) => Some(TypeExpr::named("int32")),
                Literal::Float(_, _) => Some(TypeExpr::named("float64")),
                Literal::Bool(_, _) => Some(TypeExpr::named("bool")),
                Literal::String(_, _) => Some(TypeExpr::named("string")),
                Literal::Rune(_, _) => Some(TypeExpr::named("rune")),
            },
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = self.infer_expr_type(left, bindings)?;
                let rhs = self.infer_expr_type(right, bindings)?;
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Rem => {
                        if lhs == rhs {
                            Some(lhs)
                        } else {
                            None
                        }
                    }
                    BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq
                    | BinaryOp::Eq
                    | BinaryOp::NotEq => Some(TypeExpr::named("bool")),
                    _ => None,
                }
            }
            Expr::Block(block) => block
                .tail
                .as_ref()
                .and_then(|expr| self.infer_expr_type(expr, bindings)),
            Expr::Call { .. } => None,
            Expr::Tuple(values, _) => {
                let mut types = Vec::with_capacity(values.len());
                for expr in values {
                    if let Some(ty) = self.infer_expr_type(expr, bindings) {
                        types.push(ty);
                    } else {
                        return None;
                    }
                }
                Some(TypeExpr::Tuple(types))
            }
            Expr::ArrayLiteral(_, _) => None,
            Expr::Closure {
                params,
                body,
                ret,
                captures,
                ..
            } => {
                let mut param_types = Vec::with_capacity(params.len());
                let mut local_bindings = bindings.clone();
                for param in params {
                    let Some(ty) = param.ty.as_ref().map(|ann| ann.ty.clone()) else {
                        return None;
                    };
                    local_bindings.insert(param.name.clone(), ty.clone());
                    param_types.push(ty);
                }
                if let Ok(guard) = captures.read() {
                    for captured in guard.iter() {
                        if let Some(ty) = &captured.ty {
                            local_bindings.insert(captured.name.clone(), ty.clone());
                        }
                    }
                }
                let returns = if let Some(annotation) = ret {
                    vec![annotation.ty.clone()]
                } else if let Some(inferred) = self.infer_closure_body_type(body, &local_bindings) {
                    vec![inferred]
                } else {
                    Vec::new()
                };
                Some(TypeExpr::Function {
                    params: param_types,
                    returns,
                })
            }
            Expr::If(if_expr) => match &if_expr.then_branch.tail {
                Some(expr) => self.infer_expr_type(expr, bindings),
                None => Some(TypeExpr::Unit),
            },
            Expr::Match(match_expr) => match_expr
                .arms
                .first()
                .and_then(|arm| self.infer_expr_type(&arm.value, bindings)),
            _ => None,
        }
    }

    fn build_closure_signature(
        &self,
        params: &[FunctionParam],
        body: &ClosureBody,
        ret: &Option<TypeAnnotation>,
        captures: &[CapturedVar],
        expected: Option<&TypeExpr>,
    ) -> Result<FnSignature, String> {
        if let Some(TypeExpr::Function { params, returns }) = expected {
            return Ok(FnSignature {
                params: params.clone(),
                returns: returns.clone(),
            });
        }

        let mut param_types = Vec::new();
        let mut bindings = HashMap::new();
        for param in params {
            let ty = param
                .ty
                .as_ref()
                .ok_or_else(|| {
                    format!(
                        "parameter `{}` in closure requires a type annotation in build mode",
                        param.name
                    )
                })?
                .ty
                .clone();
            bindings.insert(param.name.clone(), ty.clone());
            param_types.push(ty);
        }
        for captured in captures {
            if let Some(ty) = &captured.ty {
                bindings.insert(captured.name.clone(), ty.clone());
            }
        }

        let returns = if let Some(annotation) = ret {
            vec![annotation.ty.clone()]
        } else if let Some(inferred) = self.infer_closure_body_type(body, &bindings) {
            vec![inferred]
        } else {
            Vec::new()
        };

        Ok(FnSignature {
            params: param_types,
            returns,
        })
    }

    fn take_binding_value(&mut self, name: &str) -> Result<EvaluatedValue, String> {
        let (cell, _) = self
            .get_binding(name)
            .ok_or_else(|| format!("Unknown variable {}", name))?;
        if self.is_mut_borrowed(name) {
            return Err(format!(
                "Cannot move `{}` while it is mutably borrowed",
                name
            ));
        }
        let mut slot = cell.lock().unwrap();
        if matches!(slot.value(), Value::Moved) {
            return Err(format!("Value `{}` has been moved", name));
        }
        let moved = std::mem::replace(&mut *slot, EvaluatedValue::from_value(Value::Moved));
        self.register_move(name);
        Ok(moved)
    }

    fn build_closure_repr(
        &mut self,
        id: usize,
        env_ptr: LLVMValueRef,
        fn_ptr: LLVMValueRef,
    ) -> LLVMValueRef {
        unsafe {
            let slot = LLVMBuildAlloca(
                self.builder,
                self.closure_value_type,
                CString::new("closure_value").unwrap().as_ptr(),
            );
            let env_cast = LLVMBuildBitCast(
                self.builder,
                env_ptr,
                self.opaque_ptr_type(),
                CString::new("closure_env_cast").unwrap().as_ptr(),
            );
            let env_gep = LLVMBuildStructGEP2(
                self.builder,
                self.closure_value_type,
                slot,
                0,
                CString::new("closure_env_gep").unwrap().as_ptr(),
            );
            LLVMBuildStore(self.builder, env_cast, env_gep);

            let fn_cast = LLVMBuildBitCast(
                self.builder,
                fn_ptr,
                self.opaque_ptr_type(),
                CString::new("closure_fn_cast").unwrap().as_ptr(),
            );
            let fn_gep = LLVMBuildStructGEP2(
                self.builder,
                self.closure_value_type,
                slot,
                1,
                CString::new("closure_fn_gep").unwrap().as_ptr(),
            );
            LLVMBuildStore(self.builder, fn_cast, fn_gep);

            let id_const = LLVMConstInt(self.runtime_abi.usize_type, id as u64, 0);
            let id_gep = LLVMBuildStructGEP2(
                self.builder,
                self.closure_value_type,
                slot,
                2,
                CString::new("closure_id_gep").unwrap().as_ptr(),
            );
            LLVMBuildStore(self.builder, id_const, id_gep);

            LLVMBuildLoad2(
                self.builder,
                self.closure_value_type,
                slot,
                CString::new("closure_value_load").unwrap().as_ptr(),
            )
        }
    }

    fn value_to_llvm_for_type(
        &mut self,
        value: Value,
        ty: &TypeExpr,
    ) -> Result<LLVMValueRef, String> {
        match ty {
            TypeExpr::Named(name, _) => match name.as_str() {
                "int8" | "int16" | "int32" | "int64" | "isize" | "uint8" | "uint16" | "uint32"
                | "uint64" | "usize" | "rune" => match value {
                    Value::Int(int_value) => {
                        let target = self.expect_llvm_type(ty, "integer conversion")?;
                        let current = unsafe { LLVMTypeOf(int_value.llvm()) };
                        if target == current {
                            Ok(int_value.llvm())
                        } else {
                            Ok(unsafe {
                                LLVMBuildSExt(
                                    self.builder,
                                    int_value.llvm(),
                                    target,
                                    CString::new("int_cast").unwrap().as_ptr(),
                                )
                            })
                        }
                    }
                    other => Err(format!(
                        "Expected integer value for `{}` capture, found {}",
                        name,
                        describe_value(&other)
                    )),
                },
                "float32" | "float64" => match value {
                    Value::Float(float_value) => Ok(float_value.llvm()),
                    other => Err(format!(
                        "Expected float value for `{}` capture, found {}",
                        name,
                        describe_value(&other)
                    )),
                },
                "bool" => match value {
                    Value::Bool(flag) => Ok(self.bool_llvm_value(&flag)),
                    other => Err(format!(
                        "Expected bool value for capture, found {}",
                        describe_value(&other)
                    )),
                },
                "string" => match value {
                    Value::Str(s) => Ok(s.llvm),
                    other => Err(format!(
                        "Expected string value for capture, found {}",
                        describe_value(&other)
                    )),
                },
                "Map" | "Box" => {
                    let handle = self.build_runtime_handle(value)?;
                    Ok(handle)
                }
                "Sender" | "Receiver" | "JoinHandle" => {
                    let handle = self.build_runtime_handle(value)?;
                    Ok(handle)
                }
                _ => Err(format!("Unsupported capture type `{}`", name)),
            },
            TypeExpr::Reference { .. } => match value {
                Value::Reference(reference) => self.reference_handle(&reference),
                other => Err(format!(
                    "Expected reference value for capture, found {}",
                    describe_value(&other)
                )),
            },
            TypeExpr::Slice(_) => {
                let handle = self.build_runtime_handle(value)?;
                Ok(handle)
            }
            TypeExpr::Pointer { .. } => {
                let handle = self.build_runtime_handle(value)?;
                Ok(handle)
            }
            TypeExpr::Function { .. } => match value {
                Value::Closure(closure) => {
                    Ok(self.build_closure_repr(closure.id, closure.env_ptr, closure.fn_ptr))
                }
                other => Err(format!(
                    "Expected closure value for function type, found {}",
                    describe_value(&other)
                )),
            },
            TypeExpr::Tuple(types) => match value {
                Value::Tuple(values) => {
                    if values.len() != types.len() {
                        return Err("Tuple value length does not match tuple type".into());
                    }
                    let tuple_ty = self.tuple_llvm_type(types)?;
                    let slot = unsafe {
                        LLVMBuildAlloca(
                            self.builder,
                            tuple_ty,
                            CString::new("tuple_tmp").unwrap().as_ptr(),
                        )
                    };
                    for (idx, (val, ty)) in values.into_iter().zip(types.iter()).enumerate() {
                        let elem = self.value_to_llvm_for_type(val, ty)?;
                        let field_ptr = unsafe {
                            LLVMBuildStructGEP2(
                                self.builder,
                                tuple_ty,
                                slot,
                                idx as u32,
                                CString::new(format!("tuple_field_{idx}")).unwrap().as_ptr(),
                            )
                        };
                        unsafe {
                            LLVMBuildStore(self.builder, elem, field_ptr);
                        }
                    }
                    Ok(unsafe {
                        LLVMBuildLoad2(
                            self.builder,
                            tuple_ty,
                            slot,
                            CString::new("tuple_val").unwrap().as_ptr(),
                        )
                    })
                }
                other => Err(format!(
                    "Expected tuple value for tuple type, found {}",
                    describe_value(&other)
                )),
            },
            TypeExpr::Unit => Ok(std::ptr::null_mut()),
            _ => Err(format!(
                "Unsupported type `{}` for closure conversion",
                ty.canonical_name()
            )),
        }
    }

    fn value_from_llvm(&mut self, llvm: LLVMValueRef, ty: &TypeExpr) -> Result<Value, String> {
        match ty {
            TypeExpr::Reference { mutable, .. } => Ok(Value::Reference(ReferenceValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: *mutable,
                origin: None,
                handle: Some(llvm),
            })),
            TypeExpr::Named(name, _) => match name.as_str() {
                "int8" | "int16" | "int32" | "int64" | "isize" | "uint8" | "uint16" | "uint32"
                | "uint64" | "usize" | "rune" => Ok(Value::Int(IntValue::new(llvm, None))),
                "float32" | "float64" => Ok(Value::Float(FloatValue::new(llvm, None))),
                "bool" => {
                    let constant = unsafe {
                        if LLVMIsAConstantInt(llvm).is_null() {
                            None
                        } else {
                            Some(LLVMConstIntGetZExtValue(llvm) != 0)
                        }
                    };
                    Ok(Value::Bool(BoolValue::new(llvm, constant)))
                }
                "string" => Ok(Value::Str(StringValue::new(llvm, Arc::new(String::new())))),
                "Map" => Ok(Value::Map(MapValue::with_handle(llvm))),
                "Box" => Ok(Value::Boxed(BoxValue::with_handle(llvm))),
                "Sender" => Ok(Value::Sender(ChannelSender::with_handle(llvm))),
                "Receiver" => Ok(Value::Receiver(ChannelReceiver::with_handle(llvm))),
                "JoinHandle" => Ok(Value::JoinHandle(Box::new(JoinHandleValue::with_handle(
                    llvm,
                )))),
                other => Err(format!("Unsupported captured type `{other}` in closure")),
            },
            TypeExpr::Function { .. } => {
                let env_ptr = unsafe {
                    LLVMBuildExtractValue(
                        self.builder,
                        llvm,
                        0,
                        CString::new("closure_env_extract").unwrap().as_ptr(),
                    )
                };
                let fn_ptr = unsafe {
                    LLVMBuildExtractValue(
                        self.builder,
                        llvm,
                        1,
                        CString::new("closure_fn_extract").unwrap().as_ptr(),
                    )
                };
                let id_val = unsafe {
                    LLVMBuildExtractValue(
                        self.builder,
                        llvm,
                        2,
                        CString::new("closure_id_extract").unwrap().as_ptr(),
                    )
                };
                let id_const = unsafe { LLVMConstIntGetZExtValue(id_val) } as usize;
                if let Some(existing) = self.closures.get(&id_const) {
                    let expected_sig = match ty {
                        TypeExpr::Function { params, returns } => FnSignature {
                            params: params.clone(),
                            returns: returns.clone(),
                        },
                        _ => FnSignature {
                            params: Vec::new(),
                            returns: Vec::new(),
                        },
                    };
                    if existing.signature.params != expected_sig.params
                        || existing.signature.returns != expected_sig.returns
                    {
                        return Err(format!(
                            "Closure metadata mismatch for id {}: expected ({:?} -> {:?}), have ({:?} -> {:?})",
                            id_const,
                            expected_sig.params,
                            expected_sig.returns,
                            existing.signature.params,
                            existing.signature.returns
                        ));
                    }
                } else if let TypeExpr::Function { params, returns } = ty {
                    let signature = FnSignature {
                        params: params.clone(),
                        returns: returns.clone(),
                    };
                    let env_name = CString::new(format!("__closure_env{id_const}")).unwrap();
                    let env_type =
                        unsafe { LLVMStructCreateNamed(self.context, env_name.as_ptr()) };
                    let mut arg_types = Vec::with_capacity(signature.params.len() + 1);
                    arg_types.push(unsafe { LLVMPointerType(env_type, 0) });
                    for ty in &signature.params {
                        arg_types.push(self.expect_llvm_type(ty, "closure param")?);
                    }
                    let ret_type = match signature.return_count() {
                        0 => unsafe { LLVMVoidTypeInContext(self.context) },
                        1 => self.expect_llvm_type(&signature.returns[0], "closure return")?,
                        _ => self.tuple_llvm_type(&signature.returns)?,
                    };
                    let fn_type = unsafe {
                        LLVMFunctionType(
                            ret_type,
                            arg_types.as_mut_ptr(),
                            arg_types.len() as u32,
                            0,
                        )
                    };
                    self.closures.insert(
                        id_const,
                        ClosureInfo {
                            env_type,
                            fn_type,
                            function: ptr::null_mut(),
                            signature,
                            capture_types: Vec::new(),
                            built: true,
                        },
                    );
                } else {
                    return Err("Closure value missing function type during reconstruction".into());
                }
                Ok(Value::Closure(ClosureValue {
                    id: id_const,
                    env_ptr,
                    fn_ptr,
                }))
            }
            TypeExpr::Tuple(types) => {
                let mut elements = Vec::with_capacity(types.len());
                for (idx, elem_ty) in types.iter().enumerate() {
                    let extracted = unsafe {
                        LLVMBuildExtractValue(
                            self.builder,
                            llvm,
                            idx as u32,
                            CString::new(format!("tuple_extract_{idx}"))
                                .unwrap()
                                .as_ptr(),
                        )
                    };
                    elements.push(self.value_from_llvm(extracted, elem_ty)?);
                }
                Ok(Value::Tuple(elements))
            }
            TypeExpr::Unit => Ok(Value::Unit),
            TypeExpr::Slice(_) => Ok(Value::Slice(SliceValue::with_handle(llvm))),
            TypeExpr::Pointer { mutable, .. } => Ok(Value::Pointer(PointerValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: *mutable,
                handle: Some(llvm),
                origin: None,
            })),
            _ => Err(format!(
                "Unsupported type `{}` in closure body",
                ty.canonical_name()
            )),
        }
    }

    fn ensure_closure_info(
        &mut self,
        key: usize,
        params: &[FunctionParam],
        body: &ClosureBody,
        ret: &Option<TypeAnnotation>,
        captures: &[CapturedVar],
        expected: Option<&TypeExpr>,
    ) -> Result<(), String> {
        let signature = self.build_closure_signature(params, body, ret, captures, expected)?;
        let mut capture_types = Vec::new();
        for captured in captures {
            let ty = captured.ty.clone().ok_or_else(|| {
                format!(
                    "Capture `{}` is missing a type; ensure typechecking ran successfully",
                    captured.name
                )
            })?;
            capture_types.push(ty);
        }
        if let Some(existing) = self.closures.get(&key) {
            if !existing.capture_types.is_empty() || capture_types.is_empty() {
                return Ok(());
            }
        }
        let mut field_types = Vec::new();
        for ty in &capture_types {
            field_types.push(self.expect_llvm_type(ty, "closure environment")?);
        }
        let env_name = CString::new(format!("__closure_env{}", self.closure_counter)).unwrap();
        let env_type = unsafe { LLVMStructCreateNamed(self.context, env_name.as_ptr()) };
        unsafe {
            LLVMStructSetBody(
                env_type,
                field_types.as_mut_ptr(),
                field_types.len() as u32,
                0,
            );
        }
        self.closure_counter += 1;

        let mut arg_types = Vec::with_capacity(signature.params.len() + 1);
        arg_types.push(unsafe { LLVMPointerType(env_type, 0) });
        for ty in &signature.params {
            arg_types.push(self.expect_llvm_type(ty, "closure parameter")?);
        }
        let ret_type = match signature.return_count() {
            0 => unsafe { LLVMVoidTypeInContext(self.context) },
            1 => self.expect_llvm_type(&signature.returns[0], "closure return")?,
            _ => self.tuple_llvm_type(&signature.returns)?,
        };
        let fn_type = unsafe {
            LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as u32, 0)
        };
        let fn_name = CString::new(format!("__closure_fn{}", key)).unwrap();
        let function = unsafe { LLVMAddFunction(self.module, fn_name.as_ptr(), fn_type) };
        unsafe {
            LLVMSetLinkage(function, LLVMLinkage::LLVMPrivateLinkage);
        }

        if env::var("PRIME_DEBUG_CLOSURE_CAPTURES").is_ok() {
            let params_fmt: Vec<String> = signature
                .params
                .iter()
                .map(|t| t.canonical_name())
                .collect();
            let returns_fmt: Vec<String> = signature
                .returns
                .iter()
                .map(|t| t.canonical_name())
                .collect();
            let captures_fmt: Vec<String> =
                capture_types.iter().map(|t| t.canonical_name()).collect();
            eprintln!(
                "closure {} signature params={:?} returns={:?} captures={:?}",
                key, params_fmt, returns_fmt, captures_fmt
            );
        }

        self.closures.insert(
            key,
            ClosureInfo {
                env_type,
                fn_type,
                function,
                signature,
                capture_types,
                built: false,
            },
        );
        self.emit_closure_body(key, params, body, captures)?;
        Ok(())
    }

    fn emit_closure_body(
        &mut self,
        key: usize,
        params: &[FunctionParam],
        body: &ClosureBody,
        captures: &[CapturedVar],
    ) -> Result<(), String> {
        let info = self
            .closures
            .get(&key)
            .cloned()
            .ok_or_else(|| "closure metadata missing".to_string())?;
        if info.built {
            return Ok(());
        }
        if let Some(entry) = self.closures.get_mut(&key) {
            entry.built = true;
        }

        let saved_block = unsafe { LLVMGetInsertBlock(self.builder) };
        let entry_name = CString::new(format!("closure_entry_{key}")).unwrap();
        let entry_block = unsafe {
            LLVMAppendBasicBlockInContext(self.context, info.function, entry_name.as_ptr())
        };
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, entry_block);
        }

        self.push_scope();
        self.push_return_types(&info.signature.returns);

        let env_param = unsafe { LLVMGetParam(info.function, 0) };
        for (idx, (captured, ty)) in captures.iter().zip(info.capture_types.iter()).enumerate() {
            let field_ptr = unsafe {
                LLVMBuildStructGEP2(
                    self.builder,
                    info.env_type,
                    env_param,
                    idx as u32,
                    CString::new(format!("env_field_{idx}")).unwrap().as_ptr(),
                )
            };
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.expect_llvm_type(ty, "closure capture")?,
                    field_ptr,
                    CString::new(format!("env_load_{idx}")).unwrap().as_ptr(),
                )
            };
            let value = self.value_from_llvm(loaded, ty)?;
            let evaluated_value = self.evaluated(value);
            self.insert_var(&captured.name, evaluated_value, captured.mutable)?;
        }

        for (idx, param) in params.iter().enumerate() {
            let llvm_param = unsafe { LLVMGetParam(info.function, (idx + 1) as u32) };
            let ty = param
                .ty
                .as_ref()
                .ok_or_else(|| format!("parameter `{}` missing type", param.name))?
                .ty
                .clone();
            let value = self.value_from_llvm(llvm_param, &ty)?;
            let evaluated_value = self.evaluated(value);
            self.insert_var(
                &param.name,
                evaluated_value,
                param.mutability == Mutability::Mutable,
            )?;
        }

        let result = match body {
            ClosureBody::Block(block) => self.execute_block_contents(block)?,
            ClosureBody::Expr(expr) => match self
                .emit_expression_with_hint(expr.node.as_ref(), info.signature.returns.get(0))?
            {
                EvalOutcome::Value(value) => BlockEval::Value(value),
                EvalOutcome::Flow(flow) => BlockEval::Flow(flow),
            },
        };

        self.exit_scope()?;
        self.pop_return_types();

        match result {
            BlockEval::Value(value) => {
                self.emit_closure_return(&info.signature, value)?;
            }
            BlockEval::Flow(FlowSignal::Return(values)) => {
                let value = if values.is_empty() {
                    self.evaluated(Value::Unit)
                } else if values.len() == 1 {
                    values.into_iter().next().unwrap()
                } else {
                    let tuple_items = values
                        .into_iter()
                        .map(|v| v.into_value())
                        .collect::<Vec<_>>();
                    self.evaluated(Value::Tuple(tuple_items))
                };
                self.emit_closure_return(&info.signature, value)?;
            }
            BlockEval::Flow(flow @ FlowSignal::Break)
            | BlockEval::Flow(flow @ FlowSignal::Continue)
            | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                return Err(format!(
                    "Control flow {} cannot escape closure body in build mode",
                    flow_name(&flow)
                ));
            }
        }

        if saved_block.is_null() {
            unsafe {
                LLVMPositionBuilderAtEnd(self.builder, entry_block);
            }
        } else {
            unsafe {
                LLVMPositionBuilderAtEnd(self.builder, saved_block);
            }
        }
        Ok(())
    }

    fn emit_closure_return(
        &mut self,
        signature: &FnSignature,
        value: EvaluatedValue,
    ) -> Result<(), String> {
        match signature.return_count() {
            0 => {
                unsafe {
                    LLVMBuildRetVoid(self.builder);
                }
                Ok(())
            }
            1 => {
                let llvm =
                    self.value_to_llvm_for_type(value.into_value(), &signature.returns[0])?;
                unsafe {
                    LLVMBuildRet(self.builder, llvm);
                }
                Ok(())
            }
            _ => {
                let tuple = match value.into_value() {
                    Value::Tuple(items) => items,
                    other => {
                        return Err(format!(
                            "Closure must return tuple with {} values, found {}",
                            signature.return_count(),
                            describe_value(&other)
                        ));
                    }
                };
                if tuple.len() != signature.return_count() {
                    return Err("Returned tuple length does not match closure return type".into());
                }
                let tuple_ty = self.tuple_llvm_type(&signature.returns)?;
                let slot = unsafe {
                    LLVMBuildAlloca(
                        self.builder,
                        tuple_ty,
                        CString::new("ret_tuple").unwrap().as_ptr(),
                    )
                };
                for (idx, (item, ty)) in tuple.into_iter().zip(signature.returns.iter()).enumerate()
                {
                    let llvm = self.value_to_llvm_for_type(item, ty)?;
                    let field_ptr = unsafe {
                        LLVMBuildStructGEP2(
                            self.builder,
                            tuple_ty,
                            slot,
                            idx as u32,
                            CString::new(format!("ret_field_{idx}")).unwrap().as_ptr(),
                        )
                    };
                    unsafe {
                        LLVMBuildStore(self.builder, llvm, field_ptr);
                    }
                }
                let packed = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        tuple_ty,
                        slot,
                        CString::new("ret_tuple_val").unwrap().as_ptr(),
                    )
                };
                unsafe {
                    LLVMBuildRet(self.builder, packed);
                }
                Ok(())
            }
        }
    }

    fn value_type_hint(&self, value: &Value) -> Option<TypeExpr> {
        match value {
            Value::Int(_) => Some(TypeExpr::named("int32")),
            Value::Float(_) => Some(TypeExpr::named("float64")),
            Value::Bool(_) => Some(TypeExpr::named("bool")),
            Value::Str(_) => Some(TypeExpr::named("string")),
            Value::Tuple(values) => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.value_type_hint(v)?);
                }
                Some(TypeExpr::Tuple(types))
            }
            Value::Slice(values) => values
                .items
                .lock()
                .ok()
                .and_then(|items| items.get(0).cloned())
                .and_then(|first| self.value_type_hint(&first))
                .map(|inner| TypeExpr::Slice(Box::new(inner)))
                .or_else(|| {
                    if values.handle.is_some() {
                        Some(TypeExpr::Slice(Box::new(TypeExpr::Unit)))
                    } else {
                        None
                    }
                }),
            Value::Map(map) => {
                let guard = map.entries.lock().ok();
                if let Some(values) = guard.as_ref().and_then(|g| g.values().next()) {
                    let inner = self.value_type_hint(values)?;
                    Some(TypeExpr::Named(
                        "Map".into(),
                        vec![TypeExpr::named("string"), inner],
                    ))
                } else if map.handle.is_some() {
                    Some(TypeExpr::Named(
                        "Map".into(),
                        vec![TypeExpr::named("string"), TypeExpr::Unit],
                    ))
                } else {
                    None
                }
            }
            Value::Boxed(boxed) => boxed
                .cell
                .lock()
                .ok()
                .and_then(|inner| self.value_type_hint(&inner))
                .map(|inner| TypeExpr::Named("Box".into(), vec![inner]))
                .or_else(|| {
                    if boxed.handle.is_some() {
                        Some(TypeExpr::Named("Box".into(), vec![TypeExpr::Unit]))
                    } else {
                        None
                    }
                }),
            Value::Closure(closure) => {
                self.closures
                    .get(&closure.id)
                    .map(|info| TypeExpr::Function {
                        params: info.signature.params.clone(),
                        returns: info.signature.returns.clone(),
                    })
            }
            Value::Reference(reference) => reference
                .cell
                .lock()
                .ok()
                .and_then(|inner| self.value_type_hint(&inner.clone().into_value()))
                .map(|inner_ty| TypeExpr::Reference {
                    mutable: reference.mutable,
                    ty: Box::new(inner_ty),
                }),
            _ => None,
        }
    }

    fn allocate_closure_env(&mut self, env_type: LLVMTypeRef, key: usize) -> LLVMValueRef {
        // Allocate a slot in the entry block to hold the env pointer; fill it when building the closure.
        let current_block = unsafe { LLVMGetInsertBlock(self.builder) };
        let func = unsafe { LLVMGetBasicBlockParent(current_block) };
        let track_env = func == self.main_fn;
        let slot = if track_env {
            unsafe {
                let entry = LLVMGetFirstBasicBlock(func);
                let slot_builder = LLVMCreateBuilderInContext(self.context);
                let ptr_ty = LLVMPointerType(env_type, 0);
                let first_inst = LLVMGetFirstInstruction(entry);
                if first_inst.is_null() {
                    LLVMPositionBuilderAtEnd(slot_builder, entry);
                } else {
                    LLVMPositionBuilderBefore(slot_builder, first_inst);
                }
                let alloca = LLVMBuildAlloca(
                    slot_builder,
                    ptr_ty,
                    CString::new(format!("closure_env_slot_{key}"))
                        .unwrap()
                        .as_ptr(),
                );
                let null = LLVMConstNull(ptr_ty);
                LLVMBuildStore(slot_builder, null, alloca);
                LLVMDisposeBuilder(slot_builder);
                alloca
            }
        } else {
            std::ptr::null_mut()
        };

        let ptr = unsafe {
            let one = LLVMConstInt(self.runtime_abi.usize_type, 1, 0);
            LLVMBuildArrayMalloc(
                self.builder,
                env_type,
                one,
                CString::new(format!("closure_env_{key}")).unwrap().as_ptr(),
            )
        };
        if track_env {
            unsafe {
                LLVMBuildStore(self.builder, ptr, slot);
            }
            self.closure_envs.push((key, slot));
        }
        ptr
    }

    fn capture_type_needs_release(&self, ty: &TypeExpr) -> bool {
        matches!(ty, TypeExpr::Slice(_))
            || matches!(ty, TypeExpr::Named(name, _) if name == "Map" || name == "Box")
            || matches!(ty, TypeExpr::Reference { .. })
            || matches!(ty, TypeExpr::Named(name, _) if name == "Sender" || name == "Receiver" || name == "JoinHandle")
            || matches!(ty, TypeExpr::Pointer { .. })
    }

    fn release_closure_envs(&mut self) -> Result<(), String> {
        let envs = std::mem::take(&mut self.closure_envs);
        for (key, env_ptr) in envs {
            if env_ptr.is_null() {
                continue;
            }
            let Some(info) = self.closures.get(&key).cloned() else {
                continue;
            };
            let expected_env = unsafe { LLVMPointerType(info.env_type, 0) };
            let loaded_env = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    expected_env,
                    env_ptr,
                    CString::new("closure_env_slot_load").unwrap().as_ptr(),
                )
            };
            let null_env = unsafe { LLVMConstNull(expected_env) };
            let is_null = unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    loaded_env,
                    null_env,
                    CString::new("closure_env_is_null").unwrap().as_ptr(),
                )
            };
            let current_block = unsafe { LLVMGetInsertBlock(self.builder) };
            let skip_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    LLVMGetBasicBlockParent(current_block),
                    CString::new(format!("env_drop_skip_{key}"))
                        .unwrap()
                        .as_ptr(),
                )
            };
            let drop_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    LLVMGetBasicBlockParent(current_block),
                    CString::new(format!("env_drop_apply_{key}"))
                        .unwrap()
                        .as_ptr(),
                )
            };
            let cont_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    LLVMGetBasicBlockParent(current_block),
                    CString::new(format!("env_drop_cont_{key}"))
                        .unwrap()
                        .as_ptr(),
                )
            };
            unsafe {
                LLVMBuildCondBr(self.builder, is_null, skip_block, drop_block);
                LLVMPositionBuilderAtEnd(self.builder, drop_block);
            }
            let typed_env = loaded_env;
            for (idx, ty) in info.capture_types.iter().enumerate() {
                if !self.capture_type_needs_release(ty) {
                    continue;
                }
                let field_ptr = unsafe {
                    LLVMBuildStructGEP2(
                        self.builder,
                        info.env_type,
                        typed_env,
                        idx as u32,
                        CString::new(format!("env_drop_field_{idx}"))
                            .unwrap()
                            .as_ptr(),
                    )
                };
                let llvm_ty = self.expect_llvm_type(ty, "closure env drop")?;
                let loaded = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        llvm_ty,
                        field_ptr,
                        CString::new(format!("env_drop_load_{idx}"))
                            .unwrap()
                            .as_ptr(),
                    )
                };
                let mut args = [loaded];
                self.call_runtime(
                    self.runtime_abi.prime_value_release,
                    self.runtime_abi.prime_value_release_ty,
                    &mut args,
                    "closure_env_release",
                );
            }
            let env_i8 = unsafe {
                LLVMBuildBitCast(
                    self.builder,
                    typed_env,
                    self.runtime_abi.handle_type,
                    CString::new("closure_env_free_cast").unwrap().as_ptr(),
                )
            };
            let mut free_args = [env_i8];
            self.call_runtime(
                self.runtime_abi.prime_env_free,
                self.runtime_abi.prime_env_free_ty,
                &mut free_args,
                "closure_env_free",
            );
            unsafe {
                LLVMBuildBr(self.builder, cont_block);
                LLVMPositionBuilderAtEnd(self.builder, skip_block);
                LLVMBuildBr(self.builder, cont_block);
                LLVMPositionBuilderAtEnd(self.builder, cont_block);
            }
        }
        Ok(())
    }

    fn emit_closure_literal(
        &mut self,
        params: &[FunctionParam],
        body: &ClosureBody,
        ret: &Option<TypeAnnotation>,
        captures: &Arc<RwLock<Vec<CapturedVar>>>,
        expected: Option<&TypeExpr>,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let key = Arc::as_ptr(captures) as usize;
        let mut captures_vec = captures.read().unwrap().clone();
        let free = self.collect_free_closure_vars(body, params);
        let debug_captures = env::var("PRIME_DEBUG_CLOSURE_CAPTURES").is_ok();
        if debug_captures {
            eprintln!("closure {} preset captures: {}", key, captures_vec.len());
        }
        for name in free {
            if captures_vec.iter().any(|c| c.name == name) {
                continue;
            }
            if let Some((cell, _)) = self.get_binding(&name) {
                let value = cell.lock().unwrap().clone().into_value();
                let ty = self.value_type_hint(&value).ok_or_else(|| {
                    format!(
                        "Capture `{}` is missing a type after typechecking; cannot synthesize opaque capture",
                        name
                    )
                })?;
                captures_vec.push(CapturedVar {
                    name: name.clone(),
                    mutable: false,
                    ty: Some(ty),
                    mode: CaptureMode::Move,
                    span: Span::new(0, 0),
                });
                if debug_captures {
                    eprintln!("capturing free var `{}` for closure", name);
                }
            }
        }
        if debug_captures && captures_vec.is_empty() {
            eprintln!("closure {} still has no captures after free var scan", key);
        }
        self.ensure_closure_info(key, params, body, ret, &captures_vec, expected)?;
        let info = self
            .closures
            .get(&key)
            .cloned()
            .ok_or_else(|| "closure metadata missing".to_string())?;

        let env_alloca = self.allocate_closure_env(info.env_type, key);
        let mut captured_values_raw = Vec::with_capacity(captures_vec.len());
        for (idx, (captured, ty)) in captures_vec
            .iter()
            .zip(info.capture_types.iter())
            .enumerate()
        {
            let captured_value = match captured.mode {
                CaptureMode::Move => self.take_binding_value(&captured.name)?,
                CaptureMode::Reference { .. } => self
                    .get_var(&captured.name)
                    .ok_or_else(|| format!("Unknown capture {}", captured.name))?,
            };
            let captured_clone = captured_value.clone();
            captured_values_raw.push(captured_clone.into_value());
            let llvm_value = self.value_to_llvm_for_type(captured_value.into_value(), ty)?;
            let field_ptr = unsafe {
                LLVMBuildStructGEP2(
                    self.builder,
                    info.env_type,
                    env_alloca,
                    idx as u32,
                    CString::new(format!("env_field_store_{idx}"))
                        .unwrap()
                        .as_ptr(),
                )
            };
            unsafe {
                LLVMBuildStore(self.builder, llvm_value, field_ptr);
            }
        }
        let closure_value = Value::Closure(ClosureValue {
            id: key,
            env_ptr: env_alloca,
            fn_ptr: info.function,
        });
        self.closure_snapshots.insert(
            key,
            ClosureSnapshot {
                params: params.to_vec(),
                body: body.clone(),
                ret: ret.clone(),
                captures: captures_vec.clone(),
                values: captured_values_raw,
            },
        );
        Ok(EvalOutcome::Value(self.evaluated(closure_value)))
    }

    fn emit_expression_with_hint(
        &mut self,
        expr: &Expr,
        expected: Option<&TypeExpr>,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        match expr {
            Expr::Closure {
                params,
                body,
                ret,
                captures,
                ..
            } => self.emit_closure_literal(params, body, ret, captures, expected),
            _ => self.emit_expression(expr),
        }
    }

    fn collect_free_closure_vars(
        &self,
        body: &ClosureBody,
        params: &[FunctionParam],
    ) -> HashSet<String> {
        let mut locals: HashSet<String> = params.iter().map(|p| p.name.clone()).collect();
        let mut free = HashSet::new();
        match body {
            ClosureBody::Block(block) => self.collect_free_in_block(block, &mut locals, &mut free),
            ClosureBody::Expr(expr) => {
                self.collect_free_in_expr(expr.node.as_ref(), &mut locals, &mut free)
            }
        }
        free
    }

    fn collect_free_in_block(
        &self,
        block: &Block,
        locals: &mut HashSet<String>,
        free: &mut HashSet<String>,
    ) {
        for stmt in &block.statements {
            match stmt {
                Statement::Let(let_stmt) => {
                    if let Some(value) = &let_stmt.value {
                        self.collect_free_in_expr(value, locals, free);
                    }
                    if let Pattern::Identifier(name, _) = &let_stmt.pattern {
                        locals.insert(name.clone());
                    }
                }
                Statement::Assign(assign) => {
                    self.collect_free_in_expr(&assign.target, locals, free);
                    self.collect_free_in_expr(&assign.value, locals, free);
                }
                Statement::While(while_stmt) => {
                    match &while_stmt.condition {
                        WhileCondition::Expr(cond) => self.collect_free_in_expr(cond, locals, free),
                        WhileCondition::Let { value, pattern, .. } => {
                            self.collect_free_in_expr(value, locals, free);
                            if let Pattern::Identifier(name, _) = pattern {
                                locals.insert(name.clone());
                            }
                        }
                    }
                    self.collect_free_in_block(&while_stmt.body, locals, free);
                }
                Statement::Loop(loop_stmt) => {
                    self.collect_free_in_block(&loop_stmt.body, locals, free);
                }
                Statement::For(for_stmt) => {
                    match &for_stmt.target {
                        ForTarget::Range(range) => {
                            self.collect_free_in_expr(&range.start, locals, free);
                            self.collect_free_in_expr(&range.end, locals, free);
                        }
                        ForTarget::Collection(expr) => {
                            self.collect_free_in_expr(expr, locals, free)
                        }
                    }
                    locals.insert(for_stmt.binding.clone());
                    self.collect_free_in_block(&for_stmt.body, locals, free);
                }
                Statement::Expr(expr_stmt) => {
                    self.collect_free_in_expr(&expr_stmt.expr, locals, free)
                }
                Statement::Return(ret) => {
                    for expr in &ret.values {
                        self.collect_free_in_expr(expr, locals, free);
                    }
                }
                Statement::Block(inner) => self.collect_free_in_block(inner, locals, free),
                Statement::Defer(defer_stmt) => {
                    self.collect_free_in_expr(&defer_stmt.expr, locals, free)
                }
                Statement::Break | Statement::Continue | Statement::MacroSemi(_) => {}
            }
        }
        if let Some(tail) = &block.tail {
            self.collect_free_in_expr(tail, locals, free);
        }
    }

    fn collect_free_in_expr(
        &self,
        expr: &Expr,
        locals: &mut HashSet<String>,
        free: &mut HashSet<String>,
    ) {
        match expr {
            Expr::Identifier(ident) => {
                if !locals.contains(&ident.name) {
                    free.insert(ident.name.clone());
                }
            }
            Expr::Literal(_) => {}
            Expr::Binary { left, right, .. } => {
                self.collect_free_in_expr(left, locals, free);
                self.collect_free_in_expr(right, locals, free);
            }
            Expr::Unary { expr, .. } => self.collect_free_in_expr(expr, locals, free),
            Expr::Call { callee, args, .. } => {
                self.collect_free_in_expr(callee, locals, free);
                for arg in args {
                    self.collect_free_in_expr(arg, locals, free);
                }
            }
            Expr::Block(block) => {
                let mut nested_locals = locals.clone();
                self.collect_free_in_block(block, &mut nested_locals, free);
            }
            Expr::If(if_expr) => {
                match &if_expr.condition {
                    IfCondition::Expr(cond) => self.collect_free_in_expr(cond, locals, free),
                    IfCondition::Let { value, pattern, .. } => {
                        self.collect_free_in_expr(value, locals, free);
                        if let Pattern::Identifier(name, _) = pattern {
                            locals.insert(name.clone());
                        }
                    }
                }
                self.collect_free_in_block(&if_expr.then_branch, locals, free);
                if let Some(else_branch) = &if_expr.else_branch {
                    match else_branch {
                        ElseBranch::Block(block) => self.collect_free_in_block(block, locals, free),
                        ElseBranch::ElseIf(nested) => {
                            self.collect_free_in_expr(&Expr::If(nested.clone()), locals, free)
                        }
                    }
                }
            }
            Expr::Tuple(values, _) => {
                for v in values {
                    self.collect_free_in_expr(v, locals, free);
                }
            }
            Expr::ArrayLiteral(values, _) => {
                for v in values {
                    self.collect_free_in_expr(v, locals, free);
                }
            }
            Expr::Range(range) => {
                self.collect_free_in_expr(&range.start, locals, free);
                self.collect_free_in_expr(&range.end, locals, free);
            }
            Expr::Index { base, index, .. } => {
                self.collect_free_in_expr(base, locals, free);
                self.collect_free_in_expr(index, locals, free);
            }
            Expr::StructLiteral { fields, .. } => match fields {
                StructLiteralKind::Named(named) => {
                    for field in named {
                        self.collect_free_in_expr(&field.value, locals, free);
                    }
                }
                StructLiteralKind::Positional(values) => {
                    for v in values {
                        self.collect_free_in_expr(v, locals, free);
                    }
                }
            },
            Expr::EnumLiteral { values, .. } => {
                for v in values {
                    self.collect_free_in_expr(v, locals, free);
                }
            }
            Expr::MapLiteral { entries, .. } => {
                for entry in entries {
                    self.collect_free_in_expr(&entry.key, locals, free);
                    self.collect_free_in_expr(&entry.value, locals, free);
                }
            }
            Expr::Match(match_expr) => {
                self.collect_free_in_expr(&match_expr.expr, locals, free);
                for arm in &match_expr.arms {
                    self.collect_free_in_expr(&arm.value, locals, free);
                }
            }
            Expr::Reference { expr, .. } | Expr::Deref { expr, .. } | Expr::Move { expr, .. } => {
                self.collect_free_in_expr(expr, locals, free)
            }
            Expr::Async { block, .. } => self.collect_free_in_block(block, locals, free),
            Expr::Await { expr, .. } => self.collect_free_in_expr(expr, locals, free),
            Expr::Spawn { expr, .. } => self.collect_free_in_expr(expr, locals, free),
            Expr::Closure { .. } => {}
            Expr::FormatString(lit) => {
                for segment in &lit.segments {
                    if let FormatSegment::Expr { expr, .. } = segment {
                        self.collect_free_in_expr(expr, locals, free);
                    }
                }
            }
            Expr::Try { block, .. } => self.collect_free_in_block(block, locals, free),
            Expr::TryPropagate { expr, .. } => self.collect_free_in_expr(expr, locals, free),
            Expr::MacroCall { .. } => {}
            Expr::FieldAccess { base, .. } => self.collect_free_in_expr(base, locals, free),
        }
    }

    fn alloc_handle_array(&mut self, handles: &[LLVMValueRef]) -> LLVMValueRef {
        if handles.is_empty() {
            return std::ptr::null_mut();
        }
        unsafe {
            let array_type = LLVMArrayType2(self.runtime_abi.handle_type, handles.len() as u64);
            let name = CString::new("enum_values").unwrap();
            let alloca = LLVMBuildAlloca(self.builder, array_type, name.as_ptr());
            let zero = LLVMConstInt(self.i32_type, 0, 0);
            for (idx, handle) in handles.iter().enumerate() {
                let idx_const = LLVMConstInt(self.i32_type, idx as u64, 0);
                let mut indices = [zero, idx_const];
                let gep_name = CString::new("enum_val_gep").unwrap();
                let elem_ptr = LLVMBuildInBoundsGEP2(
                    self.builder,
                    array_type,
                    alloca,
                    indices.as_mut_ptr(),
                    2,
                    gep_name.as_ptr(),
                );
                LLVMBuildStore(self.builder, *handle, elem_ptr);
            }
            let mut indices = [zero, zero];
            let base_ptr = LLVMBuildInBoundsGEP2(
                self.builder,
                array_type,
                alloca,
                indices.as_mut_ptr(),
                2,
                CString::new("enum_vals_base").unwrap().as_ptr(),
            );
            base_ptr
        }
    }

    fn emit_statement(&mut self, statement: &Statement) -> Result<Option<FlowSignal>, String> {
        match statement {
            Statement::Let(stmt) => match &stmt.pattern {
                Pattern::Identifier(name, _) => {
                    let value = if let Some(expr) = &stmt.value {
                        let expected = stmt.ty.as_ref().map(|ann| &ann.ty);
                        match self.emit_expression_with_hint(expr, expected)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        }
                    } else {
                        self.evaluated(Value::Unit)
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
                    if !self.match_pattern(value.value(), pattern, allow_mut)? {
                        return Err(format!(
                            "Pattern did not match value {}",
                            self.describe_value(value.value())
                        ));
                    }
                }
            },
            Statement::MacroSemi(_) => {}
            Statement::Expr(expr_stmt) => {
                if let Some(flow) = self.eval_expression_statement(&expr_stmt.expr)? {
                    return Ok(Some(flow));
                }
            }
            Statement::Return(stmt) => {
                let mut values = Vec::new();
                for (idx, expr) in stmt.values.iter().enumerate() {
                    let expected = self
                        .current_return_types()
                        .and_then(|rets| rets.get(idx).cloned());
                    match self.emit_expression_with_hint(expr, expected.as_ref())? {
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
                if let Some(stack) = self.cleanup_stack.last_mut() {
                    stack.push(CleanupAction::Defer(stmt.expr.clone()));
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
                        EvalOutcome::Value(value) => value.into_value(),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    match target {
                        Value::Reference(reference) => {
                            let value = match self.emit_expression(&stmt.value)? {
                                EvalOutcome::Value(value) => value,
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            };
                            self.write_reference(&reference, value)?;
                        }
                        Value::Pointer(pointer) => {
                            if !pointer.mutable {
                                return Err("Cannot assign through immutable reference".into());
                            }
                            let value = match self.emit_expression(&stmt.value)? {
                                EvalOutcome::Value(value) => value,
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            };
                            *pointer.cell.lock().unwrap() = value;
                        }
                        _ => {
                            return Err(
                                "Cannot assign through non-reference value in build mode".into()
                            );
                        }
                    }
                }
                Expr::Index { base, index, .. } => {
                    let target = match self.emit_expression(base)? {
                        EvalOutcome::Value(value) => value.into_value(),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let index_value = match self.emit_expression(index)? {
                        EvalOutcome::Value(value) => value.into_value(),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let value = match self.emit_expression(&stmt.value)? {
                        EvalOutcome::Value(value) => value.into_value(),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    self.assign_index_value(target, index_value, value)?;
                }
                _ => {
                    return Err(
                        "Only assignments to identifiers, dereferences, or indexes are supported in build mode"
                            .into(),
                    );
                }
            },
            Statement::While(stmt) => match &stmt.condition {
                WhileCondition::Expr(expr) => loop {
                    let condition = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value.into_value(),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let cond_value = self.value_to_bool(condition)?;
                    if let Some(flag) = cond_value.constant() {
                        if !flag {
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
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow))
                            }
                        }
                    } else {
                        unsafe {
                            let current_block = LLVMGetInsertBlock(self.builder);
                            if current_block.is_null() {
                                return Err("no insertion block for while loop".into());
                            }
                            let function = LLVMGetBasicBlockParent(current_block);
                            if function.is_null() {
                                return Err("while loop outside function".into());
                            }
                            let cond_block = LLVMAppendBasicBlockInContext(
                                self.context,
                                function,
                                CString::new("while_cond").unwrap().as_ptr(),
                            );
                            let body_block = LLVMAppendBasicBlockInContext(
                                self.context,
                                function,
                                CString::new("while_body").unwrap().as_ptr(),
                            );
                            let after_block = LLVMAppendBasicBlockInContext(
                                self.context,
                                function,
                                CString::new("while_after").unwrap().as_ptr(),
                            );
                            LLVMBuildBr(self.builder, cond_block);
                            LLVMPositionBuilderAtEnd(self.builder, cond_block);
                            let cond_value = match self.emit_expression(expr)? {
                                EvalOutcome::Value(value) => value.into_value(),
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            };
                            let cond_bool = self.value_to_bool(cond_value)?;
                            let cond_llvm = self.bool_llvm_value(&cond_bool);
                            LLVMBuildCondBr(self.builder, cond_llvm, body_block, after_block);
                            LLVMPositionBuilderAtEnd(self.builder, body_block);
                            self.push_scope();
                            let result = self.execute_block_contents(&stmt.body)?;
                            self.exit_scope()?;
                            match result {
                                BlockEval::Value(_) | BlockEval::Flow(FlowSignal::Continue) => {
                                    LLVMBuildBr(self.builder, cond_block);
                                }
                                BlockEval::Flow(FlowSignal::Break) => {
                                    LLVMBuildBr(self.builder, after_block);
                                }
                                BlockEval::Flow(flow @ FlowSignal::Return(_))
                                | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                    LLVMPositionBuilderAtEnd(self.builder, after_block);
                                    return Ok(Some(flow));
                                }
                            }
                            LLVMPositionBuilderAtEnd(self.builder, after_block);
                        }
                        break;
                    }
                },
                WhileCondition::Let { pattern, value } => loop {
                    let candidate = match self.emit_expression(value)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    self.push_scope();
                    let matched = self.match_pattern(candidate.value(), pattern, false)?;
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
            Statement::Loop(loop_stmt) => {
                // For embedded targets, avoid unrolling an infinite loop at compile time;
                // instead emit a basic block that branches to itself. This keeps IR finite.
                if self.target.is_embedded() && !block_has_break_or_continue(&loop_stmt.body) {
                    unsafe {
                        let current_bb = LLVMGetInsertBlock(self.builder);
                        let func = LLVMGetBasicBlockParent(current_bb);
                        let loop_bb_name = CString::new("loop").unwrap();
                        let after_bb_name = CString::new("after_loop").unwrap();
                        let loop_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            func,
                            loop_bb_name.as_ptr(),
                        );
                        let after_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            func,
                            after_bb_name.as_ptr(),
                        );
                        LLVMBuildBr(self.builder, loop_bb);
                        LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                        self.push_scope();
                        let _ = self.execute_block_contents(&loop_stmt.body)?;
                        self.exit_scope()?;
                        LLVMBuildBr(self.builder, loop_bb);
                        // Position builder after loop to keep subsequent code reachable (though unreachable at runtime).
                        LLVMPositionBuilderAtEnd(self.builder, after_bb);
                    }
                    return Ok(None);
                }
                loop {
                    self.push_scope();
                    let result = self.execute_block_contents(&loop_stmt.body)?;
                    self.exit_scope()?;
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                        BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => return Ok(Some(flow)),
                    }
                }
                return Ok(None);
            }
            Statement::For(stmt) => match &stmt.target {
                ForTarget::Range(range_expr) => {
                    let start_value = match self.emit_expression(&range_expr.start)? {
                        EvalOutcome::Value(value) => self.expect_int(value.into_value())?,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let end_value = match self.emit_expression(&range_expr.end)? {
                        EvalOutcome::Value(value) => self.expect_int(value.into_value())?,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    if let (Some(start_const), Some(end_const)) =
                        (start_value.constant(), end_value.constant())
                    {
                        let mut current = start_const;
                        let limit = if range_expr.inclusive {
                            end_const + 1
                        } else {
                            end_const
                        };
                        while current < limit {
                            self.push_scope();
                            self.insert_var(
                                &stmt.binding,
                                Value::Int(self.const_int_value(current)).into(),
                                false,
                            )?;
                            let result = self.execute_block_contents(&stmt.body)?;
                            self.exit_scope()?;
                            match result {
                                BlockEval::Value(_) => {}
                                BlockEval::Flow(FlowSignal::Continue) => {}
                                BlockEval::Flow(FlowSignal::Break) => break,
                                BlockEval::Flow(flow @ FlowSignal::Return(_)) => {
                                    return Ok(Some(flow));
                                }
                                BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                    return Ok(Some(flow));
                                }
                            }
                            current += 1;
                        }
                    } else {
                        if let Some(flow) = self.emit_dynamic_range_for(
                            start_value,
                            end_value,
                            range_expr.inclusive,
                            stmt,
                        )? {
                            return Ok(Some(flow));
                        }
                    }
                }
                ForTarget::Collection(expr) => {
                    let iterable = match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let elements = self.collect_iterable_values(iterable.into_value())?;
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

    fn emit_expression(&mut self, expr: &Expr) -> Result<EvalOutcome<EvaluatedValue>, String> {
        match expr {
            Expr::Literal(Literal::Int(value, _)) => Ok(EvalOutcome::Value(
                self.evaluated(Value::Int(self.const_int_value(*value as i128))),
            )),
            Expr::Literal(Literal::Bool(value, _)) => Ok(EvalOutcome::Value(
                self.evaluated(Value::Bool(BoolValue::new(
                    unsafe {
                        LLVMConstInt(
                            self.runtime_abi.bool_type,
                            *value as u64,
                            0,
                        )
                    },
                    Some(*value),
                ))),
            )),
            Expr::Literal(Literal::Float(value, _)) => Ok(EvalOutcome::Value(
                self.evaluated(Value::Float(self.const_float_value(*value))),
            )),
            Expr::Literal(Literal::String(value, _)) => {
                let string = self.build_string_constant(value.clone())?;
                Ok(EvalOutcome::Value(self.evaluated(string)))
            }
            Expr::FormatString(literal) => {
                let value = self.build_format_string_value(literal)?;
                Ok(EvalOutcome::Value(self.evaluated(value)))
            }
            Expr::Literal(Literal::Rune(value, _)) => Ok(EvalOutcome::Value(
                self.evaluated(Value::Int(self.const_int_value(*value as i128))),
            )),
            Expr::Try { block, .. } => {
                self.push_scope();
                let result = self.execute_block_contents(block)?;
                self.exit_scope()?;
                match result {
                    BlockEval::Value(value) => {
                        let wrapped =
                            self.instantiate_enum_variant("Ok", vec![value.into_value()])?;
                        Ok(EvalOutcome::Value(self.evaluated(wrapped)))
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
                if matches!(value.value(), Value::Moved) {
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
                self.eval_binary(*op, lhs.into_value(), rhs.into_value())
                    .map(|v| EvalOutcome::Value(self.evaluated(v)))
            }
            Expr::StructLiteral { name, fields, .. } => self.build_struct_literal(name, fields),
            Expr::EnumLiteral {
                variant, values, ..
            } => {
                let mut evaluated = Vec::new();
                for expr in values {
                    match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => evaluated.push(value.into_value()),
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                self.instantiate_enum_variant(variant, evaluated)
                    .map(|v| EvalOutcome::Value(self.evaluated(v)))
            }
            Expr::MapLiteral { entries, .. } => self.emit_map_literal(entries),
            Expr::Match(match_expr) => self.emit_match_expression(match_expr),
            Expr::Tuple(values, _) => {
                let mut items = Vec::new();
                for value in values {
                    match self.emit_expression(value)? {
                        EvalOutcome::Value(value) => items.push(value.into_value()),
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                Ok(EvalOutcome::Value(self.evaluated(Value::Tuple(items))))
            }
            Expr::ArrayLiteral(values, _) => self.emit_array_literal(values),
            Expr::Range(range) => {
                let start = self.expect_int_value_from_expr(&range.start)?;
                let end = self.expect_int_value_from_expr(&range.end)?;
                Ok(EvalOutcome::Value(self.evaluated(Value::Range(
                    RangeValue {
                        start,
                        end,
                        inclusive: range.inclusive,
                    },
                ))))
            }
            Expr::Index { base, index, .. } => {
                let base_value = match self.emit_expression(base)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let index_value = match self.emit_expression(index)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let value = self.builtin_get(vec![base_value, index_value])?;
                Ok(EvalOutcome::Value(self.evaluated(value)))
            }
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
                EvalOutcome::Value(value) => {
                    self.deref_value(value.into_value()).map(EvalOutcome::Value)
                }
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::Move { expr, .. } => self.emit_move_expression(expr),
            Expr::Async { block, .. } => {
                self.ensure_async_supported()?;
                self.force_runtime_handles = true;
                let mut locals = HashSet::new();
                let mut free = HashSet::new();
                self.collect_free_in_block(block, &mut locals, &mut free);
                let capture_names: Vec<String> = free.into_iter().collect();
                let task = TaskValue::deferred((**block).clone(), capture_names);
                Ok(EvalOutcome::Value(Value::Task(Box::new(task)).into()))
            }
            Expr::Await { expr, .. } => {
                self.ensure_async_supported()?;
                self.force_runtime_handles = true;
                match self.emit_expression(expr)? {
                    EvalOutcome::Value(value) => {
                        let concrete = value.into_value();
                        match concrete {
                            Value::Task(task) => match task.take(self) {
                                Ok(result) => Ok(EvalOutcome::Value(result)),
                                Err(err) => Err(err),
                            },
                            other => Err(format!(
                                "`await` expects a Task, found {}",
                                self.describe_value(&other)
                            )),
                        }
                    }
                    EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                }
            }
            Expr::Spawn { expr, .. } => {
                let experimental = env::var("PRIME_BUILD_PARALLEL")
                    .map(|v| v == "1")
                    .unwrap_or(false);
                if experimental {
                    let snapshot = self.snapshot_build_state()?;
                    let expr_clone = expr.clone();
                    let handle = thread::spawn(move || {
                        let interpreter = BuildInterpreter::new(snapshot);
                        interpreter.eval_with_effects(&expr_clone)
                    });
                    Ok(EvalOutcome::Value(
                        Value::JoinHandle(Box::new(JoinHandleValue::new_build(handle))).into(),
                    ))
                } else {
                    match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => Ok(EvalOutcome::Value(
                            Value::JoinHandle(Box::new(JoinHandleValue::new(value.into_value())))
                                .into(),
                        )),
                        EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                    }
                }
            }
            Expr::Closure {
                params,
                body,
                ret,
                captures,
                ..
            } => return self.emit_closure_literal(params, body, ret, captures, None),
            Expr::FieldAccess { base, field, .. } => {
                let base_value = match self.emit_expression(base)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let mut current = base_value.into_value();
                loop {
                    current = match current {
                        Value::Struct(instance) => {
                            let value = instance
                                .get(field)
                                .ok_or_else(|| format!("Field {} not found", field))?;
                            return Ok(EvalOutcome::Value(value.into()));
                        }
                        Value::Reference(reference) => {
                            reference.cell.lock().unwrap().clone().into_value()
                        }
                        Value::Pointer(pointer) => {
                            pointer.cell.lock().unwrap().clone().into_value()
                        }
                        Value::Iterator(_) => {
                            return Err("Cannot access field on iterator value".into());
                        }
                        Value::Range(_) => {
                            return Err("Cannot access field on range value".into());
                        }
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
                        Value::FormatTemplate(_) => {
                            return Err("Cannot access field on format string value".into());
                        }
                        Value::Sender(_) | Value::Receiver(_) | Value::JoinHandle(_) => {
                            return Err("Cannot access field on concurrency value".into());
                        }
                        Value::Task(_) => {
                            return Err("Cannot access field on task value".into());
                        }
                        Value::Closure(_) => {
                            return Err("Cannot access field on closure value".into());
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
            Expr::Unary { op, expr, .. } => {
                let value = match self.emit_expression(expr)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                self.eval_unary(*op, value)
                    .map(|v| EvalOutcome::Value(self.evaluated(v)))
            }
            Expr::MacroCall { name, .. } => Err(format!(
                "macro `{}` must be expanded before code generation",
                name.name
            )),
        }
    }

    fn eval_call_args_with_hints(
        &mut self,
        args: &[Expr],
        hints: Option<&[TypeExpr]>,
    ) -> Result<Vec<EvaluatedValue>, String> {
        let mut evaluated_args = Vec::new();
        for (idx, expr) in args.iter().enumerate() {
            let expected = hints.and_then(|h| h.get(idx));
            match self.emit_expression_with_hint(expr, expected)? {
                EvalOutcome::Value(value) => evaluated_args.push(value),
                EvalOutcome::Flow(flow) => {
                    return Err(format!(
                        "Control flow {} cannot escape argument position in build mode",
                        flow_name(&flow)
                    ));
                }
            }
        }
        Ok(evaluated_args)
    }

    fn call_closure_value(
        &mut self,
        closure: ClosureValue,
        args: Vec<EvaluatedValue>,
    ) -> Result<EvaluatedValue, String> {
        let info = self
            .closures
            .get(&closure.id)
            .cloned()
            .ok_or_else(|| "closure metadata missing".to_string())?;
        if info.signature.params.len() != args.len() {
            return Err(format!(
                "Closure expects {} arguments, got {}",
                info.signature.params.len(),
                args.len()
            ));
        }
        let mut llvm_args = Vec::with_capacity(args.len() + 1);
        let expected_env = unsafe { LLVMPointerType(info.env_type, 0) };
        let env_arg = unsafe {
            if LLVMTypeOf(closure.env_ptr) == expected_env {
                closure.env_ptr
            } else {
                LLVMBuildBitCast(
                    self.builder,
                    closure.env_ptr,
                    expected_env,
                    CString::new("closure_env_cast").unwrap().as_ptr(),
                )
            }
        };
        llvm_args.push(env_arg);
        for (value, ty) in args.into_iter().zip(info.signature.params.iter()) {
            llvm_args.push(self.value_to_llvm_for_type(value.into_value(), ty)?);
        }
        let fn_cast = unsafe {
            LLVMBuildBitCast(
                self.builder,
                closure.fn_ptr,
                LLVMPointerType(info.fn_type, 0),
                CString::new("closure_fn_cast").unwrap().as_ptr(),
            )
        };
        let call_name = if info.signature.return_count() == 0 {
            CString::new("").unwrap()
        } else {
            CString::new("closure_call").unwrap()
        };
        let result = unsafe {
            LLVMBuildCall2(
                self.builder,
                info.fn_type,
                fn_cast,
                llvm_args.as_mut_ptr(),
                llvm_args.len() as u32,
                call_name.as_ptr(),
            )
        };
        let value = match info.signature.return_count() {
            0 => self.evaluated(Value::Unit),
            1 => {
                let ret = self.value_from_llvm(result, &info.signature.returns[0])?;
                self.evaluated(ret)
            }
            _ => {
                let mut items = Vec::new();
                for (idx, ty) in info.signature.returns.iter().enumerate() {
                    let extracted = unsafe {
                        LLVMBuildExtractValue(
                            self.builder,
                            result,
                            idx as u32,
                            CString::new(format!("closure_ret_{idx}")).unwrap().as_ptr(),
                        )
                    };
                    items.push(self.value_from_llvm(extracted, ty)?);
                }
                self.evaluated(Value::Tuple(items))
            }
        };
        Ok(value)
    }

    fn eval_async_block(
        &mut self,
        block: &Block,
        captures: &[(String, EvaluatedValue)],
    ) -> Result<EvaluatedValue, String> {
        self.push_scope();
        for (name, value) in captures {
            self.insert_var(name, value.clone(), true)?;
        }
        let result = self.execute_block_contents(block)?;
        self.exit_scope()?;
        match result {
            BlockEval::Value(val) => Ok(val),
            BlockEval::Flow(FlowSignal::Return(values)) => {
                if values.len() == 1 {
                    Ok(values.into_iter().next().unwrap())
                } else {
                    let mut items = Vec::new();
                    for v in values {
                        items.push(v.into_value());
                    }
                    Ok(self.evaluated(Value::Tuple(items)))
                }
            }
            BlockEval::Flow(FlowSignal::Propagate(value)) => Err(format!(
                "async block propagated error: {}",
                flow_name(&FlowSignal::Propagate(value.clone()))
            )),
            BlockEval::Flow(_) => Err("control flow cannot exit async block in build mode".into()),
        }
    }

    fn emit_call_expression(
        &mut self,
        callee: &Expr,
        type_args: &[TypeExpr],
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
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
                if ident.name == "in" {
                    return self.emit_in_call(type_args, args);
                }
                if let Some(result) = self.try_builtin_call(&ident.name, args) {
                    return result;
                }
                if let Some(binding) = self.get_var(&ident.name) {
                    match binding.value().clone() {
                        Value::Closure(closure) => {
                            let arg_hints = self
                                .closures
                                .get(&closure.id)
                                .map(|info| info.signature.params.clone());
                            let evaluated_args =
                                self.eval_call_args_with_hints(args, arg_hints.as_deref())?;
                            let value = self.call_closure_value(closure, evaluated_args)?;
                            return Ok(EvalOutcome::Value(value));
                        }
                        other => {
                            return Err(format!(
                                "`{}` is not callable in build mode (found {})",
                                ident.name,
                                describe_value(&other)
                            ));
                        }
                    }
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
            _ => {
                let callee_value = match self.emit_expression(callee)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                if let Value::Closure(closure) = callee_value {
                    let arg_hints = self
                        .closures
                        .get(&closure.id)
                        .map(|info| info.signature.params.clone());
                    let evaluated_args =
                        self.eval_call_args_with_hints(args, arg_hints.as_deref())?;
                    let value = self.call_closure_value(closure, evaluated_args)?;
                    Ok(EvalOutcome::Value(value))
                } else {
                    Err("Only direct function calls are supported in build mode expressions".into())
                }
            }
        }
    }

    fn collapse_results(&self, mut values: Vec<EvaluatedValue>) -> EvaluatedValue {
        match values.len() {
            0 => EvaluatedValue::from_value(Value::Unit),
            1 => values.pop().unwrap(),
            _ => {
                let tuple = values.into_iter().map(EvaluatedValue::into_value).collect();
                EvaluatedValue::from_value(Value::Tuple(tuple))
            }
        }
    }

    fn emit_out_call(&mut self, args: &[Expr]) -> Result<EvalOutcome<EvaluatedValue>, String> {
        if args.is_empty() {
            return Err("out() expects at least one argument".into());
        }
        let mut evaluated = Vec::new();
        for expr in args {
            match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => evaluated.push(value),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        let mut iter = evaluated.into_iter();
        let first = iter.next().unwrap();
        match first.value() {
            Value::FormatTemplate(template) => {
                let provided: Vec<EvaluatedValue> = iter.collect();
                if template.implicit_placeholders != provided.len() {
                    return Err(format!(
                        "Format string expects {} argument(s), got {}",
                        template.implicit_placeholders,
                        provided.len()
                    ));
                }
                self.emit_format_template(template.clone(), provided)?;
                Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
            }
            _ => {
                if iter.next().is_some() {
                    return Err(
                        "out() with multiple arguments requires a format string literal".into(),
                    );
                }
                self.emit_out_value(first)?;
                Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
            }
        }
    }

    fn emit_in_call(
        &mut self,
        type_args: &[TypeExpr],
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        if type_args.len() != 1 {
            return Err(
                "`in` expects exactly one type argument, e.g. in[int32](\"prompt\")".into(),
            );
        }
        if args.is_empty() {
            return Err(
                "`in` expects at least 1 argument (a prompt, optionally with format placeholders)"
                    .into(),
            );
        }

        // Evaluate prompt and arguments
        let mut evaluated_args = Vec::new();
        for expr in args {
            match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => evaluated_args.push(value.into_value()),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }

        // Render prompt and collect format handles
        let mut arg_iter = evaluated_args.into_iter();
        let first = arg_iter.next().unwrap();
        let (prompt_bytes, fmt_handles) = match first {
            Value::FormatTemplate(template) => {
                let provided: Vec<Value> = arg_iter.collect();
                if template.implicit_placeholders != provided.len() {
                    return Err(format!(
                        "Format string expects {} argument(s), got {}",
                        template.implicit_placeholders,
                        provided.len()
                    ));
                }
                let handles: Vec<LLVMValueRef> = provided
                    .into_iter()
                    .map(|v| self.build_runtime_handle(v))
                    .collect::<Result<_, _>>()?;
                let prompt = self.render_format_template_bytes(template)?;
                (prompt, handles)
            }
            other => {
                let prompt = self.render_runtime_value_bytes(&other)?;
                (prompt, Vec::new())
            }
        };

        // Map type to code
        let type_code = self.type_code_for(&type_args[0])?;

        // Find Result::Ok/Err tags
        let ok_tag = self
            .enum_variants
            .get("Ok")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Ok variant not found; ensure core::types is available in build mode"
                    .to_string()
            })?;
        let err_tag = self
            .enum_variants
            .get("Err")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Err variant not found; ensure core::types is available in build mode"
                    .to_string()
            })?;

        // Build runtime arguments
        let prompt_str =
            String::from_utf8(prompt_bytes).map_err(|_| "prompt is not valid utf-8")?;
        let (prompt_ptr, prompt_len) = self.build_runtime_bytes(&prompt_str, "rt_prompt")?;
        let handles_array = if fmt_handles.is_empty() {
            self.null_handle_ptr()
        } else {
            self.alloc_handle_array(&fmt_handles)
        };
        let fmt_len =
            unsafe { LLVMConstInt(self.runtime_abi.usize_type, fmt_handles.len() as u64, 0) };
        let type_code_const =
            unsafe { LLVMConstInt(self.runtime_abi.status_type, type_code as u64, 0) };
        let ok_tag_const = unsafe { LLVMConstInt(self.runtime_abi.status_type, ok_tag as u64, 0) };
        let err_tag_const =
            unsafe { LLVMConstInt(self.runtime_abi.status_type, err_tag as u64, 0) };

        let result_handle = self.call_runtime(
            self.runtime_abi.prime_read_value,
            self.runtime_abi.prime_read_value_ty,
            &mut [
                type_code_const,
                ok_tag_const,
                err_tag_const,
                prompt_ptr,
                prompt_len,
                handles_array,
                fmt_len,
            ],
            "read_value",
        );

        // Wrap in EvaluatedValue with runtime handle
        Ok(EvalOutcome::Value(
            self.evaluated(Value::Enum(EnumValue {
                enum_name: "Result".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }))
            .with_runtime(result_handle),
        ))
    }
    fn try_builtin_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Option<Result<EvalOutcome<EvaluatedValue>, String>> {
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
            "slice_remove" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_slice_remove(values)))
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
            "map_keys" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_map_keys(values)))
            }
            "map_values" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_map_values(values)))
            }
            "map_remove" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_map_remove(values)))
            }
            "len" => Some(self.invoke_builtin(args, |this, values| this.builtin_len(values))),
            "get" => Some(self.invoke_builtin(args, |this, values| this.builtin_get(values))),
            "remove" => Some(self.invoke_builtin(args, |this, values| this.builtin_remove(values))),
            "push" => Some(self.invoke_builtin(args, |this, values| this.builtin_push(values))),
            "insert" => Some(self.invoke_builtin(args, |this, values| this.builtin_insert(values))),
            "iter" => Some(self.invoke_builtin(args, |this, values| this.builtin_iter(values))),
            "next" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_iter_next(values)))
            }
            "assert" => Some(self.invoke_builtin(args, |this, values| this.builtin_assert(values))),
            "expect" => Some(self.invoke_builtin(args, |this, values| this.builtin_expect(values))),
            "str_len" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_str_len(values)))
            }
            "str_contains" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_str_contains(values)))
            }
            "str_trim" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_str_trim(values)))
            }
            "str_split" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_str_split(values)))
            }
            "min" => Some(self.invoke_builtin(args, |this, values| this.builtin_min(values))),
            "max" => Some(self.invoke_builtin(args, |this, values| this.builtin_max(values))),
            "abs" => Some(self.invoke_builtin(args, |this, values| this.builtin_abs(values))),
            "channel" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_channel(values)))
            }
            "debug_show" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_debug_show(values)))
            }
            "send" => Some(self.invoke_builtin(args, |this, values| this.builtin_send(values))),
            "recv" => Some(self.invoke_builtin(args, |this, values| this.builtin_recv(values))),
            "recv_timeout" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_recv_timeout(values)))
            }
            "recv_task" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_recv_task(values)))
            }
            "sleep_task" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_sleep_task(values)))
            }
            "close" => Some(self.invoke_builtin(args, |this, values| this.builtin_close(values))),
            "join" => Some(self.invoke_builtin(args, |this, values| this.builtin_join(values))),
            "sleep" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_sleep_ms(values)))
            }
            "sleep_ms" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_sleep_ms(values)))
            }
            "delay_ms" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_delay_ms(values)))
            }
            "now_ms" => Some(self.invoke_builtin(args, |this, values| this.builtin_now_ms(values))),
            "fs_exists" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_fs_exists(values)))
            }
            "fs_read" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_fs_read(values)))
            }
            "fs_write" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_fs_write(values)))
            }
            "pin_mode" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_pin_mode(values)))
            }
            "digital_write" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_digital_write(values)))
            }
            "ptr" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_ptr(values, false)))
            }
            "ptr_mut" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_ptr(values, true)))
            }
            _ => None,
        }
    }

    fn emit_if_expression(
        &mut self,
        if_expr: &IfExpr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        match &if_expr.condition {
            IfCondition::Expr(condition) => {
                let cond_value = match self.emit_expression(condition)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let cond_bool = self.value_to_bool(cond_value)?;
                if let Some(flag) = cond_bool.constant() {
                    if flag {
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
                        Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
                    }
                } else {
                    unsafe {
                        let current_block = LLVMGetInsertBlock(self.builder);
                        if current_block.is_null() {
                            return Err("no insertion block for if expression".into());
                        }
                        let function = LLVMGetBasicBlockParent(current_block);
                        if function.is_null() {
                            return Err("if expression outside function".into());
                        }
                        let then_block = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            CString::new("if_then").unwrap().as_ptr(),
                        );
                        let else_block = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            CString::new("if_else").unwrap().as_ptr(),
                        );
                        let merge_block = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            CString::new("if_merge").unwrap().as_ptr(),
                        );
                        let cond_llvm = self.bool_llvm_value(&cond_bool);
                        LLVMBuildCondBr(self.builder, cond_llvm, then_block, else_block);

                        LLVMPositionBuilderAtEnd(self.builder, then_block);
                        self.push_scope();
                        let mut then_value: Option<EvaluatedValue> = None;
                        let then_eval = self.execute_block_contents(&if_expr.then_branch)?;
                        self.exit_scope()?;
                        match then_eval {
                            BlockEval::Flow(flow @ FlowSignal::Return(_))
                            | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                LLVMPositionBuilderAtEnd(self.builder, merge_block);
                                return Ok(EvalOutcome::Flow(flow));
                            }
                            BlockEval::Value(val) => {
                                then_value = Some(val);
                                LLVMBuildBr(self.builder, merge_block);
                            }
                            BlockEval::Flow(FlowSignal::Break)
                            | BlockEval::Flow(FlowSignal::Continue) => {
                                LLVMBuildBr(self.builder, merge_block);
                            }
                        };

                        LLVMPositionBuilderAtEnd(self.builder, else_block);
                        let mut else_value: Option<EvaluatedValue> = None;
                        if let Some(else_branch) = &if_expr.else_branch {
                            match else_branch {
                                ElseBranch::Block(block) => {
                                    self.push_scope();
                                    let else_eval = self.execute_block_contents(block)?;
                                    self.exit_scope()?;
                                    match else_eval {
                                        BlockEval::Flow(flow @ FlowSignal::Return(_))
                                        | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                            LLVMPositionBuilderAtEnd(self.builder, merge_block);
                                            return Ok(EvalOutcome::Flow(flow));
                                        }
                                        BlockEval::Value(val) => {
                                            else_value = Some(val);
                                            LLVMBuildBr(self.builder, merge_block);
                                        }
                                        BlockEval::Flow(FlowSignal::Break)
                                        | BlockEval::Flow(FlowSignal::Continue) => {
                                            LLVMBuildBr(self.builder, merge_block);
                                        }
                                    };
                                }
                                ElseBranch::ElseIf(nested) => {
                                    match self.emit_if_expression(nested)? {
                                        EvalOutcome::Flow(flow) => {
                                            LLVMPositionBuilderAtEnd(self.builder, merge_block);
                                            return Ok(EvalOutcome::Flow(flow));
                                        }
                                        EvalOutcome::Value(val) => {
                                            else_value = Some(val);
                                            LLVMBuildBr(self.builder, merge_block);
                                        }
                                    }
                                }
                            }
                        } else {
                            LLVMBuildBr(self.builder, merge_block);
                        }

                        LLVMPositionBuilderAtEnd(self.builder, merge_block);
                        if let (Some(then_val), Some(else_val)) = (then_value, else_value) {
                            return Ok(EvalOutcome::Value(self.merge_if_values(
                                then_val,
                                then_block,
                                else_val,
                                else_block,
                            )?));
                        }
                    }
                    Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
                }
            }
            IfCondition::Let { pattern, value, .. } => {
                let scrutinee = match self.emit_expression(value)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                self.push_scope();
                let matches = self.match_pattern(scrutinee.value(), pattern, false)?;
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
                        Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
                    }
                }
            }
        }
    }

    fn merge_if_values(
        &mut self,
        then_val: EvaluatedValue,
        then_block: LLVMBasicBlockRef,
        else_val: EvaluatedValue,
        else_block: LLVMBasicBlockRef,
    ) -> Result<EvaluatedValue, String> {
        match (then_val.into_value(), else_val.into_value()) {
            (Value::Int(a), Value::Int(b)) => {
                let ty = unsafe { LLVMTypeOf(a.llvm()) };
                let phi = unsafe {
                    LLVMBuildPhi(
                        self.builder,
                        ty,
                        CString::new("if_int_phi").unwrap().as_ptr(),
                    )
                };
                let mut vals = [a.llvm(), b.llvm()];
                let mut blocks = [then_block, else_block];
                unsafe {
                    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                }
                let constant = a
                    .constant()
                    .zip(b.constant())
                    .and_then(|(lhs, rhs)| if lhs == rhs { Some(lhs) } else { None });
                Ok(self.evaluated(Value::Int(IntValue::new(phi, constant))))
            }
            (Value::Float(a), Value::Float(b)) => {
                let ty = unsafe { LLVMTypeOf(a.llvm()) };
                let phi = unsafe {
                    LLVMBuildPhi(
                        self.builder,
                        ty,
                        CString::new("if_float_phi").unwrap().as_ptr(),
                    )
                };
                let mut vals = [a.llvm(), b.llvm()];
                let mut blocks = [then_block, else_block];
                unsafe {
                    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                }
                let constant = a
                    .constant()
                    .zip(b.constant())
                    .and_then(|(lhs, rhs)| if (lhs - rhs).abs() < f64::EPSILON {
                        Some(lhs)
                    } else {
                        None
                    });
                Ok(self.evaluated(Value::Float(FloatValue::new(phi, constant))))
            }
            (Value::Bool(a), Value::Bool(b)) => {
                let ty = unsafe { LLVMTypeOf(a.llvm()) };
                let phi = unsafe {
                    LLVMBuildPhi(
                        self.builder,
                        ty,
                        CString::new("if_bool_phi").unwrap().as_ptr(),
                    )
                };
                let mut vals = [a.llvm(), b.llvm()];
                let mut blocks = [then_block, else_block];
                unsafe {
                    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                }
                let constant = a
                    .constant()
                    .zip(b.constant())
                    .and_then(|(lhs, rhs)| if lhs == rhs { Some(lhs) } else { None });
                Ok(self.evaluated(Value::Bool(BoolValue::new(phi, constant))))
            }
            (Value::Unit, Value::Unit) => Ok(self.evaluated(Value::Unit)),
            (lhs, rhs) => Err(format!(
                "Dynamic if expression not supported for branch values {} and {}",
                self.describe_value(&lhs),
                self.describe_value(&rhs)
            )),
        }
    }

    fn invoke_builtin<F>(
        &mut self,
        args: &[Expr],
        mut f: F,
    ) -> Result<EvalOutcome<EvaluatedValue>, String>
    where
        F: FnMut(&mut Self, Vec<Value>) -> Result<Value, String>,
    {
        let mut evaluated = Vec::with_capacity(args.len());
        for expr in args {
            match self.emit_expression(expr) {
                Ok(EvalOutcome::Value(value)) => evaluated.push(value.into_value()),
                Ok(EvalOutcome::Flow(flow)) => return Ok(EvalOutcome::Flow(flow)),
                Err(err) => return Err(err),
            }
        }
        f(self, evaluated).map(|v| EvalOutcome::Value(self.evaluated(v)))
    }

    fn build_reference(
        &mut self,
        expr: &Expr,
        mutable: bool,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
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
                if matches!(cell.lock().unwrap().value(), Value::Moved) {
                    return Err(format!("Value `{}` has been moved", ident.name));
                }
                Ok(EvalOutcome::Value(
                    Value::Reference(ReferenceValue {
                        cell,
                        mutable,
                        origin: Some(ident.name.clone()),
                        handle: None,
                    })
                    .into(),
                ))
            }
            _ => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => Ok(EvalOutcome::Value(
                    Value::Reference(ReferenceValue {
                        cell: Arc::new(Mutex::new(value)),
                        mutable,
                        origin: None,
                        handle: None,
                    })
                    .into(),
                )),
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
        }
    }

    fn emit_array_literal(
        &mut self,
        values: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let mut items = Vec::with_capacity(values.len());
        for expr in values {
            match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => items.push(value.into_value()),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        Ok(EvalOutcome::Value(
            self.evaluated(Value::Slice(SliceValue::from_vec(items))),
        ))
    }

    fn emit_map_literal(
        &mut self,
        entries: &[MapLiteralEntry],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let mut pairs = Vec::new();
        for entry in entries {
            let key_value = match self.emit_expression(&entry.key)? {
                EvalOutcome::Value(value) => value.into_value(),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            let key = self.expect_string_value(key_value, "map literal key")?;
            let value = match self.emit_expression(&entry.value)? {
                EvalOutcome::Value(value) => value.into_value(),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            pairs.push((key, value));
        }
        Ok(EvalOutcome::Value(
            self.evaluated(Value::Map(MapValue::from_entries(pairs))),
        ))
    }

    fn emit_move_expression(&mut self, expr: &Expr) -> Result<EvalOutcome<EvaluatedValue>, String> {
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
                let mut slot = cell.lock().unwrap();
                if matches!(slot.value(), Value::Moved) {
                    return Err(format!("Value `{}` has been moved", ident.name));
                }
                if !Self::is_heap_value(slot.value()) {
                    return Err(format!(
                        "`{}` cannot be moved; only boxes, slices, and maps support move semantics",
                        ident.name
                    ));
                }
                let moved = std::mem::replace(&mut *slot, EvaluatedValue::from_value(Value::Moved));
                self.register_move(&ident.name);
                Ok(EvalOutcome::Value(moved))
            }
            _ => Err("move expressions require identifiers in build mode".into()),
        }
    }

    fn is_heap_value(value: &Value) -> bool {
        matches!(value, Value::Boxed(_) | Value::Slice(_) | Value::Map(_))
    }

    fn deref_value(&self, value: Value) -> Result<EvaluatedValue, String> {
        match value {
            Value::Reference(reference) => Ok(reference.cell.lock().unwrap().clone()),
            Value::Pointer(pointer) => Ok(pointer.cell.lock().unwrap().clone()),
            _ => Err("Cannot dereference non-reference value in build mode".into()),
        }
    }

    fn emit_match_expression(
        &mut self,
        match_expr: &MatchExpr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let target = match self.emit_expression(&match_expr.expr)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        if let Some(runtime_handle) = target.runtime_handle() {
            if let Value::Enum(enum_value) = target.value() {
                if let Some(result) =
                    self.emit_runtime_enum_match(runtime_handle, enum_value, match_expr)?
                {
                    return Ok(result);
                }
            }
        }
        for arm in &match_expr.arms {
            self.push_scope();
            if self.match_pattern(target.value(), &arm.pattern, false)? {
                let value = self.emit_expression(&arm.value)?;
                self.exit_scope()?;
                return Ok(value);
            }
            self.exit_scope()?;
        }
        Err("No match arm matched in build mode".into())
    }

    fn emit_runtime_enum_match(
        &mut self,
        handle: LLVMValueRef,
        enum_value: &EnumValue,
        match_expr: &MatchExpr,
    ) -> Result<Option<EvalOutcome<EvaluatedValue>>, String> {
        let mut variant_tags = HashMap::new();
        match enum_value.enum_name.as_str() {
            "Result" => {
                let ok_tag = self
                    .enum_variants
                    .get("Ok")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Result::Ok variant not found in build mode".to_string())?;
                let err_tag = self
                    .enum_variants
                    .get("Err")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Result::Err variant not found in build mode".to_string())?;
                variant_tags.insert("Ok", ok_tag);
                variant_tags.insert("Err", err_tag);
            }
            "Option" => {
                let some_tag = self
                    .enum_variants
                    .get("Some")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Option::Some variant not found in build mode".to_string())?;
                let none_tag = self
                    .enum_variants
                    .get("None")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Option::None variant not found in build mode".to_string())?;
                variant_tags.insert("Some", some_tag);
                variant_tags.insert("None", none_tag);
            }
            _ => return Ok(None),
        }

        let supported_patterns = match_expr.arms.iter().all(|arm| match &arm.pattern {
            Pattern::EnumVariant { bindings, .. } => bindings
                .iter()
                .all(|b| matches!(b, Pattern::Identifier(_, _))),
            Pattern::Wildcard => true,
            _ => false,
        });
        if !supported_patterns {
            return Ok(None);
        }

        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let merge_block = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("match_merge").unwrap().as_ptr(),
            )
        };

        let tag = self.call_runtime(
            self.runtime_abi.prime_enum_tag,
            self.runtime_abi.prime_enum_tag_ty,
            &mut [handle],
            "enum_tag",
        );

        let mut current_block = unsafe { LLVMGetInsertBlock(self.builder) };
        let mut result_value: Option<EvaluatedValue> = None;

        for (idx, arm) in match_expr.arms.iter().enumerate() {
            let (expected_tag, bindings) = match &arm.pattern {
                Pattern::EnumVariant {
                    variant, bindings, ..
                } => {
                    let tag = variant_tags.get(variant.as_str()).copied();
                    if tag.is_none() {
                        return Ok(None);
                    }
                    (tag, Some(bindings))
                }
                Pattern::Wildcard => (None, None),
                _ => return Ok(None),
            };

            let then_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    function,
                    CString::new(format!("match_arm_{idx}")).unwrap().as_ptr(),
                )
            };
            let else_block = if idx == match_expr.arms.len() - 1 {
                merge_block
            } else {
                unsafe {
                    LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new(format!("match_else_{idx}")).unwrap().as_ptr(),
                    )
                }
            };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, current_block) };
            if let Some(tag_val) = expected_tag {
                let tag_const = unsafe { LLVMConstInt(LLVMTypeOf(tag), tag_val as u64, 0) };
                let cond = unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        tag,
                        tag_const,
                        CString::new("match_tag_cmp").unwrap().as_ptr(),
                    )
                };
                unsafe { LLVMBuildCondBr(self.builder, cond, then_block, else_block) };
            } else {
                unsafe { LLVMBuildBr(self.builder, then_block) };
            }

            unsafe { LLVMPositionBuilderAtEnd(self.builder, then_block) };
            self.push_scope();
            if let Some(bindings) = bindings {
                for (field_idx, binding) in bindings.iter().enumerate() {
                    let Pattern::Identifier(name, _) = binding else {
                        return Ok(None);
                    };
                    let field_handle = self.call_runtime(
                        self.runtime_abi.prime_enum_get,
                        self.runtime_abi.prime_enum_get_ty,
                        &mut [handle, unsafe {
                            LLVMConstInt(self.runtime_abi.usize_type, field_idx as u64, 0)
                        }],
                        "enum_get",
                    );
                    let reference = ReferenceValue {
                        cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                        mutable: false,
                        origin: None,
                        handle: Some(field_handle),
                    };
                    let bound = self.evaluated(Value::Reference(reference));
                    self.insert_var(name, bound, false)?;
                }
            }
            let arm_value = self.emit_expression(&arm.value)?;
            self.exit_scope()?;
            if let EvalOutcome::Value(val) = arm_value {
                result_value = Some(val);
            }
            unsafe { LLVMBuildBr(self.builder, merge_block) };
            current_block = else_block;
        }

        unsafe { LLVMPositionBuilderAtEnd(self.builder, merge_block) };
        Ok(Some(EvalOutcome::Value(
            result_value.unwrap_or_else(|| self.evaluated(Value::Unit)),
        )))
    }

    fn eval_try_operator(
        &mut self,
        value: EvaluatedValue,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let EvaluatedValue { value, runtime } = value;
        match value {
            Value::Enum(enum_value) => {
                if enum_value.enum_name != "Result" {
                    return Err("? operator expects Result value in build mode".into());
                }
                match enum_value.variant.as_str() {
                    "Ok" => {
                        let mut values = enum_value.values;
                        if values.is_empty() {
                            Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
                        } else if values.len() == 1 {
                            Ok(EvalOutcome::Value(self.evaluated(values.remove(0))))
                        } else {
                            Ok(EvalOutcome::Value(self.evaluated(Value::Tuple(values))))
                        }
                    }
                    "Err" => Ok(EvalOutcome::Flow(FlowSignal::Propagate(EvaluatedValue {
                        value: Value::Enum(enum_value),
                        runtime,
                    }))),
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
                    Value::Reference(reference) if reference.handle.is_none() => {
                        reference.cell.lock().unwrap().clone().into_value()
                    }
                    other => other.clone(),
                };
                self.insert_var(name, concrete.into(), mutable_bindings)?;
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
                    Value::Reference(reference) if reference.handle.is_none() => {
                        reference.cell.lock().unwrap().clone().into_value()
                    }
                    other => other.clone(),
                };
                if let Value::Enum(enum_value) = concrete {
                    if enum_value.variant != *variant {
                        return Ok(false);
                    }
                    let name_matches = if let Some(name) = enum_name {
                        if enum_value.enum_name == *name {
                            true
                        } else if let Some(binding) = self.get_var(name) {
                            matches!(binding.value(), Value::Enum(existing) if existing.enum_name == enum_value.enum_name)
                        } else {
                            false
                        }
                    } else {
                        true
                    };
                    if name_matches {
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
                    Value::Reference(reference) if reference.handle.is_none() => {
                        reference.cell.lock().unwrap().clone().into_value()
                    }
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
                    Value::Reference(reference) => {
                        reference.cell.lock().unwrap().clone().into_value()
                    }
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
                    Value::Reference(reference) => {
                        reference.cell.lock().unwrap().clone().into_value()
                    }
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
                    Value::Reference(reference) => {
                        reference.cell.lock().unwrap().clone().into_value()
                    }
                    other => other.clone(),
                };
                let elements = match concrete {
                    Value::Slice(slice) => slice.items.lock().unwrap().clone(),
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
            Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
            (Literal::Bool(expected, _), Value::Bool(actual)) => self
                .bool_constant_or_llvm(&actual, "Boolean pattern")
                .map(|val| *expected == val),
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
        // Embedded targets skip libc printf; use runtime print for string-only cases.
        if self.target.is_embedded() {
            if args.is_empty() {
                if let Ok((ptr, len)) = self.build_runtime_bytes(&fmt.to_string(), "rt_print") {
                    let mut call_args = [ptr, len];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_string_new,
                        self.runtime_abi.prime_string_new_ty,
                        &mut call_args,
                        "string_new",
                    );
                    let mut print_args = [handle];
                    self.call_runtime(
                        self.runtime_abi.prime_print,
                        self.runtime_abi.prime_print_ty,
                        &mut print_args,
                        "prime_print",
                    );
                }
            }
            return;
        }
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
        if let (Ok(a), Ok(b)) = (self.expect_int(lhs.clone()), self.expect_int(rhs.clone())) {
            return self.eval_int_binary(op, a, b);
        }
        if let (Ok(a), Ok(b)) = (
            self.expect_float(lhs.clone()),
            self.expect_float(rhs.clone()),
        ) {
            return self.eval_float_binary(op, a, b);
        }
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
            (Value::Str(a), Value::Str(b)) if matches!(op, BinaryOp::Eq | BinaryOp::NotEq) => {
                let cmp = (*a.text == *b.text) == matches!(op, BinaryOp::Eq);
                Ok(Value::Bool(self.const_bool_value(cmp)))
            }
            (other_l, other_r) => {
                if env::var_os("PRIME_DEBUG_BINOP").is_some() {
                    eprintln!(
                        "[prime-debug] binop {:?} lhs={} rhs={}",
                        op,
                        describe_value(&other_l),
                        describe_value(&other_r)
                    );
                }
                Err(format!(
                    "Operation `{op:?}` not supported in build mode for {} and {}",
                    describe_value(&other_l),
                    describe_value(&other_r)
                ))
            }
        }
    }

    fn eval_unary(&mut self, op: UnaryOp, value: Value) -> Result<Value, String> {
        match (op, Self::deref_if_reference(value)) {
            (UnaryOp::Neg, Value::Int(int_val)) => int_val
                .constant()
                .map(|c| Value::Int(self.const_int_value(-c)))
                .ok_or_else(|| {
                    "Operation `Neg` not supported in build mode for non-constant integers".into()
                }),
            (UnaryOp::Neg, Value::Float(float_val)) => float_val
                .constant()
                .map(|c| Value::Float(self.const_float_value(-c)))
                .ok_or_else(|| {
                    "Operation `Neg` not supported in build mode for non-constant floats".into()
                }),
            (UnaryOp::Not, Value::Bool(flag)) => {
                if let Some(constant) = flag.constant() {
                    Ok(Value::Bool(self.const_bool_value(!constant)))
                } else {
                    let llvm = unsafe {
                        LLVMBuildNot(
                            self.builder,
                            self.bool_llvm_value(&flag),
                            CString::new("bool_not").unwrap().as_ptr(),
                        )
                    };
                    Ok(Value::Bool(BoolValue::new(llvm, None)))
                }
            }
            (op_variant, other) => Err(format!(
                "Operation `{op_variant:?}` not supported in build mode for {}",
                describe_value(&other)
            )),
        }
    }

    fn eval_bool_binary(
        &mut self,
        op: BinaryOp,
        lhs: BoolValue,
        rhs: BoolValue,
    ) -> Result<Value, String> {
        if let Some((a, b)) = lhs.constant().zip(rhs.constant()) {
            let value = match op {
                BinaryOp::And => self.const_bool_value(a && b),
                BinaryOp::Or => self.const_bool_value(a || b),
                BinaryOp::Eq => self.const_bool_value(a == b),
                BinaryOp::NotEq => self.const_bool_value(a != b),
                _ => {
                    return Err(format!(
                        "Operation `{op:?}` not supported in build mode for booleans"
                    ));
                }
            };
            return Ok(Value::Bool(value));
        }
        let lhs_llvm = self.bool_llvm_value(&lhs);
        let rhs_llvm = self.bool_llvm_value(&rhs);
        let name = CString::new("bool_bin").unwrap();
        let llvm = match op {
            BinaryOp::And => unsafe { LLVMBuildAnd(self.builder, lhs_llvm, rhs_llvm, name.as_ptr()) },
            BinaryOp::Or => unsafe { LLVMBuildOr(self.builder, lhs_llvm, rhs_llvm, name.as_ptr()) },
            BinaryOp::Eq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    lhs_llvm,
                    rhs_llvm,
                    name.as_ptr(),
                )
            },
            BinaryOp::NotEq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntNE,
                    lhs_llvm,
                    rhs_llvm,
                    name.as_ptr(),
                )
            },
            _ => {
                return Err(format!(
                    "Operation `{op:?}` not supported in build mode for booleans"
                ))
            }
        };
        Ok(Value::Bool(BoolValue::new(llvm, None)))
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
                .map(|(a, b)| Value::Bool(self.const_bool_value(a < b))),
            BinaryOp::LtEq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(self.const_bool_value(a <= b))),
            BinaryOp::Gt => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(self.const_bool_value(a > b))),
            BinaryOp::GtEq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(self.const_bool_value(a >= b))),
            BinaryOp::Eq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(self.const_bool_value(a == b))),
            BinaryOp::NotEq => lhs
                .constant()
                .zip(rhs.constant())
                .map(|(a, b)| Value::Bool(self.const_bool_value(a != b))),
            _ => None,
        };
        if let Some(value) = result {
            return Ok(value);
        }
        let name = CString::new("int_bin").unwrap();
        let llvm = match op {
            BinaryOp::Add => unsafe {
                LLVMBuildAdd(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Sub => unsafe {
                LLVMBuildSub(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Mul => unsafe {
                LLVMBuildMul(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Div => unsafe {
                LLVMBuildSDiv(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Rem => unsafe {
                LLVMBuildSRem(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Lt => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::LtEq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::Gt => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::GtEq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::Eq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::NotEq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntNE,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            _ => {
                return Err(format!(
                    "Operation `{op:?}` not supported in build mode for integers (non-constant operands)"
                ));
            }
        };
        if matches!(
            op,
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq | BinaryOp::Eq
                | BinaryOp::NotEq
        ) {
            Ok(Value::Bool(BoolValue::new(llvm, None)))
        } else {
            Ok(Value::Int(IntValue::new(llvm, None)))
        }
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
            BinaryOp::Lt => values.map(|(a, b)| Value::Bool(self.const_bool_value(a < b))),
            BinaryOp::LtEq => values.map(|(a, b)| Value::Bool(self.const_bool_value(a <= b))),
            BinaryOp::Gt => values.map(|(a, b)| Value::Bool(self.const_bool_value(a > b))),
            BinaryOp::GtEq => values.map(|(a, b)| Value::Bool(self.const_bool_value(a >= b))),
            BinaryOp::Eq => values.map(|(a, b)| Value::Bool(self.const_bool_value(a == b))),
            BinaryOp::NotEq => values.map(|(a, b)| Value::Bool(self.const_bool_value(a != b))),
            _ => None,
        };
        if let Some(value) = result {
            return Ok(value);
        }
        let name = CString::new("float_bin").unwrap();
        let llvm = match op {
            BinaryOp::Add => unsafe {
                LLVMBuildFAdd(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Sub => unsafe {
                LLVMBuildFSub(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Mul => unsafe {
                LLVMBuildFMul(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Div => unsafe {
                LLVMBuildFDiv(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Rem => unsafe {
                LLVMBuildFRem(self.builder, lhs.llvm(), rhs.llvm(), name.as_ptr())
            },
            BinaryOp::Lt => unsafe {
                LLVMBuildFCmp(
                    self.builder,
                    llvm_sys::LLVMRealPredicate::LLVMRealOLT,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::LtEq => unsafe {
                LLVMBuildFCmp(
                    self.builder,
                    llvm_sys::LLVMRealPredicate::LLVMRealOLE,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::Gt => unsafe {
                LLVMBuildFCmp(
                    self.builder,
                    llvm_sys::LLVMRealPredicate::LLVMRealOGT,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::GtEq => unsafe {
                LLVMBuildFCmp(
                    self.builder,
                    llvm_sys::LLVMRealPredicate::LLVMRealOGE,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::Eq => unsafe {
                LLVMBuildFCmp(
                    self.builder,
                    llvm_sys::LLVMRealPredicate::LLVMRealOEQ,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            BinaryOp::NotEq => unsafe {
                LLVMBuildFCmp(
                    self.builder,
                    llvm_sys::LLVMRealPredicate::LLVMRealONE,
                    lhs.llvm(),
                    rhs.llvm(),
                    name.as_ptr(),
                )
            },
            _ => {
                return Err(format!(
                    "Operation `{op:?}` not supported in build mode for floats (non-constant operands)"
                ));
            }
        };
        if matches!(
            op,
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq | BinaryOp::Eq
                | BinaryOp::NotEq
        ) {
            Ok(Value::Bool(BoolValue::new(llvm, None)))
        } else {
            Ok(Value::Float(FloatValue::new(llvm, None)))
        }
    }

    fn int_to_float(&self, value: &IntValue) -> Result<FloatValue, String> {
        if let Some(constant) = value.constant() {
            Ok(self.const_float_value(constant as f64))
        } else {
            let converted = unsafe {
                LLVMBuildSIToFP(
                    self.builder,
                    value.llvm(),
                    self.f64_type,
                    CString::new("int_to_float").unwrap().as_ptr(),
                )
            };
            Ok(FloatValue::new(converted, None))
        }
    }

    fn deref_if_reference(value: Value) -> Value {
        match value {
            Value::Reference(reference) => {
                if reference.handle.is_some() {
                    Value::Reference(reference)
                } else {
                    reference.cell.lock().unwrap().clone().into_value()
                }
            }
            other => other,
        }
    }

    fn expect_int(&mut self, value: Value) -> Result<IntValue, String> {
        match value {
            Value::Int(v) => Ok(v),
            Value::Reference(reference) => {
                if let Some(handle) = reference.handle {
                    self.ensure_runtime_symbols();
                    let mut args = [handle];
                    let llvm = self.call_runtime(
                        self.runtime_abi.prime_value_as_int,
                        self.runtime_abi.prime_value_as_int_ty,
                        &mut args,
                        "value_as_int",
                    );
                    return Ok(IntValue::new(llvm, None));
                }
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_int(inner)
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone().into_value();
                self.expect_int(inner)
            }
            other => {
                if env::var_os("PRIME_DEBUG_EXPECT_INT").is_some() {
                    eprintln!(
                        "[prime-debug] expect_int saw {}",
                        describe_value(&other)
                    );
                }
                Err(format!(
                    "Expected integer value in build mode, got {}",
                    describe_value(&other)
                ))
            }
        }
    }

    fn int_constant_or_llvm(&self, value: &IntValue, ctx: &str) -> Result<i128, String> {
        if let Some(c) = value.constant() {
            return Ok(c);
        }
        unsafe {
            if LLVMIsAConstantInt(value.llvm()).is_null() {
                Err(format!("{ctx} must be constant in build mode"))
            } else {
                let width = LLVMGetIntTypeWidth(LLVMTypeOf(value.llvm()));
                let raw = LLVMConstIntGetZExtValue(value.llvm()) as u128;
                let signed = if width == 0 || width as usize >= u128::BITS as usize {
                    raw as i128
                } else {
                    let sign_bit = 1u128 << (width - 1);
                    let mask = (1u128 << width) - 1;
                    let masked = raw & mask;
                    if masked & sign_bit != 0 {
                        (masked | (!mask)) as i128
                    } else {
                        masked as i128
                    }
                };
                Ok(signed)
            }
        }
    }

    fn bool_constant_or_llvm(&self, value: &BoolValue, ctx: &str) -> Result<bool, String> {
        if let Some(c) = value.constant() {
            return Ok(c);
        }
        unsafe {
            if LLVMIsAConstantInt(value.llvm()).is_null() {
                Err(format!("{ctx} must be constant in build mode"))
            } else {
                Ok(LLVMConstIntGetZExtValue(value.llvm()) != 0)
            }
        }
    }

    fn bool_llvm_value(&mut self, value: &BoolValue) -> LLVMValueRef {
        let current = unsafe { LLVMTypeOf(value.llvm()) };
        if current == self.runtime_abi.bool_type {
            value.llvm()
        } else {
            unsafe {
                LLVMBuildIntCast(
                    self.builder,
                    value.llvm(),
                    self.runtime_abi.bool_type,
                    CString::new("bool_cast").unwrap().as_ptr(),
                )
            }
        }
    }

    fn emit_dynamic_range_for(
        &mut self,
        start: IntValue,
        end: IntValue,
        inclusive: bool,
        stmt: &ForStmt,
    ) -> Result<Option<FlowSignal>, String> {
        unsafe {
            let current_block = LLVMGetInsertBlock(self.builder);
            if current_block.is_null() {
                return Err("no insertion block for range loop".into());
            }
            let func = LLVMGetBasicBlockParent(current_block);
            if func.is_null() {
                return Err("range loop outside function".into());
            }
            let entry = LLVMGetFirstBasicBlock(func);
            if entry.is_null() {
                return Err("function entry block missing for range loop".into());
            }
            // Allocate loop variable in the entry block.
            let slot_builder = LLVMCreateBuilderInContext(self.context);
            let first_inst = LLVMGetFirstInstruction(entry);
            if first_inst.is_null() {
                LLVMPositionBuilderAtEnd(slot_builder, entry);
            } else {
                LLVMPositionBuilderBefore(slot_builder, first_inst);
            }
            let idx_ty = LLVMTypeOf(start.llvm());
            let loop_var = LLVMBuildAlloca(
                slot_builder,
                idx_ty,
                CString::new("for_range_idx").unwrap().as_ptr(),
            );
            LLVMBuildStore(slot_builder, start.llvm(), loop_var);
            LLVMDisposeBuilder(slot_builder);

            let one = LLVMConstInt(idx_ty, 1, 0);
            let limit = if inclusive {
                LLVMBuildAdd(
                    self.builder,
                    end.llvm(),
                    one,
                    CString::new("for_range_limit").unwrap().as_ptr(),
                )
            } else {
                end.llvm()
            };

            let cond_block = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("for_range_cond").unwrap().as_ptr(),
            );
            let body_block = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("for_range_body").unwrap().as_ptr(),
            );
            let after_block = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("for_range_after").unwrap().as_ptr(),
            );
            LLVMBuildBr(self.builder, cond_block);

            LLVMPositionBuilderAtEnd(self.builder, cond_block);
            let cur = LLVMBuildLoad2(
                self.builder,
                idx_ty,
                loop_var,
                CString::new("for_range_cur").unwrap().as_ptr(),
            );
            let cmp = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                cur,
                limit,
                CString::new("for_range_cmp").unwrap().as_ptr(),
            );
            LLVMBuildCondBr(self.builder, cmp, body_block, after_block);

            LLVMPositionBuilderAtEnd(self.builder, body_block);
            self.push_scope();
            self.insert_var(
                &stmt.binding,
                Value::Int(IntValue::new(cur, None)).into(),
                false,
            )?;
            let result = self.execute_block_contents(&stmt.body)?;
            self.exit_scope()?;

            match result {
                BlockEval::Value(_) | BlockEval::Flow(FlowSignal::Continue) => {
                    let next = LLVMBuildAdd(
                        self.builder,
                        cur,
                        one,
                        CString::new("for_range_next").unwrap().as_ptr(),
                    );
                    LLVMBuildStore(self.builder, next, loop_var);
                    LLVMBuildBr(self.builder, cond_block);
                }
                BlockEval::Flow(FlowSignal::Break) => {
                    LLVMBuildBr(self.builder, after_block);
                }
                BlockEval::Flow(flow @ FlowSignal::Return(_))
                | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                    return Ok(Some(flow));
                }
            }

            LLVMPositionBuilderAtEnd(self.builder, after_block);
            Ok(None)
        }
    }

    fn expect_int_value_from_expr(&mut self, expr: &Expr) -> Result<IntValue, String> {
        match self.emit_expression(expr)? {
            EvalOutcome::Value(value) => self.expect_int(value.into_value()),
            EvalOutcome::Flow(_) => Err("control flow not allowed here".into()),
        }
    }

    fn expect_float(&mut self, value: Value) -> Result<FloatValue, String> {
        match value {
            Value::Float(v) => Ok(v),
            Value::Reference(reference) => {
                if let Some(handle) = reference.handle {
                    self.ensure_runtime_symbols();
                    let mut args = [handle];
                    let llvm = self.call_runtime(
                        self.runtime_abi.prime_value_as_float,
                        self.runtime_abi.prime_value_as_float_ty,
                        &mut args,
                        "value_as_float",
                    );
                    return Ok(FloatValue::new(llvm, None));
                }
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_float(inner)
            }
            other => Err(format!(
                "Expected float value in build mode, got {}",
                describe_value(&other)
            )),
        }
    }

    fn int_to_usize(&mut self, value: &IntValue, ctx: &str) -> Result<LLVMValueRef, String> {
        unsafe {
            let current = LLVMTypeOf(value.llvm());
            if current == self.runtime_abi.usize_type {
                Ok(value.llvm())
            } else if !self.runtime_abi.usize_type.is_null() {
                Ok(LLVMBuildIntCast(
                    self.builder,
                    value.llvm(),
                    self.runtime_abi.usize_type,
                    CString::new(ctx).unwrap().as_ptr(),
                ))
            } else {
                Err("usize type unavailable while lowering index".into())
            }
        }
    }

    fn int_to_runtime_int(&mut self, value: &IntValue, ctx: &str) -> Result<LLVMValueRef, String> {
        unsafe {
            let target = self.runtime_abi.int_type;
            if target.is_null() {
                return Err("runtime int type unavailable".into());
            }
            let current = LLVMTypeOf(value.llvm());
            if current == target {
                Ok(value.llvm())
            } else {
                Ok(LLVMBuildIntCast(
                    self.builder,
                    value.llvm(),
                    target,
                    CString::new(ctx).unwrap().as_ptr(),
                ))
            }
        }
    }

    fn build_struct_literal(
        &mut self,
        name: &str,
        fields: &StructLiteralKind,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
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
                            map.insert(field.name.clone(), value.into_value());
                        }
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                Ok(EvalOutcome::Value(
                    Value::Struct(StructValue::new(name.to_string(), map)).into(),
                ))
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
                        EvalOutcome::Value(value) => value.into_value(),
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
                Ok(EvalOutcome::Value(
                    Value::Struct(StructValue::new(name.to_string(), map)).into(),
                ))
            }
        }
    }

    fn build_enum_literal(
        &mut self,
        enum_name: Option<&str>,
        variant: &str,
        values: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
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
                EvalOutcome::Value(value) => evaluated.push(value.into_value()),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        Ok(EvalOutcome::Value(self.evaluated(Value::Enum(EnumValue {
            enum_name: info.enum_name.clone(),
            variant: variant.to_string(),
            values: evaluated,
            variant_index: info.variant_index,
        }))))
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
            variant_index: info.variant_index,
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
    ) -> Result<Vec<EvaluatedValue>, String> {
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
        let receiver_args: Vec<Value> = evaluated_args.iter().map(|v| v.value().clone()).collect();
        let receiver_candidate = self.receiver_name_from_values(&receiver_args);
        let func_entry = self.resolve_function(
            name,
            receiver_candidate.as_deref(),
            type_args,
            &receiver_args,
        )?;
        self.run_function_with_values(func_entry, evaluated_args)
    }

    fn run_function_with_values(
        &mut self,
        func_entry: FunctionEntry,
        evaluated_args: Vec<EvaluatedValue>,
    ) -> Result<Vec<EvaluatedValue>, String> {
        let func = func_entry.def.clone();
        if func.params.len() != evaluated_args.len() {
            return Err(format!(
                "Function `{}` expects {} arguments, got {}",
                func.name,
                func.params.len(),
                evaluated_args.len()
            ));
        }

        self.ensure_item_visible(&func_entry.module, func.visibility, &func.name, "function")?;
        let value_args: Vec<Value> = evaluated_args.iter().map(|v| v.value().clone()).collect();
        self.ensure_interface_arguments(&func.params, &value_args)?;
        self.module_stack.push(func_entry.module.clone());
        let return_types: Vec<TypeExpr> = func.returns.iter().map(|ann| ann.ty.clone()).collect();
        self.push_return_types(&return_types);
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
                    } else {
                        match value.into_value() {
                            Value::Tuple(values) => {
                                Ok(values.into_iter().map(|v| self.evaluated(v)).collect())
                            }
                            other => Ok(vec![self.evaluated(other)]),
                        }
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
        self.pop_return_types();
        self.module_stack.pop();
        result
    }

    fn call_function_with_values(
        &mut self,
        name: &str,
        receiver_hint: Option<&str>,
        type_args: &[TypeExpr],
        args: Vec<EvaluatedValue>,
    ) -> Result<Vec<EvaluatedValue>, String> {
        let value_args: Vec<Value> = args.iter().map(|v| v.value().clone()).collect();
        let func_entry = self.resolve_function(name, receiver_hint, type_args, &value_args)?;
        self.run_function_with_values(func_entry, args)
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
                "Duplicate function `{}` for receiver `{:?}` (possible multiple definitions or macro expansion)",
                func.name, receiver
            ));
        }
        if func.name == "drop" {
            if let Some(ref recv) = receiver {
                if let Some(existing) = self.drop_impls.insert(recv.clone(), key.clone()) {
                    return Err(format!(
                        "`{}` already has a drop implementation (previously {:?})",
                        recv, existing
                    ));
                }
            }
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
        let is_drop_like = block.inherent || block.interface == "Drop";
        let target_is_struct = self.structs.contains_key(&block.target);
        let target_is_enum = self
            .enum_variants
            .values()
            .any(|info| info.enum_name == block.target);
        if !target_is_struct && !(is_drop_like && target_is_enum) {
            return Err(format!("Unknown target type `{}`", block.target));
        }
        if is_drop_like {
            for method in &block.methods {
                let mut method_def = method.clone();
                substitute_self_in_function(&mut method_def, &block.target);
                self.register_function(&method_def, module)?;
            }
            return Ok(());
        }
        if !self.interfaces.contains_key(&block.interface) {
            return Err(format!("Unknown interface `{}`", block.interface));
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
                if let Some(name) = first
                    .ty
                    .as_ref()
                    .and_then(|ty| type_name_from_type_expr(&ty.ty))
                {
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
            if let Some(ty) = param.ty.as_mut() {
                *ty = ty.substitute(&map);
            }
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
            if let Some(ty) = param.ty.as_ref() {
                if let Some((interface, type_args)) = self.interface_name_from_type(&ty.ty) {
                    self.ensure_interface_compat(&interface, &type_args, value)?;
                }
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
                let inner = reference.cell.lock().unwrap();
                self.value_struct_name(inner.value())
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap();
                self.value_struct_name(inner.value())
            }
            Value::Boxed(_) => None,
            _ => None,
        }
    }

    fn drop_type_for_value(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Enum(enum_value) => Some(enum_value.enum_name.clone()),
            _ => None,
        }
    }

    fn queue_drop(&mut self, name: &str, type_name: String) {
        if self.drop_impls.contains_key(&type_name) {
            if let Some(stack) = self.cleanup_stack.last_mut() {
                stack.push(CleanupAction::Drop(DropRecord {
                    binding: name.to_string(),
                    type_name,
                }));
            }
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
        match value {
            Value::Reference(reference) => {
                if reference.mutable {
                    if let Some(origin) = &reference.origin {
                        self.begin_mut_borrow_in_scope(origin, scope_index)?;
                    }
                }
            }
            Value::Pointer(pointer) => {
                if pointer.mutable {
                    if let Some(origin) = &pointer.origin {
                        self.begin_mut_borrow_in_scope(origin, scope_index)?;
                    }
                }
            }
            Value::Boxed(_boxed) => {
                // Box borrows are represented by references/pointers to their inner cell; do not
                // treat the box binding itself as a borrow origin to avoid double-counting moves.
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.track_reference_borrow_in_scope(named.value(), scope_index)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn release_reference_borrow(&mut self, value: &Value) {
        match value {
            Value::Reference(reference) => {
                if reference.mutable {
                    if let Some(origin) = &reference.origin {
                        self.end_mut_borrow(origin);
                    }
                }
            }
            Value::Pointer(pointer) => {
                if pointer.mutable {
                    if let Some(origin) = &pointer.origin {
                        self.end_mut_borrow(origin);
                    }
                }
            }
            Value::Boxed(_boxed) => {
                // Box lifetimes are managed via inner references; no explicit borrow release needed.
            }
            Value::Sender(sender) => {
                if let Some(origin) = &sender.origin {
                    self.end_mut_borrow(origin);
                }
            }
            Value::Receiver(receiver) => {
                if let Some(origin) = &receiver.origin {
                    self.end_mut_borrow(origin);
                }
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.release_reference_borrow(named.value());
                    }
                }
            }
            _ => {}
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

    fn reference_handle(&mut self, reference: &ReferenceValue) -> Result<LLVMValueRef, String> {
        if let Some(handle) = reference.handle {
            let mut args = [handle];
            return Ok(self.call_runtime(
                self.runtime_abi.prime_value_retain,
                self.runtime_abi.prime_value_retain_ty,
                &mut args,
                "reference_retain",
            ));
        }
        let inner = reference.cell.lock().unwrap().clone().into_value();
        let target = self.build_runtime_handle(inner)?;
        let mut_flag =
            unsafe { LLVMConstInt(self.runtime_abi.bool_type, reference.mutable as u64, 0) };
        Ok(self.call_runtime(
            self.runtime_abi.prime_reference_new,
            self.runtime_abi.prime_reference_new_ty,
            &mut [target, mut_flag],
            "reference_new",
        ))
    }

    fn write_reference(
        &mut self,
        reference: &ReferenceValue,
        value: EvaluatedValue,
    ) -> Result<(), String> {
        if !reference.mutable {
            return Err("Cannot assign through immutable reference".into());
        }
        if let Some(handle) = reference.handle {
            let new_handle = self.build_runtime_handle(value.into_value())?;
            let mut args = [handle, new_handle];
            self.call_runtime(
                self.runtime_abi.prime_reference_write,
                self.runtime_abi.prime_reference_write_ty,
                &mut args,
                "reference_write",
            );
            Ok(())
        } else {
            *reference.cell.lock().unwrap() = value;
            Ok(())
        }
    }

    fn expect_box_value(&mut self, value: Value, ctx: &str) -> Result<BoxValue, String> {
        match value {
            Value::Boxed(b) => {
                if b.handle.is_some() {
                    return Err(format!(
                        "{ctx} cannot inspect captured box handle in build mode; operate on it at runtime"
                    ));
                }
                Ok(b)
            }
            Value::Reference(reference) => {
                if let Some(reference_handle) = reference.handle {
                    let mut call_args = [reference_handle];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut call_args,
                        "box_ref_read",
                    );
                    Ok(BoxValue::with_handle(handle))
                } else {
                    let inner = reference.cell.lock().unwrap().clone().into_value();
                    self.expect_box_value(inner, ctx)
                }
            }
            _ => Err(format!("{ctx} expects Box value")),
        }
    }

    fn expect_slice_value(&mut self, value: Value, ctx: &str) -> Result<SliceValue, String> {
        match value {
            Value::Slice(slice) => Ok(slice),
            Value::Reference(reference) => {
                if let Some(reference_handle) = reference.handle {
                    let mut call_args = [reference_handle];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut call_args,
                        "slice_ref_read",
                    );
                    Ok(SliceValue::with_handle(handle))
                } else {
                    let inner = reference.cell.lock().unwrap().clone().into_value();
                    self.expect_slice_value(inner, ctx)
                }
            }
            _ => Err(format!("{ctx} expects slice value")),
        }
    }

    fn expect_map_value(&mut self, value: Value, ctx: &str) -> Result<MapValue, String> {
        match value {
            Value::Map(map) => Ok(map),
            Value::Reference(reference) => {
                if let Some(reference_handle) = reference.handle {
                    let mut call_args = [reference_handle];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut call_args,
                        "map_ref_read",
                    );
                    Ok(MapValue::with_handle(handle))
                } else {
                    let inner = reference.cell.lock().unwrap().clone().into_value();
                    self.expect_map_value(inner, ctx)
                }
            }
            _ => Err(format!("{ctx} expects map value")),
        }
    }

    fn expect_sender(&mut self, value: Value, ctx: &str) -> Result<ChannelSender, String> {
        match value {
            Value::Sender(tx) => Ok(tx),
            Value::Reference(reference) => {
                if let Some(reference_handle) = reference.handle {
                    let mut call_args = [reference_handle];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut call_args,
                        "sender_ref_read",
                    );
                    Ok(ChannelSender::with_handle(handle))
                } else {
                    let inner = reference.cell.lock().unwrap().clone().into_value();
                    self.expect_sender(inner, ctx)
                }
            }
            _ => Err(format!("{ctx} expects Sender value")),
        }
    }

    fn expect_receiver(&mut self, value: Value, ctx: &str) -> Result<ChannelReceiver, String> {
        match value {
            Value::Receiver(rx) => Ok(rx),
            Value::Reference(reference) => {
                if let Some(reference_handle) = reference.handle {
                    let mut call_args = [reference_handle];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut call_args,
                        "receiver_ref_read",
                    );
                    Ok(ChannelReceiver::with_handle(handle))
                } else {
                    let inner = reference.cell.lock().unwrap().clone().into_value();
                    self.expect_receiver(inner, ctx)
                }
            }
            _ => Err(format!("{ctx} expects Receiver value")),
        }
    }

    fn expect_string_value(&self, value: Value, ctx: &str) -> Result<String, String> {
        match value {
            Value::Str(s) => Ok((*s.text).clone()),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_string_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects string value")),
        }
    }

    fn make_string_value(&self, text: &str) -> Result<Value, String> {
        let c_value = CString::new(text.as_bytes())
            .map_err(|_| "string value cannot contain interior null bytes".to_string())?;
        let name = CString::new("map_key").unwrap();
        let text_rc = Arc::new(text.to_string());
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

    fn builtin_box_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_get expects 1 argument".into());
        }
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_get")?;
        Ok(boxed.cell.lock().unwrap().clone())
    }

    fn builtin_box_set(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("box_set expects 2 arguments".into());
        }
        let value = args.pop().unwrap();
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_set")?;
        boxed.replace(value);
        Ok(Value::Unit)
    }

    fn builtin_box_take(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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
        if let Some(handle) = slice.handle {
            let value_handle = self.build_runtime_handle(value)?;
            let mut call_args = [handle, value_handle];
            self.call_runtime(
                self.runtime_abi.prime_slice_push_handle,
                self.runtime_abi.prime_slice_push_handle_ty,
                &mut call_args,
                "slice_push_handle",
            );
        } else {
            slice.push(value);
        }
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
        let mut slice = self.expect_slice_value(args.pop().unwrap(), "slice_get")?;
        let int_value = self.expect_int(index_value)?;
        let idx_const = int_value.constant();
        let mut handle = slice.handle;
        if handle.is_none() && idx_const.is_none() {
            if let Ok(runtime) = self.build_runtime_handle(Value::Slice(slice.clone())) {
                slice.handle = Some(runtime);
                handle = Some(runtime);
            }
        }
        if let Some(handle) = handle {
            if let Some(idx) = idx_const {
                if idx < 0 {
                    return self.instantiate_enum_variant("None", Vec::new());
                }
            }
            let idx_arg = if let Some(idx) = idx_const {
                unsafe { LLVMConstInt(self.runtime_abi.usize_type, idx as u64, 0) }
            } else {
                self.int_to_usize(&int_value, "slice_get_index")?
            };
            let slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("slice_get_out").unwrap().as_ptr(),
                )
            };
            let mut call_args = [handle, idx_arg, slot];
            self.call_runtime(
                self.runtime_abi.prime_slice_get_handle,
                self.runtime_abi.prime_slice_get_handle_ty,
                &mut call_args,
                "slice_get_handle",
            );
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    CString::new("slice_get_loaded").unwrap().as_ptr(),
                )
            };
            let reference = ReferenceValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: false,
                origin: None,
                handle: Some(loaded),
            };
            return self
                .instantiate_enum_variant("Some", vec![Value::Reference(reference.clone())])
                .or_else(|_| Ok(Value::Reference(reference)));
        }
        let idx = idx_const
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

    fn builtin_slice_remove(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("remove expects receiver and index".into());
        }
        let index_value = args.pop().unwrap();
        let mut slice = self.expect_slice_value(args.pop().unwrap(), "remove")?;
        let int_value = self.expect_int(index_value)?;
        let idx_const = int_value.constant();
        let mut handle = slice.handle;
        if handle.is_none() && idx_const.is_none() {
            if let Ok(runtime) = self.build_runtime_handle(Value::Slice(slice.clone())) {
                slice.handle = Some(runtime);
                handle = Some(runtime);
            }
        }
        if let Some(handle) = handle {
            if let Some(idx) = idx_const {
                if idx < 0 {
                    return self.instantiate_enum_variant("None", Vec::new());
                }
            }
            let idx_arg = if let Some(idx) = idx_const {
                unsafe { LLVMConstInt(self.runtime_abi.usize_type, idx as u64, 0) }
            } else {
                self.int_to_usize(&int_value, "slice_remove_index")?
            };
            let slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("slice_remove_out").unwrap().as_ptr(),
                )
            };
            let mut call_args = [handle, idx_arg, slot];
            self.call_runtime(
                self.runtime_abi.prime_slice_remove_handle,
                self.runtime_abi.prime_slice_remove_handle_ty,
                &mut call_args,
                "slice_remove_handle",
            );
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    CString::new("slice_remove_loaded").unwrap().as_ptr(),
                )
            };
            let reference = ReferenceValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: false,
                origin: None,
                handle: Some(loaded),
            };
            return self.instantiate_enum_variant("Some", vec![Value::Reference(reference)]);
        }
        let idx =
            idx_const.ok_or_else(|| "remove index must be constant in build mode".to_string())?;
        if idx < 0 {
            return self.instantiate_enum_variant("None", Vec::new());
        }
        let removed = slice.remove(idx as usize);
        match removed {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_remove(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("remove expects receiver and argument".into());
        }
        let receiver = args.remove(0);
        match receiver {
            Value::Slice(slice) => {
                let mut forwarded = Vec::with_capacity(2);
                forwarded.push(Value::Slice(slice));
                forwarded.push(args.remove(0));
                self.builtin_slice_remove(forwarded)
            }
            Value::Map(map) => {
                let mut forwarded = Vec::with_capacity(2);
                forwarded.push(Value::Map(map));
                forwarded.push(args.remove(0));
                self.builtin_map_remove(forwarded)
            }
            Value::Reference(reference) => {
                let mut forwarded = Vec::new();
                forwarded.push(reference.cell.lock().unwrap().clone().into_value());
                forwarded.extend(args);
                self.builtin_remove(forwarded)
            }
            _ => Err("remove expects slice or map".into()),
        }
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
        if let Some(handle) = map.handle {
            let (key_ptr, key_len) = self.build_runtime_bytes(&key, "map_insert_key")?;
            let value_handle = self.build_runtime_handle(value)?;
            let mut call_args = [handle, key_ptr, key_len, value_handle];
            self.call_runtime(
                self.runtime_abi.prime_map_insert_handle,
                self.runtime_abi.prime_map_insert_handle_ty,
                &mut call_args,
                "map_insert_handle",
            );
        } else {
            map.insert(key, value);
        }
        Ok(Value::Unit)
    }

    fn builtin_map_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_get");
        if args.len() != 2 {
            return Err("map_get expects 2 arguments".into());
        }
        let key = self.expect_string_value(args.pop().unwrap(), "map_get")?;
        let map = self.expect_map_value(args.pop().unwrap(), "map_get")?;
        if let Some(handle) = map.handle {
            let (key_ptr, key_len) = self.build_runtime_bytes(&key, "map_get_key")?;
            let slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("map_get_out").unwrap().as_ptr(),
                )
            };
            let mut call_args = [handle, key_ptr, key_len, slot];
            self.call_runtime(
                self.runtime_abi.prime_map_get_handle,
                self.runtime_abi.prime_map_get_handle_ty,
                &mut call_args,
                "map_get_handle",
            );
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    CString::new("map_get_loaded").unwrap().as_ptr(),
                )
            };
            let reference = ReferenceValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: false,
                origin: None,
                handle: Some(loaded),
            };
            return self.instantiate_enum_variant("Some", vec![Value::Reference(reference)]);
        }
        match map.get(&key) {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_map_keys(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("map_keys expects 1 argument".into());
        }
        let map = args.pop().unwrap();
        let entries = self.collect_iterable_values(map)?;
        let mut keys = Vec::new();
        for entry in entries {
            match entry.into_value() {
                Value::Tuple(mut pair) if pair.len() == 2 => {
                    keys.push(pair.remove(0));
                }
                other => {
                    return Err(format!(
                        "map_keys expects (key, value) tuples, found {}",
                        self.describe_value(&other)
                    ));
                }
            }
        }
        Ok(Value::Slice(SliceValue::from_vec(keys)))
    }

    fn builtin_map_values(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("map_values expects 1 argument".into());
        }
        let map = args.pop().unwrap();
        let entries = self.collect_iterable_values(map)?;
        let mut values = Vec::new();
        for entry in entries {
            match entry.into_value() {
                Value::Tuple(pair) if pair.len() == 2 => {
                    values.push(pair[1].clone());
                }
                other => {
                    return Err(format!(
                        "map_values expects (key, value) tuples, found {}",
                        self.describe_value(&other)
                    ));
                }
            }
        }
        Ok(Value::Slice(SliceValue::from_vec(values)))
    }

    fn builtin_map_remove(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("remove expects receiver and key".into());
        }
        let key = self.expect_string_value(args.pop().unwrap(), "remove")?;
        let map = self.expect_map_value(args.pop().unwrap(), "remove")?;
        if let Some(handle) = map.handle {
            let (key_ptr, key_len) = self.build_runtime_bytes(&key, "map_remove_key")?;
            let slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("map_remove_out").unwrap().as_ptr(),
                )
            };
            let mut call_args = [handle, key_ptr, key_len, slot];
            self.call_runtime(
                self.runtime_abi.prime_map_remove_handle,
                self.runtime_abi.prime_map_remove_handle_ty,
                &mut call_args,
                "map_remove_handle",
            );
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    CString::new("map_remove_loaded").unwrap().as_ptr(),
                )
            };
            let reference = ReferenceValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: false,
                origin: None,
                handle: Some(loaded),
            };
            return self.instantiate_enum_variant("Some", vec![Value::Reference(reference)]);
        }
        let removed = map.remove(&key);
        match removed {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_iter(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("iter expects 1 argument".into());
        }
        let target = args.pop().unwrap();
        match target {
            Value::Slice(slice) => {
                let mut items = Vec::new();
                for idx in 0..slice.len() {
                    if let Some(value) = slice.get(idx) {
                        items.push(value);
                    }
                }
                Ok(Value::Iterator(IteratorValue::from_items(items)))
            }
            Value::Map(map) => {
                let entries = self.collect_iterable_values(Value::Map(map))?;
                let mut items = Vec::new();
                for entry in entries {
                    items.push(entry.into_value());
                }
                Ok(Value::Iterator(IteratorValue::from_items(items)))
            }
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.builtin_iter(vec![inner])
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone().into_value();
                self.builtin_iter(vec![inner])
            }
            Value::Iterator(iter) => {
                let start = *iter.index.lock().unwrap();
                let guard = iter.items.lock().unwrap();
                Ok(Value::Iterator(IteratorValue::from_items(
                    guard.iter().skip(start).cloned().collect(),
                )))
            }
            other => Err(format!(
                "iter not supported for {}",
                self.describe_value(&other)
            )),
        }
    }

    fn builtin_iter_next(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("next expects 1 argument".into());
        }
        let iter = match args.pop().unwrap() {
            Value::Iterator(iter) => iter,
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                if let Value::Iterator(iter) = inner {
                    iter
                } else {
                    return Err("next expects iterator".into());
                }
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone().into_value();
                if let Value::Iterator(iter) = inner {
                    iter
                } else {
                    return Err("next expects iterator".into());
                }
            }
            other => {
                return Err(format!(
                    "next expects iterator, found {}",
                    self.describe_value(&other)
                ));
            }
        };
        match iter.next() {
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
            Value::Slice(slice) => {
                if let Some(handle) = slice.handle {
                    let mut call_args = [handle];
                    let len = self.call_runtime(
                        self.runtime_abi.prime_slice_len_handle,
                        self.runtime_abi.prime_slice_len_handle_ty,
                        &mut call_args,
                        "slice_len_handle",
                    );
                    let len_i32 = unsafe {
                        LLVMBuildIntCast(
                            self.builder,
                            len,
                            self.i32_type,
                            CString::new("slice_len_cast").unwrap().as_ptr(),
                        )
                    };
                    Ok(Value::Int(IntValue::new(len_i32, None)))
                } else {
                    Ok(Value::Int(self.const_int_value(slice.len() as i128)))
                }
            }
            Value::Map(map) => {
                if let Some(handle) = map.handle {
                    let mut call_args = [handle];
                    let len = self.call_runtime(
                        self.runtime_abi.prime_map_len_handle,
                        self.runtime_abi.prime_map_len_handle_ty,
                        &mut call_args,
                        "map_len_handle",
                    );
                    let len_i32 = unsafe {
                        LLVMBuildIntCast(
                            self.builder,
                            len,
                            self.i32_type,
                            CString::new("map_len_cast").unwrap().as_ptr(),
                        )
                    };
                    Ok(Value::Int(IntValue::new(len_i32, None)))
                } else {
                    Ok(Value::Int(self.const_int_value(map.len() as i128)))
                }
            }
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
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
                let idx_const = int_value.constant();
                let mut slice = slice;
                let mut handle = slice.handle;
                if handle.is_none() && idx_const.is_none() {
                    if let Ok(runtime) = self.build_runtime_handle(Value::Slice(slice.clone())) {
                        slice.handle = Some(runtime);
                        handle = Some(runtime);
                    }
                }
                if let Some(handle) = handle {
                    if let Some(idx) = idx_const {
                        if idx < 0 {
                            return self.instantiate_enum_variant("None", Vec::new());
                        }
                    }
                    let idx_arg = if let Some(idx) = idx_const {
                        unsafe { LLVMConstInt(self.runtime_abi.usize_type, idx as u64, 0) }
                    } else {
                        self.int_to_usize(&int_value, "get_index")?
                    };
                    let slot = unsafe {
                        LLVMBuildAlloca(
                            self.builder,
                            self.runtime_abi.handle_type,
                            CString::new("slice_get_out").unwrap().as_ptr(),
                        )
                    };
                    let mut call_args = [handle, idx_arg, slot];
                    self.call_runtime(
                        self.runtime_abi.prime_slice_get_handle,
                        self.runtime_abi.prime_slice_get_handle_ty,
                        &mut call_args,
                        "slice_get_handle",
                    );
                    let loaded = unsafe {
                        LLVMBuildLoad2(
                            self.builder,
                            self.runtime_abi.handle_type,
                            slot,
                            CString::new("slice_get_loaded").unwrap().as_ptr(),
                        )
                    };
                    let reference = ReferenceValue {
                        cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                        mutable: false,
                        origin: None,
                        handle: Some(loaded),
                    };
                    return self
                        .instantiate_enum_variant("Some", vec![Value::Reference(reference.clone())])
                        .or_else(|_| Ok(Value::Reference(reference)));
                }
                let idx = idx_const
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
                if let Some(handle) = map.handle {
                    let (key_ptr, key_len) = self.build_runtime_bytes(&key, "map_get_key")?;
                    let slot = unsafe {
                        LLVMBuildAlloca(
                            self.builder,
                            self.runtime_abi.handle_type,
                            CString::new("map_get_out").unwrap().as_ptr(),
                        )
                    };
                    let mut call_args = [handle, key_ptr, key_len, slot];
                    self.call_runtime(
                        self.runtime_abi.prime_map_get_handle,
                        self.runtime_abi.prime_map_get_handle_ty,
                        &mut call_args,
                        "map_get_handle",
                    );
                    let loaded = unsafe {
                        LLVMBuildLoad2(
                            self.builder,
                            self.runtime_abi.handle_type,
                            slot,
                            CString::new("map_get_loaded").unwrap().as_ptr(),
                        )
                    };
                    let reference = ReferenceValue {
                        cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                        mutable: false,
                        origin: None,
                        handle: Some(loaded),
                    };
                    return self
                        .instantiate_enum_variant("Some", vec![Value::Reference(reference.clone())])
                        .or_else(|_| Ok(Value::Reference(reference)));
                }
                match map.get(&key) {
                    Some(value) => self.instantiate_enum_variant("Some", vec![value]),
                    None => self.instantiate_enum_variant("None", Vec::new()),
                }
            }
            Value::Reference(reference) => {
                let mut forwarded = Vec::new();
                forwarded.push(reference.cell.lock().unwrap().clone().into_value());
                forwarded.extend(args);
                self.builtin_get(forwarded)
            }
            _ => Err("get expects slice or map".into()),
        }
    }

    fn assign_index_value(
        &mut self,
        base: Value,
        index: Value,
        value: Value,
    ) -> Result<(), String> {
        match base {
            Value::Slice(slice) => {
                let int_value = self.expect_int(index)?;
                let idx = int_value
                    .constant()
                    .ok_or_else(|| "index must be constant in build mode".to_string())?;
                if idx < 0 {
                    return Err("slice index cannot be negative".into());
                }
                if slice.set(idx as usize, value) {
                    Ok(())
                } else {
                    Err(format!("slice index {} out of bounds", idx))
                }
            }
            Value::Map(map) => {
                let key = self.expect_string_value(index, "index assignment")?;
                map.insert(key, value);
                Ok(())
            }
            Value::Reference(reference) => {
                if !reference.mutable {
                    return Err("Cannot assign through immutable reference".into());
                }
                if reference.handle.is_some() {
                    let mut call_args = [reference.handle.unwrap()];
                    let handle = self.call_runtime(
                        self.runtime_abi.prime_reference_read,
                        self.runtime_abi.prime_reference_read_ty,
                        &mut call_args,
                        "index_ref_read",
                    );
                    let forwarded = match &index {
                        Value::Int(_) => Value::Slice(SliceValue::with_handle(handle)),
                        Value::Str(_) => Value::Map(MapValue::with_handle(handle)),
                        _ => {
                            return Err("reference index assignment expects slice or map".into());
                        }
                    };
                    self.assign_index_value(forwarded, index, value)
                } else {
                    let inner = reference.cell.lock().unwrap().clone().into_value();
                    self.assign_index_value(inner, index, value)
                }
            }
            Value::Pointer(pointer) => {
                if !pointer.mutable {
                    return Err("Cannot assign through immutable reference".into());
                }
                let inner = pointer.cell.lock().unwrap().clone().into_value();
                self.assign_index_value(inner, index, value)
            }
            other => Err(format!("{} cannot be indexed", describe_value(&other))),
        }
    }

    fn builtin_push(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("push expects receiver and value".into());
        }
        let value = args.pop().unwrap();
        let slice = self.expect_slice_value(args.pop().unwrap(), "push")?;
        if let Some(handle) = slice.handle {
            let value_handle = self.build_runtime_handle(value)?;
            let mut call_args = [handle, value_handle];
            self.call_runtime(
                self.runtime_abi.prime_slice_push_handle,
                self.runtime_abi.prime_slice_push_handle_ty,
                &mut call_args,
                "slice_push_handle",
            );
        } else {
            slice.push(value);
        }
        Ok(Value::Unit)
    }

    fn builtin_insert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 3 {
            return Err("insert expects receiver, key, and value".into());
        }
        let value = args.pop().unwrap();
        let key = self.expect_string_value(args.pop().unwrap(), "insert")?;
        let map = self.expect_map_value(args.pop().unwrap(), "insert")?;
        if let Some(handle) = map.handle {
            let (key_ptr, key_len) = self.build_runtime_bytes(&key, "insert_key")?;
            let value_handle = self.build_runtime_handle(value)?;
            let mut call_args = [handle, key_ptr, key_len, value_handle];
            self.call_runtime(
                self.runtime_abi.prime_map_insert_handle,
                self.runtime_abi.prime_map_insert_handle_ty,
                &mut call_args,
                "map_insert_handle",
            );
        } else {
            map.insert(key, value);
        }
        Ok(Value::Unit)
    }

    fn builtin_fs_exists(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("fs_exists expects 1 argument".into());
        }
        let path = self.expect_string_value(args.pop().unwrap(), "fs_exists")?;
        Ok(Value::Bool(
            self.const_bool_value(platform().fs_exists(&path)),
        ))
    }

    fn builtin_fs_read(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("fs_read expects 1 argument".into());
        }
        let path = self.expect_string_value(args.pop().unwrap(), "fs_read")?;
        match platform().fs_read(&path) {
            Ok(contents) => {
                let value = self.build_string_constant(contents)?;
                self.instantiate_enum_variant("Ok", vec![value])
            }
            Err(msg) => {
                let err = self.build_string_constant(msg)?;
                self.instantiate_enum_variant("Err", vec![err])
            }
        }
    }

    fn builtin_fs_write(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("fs_write expects 2 arguments".into());
        }
        let contents = self.expect_string_value(args.pop().unwrap(), "fs_write")?;
        let path = self.expect_string_value(args.pop().unwrap(), "fs_write")?;
        match platform().fs_write(&path, &contents) {
            Ok(()) => self.instantiate_enum_variant("Ok", vec![Value::Unit]),
            Err(msg) => {
                let err = self.build_string_constant(msg)?;
                self.instantiate_enum_variant("Err", vec![err])
            }
        }
    }

    fn builtin_now_ms(&mut self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err("now_ms expects 0 arguments".into());
        }
        if self.target.is_esp32c3() {
            let mut call_args: [LLVMValueRef; 0] = [];
            let now = self.call_runtime(
                self.runtime_abi.prime_now_ms,
                self.runtime_abi.prime_now_ms_ty,
                &mut call_args,
                "now_ms",
            );
            return Ok(Value::Int(IntValue::new(now, None)));
        }
        let millis = self.build_clock_ms;
        self.build_clock_ms = self.build_clock_ms.saturating_add(1);
        Ok(Value::Int(self.const_int_value(millis)))
    }

    fn builtin_sleep_ms(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if self.target.is_esp32c3() {
            return self.builtin_delay_ms(args);
        }
        if args.len() != 1 {
            return Err("sleep_ms expects 1 argument".into());
        }
        let millis = self.expect_int(args.pop().unwrap())?;
        let constant = millis
            .constant()
            .ok_or_else(|| "sleep_ms expects constant integer in build mode".to_string())?;
        platform().sleep_ms(constant);
        Ok(Value::Unit)
    }

    fn builtin_sleep_task(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("sleep_task expects 1 argument".into());
        }
        let millis = self.expect_int(args.pop().unwrap())?;
        if self.runtime_handles_enabled() {
            self.ensure_runtime_symbols();
            let arg = if let Some(constant) = millis.constant() {
                unsafe { LLVMConstInt(self.runtime_abi.int_type, constant as u64, 1) }
            } else {
                self.int_to_runtime_int(&millis, "sleep_task_millis")?
            };
            let mut call_args = [arg];
            let handle = self.call_runtime(
                self.runtime_abi.prime_sleep_task,
                self.runtime_abi.prime_sleep_task_ty,
                &mut call_args,
                "sleep_task",
            );
            let task = TaskValue::with_handle(handle, TaskResultKind::RuntimeUnit);
            return Ok(Value::Task(Box::new(task)));
        }
        if let Some(duration) = millis.constant() {
            if !self.target.is_esp32c3() {
                platform().sleep_ms(duration);
            }
        }
        let task = TaskValue::ready(self.evaluated(Value::Unit));
        Ok(Value::Task(Box::new(task)))
    }

    fn ensure_embedded_target(&self, name: &str) -> Result<(), String> {
        if self.target.is_embedded() {
            Ok(())
        } else {
            Err(format!(
                "`{}` is embedded-only; compile with --target {}",
                name,
                embedded_target_hint()
            ))
        }
    }

    fn builtin_delay_ms(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("delay_ms expects 1 argument".into());
        }
        self.ensure_embedded_target("delay_ms")?;
        let millis = self.expect_int(args.pop().unwrap())?;
        let millis_cast = unsafe {
            LLVMBuildIntCast(
                self.builder,
                millis.llvm(),
                self.i32_type,
                CString::new("delay_ms_arg").unwrap().as_ptr(),
            )
        };
        let mut call_args = [millis_cast];
        self.call_runtime(
            self.runtime_abi.prime_delay_ms,
            self.runtime_abi.prime_delay_ms_ty,
            &mut call_args,
            "delay_ms",
        );
        Ok(Value::Unit)
    }

    fn builtin_pin_mode(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("pin_mode expects 2 arguments".into());
        }
        self.ensure_embedded_target("pin_mode")?;
        let mode = self.expect_int(args.pop().unwrap())?;
        let pin = self.expect_int(args.pop().unwrap())?;
        let pin_cast = unsafe {
            LLVMBuildIntCast(
                self.builder,
                pin.llvm(),
                self.i32_type,
                CString::new("pin_mode_pin").unwrap().as_ptr(),
            )
        };
        let mode_cast = unsafe {
            LLVMBuildIntCast(
                self.builder,
                mode.llvm(),
                self.i32_type,
                CString::new("pin_mode_mode").unwrap().as_ptr(),
            )
        };
        let mut call_args = [pin_cast, mode_cast];
        self.call_runtime(
            self.runtime_abi.prime_pin_mode,
            self.runtime_abi.prime_pin_mode_ty,
            &mut call_args,
            "pin_mode",
        );
        Ok(Value::Unit)
    }

    fn builtin_digital_write(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("digital_write expects 2 arguments".into());
        }
        self.ensure_embedded_target("digital_write")?;
        let level = self.expect_int(args.pop().unwrap())?;
        let pin = self.expect_int(args.pop().unwrap())?;
        let pin_cast = unsafe {
            LLVMBuildIntCast(
                self.builder,
                pin.llvm(),
                self.i32_type,
                CString::new("digital_write_pin").unwrap().as_ptr(),
            )
        };
        let level_cast = unsafe {
            LLVMBuildIntCast(
                self.builder,
                level.llvm(),
                self.i32_type,
                CString::new("digital_write_level").unwrap().as_ptr(),
            )
        };
        let mut call_args = [pin_cast, level_cast];
        self.call_runtime(
            self.runtime_abi.prime_digital_write,
            self.runtime_abi.prime_digital_write_ty,
            &mut call_args,
            "digital_write",
        );
        Ok(Value::Unit)
    }

    fn builtin_assert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("assert expects 1 argument".into());
        }
        let cond_value = self.value_to_bool(args.pop().unwrap())?;
        let cond = self.bool_constant_or_llvm(&cond_value, "assert condition")?;
        if !cond {
            return Err("assertion failed".into());
        }
        Ok(Value::Unit)
    }

    fn builtin_expect(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("expect expects 2 arguments".into());
        }
        let cond_value = self.value_to_bool(args.remove(0))?;
        let cond = self.bool_constant_or_llvm(&cond_value, "expect condition")?;
        let msg = self.expect_string_value(args.remove(0), "expect")?;
        if !cond {
            return Err(msg);
        }
        Ok(Value::Unit)
    }

    fn builtin_str_len(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("str_len expects 1 argument".into());
        }
        let text = self.expect_string_value(args.pop().unwrap(), "str_len")?;
        Ok(Value::Int(self.const_int_value(text.len() as i128)))
    }

    fn builtin_str_contains(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("str_contains expects receiver and needle".into());
        }
        let needle = self.expect_string_value(args.pop().unwrap(), "str_contains")?;
        let haystack = self.expect_string_value(args.pop().unwrap(), "str_contains")?;
        Ok(Value::Bool(
            self.const_bool_value(haystack.contains(&needle)),
        ))
    }

    fn builtin_str_trim(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("str_trim expects 1 argument".into());
        }
        let text = self.expect_string_value(args.pop().unwrap(), "str_trim")?;
        let trimmed = text.trim().to_string();
        self.make_string_value(&trimmed)
    }

    fn builtin_str_split(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("str_split expects receiver and delimiter".into());
        }
        let delim = self.expect_string_value(args.pop().unwrap(), "str_split")?;
        let text = self.expect_string_value(args.pop().unwrap(), "str_split")?;
        let mut parts = Vec::new();
        for segment in text.split(&delim) {
            parts.push(self.make_string_value(segment)?);
        }
        Ok(Value::Slice(SliceValue::from_vec(parts)))
    }

    fn builtin_min(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("min expects 2 arguments".into());
        }
        let rhs = args.pop().unwrap();
        let lhs = args.pop().unwrap();
        if let (Ok(a), Ok(b)) = (self.expect_int(lhs.clone()), self.expect_int(rhs.clone())) {
            let a_const = a
                .constant()
                .ok_or_else(|| "min expects constant integers in build mode".to_string())?;
            let b_const = b
                .constant()
                .ok_or_else(|| "min expects constant integers in build mode".to_string())?;
            return Ok(Value::Int(self.const_int_value(a_const.min(b_const))));
        }
        if let (Ok(a), Ok(b)) = (
            self.expect_float(lhs.clone()),
            self.expect_float(rhs.clone()),
        ) {
            let a_const = a
                .constant()
                .ok_or_else(|| "min expects constant floats in build mode".to_string())?;
            let b_const = b
                .constant()
                .ok_or_else(|| "min expects constant floats in build mode".to_string())?;
            return Ok(Value::Float(self.const_float_value(a_const.min(b_const))));
        }
        Err("`min` expects numbers of the same type in build mode".into())
    }

    fn builtin_max(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("max expects 2 arguments".into());
        }
        let rhs = args.pop().unwrap();
        let lhs = args.pop().unwrap();
        if let (Ok(a), Ok(b)) = (self.expect_int(lhs.clone()), self.expect_int(rhs.clone())) {
            let a_const = a
                .constant()
                .ok_or_else(|| "max expects constant integers in build mode".to_string())?;
            let b_const = b
                .constant()
                .ok_or_else(|| "max expects constant integers in build mode".to_string())?;
            return Ok(Value::Int(self.const_int_value(a_const.max(b_const))));
        }
        if let (Ok(a), Ok(b)) = (
            self.expect_float(lhs.clone()),
            self.expect_float(rhs.clone()),
        ) {
            let a_const = a
                .constant()
                .ok_or_else(|| "max expects constant floats in build mode".to_string())?;
            let b_const = b
                .constant()
                .ok_or_else(|| "max expects constant floats in build mode".to_string())?;
            return Ok(Value::Float(self.const_float_value(a_const.max(b_const))));
        }
        Err("`max` expects numbers of the same type in build mode".into())
    }

    fn builtin_abs(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("abs expects 1 argument".into());
        }
        let value = args.pop().unwrap();
        if let Ok(int_value) = self.expect_int(value.clone()) {
            let constant = int_value
                .constant()
                .ok_or_else(|| "abs expects constant integer in build mode".to_string())?;
            return Ok(Value::Int(self.const_int_value(constant.abs())));
        }
        if let Ok(float_value) = self.expect_float(value.clone()) {
            let constant = float_value
                .constant()
                .ok_or_else(|| "abs expects constant float in build mode".to_string())?;
            return Ok(Value::Float(self.const_float_value(constant.abs())));
        }
        Err("`abs` expects int or float in build mode".into())
    }

    fn builtin_debug_show(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("debug_show expects 1 argument".into());
        }
        let value = args.pop().unwrap();
        let print_handle = match &value {
            Value::Boxed(boxed) => {
                // Try to read through the reference so boxes print their inner value, not `&val`.
                let base_handle = if let Some(h) = boxed.handle {
                    h
                } else {
                    self.build_runtime_handle(value.clone())?
                };
                let mut call_args = [base_handle];
                Some(self.call_runtime(
                    self.runtime_abi.prime_reference_read,
                    self.runtime_abi.prime_reference_read_ty,
                    &mut call_args,
                    "debug_show_box_read",
                ))
            }
            _ => None,
        };
        let handle = print_handle.unwrap_or_else(|| self.build_runtime_handle(value).unwrap());
        let mut call_args = [handle];
        self.call_runtime(
            self.runtime_abi.prime_print,
                self.runtime_abi.prime_print_ty,
                &mut call_args,
                "debug_show",
            );
        Ok(Value::Unit)
    }

    fn builtin_channel(&mut self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err("channel expects 0 arguments".into());
        }
        if self.runtime_handles_enabled() {
            self.ensure_runtime_symbols();
            if let Some((sender_handle, receiver_handle)) = self.build_channel_handles() {
                let sender = Value::Sender(ChannelSender::with_handle(sender_handle));
                let receiver = Value::Receiver(ChannelReceiver::with_handle(receiver_handle));
                return Ok(Value::Tuple(vec![sender, receiver]));
            }
        }
        let state = ChannelState {
            queue: VecDeque::new(),
            closed: false,
        };
        let shared = Arc::new((Mutex::new(state), Condvar::new()));
        let sender = Value::Sender(ChannelSender::new(shared.clone()));
        let receiver = Value::Receiver(ChannelReceiver::new(shared));
        Ok(Value::Tuple(vec![sender, receiver]))
    }

    fn builtin_send(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("send expects 2 arguments".into());
        }
        let value = args.pop().unwrap();
        let sender = self.expect_sender(args.pop().unwrap(), "send")?;
        if let Some(handle) = sender.handle {
            let value_handle = self.build_runtime_handle(value)?;
            let mut call_args = [handle, value_handle];
            let _status = self.call_runtime(
                self.runtime_abi.prime_send,
                self.runtime_abi.prime_send_ty,
                &mut call_args,
                "send_handle",
            );
            return self.instantiate_enum_variant("Ok", vec![Value::Unit]);
        }
        match sender.send(value) {
            Ok(()) => self.instantiate_enum_variant("Ok", vec![Value::Unit]),
            Err(msg) => {
                let err = self.make_string_value(&msg)?;
                self.instantiate_enum_variant("Err", vec![err])
            }
        }
    }

    fn builtin_recv(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("recv expects 1 argument".into());
        }
        let receiver = self.expect_receiver(args.pop().unwrap(), "recv")?;
        if let Some(handle) = receiver.handle {
            self.ensure_runtime_symbols();
            let slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("recv_out").unwrap().as_ptr(),
                )
            };
            let mut call_args = [handle, slot];
            let status = self.call_runtime(
                self.runtime_abi.prime_recv,
                self.runtime_abi.prime_recv_ty,
                &mut call_args,
                "recv_handle",
            );
            let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
            let ok_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    function,
                    CString::new("recv_ok").unwrap().as_ptr(),
                )
            };
            let closed_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    function,
                    CString::new("recv_closed").unwrap().as_ptr(),
                )
            };
            let other_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    function,
                    CString::new("recv_other").unwrap().as_ptr(),
                )
            };
            let merge_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    function,
                    CString::new("recv_merge").unwrap().as_ptr(),
                )
            };
            let option_slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("recv_option").unwrap().as_ptr(),
                )
            };
            let ok_const =
                unsafe { LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Ok as u64, 0) };
            let closed_const = unsafe {
                LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Closed as u64, 0)
            };
            let some_tag = unsafe {
                LLVMConstInt(
                    LLVMInt32TypeInContext(self.context),
                    self.enum_variants
                        .get("Some")
                        .map(|v| v.variant_index as u64)
                        .unwrap_or(0),
                    0,
                )
            };
            let none_tag = unsafe {
                LLVMConstInt(
                    LLVMInt32TypeInContext(self.context),
                    self.enum_variants
                        .get("None")
                        .map(|v| v.variant_index as u64)
                        .unwrap_or(0),
                    0,
                )
            };
            unsafe {
                let switch = LLVMBuildSwitch(self.builder, status, other_block, 2);
                LLVMAddCase(switch, ok_const, ok_block);
                LLVMAddCase(switch, closed_const, closed_block);

                LLVMPositionBuilderAtEnd(self.builder, ok_block);
                let loaded = LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    CString::new("recv_loaded").unwrap().as_ptr(),
                );
                let payload_ptr = LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("recv_payload").unwrap().as_ptr(),
                );
                LLVMBuildStore(self.builder, loaded, payload_ptr);
                let mut ok_args = [
                    payload_ptr,
                    LLVMConstInt(self.runtime_abi.usize_type, 1, 0),
                    some_tag,
                ];
                let option_handle = self.call_runtime(
                    self.runtime_abi.prime_enum_new,
                    self.runtime_abi.prime_enum_new_ty,
                    &mut ok_args,
                    "recv_option_some",
                );
                LLVMBuildStore(self.builder, option_handle, option_slot);
                LLVMBuildBr(self.builder, merge_block);

                LLVMPositionBuilderAtEnd(self.builder, closed_block);
                let mut none_args = [
                    LLVMConstPointerNull(LLVMPointerType(self.runtime_abi.handle_type, 0)),
                    LLVMConstInt(self.runtime_abi.usize_type, 0, 0),
                    none_tag,
                ];
                let none_handle = self.call_runtime(
                    self.runtime_abi.prime_enum_new,
                    self.runtime_abi.prime_enum_new_ty,
                    &mut none_args,
                    "recv_option_none",
                );
                LLVMBuildStore(self.builder, none_handle, option_slot);
                LLVMBuildBr(self.builder, merge_block);

                LLVMPositionBuilderAtEnd(self.builder, other_block);
                let mut other_none_args = [
                    LLVMConstPointerNull(LLVMPointerType(self.runtime_abi.handle_type, 0)),
                    LLVMConstInt(self.runtime_abi.usize_type, 0, 0),
                    none_tag,
                ];
                let other_none = self.call_runtime(
                    self.runtime_abi.prime_enum_new,
                    self.runtime_abi.prime_enum_new_ty,
                    &mut other_none_args,
                    "recv_option_none_other",
                );
                LLVMBuildStore(self.builder, other_none, option_slot);
                LLVMBuildBr(self.builder, merge_block);

                LLVMPositionBuilderAtEnd(self.builder, merge_block);
            }
            let option_loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    option_slot,
                    CString::new("recv_option_loaded").unwrap().as_ptr(),
                )
            };
            self.pending_runtime = Some(option_loaded);
            return Ok(Value::Enum(EnumValue {
                enum_name: "Option".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }));
        }
        match receiver.recv() {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_recv_timeout(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("recv_timeout expects receiver and millis".into());
        }
        let millis_val = args.pop().unwrap();
        let millis = self.expect_int(millis_val)?;
        let receiver = self.expect_receiver(args.pop().unwrap(), "recv_timeout")?;
        if let Some(handle) = receiver.handle {
            let duration = if let Some(constant) = millis.constant() {
                unsafe { LLVMConstInt(self.runtime_abi.int_type, constant as u64, 1) }
            } else {
                self.int_to_runtime_int(&millis, "recv_timeout_millis")?
            };
            let slot = unsafe {
                LLVMBuildAlloca(
                    self.builder,
                    self.runtime_abi.handle_type,
                    CString::new("recv_timeout_out").unwrap().as_ptr(),
                )
            };
            let mut call_args = [handle, duration, slot];
            let status = self.call_runtime(
                self.runtime_abi.prime_recv_timeout,
                self.runtime_abi.prime_recv_timeout_ty,
                &mut call_args,
                "recv_timeout_handle",
            );
            let closed_const = unsafe {
                LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Closed as u64, 0)
            };
            let ok_const =
                unsafe { LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Ok as u64, 0) };
            let is_closed = unsafe {
                LLVMConstIntGetZExtValue(status) == LLVMConstIntGetZExtValue(closed_const)
            };
            if is_closed {
                return self.instantiate_enum_variant("None", Vec::new());
            }
            let is_ok =
                unsafe { LLVMConstIntGetZExtValue(status) == LLVMConstIntGetZExtValue(ok_const) };
            if !is_ok {
                return self.instantiate_enum_variant("None", Vec::new());
            }
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    CString::new("recv_timeout_loaded").unwrap().as_ptr(),
                )
            };
            let reference = ReferenceValue {
                cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: false,
                origin: None,
                handle: Some(loaded),
            };
            return self.instantiate_enum_variant("Some", vec![Value::Reference(reference)]);
        }
        let millis_const = millis.constant().ok_or_else(|| {
            "recv_timeout expects constant millis in build mode without runtime handles".to_string()
        })?;
        let millis_i64: i64 = millis_const.try_into().unwrap_or_else(|_| {
            if millis_const.is_negative() {
                0
            } else {
                i64::MAX
            }
        });
        match receiver.recv_timeout(millis_i64) {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_recv_task(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("recv_task expects 1 argument".into());
        }
        let receiver = self.expect_receiver(args.pop().unwrap(), "recv_task")?;
        if let Some(handle) = receiver.handle {
            if self.runtime_handles_enabled() {
                self.ensure_runtime_symbols();
                let mut call_args = [handle];
                let task_handle = self.call_runtime(
                    self.runtime_abi.prime_recv_task,
                    self.runtime_abi.prime_recv_task_ty,
                    &mut call_args,
                    "recv_task",
                );
                let task = TaskValue::with_handle(task_handle, TaskResultKind::RuntimeOption);
                return Ok(Value::Task(Box::new(task)));
            }
        }
        let option = match receiver.recv() {
            Some(value) => self.instantiate_enum_variant("Some", vec![value])?,
            None => self.instantiate_enum_variant("None", Vec::new())?,
        };
        let task = TaskValue::ready(self.evaluated(option));
        Ok(Value::Task(Box::new(task)))
    }

    fn builtin_close(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("close expects 1 argument".into());
        }
        match args.pop().unwrap() {
            Value::Sender(tx) => {
                if let Some(handle) = tx.handle {
                    let mut call_args = [handle];
                    let _ = self.call_runtime(
                        self.runtime_abi.prime_close,
                        self.runtime_abi.prime_close_ty,
                        &mut call_args,
                        "close_handle",
                    );
                    return Ok(Value::Unit);
                }
                tx.close();
                Ok(Value::Unit)
            }
            Value::Receiver(rx) => {
                if let Some(handle) = rx.handle {
                    let mut call_args = [handle];
                    let _ = self.call_runtime(
                        self.runtime_abi.prime_close,
                        self.runtime_abi.prime_close_ty,
                        &mut call_args,
                        "close_handle",
                    );
                    return Ok(Value::Unit);
                }
                rx.close();
                Ok(Value::Unit)
            }
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.builtin_close(vec![inner])
            }
            other => Err(format!(
                "close expects channel endpoint, found {}",
                describe_value(&other)
            )),
        }
    }

    fn builtin_join(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("join expects 1 argument".into());
        }
        match args.pop().unwrap() {
            Value::JoinHandle(handle) => handle.join_with(self),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.builtin_join(vec![inner])
            }
            other => Err(format!(
                "join expects join handle, found {}",
                describe_value(&other)
            )),
        }
    }

    fn builtin_ptr(&mut self, mut args: Vec<Value>, mutable: bool) -> Result<Value, String> {
        let name = if mutable { "ptr_mut" } else { "ptr" };
        if args.len() != 1 {
            return Err(format!("{name} expects 1 argument"));
        }
        match args.pop().unwrap() {
            Value::Reference(reference) => Ok(Value::Pointer(PointerValue {
                cell: reference.cell,
                mutable,
                handle: reference.handle,
                origin: reference.origin.clone(),
            })),
            Value::Pointer(mut ptr) => {
                ptr.mutable = ptr.mutable || mutable;
                Ok(Value::Pointer(ptr))
            }
            other => Err(format!(
                "{name} expects reference or pointer, found {}",
                describe_value(&other)
            )),
        }
    }

    fn execute_block_contents(&mut self, block: &Block) -> Result<BlockEval, String> {
        let trace = env::var_os("PRIME_DEBUG_TRACE").is_some();
        for statement in &block.statements {
            if trace {
                let kind = match statement {
                    Statement::Let(_) => "let",
                    Statement::Expr(_) => "expr",
                    Statement::Return(_) => "return",
                    Statement::Break => "break",
                    Statement::Continue => "continue",
                    Statement::Loop(_) => "loop",
                    Statement::For(_) => "for",
                    Statement::While(_) => "while",
                    Statement::Assign(_) => "assign",
                    Statement::Defer(_) => "defer",
                    Statement::MacroSemi(_) => "macro",
                    Statement::Block(_) => "block",
                };
                eprintln!("[prime-debug] stmt {kind}");
            }
            if let Some(flow) = self.emit_statement(statement)? {
                return Ok(BlockEval::Flow(flow));
            }
        }
        if let Some(tail) = &block.tail {
            let single_return_hint = self
                .current_return_types()
                .and_then(|returns| {
                    if returns.len() == 1 {
                        returns.get(0)
                    } else {
                        None
                    }
                })
                .cloned();
            let tail_result = if let Some(ret) = single_return_hint.as_ref() {
                self.emit_expression_with_hint(tail, Some(ret))
            } else {
                self.emit_expression(tail)
            };
            match tail_result? {
                EvalOutcome::Value(value) => Ok(BlockEval::Value(value)),
                EvalOutcome::Flow(flow) => Ok(BlockEval::Flow(flow)),
            }
        } else {
            Ok(BlockEval::Value(self.evaluated(Value::Unit)))
        }
    }

    fn assign_var(&mut self, name: &str, value: EvaluatedValue) -> Result<(), String> {
        for index in (0..self.scopes.len()).rev() {
            let mut found = None;
            if let Some(binding) = self.scopes[index].get(name) {
                found = Some((binding.cell.clone(), binding.mutable, binding.slot));
            }
            if let Some((cell, mutable, slot)) = found {
                if !mutable {
                    return Err(format!("Variable {} is immutable", name));
                }
                self.track_reference_borrow_in_scope(value.value(), index)?;
                {
                    let current = cell.lock().unwrap();
                    self.release_reference_borrow(current.value());
                }
                let mut updated = value;
                if mutable {
                    Self::clear_scalar_constant(&mut updated);
                }
                if let (Some(slot), Value::Int(int_val)) =
                    (slot, updated.value().clone())
                {
                    unsafe {
                        LLVMBuildStore(
                            self.builder,
                            int_val.llvm(),
                            slot,
                        );
                    }
                }
                *cell.lock().unwrap() = updated;
                return Ok(());
            }
        }
        Err(format!("Unknown variable {}", name))
    }

    fn value_to_bool(&mut self, value: Value) -> Result<BoolValue, String> {
        let concrete = match value {
            Value::Reference(reference) => {
                if let Some(handle) = reference.handle {
                    self.ensure_runtime_symbols();
                    let mut args = [handle];
                    let llvm = self.call_runtime(
                        self.runtime_abi.prime_value_as_bool,
                        self.runtime_abi.prime_value_as_bool_ty,
                        &mut args,
                        "value_as_bool",
                    );
                    return Ok(BoolValue::new(llvm, None));
                }
                reference.cell.lock().unwrap().clone().into_value()
            }
            Value::Pointer(pointer) => pointer.cell.lock().unwrap().clone().into_value(),
            other => other,
        };
        match concrete {
            Value::Bool(flag) => Ok(flag),
            other => {
                if let Ok(int_value) = self.expect_int(other.clone()) {
                    if let Some(constant) = int_value.constant() {
                        return Ok(self.const_bool_value(constant != 0));
                    }
                    let zero = unsafe { LLVMConstInt(LLVMTypeOf(int_value.llvm()), 0, 0) };
                    let llvm = unsafe {
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            int_value.llvm(),
                            zero,
                            CString::new("int_to_bool").unwrap().as_ptr(),
                        )
                    };
                    return Ok(BoolValue::new(llvm, None));
                }
                if let Ok(float_value) = self.expect_float(other.clone()) {
                    if let Some(constant) = float_value.constant() {
                        return Ok(self.const_bool_value(constant != 0.0));
                    }
                    let zero = unsafe {
                        LLVMConstReal(LLVMTypeOf(float_value.llvm()), 0.0f64 as f64)
                    };
                    let llvm = unsafe {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealONE,
                            float_value.llvm(),
                            zero,
                            CString::new("float_to_bool").unwrap().as_ptr(),
                        )
                    };
                    return Ok(BoolValue::new(llvm, None));
                }
                Err(format!(
                    "Expected boolean-convertible value in build mode, got {}",
                    describe_value(&other)
                ))
            }
        }
    }

    fn snapshot_build_state(&self) -> Result<BuildSnapshot, String> {
        let mut capture_ctx = BuildCaptureContext::new();
        capture_ctx.closure_snapshots = self.closure_snapshots.clone();
        capture_ctx.clock_ms = self.build_clock_ms;
        let mut scopes = Vec::with_capacity(self.scopes.len());
        for scope in &self.scopes {
            let mut bindings = HashMap::new();
            for (name, binding) in scope {
                let guard = binding.cell.lock().map_err(|_| {
                    format!("Binding `{name}` poisoned while capturing build snapshot")
                })?;
                bindings.insert(
                    name.clone(),
                    BuildBinding {
                        cell: Arc::new(Mutex::new(value_to_build_value_with_ctx(
                            guard.value(),
                            &mut capture_ctx,
                        )?)),
                        mutable: binding.mutable,
                        borrowed_mut: false,
                        borrowed_shared: 0,
                        borrowed_shared_names: HashSet::new(),
                        origin: Span::new(0, 0),
                        last_move: None,
                    },
                );
            }
            scopes.push(BuildScope { bindings });
        }
        let mut enum_variants = HashMap::new();
        for (variant, info) in &self.enum_variants {
            enum_variants.insert(
                variant.clone(),
                BuildEnumVariant {
                    enum_name: info.enum_name.clone(),
                    variant_index: info.variant_index,
                    fields: info.fields,
                },
            );
        }
        let mut functions: HashMap<BuildFunctionKey, BuildFunction> = HashMap::new();
        for (key, entry) in &self.functions {
            let build_key = BuildFunctionKey {
                name: key.name.clone(),
                receiver: key.receiver.clone(),
            };
            functions.insert(
                build_key,
                BuildFunction {
                    def: entry.def.clone(),
                    receiver: key.receiver.clone(),
                },
            );
        }
        let mut struct_fields = HashMap::new();
        for (name, entry) in &self.structs {
            let field_names = entry
                .def
                .fields
                .iter()
                .filter_map(|f| f.name.clone())
                .collect();
            struct_fields.insert(name.clone(), field_names);
        }
        let cleanup_stack: Vec<Vec<BuildCleanup>> = self
            .cleanup_stack
            .iter()
            .map(|frame| {
                frame
                    .iter()
                    .filter_map(|action| match action {
                        CleanupAction::Defer(expr) => Some(BuildCleanup::Defer(expr.clone())),
                        CleanupAction::Drop(record) => Some(BuildCleanup::Drop(BuildDropRecord {
                            binding: record.binding.clone(),
                            type_name: record.type_name.clone(),
                        })),
                    })
                    .collect()
            })
            .collect();
        Ok(BuildSnapshot {
            scopes,
            enum_variants,
            functions,
            struct_fields,
            next_channel_id: capture_ctx.next_channel_id,
            cleanup_stack,
            clock_ms: capture_ctx.clock_ms,
        })
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.borrow_frames.push(Vec::new());
        self.cleanup_stack.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        if let Some(frame) = self.borrow_frames.pop() {
            for name in frame {
                self.active_mut_borrows.remove(&name);
            }
        }
        self.cleanup_stack.pop();
        if self.borrow_frames.is_empty() {
            self.borrow_frames.push(Vec::new());
        }
        if self.cleanup_stack.is_empty() {
            self.cleanup_stack.push(Vec::new());
        }
    }

    fn push_return_types(&mut self, returns: &[TypeExpr]) {
        self.return_type_stack.push(returns.to_vec());
    }

    fn pop_return_types(&mut self) {
        self.return_type_stack.pop();
    }

    fn current_return_types(&self) -> Option<&[TypeExpr]> {
        self.return_type_stack.last().map(|v| v.as_slice())
    }

    fn run_cleanups(&mut self) -> Result<(), String> {
        if let Some(stack) = self.cleanup_stack.last_mut() {
            let mut pending = Vec::new();
            mem::swap(stack, &mut pending);
            while let Some(action) = pending.pop() {
                match action {
                    CleanupAction::Defer(expr) => match self.emit_expression(&expr)? {
                        EvalOutcome::Value(_) => {}
                        EvalOutcome::Flow(flow) => {
                            return Err(format!(
                                "Control flow {} not allowed in deferred expression",
                                flow_name(&flow)
                            ));
                        }
                    },
                    CleanupAction::Drop(record) => self.run_drop(record)?,
                }
            }
        }
        Ok(())
    }

    fn exit_scope(&mut self) -> Result<(), String> {
        self.run_cleanups()?;
        self.pop_scope();
        Ok(())
    }

    fn run_drop(&mut self, record: DropRecord) -> Result<(), String> {
        let Some(key) = self.drop_impls.get(&record.type_name).cloned() else {
            return Ok(());
        };
        let Some((cell, _)) = self.get_binding(&record.binding) else {
            return Ok(());
        };
        {
            let guard = cell.lock().unwrap();
            if matches!(guard.value(), Value::Moved) {
                return Ok(());
            }
        }
        let reference = Value::Reference(ReferenceValue {
            cell: cell.clone(),
            mutable: true,
            origin: Some(record.binding.clone()),
            handle: None,
        });
        let eval = self.evaluated(reference);
        let _ =
            self.call_function_with_values(&key.name, key.receiver.as_deref(), &[], vec![eval])?;
        Ok(())
    }

    fn insert_var(
        &mut self,
        name: &str,
        value: EvaluatedValue,
        mutable: bool,
    ) -> Result<(), String> {
        let scope_index = self.scopes.len().saturating_sub(1);
        let drop_target = self.drop_type_for_value(value.value());
        let mut value = value;
        if mutable {
            Self::clear_scalar_constant(&mut value);
        }
        Self::attach_origin(&mut value, name);
        let mut slot = None;
        if mutable {
            if let Value::Int(int_val) = value.value() {
                unsafe {
                    let ty = LLVMTypeOf(int_val.llvm());
                    let alloca = LLVMBuildAlloca(
                        self.builder,
                        ty,
                        CString::new(format!("{name}_slot")).unwrap().as_ptr(),
                    );
                    LLVMBuildStore(self.builder, int_val.llvm(), alloca);
                    slot = Some(alloca);
                }
            }
        }
        let cell = Arc::new(Mutex::new(value));
        {
            let stored = cell.lock().unwrap();
            self.track_reference_borrow_in_scope(stored.value(), scope_index)?;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name.to_string(),
                Binding {
                    cell,
                    mutable,
                    slot,
                },
            );
            if let Some(type_name) = drop_target {
                self.queue_drop(name, type_name);
            }
        }
        Ok(())
    }

    fn attach_origin(value: &mut EvaluatedValue, name: &str) {
        match value.value_mut() {
            Value::Reference(reference) => {
                if reference.origin.is_none() {
                    reference.origin = Some(name.to_string());
                }
            }
            Value::Pointer(pointer) => {
                if pointer.origin.is_none() {
                    pointer.origin = Some(name.to_string());
                }
            }
            Value::Sender(sender) => {
                if sender.origin.is_none() {
                    sender.origin = Some(name.to_string());
                }
            }
            Value::Receiver(receiver) => {
                if receiver.origin.is_none() {
                    receiver.origin = Some(name.to_string());
                }
            }
            _ => {}
        }
    }

    fn clear_scalar_constant(value: &mut EvaluatedValue) {
        match value.value_mut() {
            Value::Int(int_value) => int_value.constant = None,
            Value::Float(float_value) => float_value.constant = None,
            Value::Bool(bool_value) => bool_value.constant = None,
            _ => {}
        }
    }

    fn get_var(&mut self, name: &str) -> Option<EvaluatedValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                if let Some(slot) = binding.slot {
                    let guard = binding.cell.lock().unwrap().clone();
                    if let Value::Int(int_val) = guard.value() {
                        unsafe {
                            let loaded = LLVMBuildLoad2(
                                self.builder,
                                LLVMTypeOf(int_val.llvm()),
                                slot,
                                CString::new(format!("{name}_load")).unwrap().as_ptr(),
                            );
                            return Some(self.evaluated(Value::Int(IntValue::new(
                                loaded, None,
                            ))));
                        }
                    }
                }
                return Some(binding.cell.lock().unwrap().clone());
            }
        }
        None
    }

    fn get_binding(&self, name: &str) -> Option<(Arc<Mutex<EvaluatedValue>>, bool)> {
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
            if let Some(triple) = self.target.triple() {
                let triple_c = CString::new(triple).unwrap();
                LLVMSetTarget(self.module, triple_c.as_ptr());
            }

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

            self.runtime_abi =
                RuntimeAbi::declare(self.context, self.module, self.target.pointer_width_bits());
            self.closure_counter = 0;
            self.closures.clear();
            self.closure_envs.clear();
            let opaque_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut elems = [opaque_ptr, opaque_ptr, self.runtime_abi.usize_type];
            self.closure_value_type = LLVMStructCreateNamed(
                self.context,
                CString::new("closure_value").unwrap().as_ptr(),
            );
            LLVMStructSetBody(
                self.closure_value_type,
                elems.as_mut_ptr(),
                elems.len() as u32,
                0,
            );
        }
    }

    fn collect_iterable_values(&mut self, value: Value) -> Result<Vec<EvaluatedValue>, String> {
        match value {
            Value::Range(range) => {
                let start_const = self.int_constant_or_llvm(&range.start, "Range start")?;
                let end_const = self.int_constant_or_llvm(&range.end, "Range end")?;
                let end = if range.inclusive {
                    end_const + 1
                } else {
                    end_const
                };
                Ok((start_const..end)
                    .map(|v| Value::Int(self.const_int_value(v)).into())
                    .collect())
            }
            Value::Slice(slice) => {
                if let Some(handle) = slice.handle {
                    let mut items = Vec::new();
                    let mut len_args = [handle];
                    let len_val = self.call_runtime(
                        self.runtime_abi.prime_slice_len_handle,
                        self.runtime_abi.prime_slice_len_handle_ty,
                        &mut len_args,
                        "slice_iter_len",
                    );
                    let maybe_len = unsafe {
                        if LLVMIsAConstantInt(len_val).is_null() {
                            None
                        } else {
                            Some(LLVMConstIntGetZExtValue(len_val) as usize)
                        }
                    };
                    let len = maybe_len.ok_or_else(|| {
                        "slice length must be constant to iterate in build mode".to_string()
                    })?;
                    for idx in 0..len {
                        let idx_const =
                            unsafe { LLVMConstInt(self.runtime_abi.usize_type, idx as u64, 0) };
                        let slot = unsafe {
                            LLVMBuildAlloca(
                                self.builder,
                                self.runtime_abi.handle_type,
                                CString::new("slice_iter_out").unwrap().as_ptr(),
                            )
                        };
                        let mut call_args = [handle, idx_const, slot];
                        self.call_runtime(
                            self.runtime_abi.prime_slice_get_handle,
                            self.runtime_abi.prime_slice_get_handle_ty,
                            &mut call_args,
                            "slice_iter_get",
                        );
                        let loaded = unsafe {
                            LLVMBuildLoad2(
                                self.builder,
                                self.runtime_abi.handle_type,
                                slot,
                                CString::new("slice_iter_loaded").unwrap().as_ptr(),
                            )
                        };
                        let reference = ReferenceValue {
                            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                            mutable: false,
                            origin: None,
                            handle: Some(loaded),
                        };
                        items.push(Value::Reference(reference).into());
                    }
                    return Ok(items);
                }
                let mut items = Vec::new();
                for idx in 0..slice.len() {
                    if let Some(item) = slice.get(idx) {
                        items.push(item.into());
                    }
                }
                Ok(items)
            }
            Value::Map(map) => {
                if let Some(handle) = map.handle {
                    let mut items = Vec::new();
                    let mut len_args = [handle];
                    let len_val = self.call_runtime(
                        self.runtime_abi.prime_map_len_handle,
                        self.runtime_abi.prime_map_len_handle_ty,
                        &mut len_args,
                        "map_iter_len",
                    );
                    let maybe_len = unsafe {
                        if LLVMIsAConstantInt(len_val).is_null() {
                            None
                        } else {
                            Some(LLVMConstIntGetZExtValue(len_val) as usize)
                        }
                    };
                    let len = maybe_len.ok_or_else(|| {
                        "map length must be constant to iterate in build mode".to_string()
                    })?;
                    for idx in 0..len {
                        let idx_const =
                            unsafe { LLVMConstInt(self.runtime_abi.usize_type, idx as u64, 0) };
                        let key_slot = unsafe {
                            LLVMBuildAlloca(
                                self.builder,
                                self.runtime_abi.handle_type,
                                CString::new("map_iter_key").unwrap().as_ptr(),
                            )
                        };
                        let value_slot = unsafe {
                            LLVMBuildAlloca(
                                self.builder,
                                self.runtime_abi.handle_type,
                                CString::new("map_iter_val").unwrap().as_ptr(),
                            )
                        };
                        let mut call_args = [handle, idx_const, key_slot, value_slot];
                        self.call_runtime(
                            self.runtime_abi.prime_map_entry_handle,
                            self.runtime_abi.prime_map_entry_handle_ty,
                            &mut call_args,
                            "map_iter_entry",
                        );
                        let key_handle = unsafe {
                            LLVMBuildLoad2(
                                self.builder,
                                self.runtime_abi.handle_type,
                                key_slot,
                                CString::new("map_iter_key_loaded").unwrap().as_ptr(),
                            )
                        };
                        let value_handle = unsafe {
                            LLVMBuildLoad2(
                                self.builder,
                                self.runtime_abi.handle_type,
                                value_slot,
                                CString::new("map_iter_val_loaded").unwrap().as_ptr(),
                            )
                        };
                        let key_ref = ReferenceValue {
                            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                            mutable: false,
                            origin: None,
                            handle: Some(key_handle),
                        };
                        let value_ref = ReferenceValue {
                            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                            mutable: false,
                            origin: None,
                            handle: Some(value_handle),
                        };
                        items.push(
                            Value::Tuple(vec![
                                Value::Reference(key_ref),
                                Value::Reference(value_ref),
                            ])
                            .into(),
                        );
                    }
                    return Ok(items);
                }
                let mut items = Vec::new();
                for (key, value) in map.entries.lock().unwrap().iter() {
                    let key_value = self.make_string_value(key)?;
                    items.push(Value::Tuple(vec![key_value, value.clone()]).into());
                }
                Ok(items)
            }
            Value::Iterator(iter) => {
                let mut items = Vec::new();
                while let Some(value) = iter.next() {
                    items.push(value.into());
                }
                Ok(items)
            }
            Value::Closure(_) => Err("Cannot iterate over closure value in build mode".into()),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.collect_iterable_values(inner)
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone().into_value();
                self.collect_iterable_values(inner)
            }
            Value::Struct(_) => {
                if let Some(struct_name) = self.value_struct_name(&value) {
                    let iter_outcome = self.call_function_with_values(
                        "iter",
                        Some(struct_name.as_str()),
                        &[],
                        vec![value.into()],
                    )?;
                    if iter_outcome.len() != 1 {
                        return Err("iter() must return a single iterable value".into());
                    }
                    self.collect_iterable_values(
                        iter_outcome.into_iter().next().unwrap().into_value(),
                    )
                } else {
                    Err("iter() requires a struct receiver".into())
                }
            }
            other => Err(format!(
                "`for ... in` only supports ranges, slices, maps, or values with iter() (found {})",
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
            Value::FormatTemplate(_) => "format string",
            Value::Sender(_) => "channel sender",
            Value::Receiver(_) => "channel receiver",
            Value::Iterator(_) => "iterator",
            Value::Task(_) => "task",
            Value::Pointer(_) => "pointer",
            Value::Range(_) => "range",
            Value::JoinHandle(_) => "join handle",
            Value::Closure(_) => "closure",
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
mod tests {
    use super::*;
    use crate::language::span::{Span, Spanned};
    use crate::language::{ast::Program, parser::parse_module};
    use crate::project::load_package;
    use std::{
        collections::HashMap,
        env,
        path::{Path, PathBuf},
        ptr,
        sync::{Arc, Mutex, OnceLock},
        thread,
    };

    fn compile_source(source: &str) -> Result<(), String> {
        let module =
            parse_module("tests::build", PathBuf::from("test.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        let mut compiler = Compiler::new();
        compiler.compile_program(&program)
    }

    fn with_rt_handles<T>(f: impl FnOnce() -> T) -> T {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        let guard = ENV_LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
        let prior = env::var("PRIME_ENABLE_RT_HANDLES").ok();
        unsafe {
            env::set_var("PRIME_ENABLE_RT_HANDLES", "1");
        }
        let result = f();
        if let Some(val) = prior {
            unsafe {
                env::set_var("PRIME_ENABLE_RT_HANDLES", val);
            }
        } else {
            unsafe {
                env::remove_var("PRIME_ENABLE_RT_HANDLES");
            }
        }
        drop(guard);
        result
    }

    fn with_build_parallel<T>(f: impl FnOnce() -> T) -> T {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        let guard = ENV_LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
        let prior = env::var("PRIME_BUILD_PARALLEL").ok();
        unsafe {
            env::set_var("PRIME_BUILD_PARALLEL", "1");
        }
        let result = f();
        if let Some(val) = prior {
            unsafe {
                env::set_var("PRIME_BUILD_PARALLEL", val);
            }
        } else {
            unsafe {
                env::remove_var("PRIME_BUILD_PARALLEL");
            }
        }
        drop(guard);
        result
    }

    #[test]
    fn build_snapshot_captures_basic_constants() {
        let mut compiler = Compiler::new();
        compiler.push_scope();
        let int_binding = Binding {
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Int(
                IntValue::new(ptr::null_mut(), Some(7)),
            )))),
            mutable: false,
            slot: None,
        };
        let tuple_binding = Binding {
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Tuple(vec![
                Value::Bool(compiler.const_bool_value(true)),
                Value::Int(IntValue::new(ptr::null_mut(), Some(2))),
            ])))),
            mutable: true,
            slot: None,
        };
        let string_binding = Binding {
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Str(
                StringValue::new(ptr::null_mut(), Arc::new("hello".to_string())),
            )))),
            mutable: false,
            slot: None,
        };
        if let Some(scope) = compiler.scopes.last_mut() {
            scope.insert("seven".into(), int_binding);
            scope.insert("pair".into(), tuple_binding);
            scope.insert("greeting".into(), string_binding);
        }

        let snapshot = compiler
            .snapshot_build_state()
            .expect("snapshot should capture constants");
        assert_eq!(snapshot.scopes.len(), 1);
        let bindings = &snapshot.scopes[0].bindings;
        match bindings
            .get("seven")
            .and_then(|b| b.cell.lock().ok())
            .map(|v| v.clone())
        {
            Some(BuildValue::Int(value)) => assert_eq!(value, 7),
            other => panic!("unexpected binding for seven: {:?}", other),
        }
        match bindings
            .get("pair")
            .and_then(|b| b.cell.lock().ok())
            .map(|v| v.clone())
        {
            Some(BuildValue::Tuple(items)) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(items[0], BuildValue::Bool(true)));
                assert!(matches!(items[1], BuildValue::Int(2)));
            }
            other => panic!("unexpected binding for pair: {:?}", other),
        }
        match bindings
            .get("greeting")
            .and_then(|b| b.cell.lock().ok())
            .map(|v| v.clone())
        {
            Some(BuildValue::String(text)) => assert_eq!(text, "hello"),
            other => panic!("unexpected binding for greeting: {:?}", other),
        }
        assert!(bindings.get("pair").map(|b| b.mutable).unwrap_or_default());
    }

    #[test]
    fn build_snapshot_rejects_non_constant_values() {
        let mut compiler = Compiler::new();
        compiler.push_scope();
        let non_const_int = Binding {
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Int(
                IntValue::new(ptr::null_mut(), None),
            )))),
            mutable: false,
        };
        if let Some(scope) = compiler.scopes.last_mut() {
            scope.insert("ephemeral".into(), non_const_int);
        }

        let snapshot_err = compiler.snapshot_build_state().unwrap_err();
        assert!(
            snapshot_err.contains("Non-constant integer"),
            "unexpected error: {snapshot_err}"
        );
    }

    #[test]
    fn for_range_accepts_dynamic_bounds() {
        let source = r#"
module tests::dynamic_range;

fn loop_from(n: int32) {
  for i in n..(n + 3) {
    out(i);
  }
}

fn main() { loop_from(2); }
"#;
        let result = compile_source(source);
        assert!(result.is_ok(), "{}", result.unwrap_err());
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

fn release_after_nested_match() {
  let mut int32 value = 0;
  match true {
    true => {
      match false {
        true => {
          let &mut int32 alias = &mut value;
          *alias = 15;
        },
        false => {
          let &mut int32 alias = &mut value;
          *alias = 16;
        },
      }
    },
    false => {},
  }
  let &mut int32 after = &mut value;
  *after = 17;
}

fn release_after_defer() {
  let mut int32 value = 0;
  {
    defer {
      let &mut int32 alias = &mut value;
      *alias = 18;
    };
  }
  let &mut int32 after = &mut value;
  *after = 19;
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
  release_after_nested_match();
  release_after_defer();
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

    #[test]
    fn compiler_supports_format_strings_in_out() {
        let source = r#"
module tests::format_out;

fn main() {
  let int32 hp = 15;
  out(`hp is {hp} delta {}`, hp + 5);
  out(`exact {hp}`);
  out("plain string");
}
"#;
        compile_source(source).expect("format string calls should compile");
    }

    #[test]
    fn build_spawn_join_returns_value() {
        with_build_parallel(|| {
            let mut compiler = Compiler::new();
            let expr = Expr::Spawn {
                expr: Box::new(Expr::Literal(Literal::Int(9, Span::new(0, 0)))),
                span: Span::new(0, 0),
            };
            let handle = match compiler.emit_expression(&expr).expect("spawn evaluates") {
                EvalOutcome::Value(value) => value.into_value(),
                EvalOutcome::Flow(_) => panic!("unexpected flow from spawn"),
            };
            let joined = compiler.builtin_join(vec![handle]).expect("join returns");
            assert!(matches!(joined, Value::Int(v) if v.constant() == Some(9)));
        });
    }

    #[test]
    fn build_spawn_join_returns_closure() {
        with_build_parallel(|| {
            let span = Span::new(0, 0);
            let param_ty = TypeAnnotation {
                ty: TypeExpr::named("int32"),
                span,
            };
            let ret_ty = TypeAnnotation {
                ty: TypeExpr::named("int32"),
                span,
            };
            let param = FunctionParam {
                name: "n".into(),
                ty: Some(param_ty.clone()),
                mutability: Mutability::Immutable,
                span,
            };
            let body_expr = Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Identifier(Identifier {
                    name: "n".into(),
                    span,
                })),
                right: Box::new(Expr::Literal(Literal::Int(1, span))),
                span,
            };
            let closure_expr = Expr::Closure {
                params: vec![param],
                body: ClosureBody::Expr(Spanned {
                    node: Box::new(body_expr),
                    span,
                }),
                ret: Some(ret_ty),
                captures: Arc::new(RwLock::new(Vec::new())),
                span,
            };
            let spawn_expr = Expr::Spawn {
                expr: Box::new(closure_expr),
                span,
            };

            let mut compiler = Compiler::new();
            let handle = match compiler
                .emit_expression(&spawn_expr)
                .expect("spawn evaluates")
            {
                EvalOutcome::Value(value) => value.into_value(),
                EvalOutcome::Flow(_) => panic!("unexpected flow from spawn"),
            };
            let joined = compiler
                .builtin_join(vec![handle])
                .expect("join returns closure");
            let Value::Closure(returned) = joined else {
                panic!("expected closure from join");
            };
            let arg = EvaluatedValue::from_value(Value::Int(compiler.const_int_value(4)));
            let result = compiler
                .call_closure_value(returned, vec![arg])
                .expect("closure callable");
            match result.value() {
                Value::Int(_) => {}
                other => panic!(
                    "expected int return, found {}",
                    compiler.describe_value(other)
                ),
            }
        });
    }

    #[test]
    fn build_nested_spawn_join_roundtrips() {
        with_build_parallel(|| {
            let mut compiler = Compiler::new();
            let span = Span::new(0, 0);
            let expr = Expr::Spawn {
                expr: Box::new(Expr::Spawn {
                    expr: Box::new(Expr::Literal(Literal::Int(3, span))),
                    span,
                }),
                span,
            };
            let outer = match compiler
                .emit_expression(&expr)
                .expect("outer spawn evaluates")
            {
                EvalOutcome::Value(value) => value.into_value(),
                EvalOutcome::Flow(_) => panic!("unexpected flow from outer spawn"),
            };
            let inner_handle = compiler
                .builtin_join(vec![outer])
                .expect("outer join returns inner handle");
            let result = compiler
                .builtin_join(vec![inner_handle])
                .expect("inner join returns value");
            assert!(matches!(result, Value::Int(v) if v.constant() == Some(3)));
        });
    }

    #[test]
    fn build_spawn_channel_effects_bridge_to_runtime_handles() {
        let mut compiler = Compiler::new();
        let effects = vec![
            BuildEffect::ChannelCreate { id: 1 },
            BuildEffect::ChannelSend {
                id: 1,
                value: BuildValue::Int(12),
            },
            BuildEffect::ChannelClose { id: 1 },
        ];
        let channels = compiler
            .apply_build_effects(effects)
            .expect("channel effects apply");
        let inner = channels.get(&1).cloned().expect("channel present");
        let receiver = ChannelReceiver::new_with_state(inner);
        match receiver.recv() {
            Some(Value::Int(v)) => assert_eq!(v.constant(), Some(12)),
            _ => panic!("unexpected channel payload"),
        }
        assert!(receiver.recv().is_none(), "channel should be closed");
    }

    #[test]
    fn build_spawn_channel_enums_roundtrip() {
        let mut compiler = Compiler::new();
        let effects = vec![
            BuildEffect::ChannelCreate { id: 7 },
            BuildEffect::ChannelSend {
                id: 7,
                value: BuildValue::Enum {
                    enum_name: "Option".into(),
                    variant: "Some".into(),
                    values: vec![BuildValue::Int(21)],
                    variant_index: 0,
                },
            },
            BuildEffect::ChannelClose { id: 7 },
        ];
        let channels = compiler
            .apply_build_effects(effects)
            .expect("channel effects apply");
        let rx =
            ChannelReceiver::new_with_state(channels.get(&7).cloned().expect("channel present"));
        match rx.recv() {
            Some(Value::Enum(enum_value)) => {
                assert_eq!(enum_value.enum_name, "Option");
                assert_eq!(enum_value.variant, "Some");
                assert_eq!(enum_value.variant_index, 0);
                assert!(
                    matches!(enum_value.values.as_slice(), [Value::Int(v)] if v.constant() == Some(21))
                );
            }
            _ => panic!("unexpected channel payload"),
        }
        assert!(rx.recv().is_none(), "channel should be closed");
    }

    #[test]
    fn build_spawn_join_applies_channel_effects_and_value() {
        let span = Span::new(0, 0);
        let block = Block {
            statements: vec![
                Statement::Let(LetStmt {
                    pattern: Pattern::Tuple(
                        vec![
                            Pattern::Identifier("tx".into(), span),
                            Pattern::Identifier("rx".into(), span),
                        ],
                        span,
                    ),
                    ty: None,
                    value: Some(Expr::Call {
                        callee: Box::new(Expr::Identifier(Identifier {
                            name: "channel".into(),
                            span,
                        })),
                        type_args: Vec::new(),
                        args: Vec::new(),
                        span,
                    }),
                    mutability: Mutability::Immutable,
                    span,
                }),
                Statement::Expr(ExprStmt {
                    expr: Expr::Call {
                        callee: Box::new(Expr::Identifier(Identifier {
                            name: "send".into(),
                            span,
                        })),
                        type_args: Vec::new(),
                        args: vec![
                            Expr::Identifier(Identifier {
                                name: "tx".into(),
                                span,
                            }),
                            Expr::Literal(Literal::Int(4, span)),
                        ],
                        span,
                    },
                }),
                Statement::Expr(ExprStmt {
                    expr: Expr::Call {
                        callee: Box::new(Expr::Identifier(Identifier {
                            name: "close".into(),
                            span,
                        })),
                        type_args: Vec::new(),
                        args: vec![Expr::Identifier(Identifier {
                            name: "tx".into(),
                            span,
                        })],
                        span,
                    },
                }),
            ],
            tail: Some(Box::new(Expr::Identifier(Identifier {
                name: "rx".into(),
                span,
            }))),
            span,
        };
        let snapshot = BuildSnapshot {
            scopes: vec![BuildScope {
                bindings: HashMap::new(),
            }],
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            struct_fields: HashMap::new(),
            next_channel_id: 0,
            cleanup_stack: vec![Vec::new()],
            clock_ms: 0,
        };
        let handle = thread::spawn(move || {
            let interpreter = BuildInterpreter::new(snapshot);
            interpreter.eval_with_effects(&Expr::Block(Box::new(block)))
        });
        let join_handle = JoinHandleValue::new_build(handle);
        let mut compiler = Compiler::new();
        let joined = join_handle
            .join_with(&mut compiler)
            .expect("join applies effects");
        match joined {
            Value::Receiver(rx) => {
                match rx.recv() {
                    Some(Value::Int(v)) => assert_eq!(v.constant(), Some(4)),
                    _ => panic!("unexpected channel payload"),
                }
                assert!(rx.recv().is_none(), "channel should be closed");
            }
            _ => panic!("unexpected join value"),
        }
    }

    fn compile_entry(entry: &str) -> Result<(), String> {
        let package = load_package(Path::new(entry)).expect(&format!("load package for {}", entry));
        let mut compiler = Compiler::new();
        compiler.compile_program(&package.program)
    }

    #[test]
    fn borrow_demo_compiles() {
        compile_entry("workspace/demos/borrow/borrow_demo.prime")
            .expect("compile borrow demo in build mode");
    }

    #[test]
    fn pattern_demo_compiles() {
        compile_entry("workspace/demos/patterns/pattern_demo.prime")
            .expect("compile pattern demo in build mode");
    }

    #[test]
    fn async_demo_compiles() {
        compile_entry("workspace/demos/async_demo/async_demo.prime")
            .expect("compile async demo in build mode");
    }

    #[test]
    fn runtime_handles_cover_collections() {
        with_rt_handles(|| {
            let source = r#"
module tests::handles;

fn main() {
  let []int32 items = [1, 2];
  out(items);
  let Map[string, int32] scores = #{
    "one": 1,
    "two": 2,
  };
  out(scores);
  let int32 base = 5;
  let &int32 alias = &base;
  out(alias);
}
"#;
            compile_source(source)
                .expect("handles should be emitted for slices, maps, and references");
        });
    }

    #[test]
    fn runtime_handles_cover_channels() {
        with_rt_handles(|| {
            let source = r#"
module tests::handles;

fn main() {
  let (sender, receiver) = channel();
  out(sender);
  out(receiver);
}
"#;
            compile_source(source).expect("handles should be emitted for channel endpoints");
        });
    }

    #[test]
    fn recv_timeout_handles_closed_status() {
        with_rt_handles(|| {
            let mut compiler = Compiler::new();
            unsafe {
                let entry = llvm_sys::core::LLVMAppendBasicBlockInContext(
                    compiler.context,
                    compiler.main_fn,
                    CString::new("rt_entry").unwrap().as_ptr(),
                );
                llvm_sys::core::LLVMPositionBuilderAtEnd(compiler.builder, entry);
            }
            compiler.ensure_runtime_symbols();
            compiler.enum_variants.insert(
                "Some".into(),
                EnumVariantInfo {
                    enum_name: "Option".into(),
                    fields: 1,
                    module: "builtins".into(),
                    visibility: Visibility::Public,
                    variant_index: 0,
                },
            );
            compiler.enum_variants.insert(
                "None".into(),
                EnumVariantInfo {
                    enum_name: "Option".into(),
                    fields: 0,
                    module: "builtins".into(),
                    visibility: Visibility::Public,
                    variant_index: 1,
                },
            );
            let (_, rx_handle) = compiler
                .build_channel_handles()
                .expect("channel handles available");
            let receiver = Value::Receiver(ChannelReceiver::with_handle(rx_handle));
            let millis = Value::Int(compiler.const_int_value(0));
            let result = compiler
                .builtin_recv_timeout(vec![receiver, millis])
                .expect("recv_timeout should succeed");
            match result {
                Value::Enum(enum_value) => {
                    assert_eq!(enum_value.enum_name, "Option");
                    assert_eq!(enum_value.variant, "None");
                }
                other => panic!(
                    "expected Option::None, found {}",
                    compiler.describe_value(&other)
                ),
            }
        });
    }

    #[test]
    fn runtime_handles_cover_structs_and_pointers() {
        with_rt_handles(|| {
            let source = r#"
module tests::handles;

struct Pair { left: int32; right: int32; };

fn main() {
  let Pair value = Pair{ left: 1, right: 2 };
  let &Pair alias = &value;
  let ptr alias_ptr = ptr(alias);
  out(value);
  out(alias);
  out(alias_ptr);
}
"#;
            compile_source(source)
                .expect("handles should be emitted for structs and pointers/references");
        });
    }

    #[test]
    fn compiler_supports_basic_closures() {
        let source = r#"
module tests::closures;

fn main() {
  let int32 base = 2;
  let add = |x: int32| base + x;
  let result = add(5);
  out(result);
}
"#;
        compile_source(source).expect("closure creation and invocation should compile");
    }

    #[test]
    fn closure_demo_compiles() {
        compile_entry("workspace/demos/closures/closure_demo.prime")
            .expect("compile closure demo in build mode");
    }

    #[test]
    fn closures_support_multi_return_in_build_mode() {
        let source = r#"
module tests::closures;

fn main() {
  let int32 base = 3;
  let pair = |x: int32| -> (int32, int32) { (x + base, x - base) };
  let (a, b) = pair(5);
  out(a);
  out(b);
}
"#;
        compile_source(source).expect("closure should support tuple returns in build mode");
    }

    #[test]
    fn many_closures_allocate_and_drop_envs() {
        let source = r#"
module tests::closures;

fn main() {
  let int32 a = 1;
  let int32 b = 2;
  let int32 c = 3;
  let int32 d = 4;
  let int32 e = 5;
  let f1 = |x: int32| x + a;
  let f2 = |x: int32| x + b;
  let f3 = |x: int32| x + c;
  let f4 = |x: int32| x + d;
  let f5 = |x: int32| x + e;
  let _ = f1(1) + f2(1) + f3(1) + f4(1) + f5(1);
}
"#;
        compile_source(source).expect("many closures should allocate and free envs in build mode");
    }

    #[test]
    fn higher_order_closure_carries_signature() {
        let source = r#"
            fn main() {
                let higher = |f: fn(int32) -> int32| -> fn(int32) -> int32 {
                    |value: int32| f(value) + 1
                };
                let double = |x: int32| x * 2;
                let inc_after_double = higher(double);
                let _ = inc_after_double(4);
            }
        "#;
        let module =
            parse_module("tests::closures", PathBuf::from("higher.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        let mut compiler = Compiler::new();
        compiler
            .compile_program(&program)
            .expect("higher-order closure should compile");
        let mut found = false;
        for info in compiler.closures.values() {
            if info.signature.params.len() == 1
                && info.signature.params[0].canonical_name() == "int32"
                && info.signature.returns.len() == 1
                && info.signature.returns[0].canonical_name() == "int32"
            {
                found = true;
                break;
            }
        }
        assert!(
            found,
            "expected captured higher-order closure to retain its (int32) -> int32 signature"
        );
    }

    #[test]
    fn heap_handles_round_trip_in_closure_env() {
        let source = r#"
            fn main() {
                let nums = slice_new();
                slice_push(nums, 1);
                let scores = map_new();
                map_insert(scores, "a", 10);
                let counter = box_new(5);
                let pass = |_: int32| { (nums, scores, counter) };
                let _ = pass(0);
            }
        "#;
        let module = parse_module(
            "tests::closures",
            PathBuf::from("heap_handles.prime"),
            source,
        )
        .expect("parse");
        let program = Program {
            modules: vec![module],
        };
        let mut compiler = Compiler::new();
        compiler
            .compile_program(&program)
            .expect("heap handles should round-trip through closure env");
    }

    #[test]
    fn closures_capture_references() {
        let source = r#"
module tests::closures;

fn main() {
  let int32 base = 5;
  let &int32 alias = &base;
  let reader = |_: int32| alias;
  let _ = reader(0);
}
"#;
        compile_source(source).expect("reference captures should compile");
    }

    #[test]
    fn captured_handles_support_runtime_ops() {
        let source = r#"
module tests::closures;

fn main() {
  let []int32 nums = slice_new();
  slice_push(nums, 1);
  let Map[string, int32] scores = map_new();
  map_insert(scores, "a", 10);
  let mutate = |_: int32| {
    push(nums, 2);
    insert(scores, "b", 20);
    let _ = get(nums, 1);
    let _ = get(scores, "b");
  };
}
"#;
        compile_source(source).expect("captured handles should support mutation and read");
    }
}
