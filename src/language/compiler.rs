use crate::{
    language::{
        ast::*,
        build::{
            BuildBinding,
            BuildEnumVariant,
            BuildEffect,
            BuildEvaluation,
            BuildFormatTemplate,
            BuildFunction,
            BuildFunctionKey,
            BuildInterpreter,
            BuildScope,
            BuildSnapshot,
            BuildValue,
        },
        runtime_abi::RuntimeAbi,
        types::{Mutability, TypeExpr},
    },
    runtime::value::{FormatRuntimeSegmentGeneric, FormatTemplateValueGeneric},
};
use llvm_sys::{
    LLVMLinkage,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMArrayType2, LLVMBuildAlloca, LLVMBuildCall2,
        LLVMBuildGlobalString, LLVMBuildInBoundsGEP2, LLVMBuildLoad2, LLVMBuildRet, LLVMBuildStore, LLVMConstInt,
        LLVMConstReal, LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder,
        LLVMDisposeMessage, LLVMDisposeModule, LLVMDoubleTypeInContext, LLVMFunctionType, LLVMGetBasicBlockParent,
        LLVMGetElementType, LLVMGetGlobalParent, LLVMGetInsertBlock, LLVMGetLastInstruction, LLVMGetModuleContext,
        LLVMGetTypeKind, LLVMInt32TypeInContext, LLVMInt8TypeInContext, LLVMIsAFunction,
        LLVMModuleCreateWithNameInContext, LLVMPositionBuilder, LLVMPositionBuilderAtEnd, LLVMPointerType,
        LLVMPrintModuleToFile, LLVMSetLinkage, LLVMTypeOf,
    },
    LLVMTypeKind,
    prelude::*,
};
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    ffi::{CStr, CString},
    mem,
    path::Path,
    ptr,
    env,
    sync::{Arc, Condvar, Mutex},
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
    runtime_abi: RuntimeAbi,
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
    FormatTemplate(FormatTemplateValue),
    Sender(ChannelSender),
    Receiver(ChannelReceiver),
    JoinHandle(Box<JoinHandleValue>),
    Pointer(PointerValue),
    Range(RangeValue),
    Moved,
}

#[derive(Clone)]
struct EnumValue {
    enum_name: String,
    variant: String,
    values: Vec<Value>,
    variant_index: u32,
}

type FormatTemplateValue = FormatTemplateValueGeneric<Value>;
type FormatRuntimeSegment = FormatRuntimeSegmentGeneric<Value>;

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
    cell: Arc<Mutex<EvaluatedValue>>,
    mutable: bool,
    origin: Option<String>,
}

#[derive(Clone)]
struct PointerValue {
    cell: Arc<Mutex<EvaluatedValue>>,
    mutable: bool,
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
    cell: Arc<Mutex<Value>>,
}

#[derive(Clone)]
struct SliceValue {
    items: Arc<Mutex<Vec<Value>>>,
}

#[derive(Clone)]
struct MapValue {
    entries: Arc<Mutex<BTreeMap<String, Value>>>,
}

#[derive(Clone)]
struct ChannelSender {
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
}

#[derive(Clone)]
struct ChannelReceiver {
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
}

enum JoinResult {
    Immediate(Option<Value>),
    BuildThread(Option<thread::JoinHandle<Result<BuildEvaluation, String>>>),
}

#[derive(Clone)]
struct JoinHandleValue {
    result: Arc<Mutex<JoinResult>>,
}

struct ChannelState {
    queue: VecDeque<Value>,
    closed: bool,
}

impl ChannelSender {
    fn new(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self { inner }
    }

    fn new_with_state(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self { inner }
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
        Self { inner }
    }

    fn new_with_state(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self { inner }
    }

    fn recv(&self) -> Option<Value> {
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

impl JoinHandleValue {
    fn new(value: Value) -> Self {
        Self {
            result: Arc::new(Mutex::new(JoinResult::Immediate(Some(value)))),
        }
    }

    fn new_build(handle: thread::JoinHandle<Result<BuildEvaluation, String>>) -> Self {
        Self {
            result: Arc::new(Mutex::new(JoinResult::BuildThread(Some(handle)))),
        }
    }

    fn join_with(&self, compiler: &mut Compiler) -> Result<Value, String> {
        let mut guard = self.result.lock().unwrap();
        match &mut *guard {
            JoinResult::Immediate(slot) => {
                slot.take()
                    .ok_or_else(|| "join handle already consumed".to_string())
            }
            JoinResult::BuildThread(handle_slot) => {
                let handle = handle_slot
                    .take()
                    .ok_or_else(|| "join handle already consumed".to_string())?;
                match handle.join() {
                    Ok(Ok(build)) => {
                        let channels = compiler.apply_build_effects(build.effects)?;
                        compiler.build_value_to_value(build.value, Some(&channels))
                    }
                    Ok(Err(err)) => Err(err),
                    Err(_) => Err("spawned task panicked".into()),
                }
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
        Self { value, runtime: None }
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

    fn set_runtime(&mut self, handle: LLVMValueRef) {
        self.runtime = Some(RuntimeValue { handle });
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
            cell: Arc::new(Mutex::new(value)),
        }
    }

    fn replace(&self, value: Value) -> Value {
        std::mem::replace(&mut *self.cell.lock().unwrap(), value)
    }
}

impl SliceValue {
    fn new() -> Self {
        Self {
            items: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn from_vec(items: Vec<Value>) -> Self {
        Self {
            items: Arc::new(Mutex::new(items)),
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
}

impl MapValue {
    fn new() -> Self {
        Self {
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

    fn insert(&self, key: String, value: Value) {
        self.entries.lock().unwrap().insert(key, value);
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.entries.lock().unwrap().get(key).cloned()
    }

    fn len(&self) -> usize {
        self.entries.lock().unwrap().len()
    }
}

fn expect_constant_int(int: &IntValue) -> Result<i128, String> {
    int.constant
        .ok_or_else(|| "Non-constant integer cannot be captured for parallel build execution".into())
}

fn expect_constant_float(float: &FloatValue) -> Result<f64, String> {
    float.constant.ok_or_else(|| {
        "Non-constant float cannot be captured for parallel build execution".into()
    })
}

fn format_template_to_build(template: &FormatTemplateValue) -> Result<BuildFormatTemplate, String> {
    let mut segments = Vec::with_capacity(template.segments.len());
    for segment in &template.segments {
        let converted = match segment {
            FormatRuntimeSegment::Literal(text) => {
                FormatRuntimeSegmentGeneric::Literal(text.clone())
            }
            FormatRuntimeSegment::Named(value) => {
                FormatRuntimeSegmentGeneric::Named(value_to_build_value(value)?)
            }
            FormatRuntimeSegment::Implicit => FormatRuntimeSegmentGeneric::Implicit,
        };
        segments.push(converted);
    }
    Ok(BuildFormatTemplate {
        segments,
        implicit_placeholders: template.implicit_placeholders,
    })
}

fn value_to_build_value(value: &Value) -> Result<BuildValue, String> {
    match value {
        Value::Unit => Ok(BuildValue::Unit),
        Value::Int(int) => Ok(BuildValue::Int(expect_constant_int(int)?)),
        Value::Float(float) => Ok(BuildValue::Float(expect_constant_float(float)?)),
        Value::Bool(flag) => Ok(BuildValue::Bool(*flag)),
        Value::Str(text) => Ok(BuildValue::String((*text.text).clone())),
        Value::Tuple(items) => {
            let mut converted = Vec::with_capacity(items.len());
            for item in items {
                converted.push(value_to_build_value(item)?);
            }
            Ok(BuildValue::Tuple(converted))
        }
        Value::Struct(instance) => {
            let mut fields = BTreeMap::new();
            for (name, field) in &instance.fields {
                fields.insert(name.clone(), value_to_build_value(field)?);
            }
            Ok(BuildValue::Struct {
                name: instance.name.clone(),
                fields,
            })
        }
        Value::Enum(value) => {
            let mut converted = Vec::with_capacity(value.values.len());
            for val in &value.values {
                converted.push(value_to_build_value(val)?);
            }
            Ok(BuildValue::Enum {
                enum_name: value.enum_name.clone(),
                variant: value.variant.clone(),
                values: converted,
                variant_index: value.variant_index,
            })
        }
        Value::Range(range) => Ok(BuildValue::Range {
            start: expect_constant_int(&range.start)?,
            end: expect_constant_int(&range.end)?,
            inclusive: range.inclusive,
        }),
        Value::Boxed(boxed) => {
            let guard = boxed
                .cell
                .lock()
                .map_err(|_| "Box value poisoned while capturing build snapshot")?;
            Ok(BuildValue::Boxed(Box::new(value_to_build_value(&guard)?)))
        }
        Value::Slice(slice) => {
            let guard = slice
                .items
                .lock()
                .map_err(|_| "Slice value poisoned while capturing build snapshot")?;
            let mut converted = Vec::with_capacity(guard.len());
            for item in guard.iter() {
                converted.push(value_to_build_value(item)?);
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
                converted.insert(key.clone(), value_to_build_value(value)?);
            }
            Ok(BuildValue::Map(converted))
        }
        Value::FormatTemplate(template) => {
            Ok(BuildValue::FormatTemplate(format_template_to_build(template)?))
        }
        Value::Sender(_) | Value::Receiver(_) => {
            Err("channel endpoints cannot be captured for parallel build execution (yet)".into())
        }
        Value::Reference(_) | Value::Pointer(_)
        | Value::JoinHandle(_)
        | Value::Moved => Err(format!(
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
    fn evaluated(&mut self, value: Value) -> EvaluatedValue {
        EvaluatedValue::from_value(value)
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
                self.runtime_abi = RuntimeAbi::declare(self.context, module);
            }
        }
    }

    fn maybe_attach_runtime_handle(&mut self, value: &mut EvaluatedValue) -> Option<LLVMValueRef> {
        if value.runtime_handle().is_some() {
            return value.runtime_handle();
        }
        if env::var("PRIME_ENABLE_RT_HANDLES").is_err() {
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
                runtime_abi: RuntimeAbi::empty(),
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
                    let first_value = self.build_value_to_value(first, Some(&channel_handles))?;
                    match first_value {
                        Value::FormatTemplate(template) => {
                            let provided: Result<Vec<Value>, String> = iter
                                .map(|v| self.build_value_to_value(v, Some(&channel_handles)))
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
                        .ok_or_else(|| "channel send without creation".to_string())?;
                    let built = self.build_value_to_value(value, Some(&channel_handles))?;
                    let (lock, cv) = &**inner;
                    let mut guard = lock.lock().unwrap();
                    if guard.closed {
                        continue;
                    }
                    guard.queue.push_back(built);
                    cv.notify_one();
                }
                BuildEffect::ChannelClose { id } => {
                    if let Some(inner) = channel_handles.get(&id) {
                        let (lock, cv) = &**inner;
                        let mut guard = lock.lock().unwrap();
                        guard.closed = true;
                        cv.notify_all();
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

    fn build_value_to_value(
        &mut self,
        value: BuildValue,
        channels: Option<&HashMap<u64, Arc<(Mutex<ChannelState>, Condvar)>>>,
    ) -> Result<Value, String> {
        match value {
            BuildValue::Unit => Ok(Value::Unit),
            BuildValue::Int(v) => Ok(Value::Int(self.const_int_value(v))),
            BuildValue::Float(v) => Ok(Value::Float(self.const_float_value(v))),
            BuildValue::Bool(v) => Ok(Value::Bool(v)),
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
            BuildValue::Boxed(inner) => {
                Ok(Value::Boxed(BoxValue::new(self.build_value_to_value(*inner, channels)?)))
            }
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
            BuildValue::JoinHandle(handle) => {
                Ok(Value::JoinHandle(Box::new(JoinHandleValue::new_build(
                    handle.into_thread(),
                ))))
            }
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
                            FormatRuntimeSegmentGeneric::Named(
                                self.build_value_to_value(value, channels)?,
                            )
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
                let inner = channels
                    .and_then(|map| map.get(&sender.id).cloned())
                    .ok_or_else(|| "channel handle missing during join".to_string())?;
                Ok(Value::Sender(ChannelSender::new_with_state(inner)))
            }
            BuildValue::ChannelReceiver(receiver) => {
                let inner = channels
                    .and_then(|map| map.get(&receiver.id).cloned())
                    .ok_or_else(|| "channel handle missing during join".to_string())?;
                Ok(Value::Receiver(ChannelReceiver::new_with_state(inner)))
            }
            BuildValue::DeferredCall { name, type_args, args } => {
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
            BuildValue::Moved => Err("moved value cannot be used in build spawn result".into()),
        }
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
                            return Err("control flow not allowed inside format placeholders".into());
                        }
                    };
                    segments.push(FormatRuntimeSegment::Named(value.into_value()));
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

    fn emit_out_value(&mut self, value: EvaluatedValue) -> Result<(), String> {
        self.print_value(value)?;
        self.emit_printf_call("\n", &mut []);
        Ok(())
    }

    fn emit_format_template(
        &mut self,
        template: FormatTemplateValue,
        mut values: Vec<Value>,
    ) -> Result<(), String> {
        let mut args_iter = values.drain(..);
        for segment in template.segments {
            match segment {
                FormatRuntimeSegment::Literal(text) => {
                    self.emit_printf_call(&text, &mut []);
                }
                FormatRuntimeSegment::Named(value) => {
                    let mut wrapped = EvaluatedValue::from_value(value);
                    self.maybe_attach_runtime_handle(&mut wrapped);
                    self.print_value(wrapped)?;
                }
                FormatRuntimeSegment::Implicit => {
                    if let Some(value) = args_iter.next() {
                        let mut wrapped = EvaluatedValue::from_value(value);
                        self.maybe_attach_runtime_handle(&mut wrapped);
                        self.print_value(wrapped)?;
                    }
                }
            }
        }
        self.emit_printf_call("\n", &mut []);
        Ok(())
    }

    fn print_value(&mut self, value: EvaluatedValue) -> Result<(), String> {
        let mut value = value;
        if let Some(handle) = value.runtime_handle().or_else(|| self.maybe_attach_runtime_handle(&mut value)) {
            self.emit_runtime_print(handle);
            return Ok(());
        }
        match value.into_value() {
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
                if pointer.mutable {
                    self.emit_printf_call("mut Pointer->", &mut []);
                } else {
                    self.emit_printf_call("Pointer->", &mut []);
                }
                let inner = pointer.cell.lock().unwrap().clone();
                self.print_value(inner)
            }
            Value::Struct(_) => Err("Cannot print struct value in build mode".into()),
            Value::Unit => {
                self.emit_printf_call("()", &mut []);
                Ok(())
            }
            Value::Reference(reference) => {
                self.emit_printf_call("&", &mut []);
                let inner = reference.cell.lock().unwrap().clone();
                self.print_value(inner)
            }
            Value::Boxed(_) | Value::Slice(_) | Value::Map(_) => {
                Err("Cannot print heap value in build mode".into())
            }
            Value::FormatTemplate(_) => Err("Format string must be printed via out()".into()),
            Value::Sender(_) | Value::Receiver(_) | Value::JoinHandle(_) => {
                Err("Cannot print concurrency values in build mode".into())
            }
            Value::Moved => Err("Cannot print moved value in build mode".into()),
        }
    }

    fn emit_runtime_print(&mut self, handle: LLVMValueRef) {
        let mut args = [handle];
        let name = CString::new("prime_print").unwrap();
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
                let constant = int_value
                    .constant()
                    .ok_or_else(|| "Non-constant int not yet supported in runtime codegen".to_string())?;
                let arg = unsafe { LLVMConstInt(self.runtime_abi.int_type, constant as u64, 1) };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_int_new,
                    self.runtime_abi.prime_int_new_ty,
                    &mut [arg],
                    "int_new",
                ))
            }
            Value::Float(float_value) => {
                let constant = float_value
                    .constant()
                    .ok_or_else(|| "Non-constant float not yet supported in runtime codegen".to_string())?;
                let arg = unsafe { LLVMConstReal(self.runtime_abi.float_type, constant) };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_float_new,
                    self.runtime_abi.prime_float_new_ty,
                    &mut [arg],
                    "float_new",
                ))
            }
            Value::Bool(flag) => {
                let arg = unsafe { LLVMConstInt(self.runtime_abi.bool_type, flag as u64, 0) };
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
                let start = range.start.constant().ok_or_else(|| "range start not constant".to_string())?;
                let end = range.end.constant().ok_or_else(|| "range end not constant".to_string())?;
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
                    (guard.queue.iter().cloned().collect::<Vec<_>>(), guard.closed)
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
                    (guard.queue.iter().cloned().collect::<Vec<_>>(), guard.closed)
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
            Value::Pointer(ptr) => {
                let inner = ptr.cell.lock().unwrap().clone().into_value();
                let target = self.build_runtime_handle(inner)?;
                let mut_flag = unsafe { LLVMConstInt(self.runtime_abi.bool_type, ptr.mutable as u64, 0) };
                Ok(self.call_runtime(
                    self.runtime_abi.prime_reference_new,
                    self.runtime_abi.prime_reference_new_ty,
                    &mut [target, mut_flag],
                    "ptr_new",
                ))
            }
            Value::Slice(slice) => {
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
                let inner = reference.cell.lock().unwrap().clone().into_value();
                let target = self.build_runtime_handle(inner)?;
                let mut_flag =
                    unsafe { LLVMConstInt(self.runtime_abi.bool_type, reference.mutable as u64, 0) };
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
        let name_c = CString::new(name).unwrap();
        let debug_handles = env::var("PRIME_DEBUG_RT_HANDLES").is_ok();
        assert!(!func.is_null(), "runtime function `{}` is null", name);
        assert!(!self.builder.is_null(), "LLVM builder is not initialized");
        unsafe {
            let block = LLVMGetInsertBlock(self.builder);
            assert!(!block.is_null(), "no insertion block for runtime call `{}`", name);
            if debug_handles {
                let parent_fn = LLVMGetBasicBlockParent(block);
                let func_mod = LLVMGetGlobalParent(func);
                let module_ctx = LLVMGetModuleContext(self.module);
                eprintln!(
                    "[rt_call] name={name} func_ptr={:?} func_mod={:?} current_mod={:?} block={:?} block_fn={:?} ctx={:?}",
                    func,
                    func_mod,
                    self.module,
                    block,
                    parent_fn,
                    module_ctx
                );
            }
        }
        unsafe {
            let mut fn_type = func_type;
            if fn_type.is_null() && LLVMGetTypeKind(LLVMTypeOf(func)) == LLVMTypeKind::LLVMPointerTypeKind {
                fn_type = LLVMGetElementType(LLVMTypeOf(func));
            }
            assert!(
                !fn_type.is_null() && LLVMGetTypeKind(fn_type) == LLVMTypeKind::LLVMFunctionTypeKind,
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
        let c_text =
            CString::new(text.as_bytes()).map_err(|_| "string contains interior null byte".to_string())?;
        let sym = CString::new(symbol).unwrap();
        let ptr = unsafe { LLVMBuildGlobalString(self.builder, c_text.as_ptr(), sym.as_ptr()) };
        let len = unsafe { LLVMConstInt(self.runtime_abi.usize_type, text.len() as u64, 0) };
        Ok((ptr, len))
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
                        match self.emit_expression(expr)? {
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
                        EvalOutcome::Value(value) => value.into_value(),
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
                            *reference.cell.lock().unwrap() = value;
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
                        EvalOutcome::Value(value) => value.into_value(),
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
                            Value::Int(self.const_int_value(current)).into(),
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
            Expr::Literal(Literal::Int(value, _)) => Ok(EvalOutcome::Value(self.evaluated(
                Value::Int(self.const_int_value(*value as i128)),
            ))),
            Expr::Literal(Literal::Bool(value, _)) => {
                Ok(EvalOutcome::Value(self.evaluated(Value::Bool(*value))))
            }
            Expr::Literal(Literal::Float(value, _)) => Ok(EvalOutcome::Value(self.evaluated(
                Value::Float(self.const_float_value(*value)),
            ))),
            Expr::Literal(Literal::String(value, _)) => {
                let string = self.build_string_constant(value.clone())?;
                Ok(EvalOutcome::Value(self.evaluated(string)))
            }
            Expr::FormatString(literal) => {
                let value = self.build_format_string_value(literal)?;
                Ok(EvalOutcome::Value(self.evaluated(value)))
            }
            Expr::Literal(Literal::Rune(value, _)) => Ok(EvalOutcome::Value(self.evaluated(
                Value::Int(self.const_int_value(*value as i128)),
            ))),
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
                variant,
                values,
                ..
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
                Ok(EvalOutcome::Value(self.evaluated(Value::Range(RangeValue {
                    start,
                    end,
                    inclusive: range.inclusive,
                }))))
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
                        Value::Pointer(pointer) => pointer.cell.lock().unwrap().clone().into_value(),
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
        match first.into_value() {
            Value::FormatTemplate(template) => {
                let provided: Vec<Value> = iter.map(EvaluatedValue::into_value).collect();
                if template.implicit_placeholders != provided.len() {
                    return Err(format!(
                        "Format string expects {} argument(s), got {}",
                        template.implicit_placeholders,
                        provided.len()
                    ));
                }
                self.emit_format_template(template, provided)?;
                Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
            }
            other => {
                if iter.next().is_some() {
                    return Err(
                        "out() with multiple arguments requires a format string literal".into(),
                    );
                }
                self.emit_out_value(other.into())?;
                Ok(EvalOutcome::Value(self.evaluated(Value::Unit)))
            }
        }
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
            "send" => Some(self.invoke_builtin(args, |this, values| this.builtin_send(values))),
            "recv" => Some(self.invoke_builtin(args, |this, values| this.builtin_recv(values))),
            "close" => Some(self.invoke_builtin(args, |this, values| this.builtin_close(values))),
            "join" => Some(self.invoke_builtin(args, |this, values| this.builtin_join(values))),
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
        f(self, evaluated)
            .map(|v| EvalOutcome::Value(self.evaluated(v)))
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
                Ok(EvalOutcome::Value(Value::Reference(ReferenceValue {
                    cell,
                    mutable,
                    origin: Some(ident.name.clone()),
                })
                .into()))
            }
            _ => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => {
                    Ok(EvalOutcome::Value(
                        Value::Reference(ReferenceValue {
                            cell: Arc::new(Mutex::new(value)),
                            mutable,
                            origin: None,
                        })
                        .into(),
                    ))
                }
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
        }
    }

    fn emit_array_literal(&mut self, values: &[Expr]) -> Result<EvalOutcome<EvaluatedValue>, String> {
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

    fn emit_move_expression(
        &mut self,
        expr: &Expr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
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
                let moved =
                    std::mem::replace(&mut *slot, EvaluatedValue::from_value(Value::Moved));
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
                    "Err" => Ok(EvalOutcome::Flow(FlowSignal::Propagate(
                        EvaluatedValue {
                            value: Value::Enum(enum_value),
                            runtime,
                        },
                    ))),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
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
            (Value::Str(a), Value::Str(b)) if matches!(op, BinaryOp::Eq | BinaryOp::NotEq) => {
                let cmp = (*a.text == *b.text) == matches!(op, BinaryOp::Eq);
                Ok(Value::Bool(cmp))
            }
            (other_l, other_r) => Err(format!(
                "Operation `{op:?}` not supported in build mode for {} and {}",
                describe_value(&other_l),
                describe_value(&other_r)
            )),
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
            (UnaryOp::Not, Value::Bool(flag)) => Ok(Value::Bool(!flag)),
            (op_variant, other) => Err(format!(
                "Operation `{op_variant:?}` not supported in build mode for {}",
                describe_value(&other)
            )),
        }
    }

    fn eval_bool_binary(&self, op: BinaryOp, lhs: bool, rhs: bool) -> Result<Value, String> {
        let value = match op {
            BinaryOp::And => Value::Bool(lhs && rhs),
            BinaryOp::Or => Value::Bool(lhs || rhs),
            BinaryOp::Eq => Value::Bool(lhs == rhs),
            BinaryOp::NotEq => Value::Bool(lhs != rhs),
            _ => {
                return Err(format!(
                    "Operation `{op:?}` not supported in build mode for booleans"
                ));
            }
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
        result.ok_or_else(|| {
            format!(
                "Operation `{op:?}` not supported in build mode for integers (non-constant operands)"
            )
        })
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
        result.ok_or_else(|| {
            format!(
                "Operation `{op:?}` not supported in build mode for floats (non-constant operands)"
            )
        })
    }

    fn int_to_float(&self, value: &IntValue) -> Result<FloatValue, String> {
        value
            .constant()
            .map(|constant| self.const_float_value(constant as f64))
            .ok_or_else(|| {
                "Operation `IntToFloat` not supported in build mode for non-constant integers"
                    .into()
            })
    }

    fn deref_if_reference(value: Value) -> Value {
        match value {
            Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
            other => other,
        }
    }

    fn expect_int(&self, value: Value) -> Result<IntValue, String> {
        match value {
            Value::Int(v) => Ok(v),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_int(inner)
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone().into_value();
                self.expect_int(inner)
            }
            other => Err(format!(
                "Expected integer value in build mode, got {}",
                describe_value(&other)
            )),
        }
    }

    fn expect_int_value_from_expr(&mut self, expr: &Expr) -> Result<IntValue, String> {
        match self.emit_expression(expr)? {
            EvalOutcome::Value(value) => self.expect_int(value.into_value()),
            EvalOutcome::Flow(_) => Err("control flow not allowed here".into()),
        }
    }

    fn expect_float(&self, value: Value) -> Result<FloatValue, String> {
        match value {
            Value::Float(v) => Ok(v),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_float(inner)
            }
            other => Err(format!(
                "Expected float value in build mode, got {}",
                describe_value(&other)
            )),
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
        let receiver_args: Vec<Value> =
            evaluated_args.iter().map(|v| v.value().clone()).collect();
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
                            Value::Tuple(values) => Ok(values
                                .into_iter()
                                .map(|v| self.evaluated(v))
                                .collect()),
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
                let inner = reference.cell.lock().unwrap();
                self.value_struct_name(inner.value())
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap();
                self.value_struct_name(inner.value())
            }
            Value::Boxed(inner) => {
                let value = inner.cell.lock().unwrap();
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
        match value {
            Value::Reference(reference) => {
                if reference.mutable {
                    if let Some(origin) = &reference.origin {
                        self.begin_mut_borrow_in_scope(origin, scope_index)?;
                    }
                }
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.track_reference_borrow_in_scope(named, scope_index)?;
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
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.release_reference_borrow(named);
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

    fn expect_box_value(&self, value: Value, ctx: &str) -> Result<BoxValue, String> {
        match value {
            Value::Boxed(b) => Ok(b),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_box_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects Box value")),
        }
    }

    fn expect_slice_value(&self, value: Value, ctx: &str) -> Result<SliceValue, String> {
        match value {
            Value::Slice(slice) => Ok(slice),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_slice_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects slice value")),
        }
    }

    fn expect_map_value(&self, value: Value, ctx: &str) -> Result<MapValue, String> {
        match value {
            Value::Map(map) => Ok(map),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_map_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects map value")),
        }
    }

    fn expect_sender(&self, value: Value, ctx: &str) -> Result<ChannelSender, String> {
        match value {
            Value::Sender(tx) => Ok(tx),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_sender(inner, ctx)
            }
            _ => Err(format!("{ctx} expects Sender value")),
        }
    }

    fn expect_receiver(&self, value: Value, ctx: &str) -> Result<ChannelReceiver, String> {
        match value {
            Value::Receiver(rx) => Ok(rx),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                self.expect_receiver(inner, ctx)
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

    fn builtin_box_get(&self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_get expects 1 argument".into());
        }
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_get")?;
        Ok(boxed.cell.lock().unwrap().clone())
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
                forwarded.push(reference.cell.lock().unwrap().clone().into_value());
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

    fn builtin_assert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("assert expects 1 argument".into());
        }
        let cond = self.value_to_bool(args.pop().unwrap())?;
        if !cond {
            return Err("assertion failed".into());
        }
        Ok(Value::Unit)
    }

    fn builtin_expect(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("expect expects 2 arguments".into());
        }
        let cond = self.value_to_bool(args.remove(0))?;
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
        Ok(Value::Bool(haystack.contains(&needle)))
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

    fn builtin_channel(&mut self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err("channel expects 0 arguments".into());
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
        match receiver.recv() {
            Some(value) => self.instantiate_enum_variant("Some", vec![value]),
            None => self.instantiate_enum_variant("None", Vec::new()),
        }
    }

    fn builtin_close(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("close expects 1 argument".into());
        }
        match args.pop().unwrap() {
            Value::Sender(tx) => {
                tx.close();
                Ok(Value::Unit)
            }
            Value::Receiver(rx) => {
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
            })),
            Value::Pointer(ptr) => Ok(Value::Pointer(PointerValue {
                cell: ptr.cell,
                mutable: ptr.mutable || mutable,
            })),
            other => Err(format!(
                "{name} expects reference or pointer, found {}",
                describe_value(&other)
            )),
        }
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
            Ok(BlockEval::Value(self.evaluated(Value::Unit)))
        }
    }

    fn assign_var(&mut self, name: &str, value: EvaluatedValue) -> Result<(), String> {
        for index in (0..self.scopes.len()).rev() {
            if let Some(binding) = self.scopes[index].get(name) {
                if !binding.mutable {
                    return Err(format!("Variable {} is immutable", name));
                }
                let cell = binding.cell.clone();
                self.track_reference_borrow_in_scope(value.value(), index)?;
                {
                    let current = cell.lock().unwrap();
                    self.release_reference_borrow(current.value());
                }
                *cell.lock().unwrap() = value;
                return Ok(());
            }
        }
        Err(format!("Unknown variable {}", name))
    }

    fn value_to_bool(&self, value: Value) -> Result<bool, String> {
        let concrete = match value {
            Value::Reference(reference) => reference.cell.lock().unwrap().clone().into_value(),
            Value::Pointer(pointer) => pointer.cell.lock().unwrap().clone().into_value(),
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
        let start_value = match self.emit_expression(&range.start)? {
            EvalOutcome::Value(value) => self.expect_int(value.into_value())?,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let end_value = match self.emit_expression(&range.end)? {
            EvalOutcome::Value(value) => self.expect_int(value.into_value())?,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
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

    fn snapshot_build_state(&self) -> Result<BuildSnapshot, String> {
        let mut scopes = Vec::with_capacity(self.scopes.len());
        for scope in &self.scopes {
            let mut bindings = HashMap::new();
            for (name, binding) in scope {
                let guard = binding
                    .cell
                    .lock()
                    .map_err(|_| format!("Binding `{name}` poisoned while capturing build snapshot"))?;
                bindings.insert(
                    name.clone(),
                    BuildBinding {
                        cell: Arc::new(Mutex::new(value_to_build_value(guard.value())?)),
                        mutable: binding.mutable,
                        borrowed_mut: false,
                        borrowed_shared: 0,
                        borrowed_shared_names: HashSet::new(),
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
        Ok(BuildSnapshot {
            scopes,
            enum_variants,
            functions,
            struct_fields,
        })
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

    fn insert_var(
        &mut self,
        name: &str,
        value: EvaluatedValue,
        mutable: bool,
    ) -> Result<(), String> {
        let scope_index = self.scopes.len().saturating_sub(1);
        let cell = Arc::new(Mutex::new(value));
        {
            let stored = cell.lock().unwrap();
            self.track_reference_borrow_in_scope(stored.value(), scope_index)?;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), Binding { cell, mutable });
        }
        Ok(())
    }

    fn get_var(&self, name: &str) -> Option<EvaluatedValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
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

            self.runtime_abi = RuntimeAbi::declare(self.context, self.module);
        }
    }

    fn collect_iterable_values(&mut self, value: Value) -> Result<Vec<EvaluatedValue>, String> {
        match value {
            Value::Range(range) => {
                let start_const = range
                    .start
                    .constant
                    .ok_or_else(|| "Range bounds must be constant in build mode".to_string())?;
                let end_const = range
                    .end
                    .constant
                    .ok_or_else(|| "Range bounds must be constant in build mode".to_string())?;
                let end = if range.inclusive { end_const + 1 } else { end_const };
                Ok((start_const..end)
                    .map(|v| Value::Int(self.const_int_value(v)).into())
                    .collect())
            }
            Value::Slice(slice) => {
                let mut items = Vec::new();
                for idx in 0..slice.len() {
                    if let Some(item) = slice.get(idx) {
                        items.push(item.into());
                    }
                }
                Ok(items)
            }
            Value::Map(map) => {
                let mut items = Vec::new();
                for (key, value) in map.entries.lock().unwrap().iter() {
                    let key_value = self.make_string_value(key)?;
                    items.push(Value::Tuple(vec![key_value, value.clone()]).into());
                }
                Ok(items)
            }
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
            Value::Pointer(_) => "pointer",
            Value::Range(_) => "range",
            Value::JoinHandle(_) => "join handle",
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
        Value::FormatTemplate(_) => "format string",
        Value::Sender(_) => "channel sender",
        Value::Receiver(_) => "channel receiver",
        Value::Range(_) => "range",
        Value::Pointer(_) => "pointer",
        Value::JoinHandle(_) => "join handle",
        Value::Moved => "moved",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::{ast::Program, parser::parse_module};
    use crate::language::span::Span;
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
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Int(IntValue::new(
                ptr::null_mut(),
                Some(7),
            ))))),
            mutable: false,
        };
        let tuple_binding = Binding {
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Tuple(vec![
                Value::Bool(true),
                Value::Int(IntValue::new(ptr::null_mut(), Some(2))),
            ])))),
            mutable: true,
        };
        let string_binding = Binding {
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Str(StringValue::new(
                ptr::null_mut(),
                Arc::new("hello".to_string()),
            ))))),
            mutable: false,
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
            cell: Arc::new(Mutex::new(EvaluatedValue::from_value(Value::Int(IntValue::new(
                ptr::null_mut(),
                None,
            ))))),
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
            let joined = compiler
                .builtin_join(vec![handle])
                .expect("join returns");
            assert!(matches!(joined, Value::Int(v) if v.constant() == Some(9)));
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
            let outer = match compiler.emit_expression(&expr).expect("outer spawn evaluates") {
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
        let rx = ChannelReceiver::new_with_state(
            channels.get(&7).cloned().expect("channel present"),
        );
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
        compile_entry("borrow_demo.prime").expect("compile borrow demo in build mode");
    }

    #[test]
    fn pattern_demo_compiles() {
        compile_entry("pattern_demo.prime").expect("compile pattern demo in build mode");
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
}
