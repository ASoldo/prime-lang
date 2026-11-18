//! Host async blocks run in a separate entry function. Only runtime values cross
//! that boundary. The compiler keeps a type/shape template for ordinary `await`.
use super::*;

impl Compiler {
    pub(super) fn async_channel_handle(
        &mut self,
        channel: &ChannelHandle,
        existing: Option<LLVMValueRef>,
        sender: bool,
    ) -> Result<LLVMValueRef, String> {
        if let Some(handle) = existing {
            return Ok(handle);
        }
        let mut state = channel.0.lock().unwrap();
        let pair = match state.runtime_pair {
            Some(pair) => pair,
            None => {
                let pair = self.build_channel_handles().ok_or("missing channel ABI")?;
                self.seed_channel_queue(pair.0, state.queue.make_contiguous())?;
                if state.closed {
                    self.runtime_close(pair.0);
                }
                state.runtime_pair = Some(pair);
                pair
            }
        };
        Ok(if sender { pair.0 } else { pair.1 })
    }

    fn task_record(&mut self, fields: &[LLVMValueRef], tag: LLVMValueRef) -> LLVMValueRef {
        let ptr = if fields.is_empty() {
            self.null_handle_ptr()
        } else {
            self.alloc_handle_array(fields)
        };
        let len = unsafe { LLVMConstInt(self.runtime_abi.usize_type, fields.len() as u64, 0) };
        self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut [ptr, len, tag],
            "task_record",
        )
    }

    fn task_field(&mut self, handle: LLVMValueRef, index: usize) -> LLVMValueRef {
        let index = unsafe { LLVMConstInt(self.runtime_abi.usize_type, index as u64, 0) };
        self.call_runtime(
            self.runtime_abi.prime_enum_get,
            self.runtime_abi.prime_enum_get_ty,
            &mut [handle, index],
            "task_field",
        )
    }

    pub(super) fn task_value_handle(
        &mut self,
        value: &EvaluatedValue,
    ) -> Result<LLVMValueRef, String> {
        // An enum already produced by the runtime may have no static payload.
        // Use the real value instead of boxing its compiler placeholder.
        match value.value() {
            Value::Sender(tx) => self.async_channel_handle(&tx.inner, tx.handle, true),
            Value::Receiver(rx) => self.async_channel_handle(&rx.inner, rx.handle, false),
            Value::Task(task) => task
                .handle
                .ok_or_else(|| "cannot send a build-only task to a host task".into()),
            Value::Tuple(items) => {
                let handles = items
                    .iter()
                    .map(|v| self.task_value_handle(&v.clone().into()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(self.task_record(&handles, unsafe { LLVMConstNull(self.i32_type) }))
            }
            Value::Struct(s) => {
                let fields: BTreeMap<_, _> = s.fields.iter().collect();
                let handles = fields
                    .values()
                    .map(|v| self.task_value_handle(&(*v).clone().into()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(self.task_record(&handles, unsafe { LLVMConstNull(self.i32_type) }))
            }
            _ if value.runtime_handle().is_some() => Ok(value.runtime_handle().unwrap()),
            other => self.build_runtime_handle(other.clone()).map_err(|err| {
                format!(
                    "cannot transfer {} across a host task boundary: {err}",
                    describe_value(other)
                )
            }),
        }
    }

    pub(super) fn task_value_from_handle(
        &mut self,
        template: &Value,
        handle: LLVMValueRef,
        preserve_constants: bool,
    ) -> Result<EvaluatedValue, String> {
        let value = match template {
            Value::Unit => Value::Unit,
            Value::Int(int) => {
                let llvm = self.call_runtime(
                    self.runtime_abi.prime_value_as_int,
                    self.runtime_abi.prime_value_as_int_ty,
                    &mut [handle],
                    "task_int",
                );
                let cast = unsafe {
                    LLVMBuildIntCast(
                        self.builder,
                        llvm,
                        LLVMTypeOf(int.llvm()),
                        c"task_int_cast".as_ptr(),
                    )
                };
                Value::Int(IntValue::new(
                    cast,
                    int.constant().filter(|_| preserve_constants),
                ))
            }
            Value::Float(float) => {
                let llvm = self.call_runtime(
                    self.runtime_abi.prime_value_as_float,
                    self.runtime_abi.prime_value_as_float_ty,
                    &mut [handle],
                    "task_float",
                );
                Value::Float(FloatValue::new(
                    llvm,
                    float.constant().filter(|_| preserve_constants),
                ))
            }
            Value::Bool(flag) => {
                let llvm = self.call_runtime(
                    self.runtime_abi.prime_value_as_bool,
                    self.runtime_abi.prime_value_as_bool_ty,
                    &mut [handle],
                    "task_bool",
                );
                Value::Bool(BoolValue::new(
                    llvm,
                    flag.constant().filter(|_| preserve_constants),
                ))
            }
            Value::Str(s)
                if preserve_constants && unsafe { llvm_sys::core::LLVMIsConstant(s.llvm) != 0 } =>
            {
                Value::Str(s.clone())
            }
            Value::Str(_) | Value::Reference(_) => Value::Reference(ReferenceValue {
                slot: None,
                cell: Rc::new(Mutex::new(Value::Unit.into())),
                mutable: false,
                origin: None,
                handle: Some(handle),
            }),
            Value::Enum(e) => Value::Enum(EnumValue {
                enum_name: e.enum_name.clone(),
                variant: "<runtime>".into(),
                variant_index: 0,
                values: Vec::new(),
            }),
            Value::Task(task) => {
                Value::Task(Box::new(TaskValue::with_handle(handle, task.kind.clone())))
            }
            Value::CancelToken(_) => Value::CancelToken(CancelTokenValue::with_handle(handle)),
            Value::Sender(_) => Value::Sender(ChannelSender::with_handle(handle)),
            Value::Receiver(_) => Value::Receiver(ChannelReceiver::with_handle(handle)),
            Value::Slice(_) => Value::Slice(SliceValue::with_handle(handle)),
            Value::Map(_) => Value::Map(MapValue::with_handle(handle)),
            Value::Boxed(_) => Value::Boxed(BoxValue::with_handle(handle)),
            Value::Tuple(items) => {
                let mut decoded = Vec::with_capacity(items.len());
                for (idx, field) in items.iter().enumerate() {
                    let field_handle = self.task_field(handle, idx);
                    decoded.push(
                        self.task_value_from_handle(field, field_handle, preserve_constants)?
                            .into_value(),
                    );
                }
                Value::Tuple(decoded)
            }
            Value::Struct(s) => {
                let fields: BTreeMap<_, _> = s.fields.iter().collect();
                let mut decoded = HashMap::new();
                for (idx, (name, field)) in fields.into_iter().enumerate() {
                    let field_handle = self.task_field(handle, idx);
                    decoded.insert(
                        name.clone(),
                        self.task_value_from_handle(field, field_handle, preserve_constants)?
                            .into_value(),
                    );
                }
                Value::Struct(StructValue::new(s.name.clone(), decoded))
            }
            other => {
                return Err(format!(
                    "cannot decode {} across a host task boundary",
                    describe_value(other)
                ));
            }
        };
        Ok(EvaluatedValue::from_value(value).with_runtime(handle))
    }

    pub(super) fn emit_host_async(
        &mut self,
        block: &Block,
        mut capture_names: Vec<String>,
    ) -> Result<TaskValue, String> {
        self.ensure_runtime_symbols();
        capture_names.sort();
        let mut captures = Vec::new();
        let mut handles = Vec::new();
        for name in capture_names {
            if let Some(value) = self.get_var(&name) {
                if self.get_binding(&name).is_some_and(|binding| binding.mutable)
                    || matches!(value.value(), Value::Reference(r) if r.mutable)
                    || matches!(value.value(), Value::Pointer(p) if p.mutable)
                {
                    return Err(format!(
                        "native async blocks cannot yet share mutable capture `{name}`; capture an immutable value or use a channel"
                    ));
                }
                handles.push(self.task_value_handle(&value)?);
                captures.push((name, value));
            }
        }
        let env = self.task_record(&handles, unsafe { LLVMConstNull(self.i32_type) });
        let caller = unsafe { LLVMGetInsertBlock(self.builder) };
        let function = unsafe {
            let mut params = [self.runtime_abi.handle_type];
            let ty = LLVMFunctionType(self.runtime_abi.handle_type, params.as_mut_ptr(), 1, 0);
            let function = LLVMAddFunction(self.module, c"prime.async".as_ptr(), ty);
            LLVMSetLinkage(function, LLVMLinkage::LLVMPrivateLinkage);
            let entry = LLVMAppendBasicBlockInContext(self.context, function, c"entry".as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, entry);
            function
        };
        let outer_task = self.host_task_entry.replace(function);
        let loop_controls = mem::take(&mut self.loop_controls);
        let lexical_scopes = mem::take(&mut self.scopes);
        let env_param = unsafe { LLVMGetParam(function, 0) };
        let mut bindings = Vec::new();
        for (index, (name, original)) in captures.iter().enumerate() {
            let handle = self.task_field(env_param, index);
            bindings.push((
                name.clone(),
                self.task_value_from_handle(original.value(), handle, true)?,
                false,
            ));
        }
        self.runtime_release(env_param);
        self.push_return_types(&[]);
        let result = self.eval_async_block(block, &bindings)?;
        self.pop_return_types();
        let output = self.task_value_handle(&result)?;
        unsafe {
            LLVMBuildRet(self.builder, output);
            LLVMPositionBuilderAtEnd(self.builder, caller);
        }
        self.scopes = lexical_scopes;
        self.loop_controls = loop_controls;
        self.host_task_entry = outer_task;
        let task = self.call_runtime(
            self.runtime_abi.prime_task_new,
            self.runtime_abi.prime_task_new_ty,
            &mut [function, env],
            "async_task",
        );
        Ok(TaskValue::with_handle(
            task,
            TaskResultKind::Host(Box::new(result.into_value())),
        ))
    }

    fn wait_host_task(
        &mut self,
        task: LLVMValueRef,
        token: Option<LLVMValueRef>,
        millis: Option<&IntValue>,
    ) -> Result<(LLVMValueRef, LLVMValueRef), String> {
        let ty = self.runtime_abi.int_type;
        let timeout = if let Some(millis) = millis {
            let millis = if let Some(constant) = millis.constant() {
                unsafe { LLVMConstInt(ty, constant as u64, 1) }
            } else {
                self.int_to_runtime_int(millis, "await_millis")?
            };
            unsafe {
                let zero = LLVMConstNull(ty);
                let negative = LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                    millis,
                    zero,
                    c"negative".as_ptr(),
                );
                LLVMBuildSelect(self.builder, negative, zero, millis, c"timeout".as_ptr())
            }
        } else {
            unsafe { LLVMConstInt(ty, u64::MAX, 1) }
        };
        let slot = self.build_entry_alloca(self.runtime_abi.handle_type, "task_out");
        let token = token.unwrap_or_else(|| unsafe { LLVMConstNull(self.runtime_abi.handle_type) });
        let status = self.call_runtime(
            self.runtime_abi.prime_host_task_wait,
            self.runtime_abi.prime_host_task_wait_ty,
            &mut [task, token, timeout, slot],
            "await_status",
        );
        let payload = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                slot,
                c"task_payload".as_ptr(),
            )
        };
        let ok = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                status,
                LLVMConstNull(self.runtime_abi.status_type),
                c"task_ok".as_ptr(),
            )
        };
        Ok((ok, payload))
    }

    pub(super) fn emit_host_guarded_await(
        &mut self,
        task: &TaskValue,
        token: Option<&CancelTokenValue>,
        millis: Option<&IntValue>,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let (is_ok, payload) = match task.kind {
            TaskResultKind::Ready => {
                let value = task.take(self)?;
                (
                    unsafe { LLVMConstInt(self.runtime_abi.bool_type, 1, 0) },
                    self.task_value_handle(&value)?,
                )
            }
            _ => {
                let handle = task
                    .handle
                    .ok_or("cannot guard a task without runtime backing")?;
                let token = token
                    .map(|token| token.handle.ok_or("cancel token missing runtime backing"))
                    .transpose()?;
                self.wait_host_task(handle, token, millis)?
            }
        };
        let ok_tag = self
            .enum_variants
            .get("Ok")
            .ok_or("missing Result::Ok")?
            .variant_index;
        let err_tag = self
            .enum_variants
            .get("Err")
            .ok_or("missing Result::Err")?
            .variant_index;
        let tag = unsafe {
            LLVMBuildSelect(
                self.builder,
                is_ok,
                LLVMConstInt(self.i32_type, ok_tag.into(), 0),
                LLVMConstInt(self.i32_type, err_tag.into(), 0),
                c"await_tag".as_ptr(),
            )
        };
        let result = self.host_result_handle(payload, tag, ok_tag);
        // The runtime wait transfers ownership. The Result has copied the value.
        if !matches!(task.kind, TaskResultKind::Ready) {
            self.runtime_release(payload);
        }
        Ok(EvalOutcome::Value(
            EvaluatedValue::from_value(Value::Enum(EnumValue {
                enum_name: "Result".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }))
            .with_runtime(result),
        ))
    }

    pub(super) fn join_host_task(
        &mut self,
        task: LLVMValueRef,
        template: &Value,
    ) -> Result<EvaluatedValue, String> {
        let (ok, payload) = self.wait_host_task(task, None, None)?;
        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        unsafe {
            let success =
                LLVMAppendBasicBlockInContext(self.context, function, c"task_ready".as_ptr());
            let failure =
                LLVMAppendBasicBlockInContext(self.context, function, c"task_failed".as_ptr());
            LLVMBuildCondBr(self.builder, ok, success, failure);
            LLVMPositionBuilderAtEnd(self.builder, failure);
            if self.host_task_entry == Some(function) {
                self.return_host_task_error(payload);
            } else {
                self.call_runtime(
                    self.runtime_abi.prime_host_task_abort,
                    self.runtime_abi.prime_host_task_abort_ty,
                    &mut [payload],
                    "task_abort",
                );
                llvm_sys::core::LLVMBuildUnreachable(self.builder);
            }
            LLVMPositionBuilderAtEnd(self.builder, success);
        }
        self.task_value_from_handle(template, payload, true)
    }

    pub(super) fn in_host_task(&self) -> bool {
        self.host_task_entry.is_some_and(|entry| unsafe {
            entry == LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder))
        })
    }

    /// Emit a task-local failure exit, then resume code generation on the
    /// successful path (unreachable for an unconditional panic).
    pub(super) fn host_task_check(
        &mut self,
        success: LLVMValueRef,
        message: &str,
    ) -> Result<(), String> {
        let function = self.host_task_entry.ok_or("no host task entry")?;
        unsafe {
            let resume =
                LLVMAppendBasicBlockInContext(self.context, function, c"task_continue".as_ptr());
            let fail =
                LLVMAppendBasicBlockInContext(self.context, function, c"task_panic".as_ptr());
            LLVMBuildCondBr(self.builder, success, resume, fail);
            LLVMPositionBuilderAtEnd(self.builder, fail);
            let message = self.make_string_value(&format!("Runtime panic: {message}"))?;
            let handle = self.build_runtime_handle(message)?;
            self.return_host_task_error(handle);
            LLVMPositionBuilderAtEnd(self.builder, resume);
        }
        Ok(())
    }

    pub(super) fn return_host_task_error(&mut self, message: LLVMValueRef) {
        let error = self.call_runtime(
            self.runtime_abi.prime_host_task_error,
            self.runtime_abi.prime_host_task_error_ty,
            &mut [message],
            "task_error",
        );
        unsafe { LLVMBuildRet(self.builder, error) };
    }
}
