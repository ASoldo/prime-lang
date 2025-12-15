use super::*;

impl Compiler {
    pub(super) fn execute_block_contents(&mut self, block: &Block) -> Result<BlockEval, String> {
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
                    Statement::Comment { .. } => "comment",
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
                        returns.first()
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

    pub(super) fn assign_var(&mut self, name: &str, value: EvaluatedValue) -> Result<(), String> {
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
                Self::clear_scalar_constant(&mut updated);
                if let Some(slot) = slot {
                    let int_val = match updated.value() {
                        Value::Int(int_val) => Some(int_val.clone()),
                        _ => None,
                    };
                    if let Some(int_val) = int_val {
                        unsafe {
                            let slot_elem_ty = LLVMGetAllocatedType(slot);
                            let value_ty = LLVMTypeOf(int_val.llvm());
                            let stored = if value_ty != slot_elem_ty {
                                LLVMBuildIntCast(
                                    self.builder,
                                    int_val.llvm(),
                                    slot_elem_ty,
                                    CString::new(format!("{name}_cast")).unwrap().as_ptr(),
                                )
                            } else {
                                int_val.llvm()
                            };
                            LLVMBuildStore(self.builder, stored, slot);
                            if value_ty != slot_elem_ty {
                                if let Value::Int(int_mut) = updated.value_mut() {
                                    *int_mut = IntValue::new(stored, None);
                                }
                            }
                        }
                    }
                }
                *cell.lock().unwrap() = updated;
                return Ok(());
            }
        }
        Err(format!("Unknown variable {}", name))
    }

    pub(super) fn value_to_bool(&mut self, value: Value) -> Result<BoolValue, String> {
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
                    let zero = unsafe { LLVMConstReal(LLVMTypeOf(float_value.llvm()), 0.0) };
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

    pub(super) fn snapshot_build_state(&self) -> Result<BuildSnapshot, String> {
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
                    .map(|action| match action {
                        CleanupAction::Defer(expr) => BuildCleanup::Defer(expr.clone()),
                        CleanupAction::Drop(record) => BuildCleanup::Drop(BuildDropRecord {
                            binding: record.binding.clone(),
                            type_name: record.type_name.clone(),
                        }),
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
            embedded: self.target.is_embedded(),
        })
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.borrow_frames.push(Vec::new());
        self.cleanup_stack.push(Vec::new());
    }

    pub(super) fn pop_scope(&mut self) {
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

    pub(super) fn push_return_types(&mut self, returns: &[TypeExpr]) {
        self.return_type_stack.push(returns.to_vec());
    }

    pub(super) fn pop_return_types(&mut self) {
        self.return_type_stack.pop();
    }

    pub(super) fn current_return_types(&self) -> Option<&[TypeExpr]> {
        self.return_type_stack.last().map(|v| v.as_slice())
    }

    pub(super) fn run_cleanups(&mut self) -> Result<(), String> {
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

    pub(super) fn exit_scope(&mut self) -> Result<(), String> {
        self.run_cleanups()?;
        self.pop_scope();
        Ok(())
    }

    pub(super) fn run_drop(&mut self, record: DropRecord) -> Result<(), String> {
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

    pub(super) fn insert_var(
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
        let cell = Rc::new(Mutex::new(value));
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

    pub(super) fn attach_origin(value: &mut EvaluatedValue, name: &str) {
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

    pub(super) fn clear_scalar_constant(value: &mut EvaluatedValue) {
        match value.value_mut() {
            Value::Int(int_value) => int_value.constant = None,
            Value::Float(float_value) => float_value.constant = None,
            Value::Bool(bool_value) => bool_value.constant = None,
            _ => {}
        }
    }

    pub(super) fn get_var(&mut self, name: &str) -> Option<EvaluatedValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                if let Some(slot) = binding.slot {
                    let guard = binding.cell.lock().unwrap().clone();
                    if let Value::Int(int_val) = guard.value() {
                        let constant = int_val.constant();
                        unsafe {
                            let loaded = LLVMBuildLoad2(
                                self.builder,
                                LLVMTypeOf(int_val.llvm()),
                                slot,
                                CString::new(format!("{name}_load")).unwrap().as_ptr(),
                            );
                            return Some(
                                self.evaluated(Value::Int(IntValue::new(loaded, constant))),
                            );
                        }
                    }
                }
                return Some(binding.cell.lock().unwrap().clone());
            }
        }
        None
    }

    pub(super) fn get_binding(&self, name: &str) -> Option<(Rc<Mutex<EvaluatedValue>>, bool)> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some((binding.cell.clone(), binding.mutable));
            }
        }
        None
    }

    pub(super) fn reset_module(&mut self) {
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

    pub(super) fn collect_iterable_values(
        &mut self,
        value: Value,
    ) -> Result<Vec<EvaluatedValue>, String> {
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
                            cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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
                            cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                            mutable: false,
                            origin: None,
                            handle: Some(key_handle),
                        };
                        let value_ref = ReferenceValue {
                            cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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
                if let Some(struct_name) = Self::value_struct_name(&value) {
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

    pub(super) fn describe_value(&self, value: &Value) -> &'static str {
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
            Value::CancelToken(_) => "cancel token",
            Value::Unit => "unit",
            Value::Moved => "moved value",
        }
    }
}
