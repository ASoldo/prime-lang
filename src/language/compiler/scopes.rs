use super::*;

impl Compiler {
    pub(super) fn retain_handle(&mut self, handle: LLVMValueRef) -> LLVMValueRef {
        self.call_runtime(
            self.runtime_abi.prime_value_retain,
            self.runtime_abi.prime_value_retain_ty,
            &mut [handle],
            "retained_value",
        )
    }

    // Initialize in the entry block so cleanup is valid at a control-flow merge,
    // including paths on which the owning expression never ran.
    pub(super) fn null_handle_slot(&mut self, name: &str) -> LLVMValueRef {
        let ty = self.runtime_abi.handle_type;
        let slot = self.build_entry_alloca(ty, name);
        unsafe {
            let init = LLVMCreateBuilderInContext(self.context);
            let next = llvm_sys::core::LLVMGetNextInstruction(slot);
            if next.is_null() {
                let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                LLVMPositionBuilderAtEnd(init, LLVMGetFirstBasicBlock(function));
            } else {
                LLVMPositionBuilderBefore(init, next);
            }
            LLVMBuildStore(init, LLVMConstNull(ty), slot);
            LLVMDisposeBuilder(init);
        }
        slot
    }

    fn clear_token_slot(&mut self, slot: LLVMValueRef) {
        unsafe {
            let ty = self.runtime_abi.handle_type;
            let old = LLVMBuildLoad2(self.builder, ty, slot, c"old_token".as_ptr());
            LLVMBuildStore(self.builder, LLVMConstNull(ty), slot);
            self.runtime_release(old);
        }
    }

    pub(super) fn track_owned_token(
        &mut self,
        token: &CancelTokenValue,
        handle: LLVMValueRef,
    ) -> LLVMValueRef {
        let slot = self.null_handle_slot("token_owner");
        self.clear_token_slot(slot);
        unsafe {
            LLVMBuildStore(self.builder, handle, slot);
        }
        self.token_owners.push(TokenOwner {
            lifetime: Arc::downgrade(&token.cancelled),
            loop_depth: self.loop_controls.len(),
            early_exits: Vec::new(),
            slot,
        });
        slot
    }

    pub(super) fn copy_token(&mut self, handle: LLVMValueRef) -> Value {
        let owned = self.retain_handle(handle);
        let token = CancelTokenValue::with_handle(owned);
        if self.target.is_embedded() {
            self.track_owned_token(&token, owned);
        }
        Value::CancelToken(token)
    }

    pub(super) fn read_token_binding(&mut self, value: EvaluatedValue) -> EvaluatedValue {
        if let Value::CancelToken(token) = value.value()
            && let Some(slot) = token.binding_slot
        {
            let loaded = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    c"token_read".as_ptr(),
                )
            };
            return self.copy_token(loaded).into();
        }
        value
    }

    pub(super) fn store_cell(&mut self, cell: &Mutex<EvaluatedValue>, value: EvaluatedValue) {
        let mut current = cell.lock().unwrap();
        if let Value::CancelToken(token) = current.value()
            && let Some(slot) = token.binding_slot
            && let Value::CancelToken(new) = value.value()
            && let Some(handle) = new.handle
        {
            let owned = self.retain_handle(handle);
            self.clear_token_slot(slot);
            unsafe {
                LLVMBuildStore(self.builder, owned, slot);
            }
        } else {
            *current = value;
        }
    }

    pub(super) fn token_loop_exit(&mut self, control: LoopControl, branch: LLVMValueRef) {
        for owner in &mut self.token_owners {
            if owner.loop_depth >= self.loop_controls.len() {
                owner.early_exits.push((control.break_block, branch));
            }
        }
    }

    pub(super) fn release_dead_tokens(&mut self) {
        if self.token_owners.is_empty() {
            return;
        }
        unsafe {
            let block = LLVMGetInsertBlock(self.builder);
            let terminator = LLVMGetBasicBlockTerminator(block);
            if !terminator.is_null() {
                LLVMPositionBuilderBefore(self.builder, terminator);
            }
            let mut owners = mem::take(&mut self.token_owners);
            owners.retain(|owner| {
                if owner.lifetime.strong_count() > 0 {
                    return true;
                }
                for (destination, branch) in &owner.early_exits {
                    if *branch != terminator
                        && (self
                            .loop_controls
                            .iter()
                            .any(|c| c.break_block == *destination)
                            || self
                                .exits
                                .iter()
                                .any(|exit| exit.join == Some(*destination)))
                    {
                        LLVMPositionBuilderBefore(self.builder, *branch);
                        self.clear_token_slot(owner.slot);
                    }
                }
                if terminator.is_null() {
                    LLVMPositionBuilderAtEnd(self.builder, block);
                } else {
                    LLVMPositionBuilderBefore(self.builder, terminator);
                }
                self.clear_token_slot(owner.slot);
                false
            });
            self.token_owners = owners;
            LLVMPositionBuilderAtEnd(self.builder, block);
        }
    }

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
            let flow = self.emit_statement(statement)?;
            self.release_dead_tokens();
            if let Some(flow) = flow {
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

    fn store_enum_handle(
        &mut self,
        slot: LLVMValueRef,
        value: &EvaluatedValue,
    ) -> Result<(), String> {
        let handle = self.task_value_handle(value)?;
        if self.target.is_embedded() {
            self.copy_embedded_handle(slot, handle);
        } else {
            unsafe {
                LLVMBuildStore(self.builder, handle, slot);
            }
        }
        Ok(())
    }

    pub(super) fn assign_var(&mut self, name: &str, value: EvaluatedValue) -> Result<(), String> {
        let (index, binding) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|(index, scope)| scope.get(name).cloned().map(|binding| (index, binding)))
            .ok_or_else(|| format!("Unknown variable {name}"))?;
        if !binding.mutable {
            return Err(format!("Variable {name} is immutable"));
        }
        self.track_reference_borrow_in_scope(value.value(), index, false)?;
        self.release_reference_borrow(binding.cell.lock().unwrap().value(), index);
        self.store_binding_value(&binding.cell, binding.slot, value)
    }

    pub(super) fn initialize_array(
        &mut self,
        array: FixedArray,
        slice: &SliceValue,
    ) -> Result<(), String> {
        if array.len != slice.len() {
            return Err("fixed array length mismatch".into());
        }
        for (i, value) in slice.items.lock().unwrap().iter().enumerate() {
            let index = unsafe { LLVMConstInt(self.runtime_abi.usize_type, i as u64, 0) };
            let slot = array.element(self.builder, index);
            self.store_slot(slot, value.clone().into())?;
        }
        Ok(())
    }

    pub(super) fn store_binding_value(
        &mut self,
        cell: &Mutex<EvaluatedValue>,
        slot: Option<StorageSlot>,
        mut value: EvaluatedValue,
    ) -> Result<(), String> {
        if let Value::Slice(new) = value.value_mut()
            && let Value::Slice(old) = cell.lock().unwrap().value()
            && let Some(array) = old.fixed
        {
            self.initialize_array(array, new)?;
            new.fixed = Some(array);
        }
        if let Value::Struct(new) = value.value_mut() {
            let old = cell.lock().unwrap();
            if let Value::Struct(old) = old.value() {
                for (field, storage) in &old.field_slots {
                    if let Some(v) = new.fields.get(field) {
                        self.store_binding_value(&storage.cell, storage.slot, v.clone().into())?;
                    }
                }
                new.field_slots.clone_from(&old.field_slots);
            }
        }
        if let Some(slot) = slot {
            if slot.ty == self.runtime_abi.handle_type && matches!(value.value(), Value::Enum(_)) {
                return self.store_enum_handle(slot.ptr, &value);
            }
            value = self.store_slot(slot, value)?;
        }
        Self::clear_scalar_constant(&mut value);
        self.store_cell(cell, value);
        Ok(())
    }

    pub(super) fn store_slot(
        &mut self,
        slot: StorageSlot,
        value: EvaluatedValue,
    ) -> Result<EvaluatedValue, String> {
        let ty = slot.ty;
        let (scalar, updated) = if matches!(slot.contents, SlotContents::String) {
            let handle = self.host_string_operand(value.clone())?;
            let known = match value.into_value() {
                Value::Str(mut literal) => {
                    literal.stored_in = Some(unsafe { LLVMGetInsertBlock(self.builder) });
                    Value::Str(literal)
                }
                _ => Value::from_runtime_payload(handle),
            };
            (handle, known)
        } else if matches!(slot.contents, SlotContents::Slice) {
            let Value::Slice(mut slice) = value.into_value() else {
                return Err("slice assignment requires a slice".into());
            };
            let handle = match slice.handle {
                Some(handle) => handle,
                None => self.build_runtime_handle(Value::Slice(slice.clone()))?,
            };
            slice.handle = Some(handle);
            (handle, Value::Slice(slice))
        } else if ty == self.runtime_abi.int_type {
            let int = self.expect_int(value.into_value())?;
            let scalar = self.int_to_runtime_int(&int, "integer_store")?;
            (scalar, Value::Int(IntValue::new(scalar, None)))
        } else if ty == self.f64_type {
            let float = self.expect_float(value.into_value())?;
            let scalar = unsafe {
                llvm_sys::core::LLVMBuildFPCast(
                    self.builder,
                    float.llvm(),
                    ty,
                    c"float_store".as_ptr(),
                )
            };
            (scalar, Value::Float(FloatValue::new(scalar, None)))
        } else {
            let flag = self.value_to_bool(value.into_value())?;
            let scalar = self.bool_llvm_value(&flag);
            (scalar, Value::Bool(BoolValue::new(scalar, None)))
        };
        unsafe {
            LLVMBuildStore(self.builder, scalar, slot.ptr);
        }
        Ok(updated.into())
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
                    fields: info.fields.len(),
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
        self.borrows.push_scope();
        self.cleanup_stack.push(Vec::new());
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop();
        self.borrows.pop_scope();
        self.cleanup_stack.pop();
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
                    CleanupAction::Defer(expr) => {
                        // Deferred expressions may call functions or contain their own
                        // try, but control flow cannot escape to the scope being unwound.
                        let outer = mem::take(&mut self.exits);
                        let result = self.emit_expression(&expr);
                        self.exits = outer;
                        if let EvalOutcome::Flow(flow) = result? {
                            return Err(format!(
                                "Control flow {} not allowed in deferred expression",
                                flow_name(&flow)
                            ));
                        }
                    }
                    CleanupAction::Drop(record) => self.run_drop(record)?,
                }
            }
        }
        Ok(())
    }

    /// Emit precisely the actions registered up to this exit. Temporarily
    /// unwind the lexical scopes so outer cleanups resolve shadowed bindings
    /// correctly, then restore them for the continuing runtime path.
    pub(super) fn emit_exit_cleanups(&mut self, depth: usize) -> Result<(), String> {
        if self.cleanup_stack[depth..].iter().all(Vec::is_empty) {
            return Ok(());
        }
        // Dropping and deferring may assign through a binding. Slot stores
        // belong to this LLVM edge, but changes to the compiler's value cells
        // must not affect code emitted for its sibling.
        let bindings: Vec<_> = self
            .scopes
            .iter()
            .flat_map(|scope| scope.values())
            .map(|binding| (binding.cell.clone(), binding.cell.lock().unwrap().clone()))
            .collect();
        let scopes = self.scopes.clone();
        let cleanups = self.cleanup_stack.clone();
        let borrows = self.borrows.clone();
        while self.scopes.len() > depth {
            self.exit_scope()?;
        }
        for (cell, value) in bindings {
            *cell.lock().unwrap() = value;
        }
        self.scopes = scopes;
        self.cleanup_stack = cleanups;
        self.borrows = borrows;
        Ok(())
    }

    pub(super) fn exit_scope(&mut self) -> Result<(), String> {
        // A match with no continuing arm has already emitted the cleanup on
        // each outgoing edge. Its empty merge block ends in unreachable.
        let terminated = unsafe {
            let block = LLVMGetInsertBlock(self.builder);
            !block.is_null()
                && !LLVMGetBasicBlockTerminator(block).is_null()
                && !llvm_sys::core::LLVMIsAUnreachableInst(LLVMGetBasicBlockTerminator(block))
                    .is_null()
        };
        if !terminated {
            self.run_cleanups()?;
        }
        self.pop_scope();
        self.release_dead_tokens();
        Ok(())
    }

    pub(super) fn run_drop(&mut self, record: DropRecord) -> Result<(), String> {
        let Some(key) = self.drop_impls.get(&record.type_name).cloned() else {
            return Ok(());
        };
        let Some(Binding { cell, slot, .. }) = self.get_binding(&record.binding) else {
            return Ok(());
        };
        {
            let guard = cell.lock().unwrap();
            if matches!(guard.value(), Value::Moved) {
                return Ok(());
            }
        }
        let reference = Value::Reference(ReferenceValue {
            indexed: None,
            slot,
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
        self.bind_var(name, value, mutable, false)
    }

    // A copied slice still denotes the same collection. Materialize a shared
    // owner at its declaration, where the handle dominates all uses and aliases.
    pub(super) fn prepare_shared_collection(
        &mut self,
        value: &mut EvaluatedValue,
        span: Span,
    ) -> Result<(), String> {
        let shared = self.target.is_host()
            && self.module_stack.last().is_some_and(|module| {
                self.checked.as_ref().is_some_and(|facts| {
                    !facts.indexed_loans.is_empty()
                        && facts.shared_collections.contains(&(
                            module.clone(),
                            span.start,
                            span.end,
                        ))
                })
            });
        if shared {
            self.materialize_slice(value)?;
        }
        Ok(())
    }

    fn materialize_slice(&mut self, value: &mut EvaluatedValue) -> Result<(), String> {
        if let Value::Slice(slice) = value.value_mut()
            && slice.handle.is_none()
        {
            let handle = self.build_runtime_handle(Value::Slice(slice.clone()))?;
            slice.handle = Some(handle);
            value.set_runtime(handle);
        }
        Ok(())
    }

    pub(super) fn bind_var(
        &mut self,
        name: &str,
        value: EvaluatedValue,
        mutable: bool,
        parameter: bool,
    ) -> Result<(), String> {
        let scope_index = self.scopes.len().saturating_sub(1);
        let drop_target = self.drop_type_for_value(value.value());
        let mut value = value;
        if mutable {
            Self::clear_scalar_constant(&mut value);
            if self.target.is_host() {
                self.materialize_slice(&mut value)?;
            }
        }
        if mutable && let Value::Struct(instance) = value.value_mut() {
            for (field, v) in &instance.fields {
                let Some(ty) = self
                    .checked
                    .as_ref()
                    .and_then(|c| {
                        c.borrowed_fields
                            .get(&(instance.name.clone(), field.clone()))
                    })
                    .and_then(|ty| self.scalar_storage_type(ty))
                else {
                    continue;
                };
                let slot = Some(StorageSlot {
                    contents: SlotContents::Scalar,
                    projected: true,
                    ptr: self.build_entry_alloca(ty, field),
                    ty,
                });
                let cell = Rc::new(Mutex::new(Value::Unit.into()));
                self.store_binding_value(&cell, slot, v.clone().into())?;
                instance.field_slots.insert(
                    field.clone(),
                    Binding {
                        cell,
                        mutable: true,
                        slot,
                    },
                );
            }
        }
        Self::attach_origin(&mut value, name);
        let mut slot = None;
        if !mutable
            && self.target.is_embedded()
            && let Value::CancelToken(token) = value.value()
            && let Some(handle) = token.handle
        {
            value = self.copy_token(handle).into();
        }
        if mutable {
            match value.value().clone() {
                Value::CancelToken(token) if self.target.is_embedded() => {
                    if let Some(handle) = token.handle {
                        let owned = self.retain_handle(handle);
                        let mut binding = CancelTokenValue::with_handle(owned);
                        binding.binding_slot = Some(self.track_owned_token(&binding, owned));
                        value = Value::CancelToken(binding).into();
                    }
                }
                Value::Enum(enumeration) => {
                    let ty = self.runtime_abi.handle_type;
                    let ptr = if self.target.is_embedded() {
                        self.embedded_owner_slot()
                    } else {
                        self.build_entry_alloca(ty, "enum_binding")
                    };
                    self.store_enum_handle(ptr, &value)?;
                    value = Value::runtime_enum(&enumeration.enum_name).into();
                    slot = Some(StorageSlot {
                        ptr,
                        ty,
                        projected: false,
                    });
                }
                Value::Slice(slice)
                    if self
                        .checked
                        .as_ref()
                        .is_some_and(|c| !c.indexed_loans.is_empty()) =>
                {
                    if let Some(handle) = slice.handle {
                        let alloca =
                            self.build_entry_alloca(self.runtime_abi.handle_type, "slice_binding");
                        unsafe { LLVMBuildStore(self.builder, handle, alloca) };
                        slot = Some(StorageSlot {
                            contents: SlotContents::Slice,
                            projected: false,
                            ptr: alloca,
                            ty: self.runtime_abi.handle_type,
                        });
                    }
                }
                v if self.target.is_host() && self.is_host_string(&v) => {
                    let ty = self.runtime_abi.handle_type;
                    let ptr = self.build_entry_alloca(ty, name);
                    let storage = StorageSlot {
                        contents: SlotContents::String,
                        projected: false,
                        ptr,
                        ty,
                    };
                    value = self.store_slot(storage, value)?;
                    slot = Some(storage);
                }
                Value::Int(int_val) => {
                    let wide = self.int_to_runtime_int(&int_val, "integer_initial_value")?;
                    let alloca =
                        self.build_entry_alloca(self.runtime_abi.int_type, &format!("{name}_slot"));
                    unsafe {
                        LLVMBuildStore(self.builder, wide, alloca);
                    }
                    *value.value_mut() = Value::Int(IntValue::new(wide, None));
                    slot = Some(StorageSlot {
                        contents: SlotContents::Scalar,
                        projected: false,
                        ptr: alloca,
                        ty: self.runtime_abi.int_type,
                    });
                }
                Value::Bool(flag) => {
                    let alloca = self
                        .build_entry_alloca(self.runtime_abi.bool_type, &format!("{name}_slot"));
                    let stored = self.bool_llvm_value(&flag);
                    unsafe {
                        LLVMBuildStore(self.builder, stored, alloca);
                    }
                    slot = Some(StorageSlot {
                        contents: SlotContents::Scalar,
                        projected: false,
                        ptr: alloca,
                        ty: self.runtime_abi.bool_type,
                    });
                }
                _ => {}
            }
        }
        let cell = Rc::new(Mutex::new(value));
        {
            let stored = cell.lock().unwrap();
            self.track_reference_borrow_in_scope(stored.value(), scope_index, parameter)?;
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
            Value::Reference(reference) if reference.origin.is_none() => {
                reference.origin = Some(name.to_string());
            }
            Value::Pointer(pointer) if pointer.origin.is_none() => {
                pointer.origin = Some(name.to_string());
            }
            Value::Sender(sender) if sender.origin.is_none() => {
                sender.origin = Some(name.to_string());
            }
            Value::Receiver(receiver) if receiver.origin.is_none() => {
                receiver.origin = Some(name.to_string());
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
        let binding = self.get_binding(name)?;
        Some(self.load_binding_value(&binding.cell, binding.slot))
    }

    pub(super) fn load_binding_value(
        &mut self,
        cell: &Mutex<EvaluatedValue>,
        slot: Option<StorageSlot>,
    ) -> EvaluatedValue {
        let Some(slot) = slot else {
            let mut value = cell.lock().unwrap().clone();
            if let Value::Struct(instance) = value.value_mut() {
                for (field, binding) in mem::take(&mut instance.field_slots) {
                    let current = self.load_binding_value(&binding.cell, binding.slot);
                    instance.fields.insert(field, current.into_value());
                }
            }
            if let Value::Slice(slice) = value.value_mut()
                && let Some(array) = slice.fixed
            {
                let mut loaded = Vec::with_capacity(array.len);
                for i in 0..array.len {
                    let index = unsafe { LLVMConstInt(self.runtime_abi.usize_type, i as u64, 0) };
                    loaded.push(
                        self.load_slot(array.element(self.builder, index))
                            .into_value(),
                    );
                }
                slice.items = Rc::new(Mutex::new(loaded));
            }
            return self.read_token_binding(value);
        };
        if slot.ty == self.runtime_abi.handle_type
            && matches!(cell.lock().unwrap().value(), Value::Enum(_))
        {
            let handle =
                unsafe { LLVMBuildLoad2(self.builder, slot.ty, slot.ptr, c"enum_read".as_ptr()) };
            if self.target.is_embedded() {
                let snapshot = self.embedded_owner_slot();
                self.copy_embedded_handle(snapshot, handle);
            }
            return cell.lock().unwrap().clone().with_runtime(handle);
        }
        if matches!(slot.contents, SlotContents::String)
            && let Value::Str(literal) = cell.lock().unwrap().value()
            && literal.stored_in == Some(unsafe { LLVMGetInsertBlock(self.builder) })
        {
            // Keep straight-line literal folding, including compile-time string intrinsics.
            return Value::Str(literal.clone()).into();
        }
        self.load_slot(slot)
    }

    pub(super) fn load_slot(&mut self, slot: StorageSlot) -> EvaluatedValue {
        let ty = slot.ty;
        let loaded =
            unsafe { LLVMBuildLoad2(self.builder, ty, slot.ptr, c"binding_load".as_ptr()) };
        if matches!(slot.contents, SlotContents::String) {
            self.handle_types.insert(loaded, TypeExpr::named("string"));
            EvaluatedValue::from_value(Value::from_runtime_payload(loaded)).with_runtime(loaded)
        } else if matches!(slot.contents, SlotContents::Slice) {
            self.evaluated(Value::Slice(SliceValue::with_handle(loaded)))
                .with_runtime(loaded)
        } else if ty == self.runtime_abi.int_type {
            self.evaluated(Value::Int(IntValue::new(loaded, None)))
        } else if ty == self.f64_type {
            self.evaluated(Value::Float(FloatValue::new(loaded, None)))
        } else {
            self.evaluated(Value::Bool(BoolValue::new(loaded, None)))
        }
    }

    pub(super) fn get_binding(&self, name: &str) -> Option<Binding> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
            .cloned()
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
                        let slot =
                            self.build_entry_alloca(self.runtime_abi.handle_type, "slice_iter_out");
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
                            indexed: None,
                            slot: None,
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
                        let key_slot =
                            self.build_entry_alloca(self.runtime_abi.handle_type, "map_iter_key");
                        let value_slot =
                            self.build_entry_alloca(self.runtime_abi.handle_type, "map_iter_val");
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
                            indexed: None,
                            slot: None,
                            cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                            mutable: false,
                            origin: None,
                            handle: Some(key_handle),
                        };
                        let value_ref = ReferenceValue {
                            indexed: None,
                            slot: None,
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
