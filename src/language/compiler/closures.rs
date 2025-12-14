use super::*;

impl Compiler {
    pub(super) fn build_closure_signature(
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

    pub(super) fn take_binding_value(&mut self, name: &str) -> Result<EvaluatedValue, String> {
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

    pub(super) fn build_closure_repr(
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

    pub(super) fn value_to_llvm_for_type(
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

    pub(super) fn value_from_llvm(&mut self, llvm: LLVMValueRef, ty: &TypeExpr) -> Result<Value, String> {
        match ty {
            TypeExpr::Reference { mutable, .. } => Ok(Value::Reference(ReferenceValue {
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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

    pub(super) fn ensure_closure_info(
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

    pub(super) fn emit_closure_body(
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
                .emit_expression_with_hint(expr.node.as_ref(), info.signature.returns.first())?
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

    pub(super) fn emit_closure_return(
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

    pub(super) fn value_type_hint(&self, value: &Value) -> Option<TypeExpr> {
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
                .and_then(|items| items.first().cloned())
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

    pub(super) fn allocate_closure_env(&mut self, env_type: LLVMTypeRef, key: usize) -> LLVMValueRef {
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

    pub(super) fn capture_type_needs_release(&self, ty: &TypeExpr) -> bool {
        matches!(ty, TypeExpr::Slice(_))
            || matches!(ty, TypeExpr::Named(name, _) if name == "Map" || name == "Box")
            || matches!(ty, TypeExpr::Reference { .. })
            || matches!(ty, TypeExpr::Named(name, _) if name == "Sender" || name == "Receiver" || name == "JoinHandle")
            || matches!(ty, TypeExpr::Pointer { .. })
    }

    pub(super) fn release_closure_envs(&mut self) -> Result<(), String> {
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

    pub(super) fn emit_closure_literal(
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

}
