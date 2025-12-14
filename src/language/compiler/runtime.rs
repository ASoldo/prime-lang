use super::*;

impl Compiler {
    pub(super) fn emit_out_value(&mut self, value: EvaluatedValue) -> Result<(), String> {
        self.print_value(value)?;
        if self.target.is_embedded() {
            // Terminate embedded prints with a newline.
            self.emit_embedded_newline();
        } else {
            self.emit_printf_call("\n", &mut []);
        }
        Ok(())
    }

    pub(super) fn emit_format_template(
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

    pub(super) fn emit_embedded_newline(&mut self) {
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

    pub(super) fn emit_embedded_labeled_print(&mut self, label: &str, handle: LLVMValueRef) {
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

    pub(super) fn print_value(&mut self, value: EvaluatedValue) -> Result<(), String> {
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
                    if let Some(handle) = reference.handle {
                        // Best-effort print via runtime handle (common for Option/Result payloads).
                        self.ensure_runtime_symbols();
                        let mut as_int_args = [handle];
                        let int_val = self.call_runtime(
                            self.runtime_abi.prime_value_as_int,
                            self.runtime_abi.prime_value_as_int_ty,
                            &mut as_int_args,
                            "ref_value_as_int",
                        );
                        let mut to_string_args = [int_val];
                        let string_handle = self.call_runtime(
                            self.runtime_abi.prime_int_to_string,
                            self.runtime_abi.prime_int_to_string_ty,
                            &mut to_string_args,
                            "ref_int_to_string",
                        );
                        let mut print_args = [string_handle];
                        self.call_runtime(
                            self.runtime_abi.prime_print,
                            self.runtime_abi.prime_print_ty,
                            &mut print_args,
                            "prime_print_ref",
                        );
                        return Ok(());
                    }
                    // Print the current pointed-to value (non-runtime).
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
                        if let Some(rt_handle) =
                            runtime_handle.or_else(|| self.maybe_attach_runtime_handle(&mut value))
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
        if let Some(handle) =
            runtime_handle.or_else(|| self.maybe_attach_runtime_handle(&mut value))
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
            Value::Task(_) => {
                // Task handles don’t have a printable payload in build mode; emit a placeholder.
                let (ptr, len) = self.build_runtime_bytes("<task>", "print_task")?;
                let mut args = [ptr, len];
                self.emit_printf_call("%.*s", &mut args);
                Ok(())
            }
            Value::Moved => Err("Cannot print moved value in build mode".into()),
        }
    }

    pub(super) fn emit_runtime_print(&mut self, handle: LLVMValueRef) {
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

    pub(super) fn build_runtime_handle(&mut self, value: Value) -> Result<LLVMValueRef, String> {
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

    pub(super) fn build_runtime_handle_scoped(&mut self, value: Value) -> Option<LLVMValueRef> {
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

    pub(super) fn call_runtime(
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

    pub(super) fn build_runtime_bytes(
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

    pub(super) fn null_handle_ptr(&self) -> LLVMValueRef {
        unsafe { LLVMConstNull(LLVMPointerType(self.runtime_abi.handle_type, 0)) }
    }

    pub(super) fn render_runtime_value_bytes(value: &Value) -> Result<Vec<u8>, String> {
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
                        Self::render_runtime_value_bytes(v).and_then(|b| {
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
                        Self::render_runtime_value_bytes(v).and_then(|b| {
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

    pub(super) fn render_format_template_bytes(
        &mut self,
        template: FormatTemplateValue,
    ) -> Result<Vec<u8>, String> {
        let mut output = String::new();
        for segment in template.segments {
            match segment {
                FormatRuntimeSegment::Literal(text) => output.push_str(&text),
                FormatRuntimeSegment::Named(value) => {
                    let rendered = Self::render_runtime_value_bytes(value.value())?;
                    output.push_str(&String::from_utf8(rendered).map_err(|_| "invalid utf8")?);
                }
                FormatRuntimeSegment::Implicit => output.push_str("{}"),
            }
        }
        Ok(output.into_bytes())
    }

    pub(super) fn type_code_for(&self, ty: &TypeExpr) -> Result<u32, String> {
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

    pub(super) fn opaque_ptr_type(&self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(LLVMInt8TypeInContext(self.context), 0) }
    }

    pub(super) fn llvm_type_for(&self, ty: &TypeExpr) -> Option<LLVMTypeRef> {
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

    pub(super) fn expect_llvm_type(&self, ty: &TypeExpr, ctx: &str) -> Result<LLVMTypeRef, String> {
        self.llvm_type_for(ty)
            .ok_or_else(|| format!("Unsupported type `{}` for {ctx}", ty.canonical_name()))
    }

    pub(super) fn tuple_llvm_type(&self, elems: &[TypeExpr]) -> Result<LLVMTypeRef, String> {
        let mut fields = Vec::with_capacity(elems.len());
        for ty in elems {
            fields.push(self.expect_llvm_type(ty, "tuple element")?);
        }
        Ok(unsafe {
            LLVMStructTypeInContext(self.context, fields.as_mut_ptr(), fields.len() as u32, 0)
        })
    }

    pub(super) fn infer_closure_body_type(
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

    pub(super) fn infer_expr_type(
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
                    let ty = param.ty.as_ref().map(|ann| ann.ty.clone())?;
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

}
