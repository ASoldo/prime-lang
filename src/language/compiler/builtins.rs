use super::*;

impl Compiler {
    pub(super) fn reference_handle(
        &mut self,
        reference: &ReferenceValue,
    ) -> Result<LLVMValueRef, String> {
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

    pub(super) fn write_reference(
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

    pub(super) fn expect_box_value(&mut self, value: Value, ctx: &str) -> Result<BoxValue, String> {
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

    pub(super) fn expect_slice_value(
        &mut self,
        value: Value,
        ctx: &str,
    ) -> Result<SliceValue, String> {
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

    pub(super) fn expect_map_value(&mut self, value: Value, ctx: &str) -> Result<MapValue, String> {
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

    pub(super) fn expect_sender(
        &mut self,
        value: Value,
        ctx: &str,
    ) -> Result<ChannelSender, String> {
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

    pub(super) fn expect_receiver(
        &mut self,
        value: Value,
        ctx: &str,
    ) -> Result<ChannelReceiver, String> {
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

    pub(super) fn expect_string_value(value: Value, ctx: &str) -> Result<String, String> {
        match value {
            Value::Str(s) => Ok((*s.text).clone()),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone().into_value();
                Self::expect_string_value(inner, ctx)
            }
            _ => Err(format!("{ctx} expects string value")),
        }
    }

    pub(super) fn make_string_value(&self, text: &str) -> Result<Value, String> {
        let c_value = CString::new(text.as_bytes())
            .map_err(|_| "string value cannot contain interior null bytes".to_string())?;
        let name = CString::new("map_key").unwrap();
        let text_rc = Arc::new(text.to_string());
        unsafe {
            let ptr = LLVMBuildGlobalString(self.builder, c_value.as_ptr(), name.as_ptr());
            Ok(Value::Str(StringValue::new(ptr, text_rc)))
        }
    }

    pub(super) fn builtin_box_new(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_new expects 1 argument".into());
        }
        let value = args.pop().unwrap();
        Ok(Value::Boxed(BoxValue::new(value)))
    }

    pub(super) fn builtin_box_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_get expects 1 argument".into());
        }
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_get")?;
        Ok(boxed.cell.lock().unwrap().clone())
    }

    pub(super) fn builtin_box_set(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("box_set expects 2 arguments".into());
        }
        let value = args.pop().unwrap();
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_set")?;
        boxed.replace(value);
        Ok(Value::Unit)
    }

    pub(super) fn builtin_box_take(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("box_take expects 1 argument".into());
        }
        let boxed = self.expect_box_value(args.pop().unwrap(), "box_take")?;
        Ok(boxed.replace(Value::Unit))
    }

    pub(super) fn builtin_slice_new(&mut self, args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_new");
        if !args.is_empty() {
            return Err("slice_new expects 0 arguments".into());
        }
        Ok(Value::Slice(SliceValue::new()))
    }

    pub(super) fn builtin_slice_push(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_slice_len(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_len");
        if args.len() != 1 {
            return Err("slice_len expects 1 argument".into());
        }
        let slice = self.expect_slice_value(args.pop().unwrap(), "slice_len")?;
        let len = slice.len() as i128;
        Ok(Value::Int(self.const_int_value(len)))
    }

    pub(super) fn builtin_slice_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("slice_get");
        if args.len() != 2 {
            return Err("slice_get expects 2 arguments".into());
        }
        let index_value = args.pop().unwrap();
        let mut slice = self.expect_slice_value(args.pop().unwrap(), "slice_get")?;
        let int_value = self.expect_int(index_value)?;
        let idx_const = int_value.constant();
        if env::var_os("PRIME_DEBUG_SLICE_GET").is_some() {
            if let Some(c) = idx_const {
                eprintln!("[prime-debug] slice_get idx_const={c} len={}", slice.len());
            } else {
                eprintln!("[prime-debug] slice_get idx_const=None len={}", slice.len());
            }
        }
        if env::var_os("PRIME_DEBUG_SLICE_GET").is_some() {
            if let Some(c) = idx_const {
                eprintln!("[prime-debug] slice_get idx const={c} len={}", slice.len());
            } else {
                eprintln!("[prime-debug] slice_get idx non-const len={}", slice.len());
            }
        }
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
            unsafe {
                LLVMBuildStore(self.builder, self.null_handle_ptr(), slot);
            }
            let mut call_args = [handle, idx_arg, slot];
            let status = self.call_runtime(
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
            if unsafe { LLVMIsAConstantInt(status).is_null() } {
                return self.instantiate_enum_variant("None", Vec::new());
            }
            let ok_const = unsafe { LLVMConstIntGetZExtValue(status) == PrimeStatus::Ok as u64 };
            if !ok_const {
                return self.instantiate_enum_variant("None", Vec::new());
            }
            let reference = ReferenceValue {
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                mutable: false,
                origin: None,
                handle: Some(loaded),
            };
            return self.instantiate_enum_variant("Some", vec![Value::Reference(reference)]);
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

    pub(super) fn builtin_slice_remove(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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

    pub(super) fn builtin_remove(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("remove expects receiver and argument".into());
        }
        let receiver = args.remove(0);
        match receiver {
            Value::Slice(slice) => {
                let forwarded = vec![Value::Slice(slice), args.remove(0)];
                self.builtin_slice_remove(forwarded)
            }
            Value::Map(map) => {
                let forwarded = vec![Value::Map(map), args.remove(0)];
                self.builtin_map_remove(forwarded)
            }
            Value::Reference(reference) => {
                let mut forwarded = vec![reference.cell.lock().unwrap().clone().into_value()];
                forwarded.extend(args);
                self.builtin_remove(forwarded)
            }
            _ => Err("remove expects slice or map".into()),
        }
    }

    pub(super) fn builtin_map_new(&mut self, args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_new");
        if !args.is_empty() {
            return Err("map_new expects 0 arguments".into());
        }
        Ok(Value::Map(MapValue::new()))
    }

    pub(super) fn builtin_map_insert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_insert");
        if args.len() != 3 {
            return Err("map_insert expects 3 arguments".into());
        }
        let value = args.pop().unwrap();
        let key = Self::expect_string_value(args.pop().unwrap(), "map_insert")?;
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

    pub(super) fn builtin_map_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        self.warn_deprecated("map_get");
        if args.len() != 2 {
            return Err("map_get expects 2 arguments".into());
        }
        let key = Self::expect_string_value(args.pop().unwrap(), "map_get")?;
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
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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

    pub(super) fn builtin_map_keys(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_map_values(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_map_remove(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("remove expects receiver and key".into());
        }
        let key = Self::expect_string_value(args.pop().unwrap(), "remove")?;
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
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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

    pub(super) fn builtin_iter(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_iter_next(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_len(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_get(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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
                        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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
                let key = Self::expect_string_value(args.remove(0), "get")?;
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
                        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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

    pub(super) fn assign_index_value(
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
                let key = Self::expect_string_value(index, "index assignment")?;
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

    pub(super) fn builtin_push(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_insert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 3 {
            return Err("insert expects receiver, key, and value".into());
        }
        let value = args.pop().unwrap();
        let key = Self::expect_string_value(args.pop().unwrap(), "insert")?;
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

    pub(super) fn builtin_fs_exists(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("fs_exists expects 1 argument".into());
        }
        let path = Self::expect_string_value(args.pop().unwrap(), "fs_exists")?;
        Ok(Value::Bool(
            self.const_bool_value(platform().fs_exists(&path)),
        ))
    }

    pub(super) fn builtin_fs_read(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("fs_read expects 1 argument".into());
        }
        let path = Self::expect_string_value(args.pop().unwrap(), "fs_read")?;
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

    pub(super) fn builtin_fs_write(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("fs_write expects 2 arguments".into());
        }
        let contents = Self::expect_string_value(args.pop().unwrap(), "fs_write")?;
        let path = Self::expect_string_value(args.pop().unwrap(), "fs_write")?;
        match platform().fs_write(&path, &contents) {
            Ok(()) => self.instantiate_enum_variant("Ok", vec![Value::Unit]),
            Err(msg) => {
                let err = self.build_string_constant(msg)?;
                self.instantiate_enum_variant("Err", vec![err])
            }
        }
    }

    pub(super) fn builtin_now_ms(&mut self, args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_sleep_ms(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_sleep_task(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn ensure_embedded_target(&self, name: &str) -> Result<(), String> {
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

    pub(super) fn builtin_delay_ms(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_pin_mode(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_digital_write(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_digital_read(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("digital_read expects 1 argument (pin)".into());
        }
        self.ensure_embedded_target("digital_read")?;
        let pin = self.expect_int(args.pop().unwrap())?;
        let pin_cast = unsafe {
            LLVMBuildIntCast(
                self.builder,
                pin.llvm(),
                self.i32_type,
                CString::new("digital_read_pin").unwrap().as_ptr(),
            )
        };
        let mut call_args = [pin_cast];
        let llvm = self.call_runtime(
            self.runtime_abi.prime_digital_read,
            self.runtime_abi.prime_digital_read_ty,
            &mut call_args,
            "digital_read",
        );
        Ok(Value::Int(IntValue::new(llvm, None)))
    }

    pub(super) fn builtin_reset_reason(&mut self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err("reset_reason expects no arguments".into());
        }
        self.ensure_embedded_target("reset_reason")?;
        let mut call_args = [];
        let llvm = self.call_runtime(
            self.runtime_abi.prime_reset_reason,
            self.runtime_abi.prime_reset_reason_ty,
            &mut call_args,
            "reset_reason",
        );
        Ok(Value::Int(IntValue::new(llvm, None)))
    }

    pub(super) fn builtin_assert(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_expect(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("expect expects 2 arguments".into());
        }
        let cond_value = self.value_to_bool(args.remove(0))?;
        let cond = self.bool_constant_or_llvm(&cond_value, "expect condition")?;
        let msg = Self::expect_string_value(args.remove(0), "expect")?;
        if !cond {
            return Err(msg);
        }
        Ok(Value::Unit)
    }

    pub(super) fn builtin_str_len(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("str_len expects 1 argument".into());
        }
        let text = Self::expect_string_value(args.pop().unwrap(), "str_len")?;
        Ok(Value::Int(self.const_int_value(text.len() as i128)))
    }

    pub(super) fn builtin_str_contains(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("str_contains expects receiver and needle".into());
        }
        let needle = Self::expect_string_value(args.pop().unwrap(), "str_contains")?;
        let haystack = Self::expect_string_value(args.pop().unwrap(), "str_contains")?;
        Ok(Value::Bool(
            self.const_bool_value(haystack.contains(&needle)),
        ))
    }

    pub(super) fn builtin_str_trim(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("str_trim expects 1 argument".into());
        }
        let text = Self::expect_string_value(args.pop().unwrap(), "str_trim")?;
        let trimmed = text.trim().to_string();
        self.make_string_value(&trimmed)
    }

    pub(super) fn builtin_str_split(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("str_split expects receiver and delimiter".into());
        }
        let delim = Self::expect_string_value(args.pop().unwrap(), "str_split")?;
        let text = Self::expect_string_value(args.pop().unwrap(), "str_split")?;
        let mut parts = Vec::new();
        for segment in text.split(&delim) {
            parts.push(self.make_string_value(segment)?);
        }
        Ok(Value::Slice(SliceValue::from_vec(parts)))
    }

    pub(super) fn builtin_min(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_max(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_abs(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_debug_show(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_channel(&mut self, args: Vec<Value>) -> Result<Value, String> {
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
        let shared = Rc::new((Mutex::new(state), Condvar::new()));
        let sender = Value::Sender(ChannelSender::new(shared.clone()));
        let receiver = Value::Receiver(ChannelReceiver::new(shared));
        Ok(Value::Tuple(vec![sender, receiver]))
    }

    pub(super) fn builtin_send(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_recv(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_recv_timeout(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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
                cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
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

    pub(super) fn builtin_recv_task(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_close(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_join(&mut self, mut args: Vec<Value>) -> Result<Value, String> {
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

    pub(super) fn builtin_ptr(
        &mut self,
        mut args: Vec<Value>,
        mutable: bool,
    ) -> Result<Value, String> {
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
}
