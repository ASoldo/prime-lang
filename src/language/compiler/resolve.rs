use super::*;

impl Compiler {
    pub(super) fn invoke_function(
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

    pub(super) fn run_function_with_values(
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

    pub(super) fn call_function_with_values(
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

    pub(super) fn register_function(&mut self, func: &FunctionDef, module: &str) -> Result<(), String> {
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

    pub(super) fn register_impl_block(&mut self, module: &str, block: &ImplBlock) -> Result<(), String> {
        let is_drop_like = block.inherent || block.interface == "Drop";
        let target_is_struct = self.structs.contains_key(&block.target);
        let target_is_enum = self
            .enum_variants
            .values()
            .any(|info| info.enum_name == block.target);
        if !(target_is_struct || (is_drop_like && target_is_enum)) {
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

    pub(super) fn resolve_function(
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

    pub(super) fn instantiate_function(
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

    pub(super) fn ensure_interface_arguments(
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

    pub(super) fn interface_name_from_type(&self, ty: &TypeExpr) -> Option<(String, Vec<TypeExpr>)> {
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

    pub(super) fn ensure_interface_compat(
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
        let struct_name = Self::value_struct_name(value).ok_or_else(|| {
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

    pub(super) fn current_module(&self) -> Option<&str> {
        self.module_stack.last().map(|s| s.as_str())
    }

    pub(super) fn can_access(&self, owner: &str, visibility: Visibility) -> bool {
        matches!(visibility, Visibility::Public)
            || self
                .current_module()
                .is_none_or(|current| current == owner)
    }

    pub(super) fn ensure_item_visible(
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

    pub(super) fn receiver_name_from_values(&self, args: &[Value]) -> Option<String> {
        args.first().and_then(Self::value_struct_name)
    }

    pub(super) fn value_struct_name(value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap();
                Self::value_struct_name(inner.value())
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap();
                Self::value_struct_name(inner.value())
            }
            Value::Boxed(_) => None,
            _ => None,
        }
    }

    pub(super) fn drop_type_for_value(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Enum(enum_value) => Some(enum_value.enum_name.clone()),
            _ => None,
        }
    }

    pub(super) fn queue_drop(&mut self, name: &str, type_name: String) {
        if self.drop_impls.contains_key(&type_name) {
            if let Some(stack) = self.cleanup_stack.last_mut() {
                stack.push(CleanupAction::Drop(DropRecord {
                    binding: name.to_string(),
                    type_name,
                }));
            }
        }
    }
    pub(super) fn begin_mut_borrow_in_scope(&mut self, name: &str, scope_index: usize) -> Result<(), String> {
        if self.active_mut_borrows.contains(name) {
            return Err(format!("`{}` is already mutably borrowed", name));
        }
        self.active_mut_borrows.insert(name.to_string());
        if let Some(frame) = self.borrow_frames.get_mut(scope_index) {
            frame.push(name.to_string());
        }
        Ok(())
    }

    pub(super) fn end_mut_borrow(&mut self, name: &str) {
        self.active_mut_borrows.remove(name);
    }

    pub(super) fn is_mut_borrowed(&self, name: &str) -> bool {
        self.active_mut_borrows.contains(name)
    }

    pub(super) fn register_move(&mut self, name: &str) {
        self.end_mut_borrow(name);
    }

    pub(super) fn track_reference_borrow_in_scope(
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

    pub(super) fn release_reference_borrow(&mut self, value: &Value) {
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

    pub(super) fn warn_deprecated(&mut self, name: &str) {
        if self.deprecated_warnings.insert(name.to_string()) {
            eprintln!(
                "warning: `{}` is deprecated; prefer slice/map literals or direct methods",
                name
            );
        }
    }

}
