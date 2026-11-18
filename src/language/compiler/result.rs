use super::*;

// Enum fields and task payloads share the boxed record ABI. Struct records use
// field-name order, independent of declaration and literal initialization order.
enum PayloadLayout {
    Scalar(TypeExpr),
    Enum(String, Vec<TypeExpr>),
    Tuple(Vec<PayloadLayout>),
    Struct(String, BTreeMap<String, PayloadLayout>),
}

fn contains_type_argument(outer: &TypeExpr, nested: &TypeExpr) -> bool {
    match outer {
        TypeExpr::Named(_, args) | TypeExpr::Tuple(args) => args
            .iter()
            .any(|arg| arg == nested || contains_type_argument(arg, nested)),
        _ => false,
    }
}

impl Compiler {
    fn payload_layout(
        &self,
        ty: &TypeExpr,
        parents: &mut Vec<TypeExpr>,
    ) -> Result<PayloadLayout, String> {
        if *ty == TypeExpr::Unit || self.type_code_for(ty).is_ok() {
            if self.target.is_embedded()
                && matches!(ty, TypeExpr::Named(name, _) if matches!(name.as_str(), "bool" | "float32" | "float64"))
            {
                return Err(format!(
                    "cannot box `{}` in the embedded value ABI",
                    ty.canonical_name()
                ));
            }
            return Ok(PayloadLayout::Scalar(ty.clone()));
        }
        // A repeated declaration can still be finite, e.g. Option[Option[T]].
        // Follow decreasing type arguments but stop recursive field expansion.
        if let TypeExpr::Named(name, _) = ty
            && let Some(parent) = parents
                .iter()
                .rev()
                .find(|p| matches!(p, TypeExpr::Named(n, _) if n == name))
            && !contains_type_argument(parent, ty)
        {
            return Err(format!(
                "cannot reconstruct recursive payload `{}`",
                ty.canonical_name()
            ));
        }
        match ty {
            TypeExpr::Named(name, args)
                if self.enum_variants.values().any(|v| v.enum_name == *name) =>
            {
                // The handle carries its own tag. Read the active fields only
                // on a later match or ?. Embedded boxing also has to represent
                // every possible variant, including its error fields.
                if self.target.is_embedded() {
                    parents.push(ty.clone());
                    let mut variants: Vec<_> = self
                        .enum_variants
                        .values()
                        .filter(|variant| variant.enum_name == *name)
                        .collect();
                    variants.sort_by_key(|variant| variant.variant_index);
                    for variant in variants {
                        self.check_record_width(ty, variant.fields.len())?;
                        for field in variant.field_types_for(Some(ty)) {
                            self.payload_layout(&field, parents)?;
                        }
                    }
                    parents.pop();
                }
                Ok(PayloadLayout::Enum(name.clone(), args.clone()))
            }
            TypeExpr::Tuple(items) => {
                self.check_record_width(ty, items.len())?;
                let fields = items
                    .iter()
                    .map(|item| self.payload_layout(item, parents))
                    .collect::<Result<_, _>>()?;
                Ok(PayloadLayout::Tuple(fields))
            }
            TypeExpr::Named(name, args) if self.structs.contains_key(name) => {
                let def = &self.structs[name].def;
                self.check_record_width(ty, def.fields.len())?;
                parents.push(ty.clone());
                let substitutions = def
                    .type_params
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .collect();
                let mut fields = BTreeMap::new();
                for field in &def.fields {
                    let Some(field_name) = field.name.as_ref().filter(|_| !field.embedded) else {
                        return Err(format!(
                            "cannot reconstruct promoted fields of struct `{name}`"
                        ));
                    };
                    fields.insert(
                        field_name.clone(),
                        self.payload_layout(&field.ty.ty.substitute(&substitutions), parents)?,
                    );
                }
                parents.pop();
                Ok(PayloadLayout::Struct(name.clone(), fields))
            }
            _ => Err(format!("cannot reconstruct `{}`", ty.canonical_name())),
        }
    }

    fn check_record_width(&self, ty: &TypeExpr, count: usize) -> Result<(), String> {
        if self.target.is_embedded() && count > 1 {
            return Err(format!(
                "cannot represent `{}` with {count} fields in the single-field embedded record ABI",
                ty.canonical_name()
            ));
        }
        Ok(())
    }

    fn decode_payload(
        &mut self,
        handle: LLVMValueRef,
        layout: &PayloadLayout,
    ) -> Result<EvaluatedValue, String> {
        let value = match layout {
            PayloadLayout::Scalar(ty) => {
                self.handle_types.insert(handle, ty.clone());
                self.decode_enum_field(handle)?
            }
            PayloadLayout::Enum(name, args) => {
                self.handle_types
                    .insert(handle, TypeExpr::Named(name.clone(), args.clone()));
                Value::runtime_enum(name)
            }
            PayloadLayout::Tuple(fields) => {
                let mut values = Vec::new();
                for (i, field) in fields.iter().enumerate() {
                    let child = self.task_field(handle, i);
                    values.push(self.decode_payload(child, field)?.into_value());
                }
                Value::Tuple(values)
            }
            PayloadLayout::Struct(name, fields) => {
                let mut values = HashMap::new();
                for (i, (key, field)) in fields.iter().enumerate() {
                    let child = self.task_field(handle, i);
                    values.insert(key.clone(), self.decode_payload(child, field)?.into_value());
                }
                Value::Struct(StructValue::new(name.clone(), values))
            }
        };
        Ok(EvaluatedValue::from_value(value).with_runtime(handle))
    }

    pub(super) fn decode_match_field(
        &mut self,
        handle: LLVMValueRef,
    ) -> Result<EvaluatedValue, String> {
        // Other enum field types retain the match lowering's existing handle representation.
        if let Some(ty) = self.handle_types.get(&handle)
            && let Ok(layout) = self.payload_layout(ty, &mut Vec::new())
        {
            return self.decode_payload(handle, &layout);
        }
        Ok(EvaluatedValue::from_value(self.decode_enum_field(handle)?).with_runtime(handle))
    }

    fn try_diagnostic(&mut self, span: Span, reason: &str) -> String {
        self.error_location = self.module_stack.last().map(|module| CodegenLocation {
            module: module.clone(),
            span: Span::new(span.end.saturating_sub(1), span.end),
            code: "E0NATIVETRY",
        });
        format!("native runtime Result propagation {reason}")
    }

    pub(super) fn eval_try_operator(
        &mut self,
        value: EvaluatedValue,
        id: ExprId,
        span: Span,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let handle = match value.value() {
            Value::Enum(e) if e.enum_name == "Result" => match e.variant.as_str() {
                "Ok" => {
                    let payload = match e.values.as_slice() {
                        [] => Value::Unit,
                        [one] => one.clone(),
                        many => Value::Tuple(many.to_vec()),
                    };
                    return Ok(EvalOutcome::Value(payload.into()));
                }
                "Err" => return Ok(EvalOutcome::Flow(FlowSignal::Propagate(value))),
                _ => value.runtime_handle(),
            },
            Value::Reference(r) => r.handle,
            _ => None,
        }
        .ok_or_else(|| self.try_diagnostic(span, "requires a runtime Result handle"))?;

        if !self.can_propagate_result() {
            return Err(self.try_diagnostic(
                span,
                "requires a Result-returning function or a local try block",
            ));
        }
        let Some(ok) = self.enum_variants.get("Ok") else {
            return Err(self.try_diagnostic(span, "requires Result::Ok"));
        };
        let ok_tag = ok.variant_index;
        let canonical = |variant: &EnumVariantInfo, index: usize| {
            variant.enum_name == "Result"
                && variant.type_params.len() == 2
                && variant.fields == [TypeExpr::named(&variant.type_params[index])]
        };
        if !canonical(ok, 0)
            || !self
                .enum_variants
                .get("Err")
                .is_some_and(|err| canonical(err, 1))
            || self
                .enum_variants
                .iter()
                .any(|(name, v)| v.enum_name == "Result" && !matches!(name.as_str(), "Ok" | "Err"))
        {
            return Err(self.try_diagnostic(
                span,
                "requires a two-variant Result[T, E] with Ok(T) and Err(E)",
            ));
        }
        let checked_type = self
            .checked
            .as_ref()
            .and_then(|facts| facts.try_results.get(&id));
        let Some(TypeExpr::Named(name, args)) =
            checked_type.or_else(|| self.handle_types.get(&handle))
        else {
            return Err(self.try_diagnostic(span, "requires a known Result payload type"));
        };
        let [ok_ty, err_ty] = args.as_slice() else {
            return Err(self.try_diagnostic(span, "requires both Result type arguments"));
        };
        if name != "Result" {
            return Err(self.try_diagnostic(span, "requires a Result operand"));
        }
        let layout = self.payload_layout(ok_ty, &mut Vec::new());
        // Embedded handles have no dynamic type information and several boxing
        // operations are stubs. Forwarding an Err must preserve its value too.
        if self.target.is_embedded() {
            self.payload_layout(err_ty, &mut Vec::new())
                .map_err(|reason| self.try_diagnostic(span, &format!("of Err payload {reason}")))?;
        }
        let layout = layout
            .map_err(|reason| self.try_diagnostic(span, &format!("of Ok payload {reason}")))?;
        let tag = self.call_runtime(
            self.runtime_abi.prime_enum_tag,
            self.runtime_abi.prime_enum_tag_ty,
            &mut [handle],
            "try_tag",
        );
        let success = self.match_block(c"try_ok");
        let failure = self.match_block(c"try_err");
        unsafe {
            let is_ok = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                tag,
                LLVMConstInt(LLVMTypeOf(tag), ok_tag.into(), 0),
                c"try_is_ok".as_ptr(),
            );
            LLVMBuildCondBr(self.builder, is_ok, success, failure);
            LLVMPositionBuilderAtEnd(self.builder, failure);
        }
        // Carry the original enum (including its error payload) across the edge.
        let error = EvaluatedValue::from_value(Value::runtime_enum("Result")).with_runtime(handle);
        self.branch_to_exit(FlowSignal::Propagate(error))?;
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, success);
        }
        if matches!(layout, PayloadLayout::Scalar(TypeExpr::Unit)) {
            return Ok(EvalOutcome::Value(Value::Unit.into()));
        }
        let payload = self.call_runtime(
            self.runtime_abi.prime_enum_get,
            self.runtime_abi.prime_enum_get_ty,
            &mut [handle, unsafe {
                LLVMConstNull(self.runtime_abi.usize_type)
            }],
            "try_payload",
        );
        Ok(EvalOutcome::Value(self.decode_payload(payload, &layout)?))
    }
}
