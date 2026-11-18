use super::*;
use crate::language::tcp_builtin::TcpBuiltin;

struct PatternGuard<'a> {
    condition: LLVMValueRef,
    binding: Option<(&'a str, LLVMValueRef, Option<TypeExpr>)>,
}

enum EnumArm<'a> {
    Variant {
        tag: u32,
        fields: Vec<TypeExpr>,
        bindings: &'a [Pattern],
    },
    CatchAll(Option<&'a str>),
}

struct RuntimeEnumMatch<'a> {
    handle: LLVMValueRef,
    tag: LLVMValueRef,
    arms: Vec<EnumArm<'a>>,
}

impl Compiler {
    pub(super) fn emit_expression_with_hint(
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
            _ => {
                let result = self.emit_expression(expr)?;
                if let Some(ty @ TypeExpr::Named(name, _)) = expected
                    && let EvalOutcome::Value(value) = &result
                    && matches!(value.value(), Value::Enum(e) if e.enum_name == *name)
                    && let Some(handle) = value.runtime_handle()
                {
                    self.handle_types.insert(handle, ty.clone());
                }
                Ok(result)
            }
        }
    }

    pub(super) fn collect_free_closure_vars(
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

    pub(super) fn collect_free_in_block(
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
                Statement::Comment { .. } => {}
            }
        }
        if let Some(tail) = &block.tail {
            self.collect_free_in_expr(tail, locals, free);
        }
    }

    pub(super) fn collect_free_in_expr(
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
                    let mut bindings = locals.clone();
                    bindings.extend(crate::language::macro_expander::collect_bindings_pattern(
                        &arm.pattern,
                    ));
                    if let Some(guard) = &arm.guard {
                        self.collect_free_in_expr(guard, &mut bindings, free);
                    }
                    self.collect_free_in_expr(&arm.value, &mut bindings, free);
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

    pub(super) fn alloc_handle_array(&mut self, handles: &[LLVMValueRef]) -> LLVMValueRef {
        if handles.is_empty() {
            return self.null_handle_ptr();
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
            LLVMBuildInBoundsGEP2(
                self.builder,
                array_type,
                alloca,
                indices.as_mut_ptr(),
                2,
                CString::new("enum_vals_base").unwrap().as_ptr(),
            )
        }
    }

    pub(super) fn emit_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<Option<FlowSignal>, String> {
        match statement {
            Statement::Let(stmt) => match &stmt.pattern {
                Pattern::Identifier(name, _) => {
                    let mut value = if let Some(expr) = &stmt.value {
                        let expected = stmt.ty.as_ref().map(|ann| &ann.ty);
                        match self.emit_expression_with_hint(expr, expected)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        }
                    } else {
                        self.evaluated(Value::Unit)
                    };
                    if self.target.is_embedded()
                        && stmt.mutability == Mutability::Mutable
                        && let Some(TypeExpr::Array { ty, size }) =
                            stmt.ty.as_ref().map(|ann| &ann.ty)
                        && let Value::Slice(slice) = value.value_mut()
                        && let Some(element) = self.scalar_storage_type(ty)
                    {
                        let array_ty = unsafe { LLVMArrayType2(element, *size as u64) };
                        let ptr = self.build_entry_alloca(array_ty, name);
                        let array = FixedArray {
                            base: StorageSlot {
                                ptr,
                                ty: element,
                                projected: true,
                            },
                            len: *size,
                        };
                        self.initialize_array(array, slice)?;
                        slice.fixed = Some(array);
                    }
                    self.prepare_shared_collection(&mut value, stmt.span)?;
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
            Statement::Comment { .. } => {}
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
            Statement::Break => {
                if let Some(control) = self.loop_controls.last().copied() {
                    unsafe {
                        let branch = LLVMBuildBr(self.builder, control.break_block);
                        self.token_loop_exit(control, branch);
                    }
                }
                return Ok(Some(FlowSignal::Break));
            }
            Statement::Continue => {
                if let Some(control) = self.loop_controls.last().copied() {
                    unsafe {
                        let branch = LLVMBuildBr(self.builder, control.continue_block);
                        self.token_loop_exit(control, branch);
                    }
                }
                return Ok(Some(FlowSignal::Continue));
            }
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
                            self.write_reference(&pointer, value)?;
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
                    if let Some(flag) = cond_value
                        .constant()
                        .filter(|_| !self.runtime_handles_enabled())
                    {
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
                            BlockEval::Flow(flow) => return Ok(Some(flow)),
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
                            // The first condition has already been emitted. Use its value
                            // here, then evaluate it again only on the loop back edge.
                            let first = self.bool_llvm_value(&cond_value);
                            LLVMBuildCondBr(self.builder, first, body_block, after_block);
                            LLVMPositionBuilderAtEnd(self.builder, cond_block);
                            let cond_value = match self.emit_expression(expr)? {
                                EvalOutcome::Value(value) => value.into_value(),
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            };
                            let cond_bool = self.value_to_bool(cond_value)?;
                            let cond_llvm = self.bool_llvm_value(&cond_bool);
                            LLVMBuildCondBr(self.builder, cond_llvm, body_block, after_block);
                            LLVMPositionBuilderAtEnd(self.builder, body_block);
                            self.loop_controls.push(LoopControl {
                                break_block: after_block,
                                continue_block: cond_block,
                            });
                            let result = (|| {
                                self.push_scope();
                                let result = self.execute_block_contents(&stmt.body)?;
                                self.exit_scope()?;
                                Ok::<_, String>(result)
                            })();
                            self.loop_controls.pop();
                            self.finish_loop_body(result?, after_block, cond_block)?;
                            LLVMPositionBuilderAtEnd(self.builder, after_block);
                        }
                        break;
                    }
                },
                WhileCondition::Let { pattern, value } => loop {
                    let header = unsafe {
                        let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                        let header = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            c"while_let_test".as_ptr(),
                        );
                        LLVMBuildBr(self.builder, header);
                        LLVMPositionBuilderAtEnd(self.builder, header);
                        header
                    };
                    let candidate = match self.emit_expression(value)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let guard = if let Pattern::Literal(Literal::Bool(expected, _)) = pattern {
                        let flag = self.value_to_bool(candidate.value().clone())?;
                        let condition = self.bool_llvm_value(&flag);
                        let condition = if *expected {
                            condition
                        } else {
                            unsafe {
                                LLVMBuildNot(self.builder, condition, c"while_let_not".as_ptr())
                            }
                        };
                        Some(PatternGuard {
                            condition,
                            binding: None,
                        })
                    } else {
                        self.lookup_pattern(&candidate, pattern)?
                    };
                    if let Some(guard) = guard {
                        let flow = self.emit_while_let_body(header, guard, &stmt.body)?;
                        if flow.is_some() && candidate.runtime_info().is_some() {
                            return Err("conditional return or propagation in a runtime lookup loop is unsupported".into());
                        }
                        return Ok(flow);
                    }

                    self.push_scope();
                    if !self.match_pattern(candidate.value(), pattern, false)? {
                        self.exit_scope()?;
                        break;
                    }
                    let result = self.execute_block_contents(&stmt.body)?;
                    self.exit_scope()?;
                    match result {
                        BlockEval::Value(_) | BlockEval::Flow(FlowSignal::Continue) => {}
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow) => return Ok(Some(flow)),
                    }
                },
            },
            Statement::Loop(loop_stmt) => {
                if self.runtime_handles_enabled() {
                    unsafe {
                        let current_block = LLVMGetInsertBlock(self.builder);
                        if current_block.is_null() {
                            return Err("no insertion block for loop".into());
                        }
                        let func = LLVMGetBasicBlockParent(current_block);
                        if func.is_null() {
                            return Err("loop outside function".into());
                        }

                        let loop_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            func,
                            CString::new("loop").unwrap().as_ptr(),
                        );
                        let after_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            func,
                            CString::new("after_loop").unwrap().as_ptr(),
                        );

                        LLVMBuildBr(self.builder, loop_bb);
                        LLVMPositionBuilderAtEnd(self.builder, loop_bb);

                        self.loop_controls.push(LoopControl {
                            break_block: after_bb,
                            continue_block: loop_bb,
                        });
                        let result = (|| {
                            self.push_scope();
                            let result = self.execute_block_contents(&loop_stmt.body)?;
                            self.exit_scope()?;
                            Ok::<_, String>(result)
                        })();
                        self.loop_controls.pop();
                        self.finish_loop_body(result?, after_bb, loop_bb)?;

                        // Continue compiling after the loop in an "after" block (may be unreachable).
                        LLVMPositionBuilderAtEnd(self.builder, after_bb);
                    }
                } else {
                    loop {
                        self.push_scope();
                        let result = self.execute_block_contents(&loop_stmt.body)?;
                        self.exit_scope()?;
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => continue,
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow) => return Ok(Some(flow)),
                        }
                    }
                }
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
                    if !self.runtime_handles_enabled() {
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
                                    BlockEval::Flow(flow) => return Ok(Some(flow)),
                                }
                                current += 1;
                            }
                        } else if let Some(flow) = self.emit_dynamic_range_for(
                            start_value,
                            end_value,
                            range_expr.inclusive,
                            stmt,
                        )? {
                            return Ok(Some(flow));
                        }
                    } else if let Some(flow) = self.emit_dynamic_range_for(
                        start_value,
                        end_value,
                        range_expr.inclusive,
                        stmt,
                    )? {
                        return Ok(Some(flow));
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
                            BlockEval::Flow(flow) => return Ok(Some(flow)),
                        }
                    }
                }
            },
        }
        Ok(None)
    }

    fn lookup_pattern<'a>(
        &mut self,
        candidate: &EvaluatedValue,
        pattern: &'a Pattern,
    ) -> Result<Option<PatternGuard<'a>>, String> {
        let Some(runtime) = candidate.runtime_info() else {
            return Ok(None);
        };
        let handle = runtime.handle;
        let Pattern::EnumVariant {
            enum_name,
            variant,
            bindings,
        } = pattern
        else {
            return Ok(None);
        };
        if matches!(candidate.value(), Value::Enum(e) if e.enum_name == "Result" && e.variant == "<runtime>")
        {
            let info = self
                .enum_variants
                .get(variant)
                .filter(|info| info.enum_name == "Result")
                .ok_or_else(|| format!("unknown Result variant `{variant}`"))?;
            let tag = unsafe { LLVMConstInt(self.i32_type, info.variant_index as u64, 0) };
            let field = info
                .field_types_for(self.handle_types.get(&handle))
                .into_iter()
                .next();
            let binding = match bindings.as_slice() {
                [Pattern::Identifier(name, _)] => Some((name.as_str(), handle, field)),
                [Pattern::Wildcard] => None,
                _ => return Err("runtime Result pattern requires a binding or wildcard".into()),
            };
            let actual = self.call_runtime(
                self.runtime_abi.prime_enum_tag,
                self.runtime_abi.prime_enum_tag_ty,
                &mut [handle],
                "result_tag",
            );
            let condition = unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    actual,
                    tag,
                    c"result_pattern".as_ptr(),
                )
            };
            return Ok(Some(PatternGuard { condition, binding }));
        }
        let Some(found) = runtime.lookup_found else {
            return Ok(None);
        };
        if enum_name.as_deref().is_some_and(|name| name != "Option") {
            return Ok(None);
        }
        let (condition, binding) = match (variant.as_str(), bindings.as_slice()) {
            ("Some", [Pattern::Identifier(name, _)]) => {
                (found, Some((name.as_str(), handle, None)))
            }
            ("Some", [Pattern::Wildcard]) => (found, None),
            ("None", []) => (
                unsafe { LLVMBuildNot(self.builder, found, c"lookup_missing".as_ptr()) },
                None,
            ),
            _ => return Err("unsupported pattern on runtime collection lookup".into()),
        };
        Ok(Some(PatternGuard { condition, binding }))
    }

    fn bind_lookup_payload(
        &mut self,
        binding: Option<(&str, LLVMValueRef, Option<TypeExpr>)>,
    ) -> Result<(), String> {
        if let Some((name, enum_handle, ty)) = binding {
            let index = unsafe { LLVMConstInt(self.runtime_abi.usize_type, 0, 0) };
            let handle = self.call_runtime(
                self.runtime_abi.prime_enum_get,
                self.runtime_abi.prime_enum_get_ty,
                &mut [enum_handle, index],
                "lookup_value",
            );
            if let Some(ty) = ty {
                self.handle_types.insert(handle, ty);
            }
            let payload = self.decode_match_field(handle)?;
            self.insert_var(name, payload, false)?;
        }
        Ok(())
    }

    fn finish_loop_body(
        &mut self,
        result: BlockEval,
        after: LLVMBasicBlockRef,
        next: LLVMBasicBlockRef,
    ) -> Result<(), String> {
        let destination = match result {
            BlockEval::Flow(flow @ (FlowSignal::Return(_) | FlowSignal::Propagate(_))) => {
                return self.branch_to_exit(flow);
            }
            BlockEval::Flow(FlowSignal::Break) => after,
            _ => next,
        };
        unsafe {
            if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder)).is_null() {
                LLVMBuildBr(self.builder, destination);
            }
        }
        Ok(())
    }

    fn emit_while_let_body(
        &mut self,
        header: LLVMBasicBlockRef,
        pattern: PatternGuard<'_>,
        body: &Block,
    ) -> Result<Option<FlowSignal>, String> {
        unsafe {
            let function = LLVMGetBasicBlockParent(header);
            let body_block =
                LLVMAppendBasicBlockInContext(self.context, function, c"while_let_body".as_ptr());
            let after =
                LLVMAppendBasicBlockInContext(self.context, function, c"while_let_after".as_ptr());
            LLVMBuildCondBr(self.builder, pattern.condition, body_block, after);
            LLVMPositionBuilderAtEnd(self.builder, body_block);
            self.push_scope();
            self.bind_lookup_payload(pattern.binding)?;
            self.loop_controls.push(LoopControl {
                break_block: after,
                continue_block: header,
            });
            let result = self.execute_block_contents(body);
            self.loop_controls.pop();
            self.exit_scope()?;
            self.finish_loop_body(result?, after, header)?;
            LLVMPositionBuilderAtEnd(self.builder, after);
        }
        Ok(None)
    }

    pub(super) fn emit_expression(
        &mut self,
        expr: &Expr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let result = self.emit_expression_inner(expr)?;
        if let EvalOutcome::Value(value) = &result
            && let Some(dest) = crate::language::integer::value_site(expr)
                .and_then(|id| self.checked.as_ref()?.integers.narrowing.get(&id))
                .copied()
        {
            let n = self.expect_int(value.value().clone())?;
            let wide = self.int_to_runtime_int(&n, "integer_destination")?;
            self.range_guard(wide, dest)?;
        }
        Ok(result)
    }

    fn emit_expression_inner(
        &mut self,
        expr: &Expr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        match expr {
            Expr::Literal(Literal::Int(value, _)) => Ok(EvalOutcome::Value(
                self.evaluated(Value::Int(self.const_int_value(*value))),
            )),
            Expr::Literal(Literal::Bool(value, _)) => Ok(EvalOutcome::Value(self.evaluated(
                Value::Bool(BoolValue::new(
                    unsafe { LLVMConstInt(self.runtime_abi.bool_type, *value as u64, 0) },
                    Some(*value),
                )),
            ))),
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
                self.begin_exit_boundary(ExitKind::Try, self.scopes.len());
                self.push_scope();
                let result = self.execute_block_contents(block)?;
                self.exit_scope()?;
                let result = match result {
                    BlockEval::Value(value) => {
                        let wrapped =
                            self.instantiate_enum_variant("Ok", vec![value.into_value()])?;
                        BlockEval::Value(self.evaluated(wrapped))
                    }
                    other => other,
                };
                match self.finish_exit_boundary(result)? {
                    BlockEval::Value(value) | BlockEval::Flow(FlowSignal::Propagate(value)) => {
                        Ok(EvalOutcome::Value(value))
                    }
                    BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                }
            }
            Expr::TryPropagate { id, expr, span } => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => self.eval_try_operator(value, *id, *span),
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
                id,
                op,
                left,
                right,
                ..
            } => {
                let lhs = match self.emit_expression(left)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let rhs = match self.emit_expression(right)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                if let Some(plan) = self.integer_operation(*id) {
                    let a = self.expect_int(lhs.into_value())?;
                    let b = self.expect_int(rhs.into_value())?;
                    let value = self.emit_integer_op(plan, a, Some(b))?;
                    return Ok(EvalOutcome::Value(self.evaluated(value)));
                }
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
            Expr::Async {
                block, captures, ..
            } => {
                self.ensure_async_supported()?;
                self.force_runtime_handles = true;
                let capture_names = captures.read().unwrap().clone().unwrap_or_else(|| {
                    let mut free = HashSet::new();
                    self.collect_free_in_block(block, &mut HashSet::new(), &mut free);
                    free.into_iter().collect()
                });
                let task = if self.target.is_host() {
                    self.emit_host_async(block, capture_names)?
                } else {
                    let captures = capture_names
                        .into_iter()
                        .filter_map(|name| {
                            let mutable = self.get_binding(&name)?.mutable;
                            let value = self.get_var(&name)?;
                            Some((name, value, mutable))
                        })
                        .collect();
                    TaskValue::deferred((**block).clone(), captures)
                };
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
                    let integers = self
                        .checked
                        .as_ref()
                        .map(|f| f.integers.clone())
                        .unwrap_or_default();
                    let handle = thread::spawn(move || {
                        let interpreter =
                            BuildInterpreter::new(snapshot).with_checked_integers(integers);
                        interpreter.eval_with_effects(&expr_clone)
                    });
                    Ok(EvalOutcome::Value(
                        Value::JoinHandle(Box::new(JoinHandleValue::new_build(handle))).into(),
                    ))
                } else {
                    match self.emit_expression(expr)? {
                        EvalOutcome::Value(value) => Ok(EvalOutcome::Value(
                            Value::JoinHandle(Box::new(JoinHandleValue::new(value))).into(),
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
            } => self.emit_closure_literal(params, body, ret, captures, None),
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
                        Value::Reference(reference) => self
                            .load_binding_value(&reference.cell, reference.slot)
                            .into_value(),
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
                        Value::CancelToken(_) => {
                            return Err("Cannot access field on cancel token value".into());
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
                id,
                callee,
                type_args,
                args,
                ..
            } => self.emit_call_expression(*id, callee, type_args, args),
            Expr::Unary { id, op, expr, .. } => {
                let value = match self.emit_expression(expr)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                if let Some(plan) = self.integer_operation(*id) {
                    let a = self.expect_int(value)?;
                    let value = self.emit_integer_op(plan, a, None)?;
                    return Ok(EvalOutcome::Value(self.evaluated(value)));
                }
                self.eval_unary(*op, value)
                    .map(|v| EvalOutcome::Value(self.evaluated(v)))
            }
            Expr::MacroCall { name, .. } => Err(format!(
                "macro `{}` must be expanded before code generation",
                name.name
            )),
        }
    }

    pub(super) fn eval_call_args_with_hints(
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

    pub(super) fn call_closure_value(
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

    pub(super) fn eval_async_block(
        &mut self,
        block: &Block,
        captures: &[(String, EvaluatedValue, bool)],
    ) -> Result<EvaluatedValue, String> {
        self.push_scope();
        for (name, value, mutable) in captures {
            self.insert_var(name, value.clone(), *mutable)?;
        }
        // Embedded deferred tasks emit into the caller's LLVM function. A ?
        // inside the task must never jump to the caller's Result boundary.
        self.push_return_types(&[]);
        let result = self.execute_block_contents(block);
        self.pop_return_types();
        let result = result?;
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
            BlockEval::Flow(FlowSignal::Propagate(_)) if self.in_host_task() => {
                let never = unsafe { LLVMConstNull(self.runtime_abi.bool_type) };
                self.host_task_check(never, "async task encountered error propagation")?;
                Ok(self.evaluated(Value::Unit))
            }
            BlockEval::Flow(FlowSignal::Propagate(value)) => Err(format!(
                "async block propagated error: {}",
                flow_name(&FlowSignal::Propagate(value.clone()))
            )),
            BlockEval::Flow(_) => Err("control flow cannot exit async block in build mode".into()),
        }
    }

    pub(super) fn emit_call_expression(
        &mut self,
        id: ExprId,
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
                if ident.name == "cast" {
                    let ([target], [argument]) = (type_args, args) else {
                        return Err("cast expects one target type and one value".into());
                    };
                    let plan = self
                        .integer_operation(id)
                        .or_else(|| {
                            IntegerType::from_type(target, self.target.pointer_width_bits())
                                .map(IntegerOp::Cast)
                        })
                        .ok_or("LLVM cast requires an integer target")?;
                    let a = match self.emit_expression(argument)? {
                        EvalOutcome::Value(v) => self.expect_int(v.into_value())?,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    let value = self.emit_integer_op(plan, a, None)?;
                    return Ok(EvalOutcome::Value(self.evaluated(value)));
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

    pub(super) fn collapse_results(&self, mut values: Vec<EvaluatedValue>) -> EvaluatedValue {
        match values.len() {
            0 => EvaluatedValue::from_value(Value::Unit),
            1 => values.pop().unwrap(),
            _ => {
                let tuple = values.into_iter().map(EvaluatedValue::into_value).collect();
                EvaluatedValue::from_value(Value::Tuple(tuple))
            }
        }
    }

    pub(super) fn emit_out_call(
        &mut self,
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
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

    pub(super) fn emit_in_call(
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
                let prompt = Self::render_runtime_value_bytes(&other)?;
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
        let type_code_const = unsafe { LLVMConstInt(self.i32_type, type_code as u64, 0) };
        let ok_tag_const = unsafe { LLVMConstInt(self.i32_type, ok_tag as u64, 0) };
        let err_tag_const = unsafe { LLVMConstInt(self.i32_type, err_tag as u64, 0) };

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

        self.handle_types.insert(
            result_handle,
            TypeExpr::Named(
                "Result".into(),
                vec![type_args[0].clone(), TypeExpr::named("string")],
            ),
        );
        // Wrap in EvaluatedValue with runtime handle
        Ok(EvalOutcome::Value(
            self.evaluated(Value::runtime_enum("Result"))
                .with_runtime(result_handle),
        ))
    }

    // A deferred embedded block has not started. Test its real token before
    // emitting the work on the runnable path; a failed allocation is cancelled.
    fn guard_deferred_task(
        &mut self,
        task: &TaskValue,
        token: LLVMValueRef,
        millis: Option<&IntValue>,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let cancelled = self.call_runtime(
            self.runtime_abi.prime_cancel_token_is_cancelled,
            self.runtime_abi.prime_cancel_token_is_cancelled_ty,
            &mut [token],
            "deferred_cancelled",
        );
        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let block = |name: &CStr| unsafe {
            LLVMAppendBasicBlockInContext(self.context, function, name.as_ptr())
        };
        let cancel = block(c"deferred_cancel");
        let check_time = block(c"deferred_check_time");
        let run = block(c"deferred_run");
        let merge = block(c"deferred_merge");
        let mut incoming = Vec::new();
        unsafe {
            LLVMBuildCondBr(self.builder, cancelled, cancel, check_time);
            LLVMPositionBuilderAtEnd(self.builder, cancel);
        }
        let message = self.make_string_value("cancelled")?;
        incoming.push((
            self.instantiate_enum_variant("Err", vec![message])?.into(),
            cancel,
        ));
        unsafe {
            LLVMBuildBr(self.builder, merge);
            LLVMPositionBuilderAtEnd(self.builder, check_time);
        }
        if let Some(millis) = millis {
            let duration = millis.llvm();
            let expired = unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                    duration,
                    LLVMConstNull(LLVMTypeOf(duration)),
                    c"expired".as_ptr(),
                )
            };
            let timeout = unsafe {
                LLVMAppendBasicBlockInContext(self.context, function, c"deferred_timeout".as_ptr())
            };
            unsafe {
                LLVMBuildCondBr(self.builder, expired, timeout, run);
                LLVMPositionBuilderAtEnd(self.builder, timeout);
            }
            let message = self.make_string_value("timeout")?;
            incoming.push((
                self.instantiate_enum_variant("Err", vec![message])?.into(),
                timeout,
            ));
            unsafe {
                LLVMBuildBr(self.builder, merge);
            }
        } else {
            unsafe {
                LLVMBuildBr(self.builder, run);
            }
        }
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, run);
        }
        let result = task.take(self)?;
        let ok = self.instantiate_enum_variant("Ok", vec![result.into_value()])?;
        incoming.push((ok.into(), unsafe { LLVMGetInsertBlock(self.builder) }));
        unsafe {
            LLVMBuildBr(self.builder, merge);
            LLVMPositionBuilderAtEnd(self.builder, merge);
        }
        Ok(EvalOutcome::Value(
            self.merge_match_values(&incoming, merge)?,
        ))
    }

    fn load_guarded_await_result(
        &mut self,
        result_slot: LLVMValueRef,
        err_tag: u32,
        name: &CStr,
    ) -> EvalOutcome<EvaluatedValue> {
        let handle = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                result_slot,
                name.as_ptr(),
            )
        };
        // All failure paths construct Err(string). Match bindings need this
        // field type because embedded runtime handles carry no type tag.
        self.enum_string_fields
            .insert(handle, HashSet::from([(err_tag, 0)]));
        EvalOutcome::Value(
            self.evaluated(Value::runtime_enum("Result"))
                .with_runtime(handle),
        )
    }

    pub(super) fn emit_await_timeout_call(
        &mut self,
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        self.ensure_async_supported()?;
        self.force_runtime_handles = true;
        if args.len() != 2 {
            return Err("`await_timeout` expects 2 arguments (Task[T], millis)".into());
        }

        let task_eval = match self.emit_expression(&args[0])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let millis_eval = match self.emit_expression(&args[1])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };

        let task = match task_eval.into_value() {
            Value::Task(task) => task,
            other => {
                return Err(format!(
                    "`await_timeout` expects a Task as the first argument (found {})",
                    self.describe_value(&other)
                ));
            }
        };
        let millis = self.expect_int(millis_eval.into_value())?;

        if self.target.is_host() {
            return self.emit_host_guarded_await(&task, None, Some(&millis));
        }
        if task.handle.is_none() || !self.runtime_handles_enabled() {
            let value = task.take(self)?;
            let ok = self.instantiate_enum_variant("Ok", vec![value.into_value()])?;
            return Ok(EvalOutcome::Value(self.evaluated(ok)));
        }

        let task_handle = task
            .handle
            .ok_or_else(|| "`await_timeout` task missing runtime backing".to_string())?;

        self.ensure_runtime_symbols();
        unsafe {
            if self.builder.is_null() || llvm_sys::core::LLVMGetInsertBlock(self.builder).is_null()
            {
                return Err("`await_timeout` requires active IR builder".into());
            }
        }

        let ok_tag = self
            .enum_variants
            .get("Ok")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Ok variant not found; ensure Result is available in build mode".to_string()
            })?;
        let err_tag = self
            .enum_variants
            .get("Err")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Err variant not found; ensure Result is available in build mode"
                    .to_string()
            })?;

        let slot = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_timeout_task_result").unwrap().as_ptr(),
            )
        };
        let result_slot = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_timeout_result_handle")
                    .unwrap()
                    .as_ptr(),
            )
        };

        let millis_rt = if let Some(constant) = millis.constant() {
            unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.int_type, constant as u64, 1) }
        } else {
            self.int_to_runtime_int(&millis, "await_timeout_millis")?
        };
        let zero = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.int_type, 0, 1) };
        let neg = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                millis_rt,
                zero,
                CString::new("await_timeout_neg").unwrap().as_ptr(),
            )
        };
        let millis_clamped = unsafe {
            llvm_sys::core::LLVMBuildSelect(
                self.builder,
                neg,
                zero,
                millis_rt,
                CString::new("await_timeout_millis_clamped")
                    .unwrap()
                    .as_ptr(),
            )
        };

        let mut now_args: [LLVMValueRef; 0] = [];
        let start = self.call_runtime(
            self.runtime_abi.prime_now_ms,
            self.runtime_abi.prime_now_ms_ty,
            &mut now_args,
            "now_ms",
        );
        let deadline = unsafe {
            llvm_sys::core::LLVMBuildAdd(
                self.builder,
                start,
                millis_clamped,
                CString::new("await_timeout_deadline").unwrap().as_ptr(),
            )
        };

        let function =
            unsafe { llvm_sys::core::LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let poll_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_timeout_poll").unwrap().as_ptr(),
            )
        };
        let ready_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_timeout_ready").unwrap().as_ptr(),
            )
        };
        let check_timeout_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_timeout_check").unwrap().as_ptr(),
            )
        };
        let timeout_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_timeout_timeout").unwrap().as_ptr(),
            )
        };
        let merge_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_timeout_merge").unwrap().as_ptr(),
            )
        };

        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, poll_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, poll_block) };
        let mut poll_args = [task_handle, slot];
        let status = self.call_runtime(
            self.runtime_abi.prime_task_poll,
            self.runtime_abi.prime_task_poll_ty,
            &mut poll_args,
            "task_poll",
        );
        let ok_const = unsafe {
            llvm_sys::core::LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Ok as u64, 0)
        };
        let is_ready = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                status,
                ok_const,
                CString::new("await_timeout_ready_flag").unwrap().as_ptr(),
            )
        };
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(
                self.builder,
                is_ready,
                ready_block,
                check_timeout_block,
            )
        };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, check_timeout_block) };
        let mut now_args: [LLVMValueRef; 0] = [];
        let now = self.call_runtime(
            self.runtime_abi.prime_now_ms,
            self.runtime_abi.prime_now_ms_ty,
            &mut now_args,
            "now_ms",
        );
        let timed_out = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                now,
                deadline,
                CString::new("await_timeout_timed_out").unwrap().as_ptr(),
            )
        };
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(self.builder, timed_out, timeout_block, poll_block)
        };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, ready_block) };
        let loaded = unsafe {
            llvm_sys::core::LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                slot,
                CString::new("await_timeout_value").unwrap().as_ptr(),
            )
        };
        let payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_timeout_ok_payload").unwrap().as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, loaded, payload_ptr) };
        let ok_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), ok_tag as u64, 0)
        };
        let ok_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut ok_args = [payload_ptr, ok_len, ok_tag_const];
        let ok_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut ok_args,
            "result_ok",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, ok_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, timeout_block) };
        let mut cancel_args = [task_handle];
        let _ = self.call_runtime(
            self.runtime_abi.prime_task_cancel,
            self.runtime_abi.prime_task_cancel_ty,
            &mut cancel_args,
            "task_cancel",
        );
        let (ptr, len) = self.build_runtime_bytes("timeout", "await_timeout_err")?;
        let mut str_args = [ptr, len];
        let str_handle = self.call_runtime(
            self.runtime_abi.prime_string_new,
            self.runtime_abi.prime_string_new_ty,
            &mut str_args,
            "string_new",
        );
        let err_payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_timeout_err_payload").unwrap().as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, str_handle, err_payload_ptr) };
        let err_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), err_tag as u64, 0)
        };
        let err_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut err_args = [err_payload_ptr, err_len, err_tag_const];
        let err_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut err_args,
            "result_err",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, err_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, merge_block) };
        Ok(self.load_guarded_await_result(result_slot, err_tag, c"await_timeout_result"))
    }

    pub(super) fn emit_await_cancel_call(
        &mut self,
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        self.ensure_async_supported()?;
        self.force_runtime_handles = true;
        if args.len() != 2 {
            return Err("`await_cancel` expects 2 arguments (Task[T], CancelToken)".into());
        }

        let task_eval = match self.emit_expression(&args[0])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let token_eval = match self.emit_expression(&args[1])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };

        let task = match task_eval.into_value() {
            Value::Task(task) => task,
            other => {
                return Err(format!(
                    "`await_cancel` expects a Task as the first argument (found {})",
                    self.describe_value(&other)
                ));
            }
        };
        let token = match token_eval.into_value() {
            Value::CancelToken(token) => token,
            other => {
                return Err(format!(
                    "`await_cancel` expects a CancelToken as the second argument (found {})",
                    self.describe_value(&other)
                ));
            }
        };

        if self.target.is_host() {
            return self.emit_host_guarded_await(&task, Some(&token), None);
        }
        if matches!(task.kind, TaskResultKind::Deferred)
            && let Some(handle) = token.handle
        {
            return self.guard_deferred_task(&task, handle, None);
        }
        if task.handle.is_none() || !self.runtime_handles_enabled() {
            if token.is_cancelled() {
                let err = self
                    .instantiate_enum_variant("Err", vec![self.make_string_value("cancelled")?])?;
                return Ok(EvalOutcome::Value(self.evaluated(err)));
            }
            let value = task.take(self)?;
            let ok = self.instantiate_enum_variant("Ok", vec![value.into_value()])?;
            return Ok(EvalOutcome::Value(self.evaluated(ok)));
        }

        let task_handle = task
            .handle
            .ok_or_else(|| "`await_cancel` task missing runtime backing".to_string())?;
        let token_handle = token
            .handle
            .ok_or_else(|| "`await_cancel` cancel token missing runtime backing".to_string())?;

        self.ensure_runtime_symbols();
        unsafe {
            if self.builder.is_null() || llvm_sys::core::LLVMGetInsertBlock(self.builder).is_null()
            {
                return Err("`await_cancel` requires active IR builder".into());
            }
        }

        let ok_tag = self
            .enum_variants
            .get("Ok")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Ok variant not found; ensure Result is available in build mode".to_string()
            })?;
        let err_tag = self
            .enum_variants
            .get("Err")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Err variant not found; ensure Result is available in build mode"
                    .to_string()
            })?;

        let slot = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_task_result").unwrap().as_ptr(),
            )
        };
        let result_slot = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_result_handle").unwrap().as_ptr(),
            )
        };

        let function =
            unsafe { llvm_sys::core::LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let poll_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_poll").unwrap().as_ptr(),
            )
        };
        let ready_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_ready").unwrap().as_ptr(),
            )
        };
        let check_cancel_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_check").unwrap().as_ptr(),
            )
        };
        let cancel_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_cancelled").unwrap().as_ptr(),
            )
        };
        let merge_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_merge").unwrap().as_ptr(),
            )
        };

        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, poll_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, poll_block) };
        let mut poll_args = [task_handle, slot];
        let status = self.call_runtime(
            self.runtime_abi.prime_task_poll,
            self.runtime_abi.prime_task_poll_ty,
            &mut poll_args,
            "task_poll",
        );
        let ok_const = unsafe {
            llvm_sys::core::LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Ok as u64, 0)
        };
        let is_ready = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                status,
                ok_const,
                CString::new("await_cancel_ready_flag").unwrap().as_ptr(),
            )
        };
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(self.builder, is_ready, ready_block, check_cancel_block)
        };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, check_cancel_block) };
        let mut cancel_args = [token_handle];
        let cancelled = self.call_runtime(
            self.runtime_abi.prime_cancel_token_is_cancelled,
            self.runtime_abi.prime_cancel_token_is_cancelled_ty,
            &mut cancel_args,
            "token_is_cancelled",
        );
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(self.builder, cancelled, cancel_block, poll_block)
        };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, ready_block) };
        let loaded = unsafe {
            llvm_sys::core::LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                slot,
                CString::new("await_cancel_value").unwrap().as_ptr(),
            )
        };
        let payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_ok_payload").unwrap().as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, loaded, payload_ptr) };
        let ok_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), ok_tag as u64, 0)
        };
        let ok_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut ok_args = [payload_ptr, ok_len, ok_tag_const];
        let ok_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut ok_args,
            "result_ok",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, ok_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, cancel_block) };
        let mut cancel_task_args = [task_handle];
        let _ = self.call_runtime(
            self.runtime_abi.prime_task_cancel,
            self.runtime_abi.prime_task_cancel_ty,
            &mut cancel_task_args,
            "task_cancel",
        );
        let (ptr, len) = self.build_runtime_bytes("cancelled", "await_cancel_err")?;
        let mut str_args = [ptr, len];
        let str_handle = self.call_runtime(
            self.runtime_abi.prime_string_new,
            self.runtime_abi.prime_string_new_ty,
            &mut str_args,
            "string_new",
        );
        let err_payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_err_payload").unwrap().as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, str_handle, err_payload_ptr) };
        let err_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), err_tag as u64, 0)
        };
        let err_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut err_args = [err_payload_ptr, err_len, err_tag_const];
        let err_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut err_args,
            "result_err",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, err_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, merge_block) };
        Ok(self.load_guarded_await_result(result_slot, err_tag, c"await_cancel_result"))
    }

    pub(super) fn emit_await_cancel_timeout_call(
        &mut self,
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        self.ensure_async_supported()?;
        self.force_runtime_handles = true;
        if args.len() != 3 {
            return Err(
                "`await_cancel_timeout` expects 3 arguments (Task[T], CancelToken, millis)".into(),
            );
        }

        let task_eval = match self.emit_expression(&args[0])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let token_eval = match self.emit_expression(&args[1])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let millis_eval = match self.emit_expression(&args[2])? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };

        let task = match task_eval.into_value() {
            Value::Task(task) => task,
            other => {
                return Err(format!(
                    "`await_cancel_timeout` expects a Task as the first argument (found {})",
                    self.describe_value(&other)
                ));
            }
        };
        let token = match token_eval.into_value() {
            Value::CancelToken(token) => token,
            other => {
                return Err(format!(
                    "`await_cancel_timeout` expects a CancelToken as the second argument (found {})",
                    self.describe_value(&other)
                ));
            }
        };
        let millis = self.expect_int(millis_eval.into_value())?;

        if self.target.is_host() {
            return self.emit_host_guarded_await(&task, Some(&token), Some(&millis));
        }
        if matches!(task.kind, TaskResultKind::Deferred)
            && let Some(handle) = token.handle
        {
            return self.guard_deferred_task(&task, handle, Some(&millis));
        }
        if task.handle.is_none() || !self.runtime_handles_enabled() {
            if token.is_cancelled() {
                let err = self
                    .instantiate_enum_variant("Err", vec![self.make_string_value("cancelled")?])?;
                return Ok(EvalOutcome::Value(self.evaluated(err)));
            }
            let value = task.take(self)?;
            let ok = self.instantiate_enum_variant("Ok", vec![value.into_value()])?;
            return Ok(EvalOutcome::Value(self.evaluated(ok)));
        }

        let task_handle = task
            .handle
            .ok_or_else(|| "`await_cancel_timeout` task missing runtime backing".to_string())?;
        let token_handle = token.handle.ok_or_else(|| {
            "`await_cancel_timeout` cancel token missing runtime backing".to_string()
        })?;

        self.ensure_runtime_symbols();
        unsafe {
            if self.builder.is_null() || llvm_sys::core::LLVMGetInsertBlock(self.builder).is_null()
            {
                return Err("`await_cancel_timeout` requires active IR builder".into());
            }
        }

        let ok_tag = self
            .enum_variants
            .get("Ok")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Ok variant not found; ensure Result is available in build mode".to_string()
            })?;
        let err_tag = self
            .enum_variants
            .get("Err")
            .map(|v| v.variant_index)
            .ok_or_else(|| {
                "Result::Err variant not found; ensure Result is available in build mode"
                    .to_string()
            })?;

        let slot = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_timeout_task_result")
                    .unwrap()
                    .as_ptr(),
            )
        };
        let result_slot = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_timeout_result_handle")
                    .unwrap()
                    .as_ptr(),
            )
        };

        let millis_rt = if let Some(constant) = millis.constant() {
            unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.int_type, constant as u64, 1) }
        } else {
            self.int_to_runtime_int(&millis, "await_cancel_timeout_millis")?
        };
        let zero = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.int_type, 0, 1) };
        let neg = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                millis_rt,
                zero,
                CString::new("await_cancel_timeout_neg").unwrap().as_ptr(),
            )
        };
        let millis_clamped = unsafe {
            llvm_sys::core::LLVMBuildSelect(
                self.builder,
                neg,
                zero,
                millis_rt,
                CString::new("await_cancel_timeout_millis_clamped")
                    .unwrap()
                    .as_ptr(),
            )
        };

        let mut now_args: [LLVMValueRef; 0] = [];
        let start = self.call_runtime(
            self.runtime_abi.prime_now_ms,
            self.runtime_abi.prime_now_ms_ty,
            &mut now_args,
            "now_ms",
        );
        let deadline = unsafe {
            llvm_sys::core::LLVMBuildAdd(
                self.builder,
                start,
                millis_clamped,
                CString::new("await_cancel_timeout_deadline")
                    .unwrap()
                    .as_ptr(),
            )
        };

        let function =
            unsafe { llvm_sys::core::LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let check_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_check").unwrap().as_ptr(),
            )
        };
        let poll_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_poll").unwrap().as_ptr(),
            )
        };
        let ready_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_ready").unwrap().as_ptr(),
            )
        };
        let cancel_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_cancelled")
                    .unwrap()
                    .as_ptr(),
            )
        };
        let timeout_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_timeout")
                    .unwrap()
                    .as_ptr(),
            )
        };
        let merge_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_merge").unwrap().as_ptr(),
            )
        };

        // Initial poll for immediate completion.
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, poll_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, poll_block) };
        let mut poll_args = [task_handle, slot];
        let status = self.call_runtime(
            self.runtime_abi.prime_task_poll,
            self.runtime_abi.prime_task_poll_ty,
            &mut poll_args,
            "task_poll",
        );
        let ok_const = unsafe {
            llvm_sys::core::LLVMConstInt(self.runtime_abi.status_type, PrimeStatus::Ok as u64, 0)
        };
        let is_ready = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                status,
                ok_const,
                CString::new("await_cancel_timeout_ready_flag")
                    .unwrap()
                    .as_ptr(),
            )
        };
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(self.builder, is_ready, ready_block, check_block)
        };

        // Interpreter parity: cancellation/timeout checks happen before the next poll.
        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, check_block) };
        let mut token_args = [token_handle];
        let cancelled = self.call_runtime(
            self.runtime_abi.prime_cancel_token_is_cancelled,
            self.runtime_abi.prime_cancel_token_is_cancelled_ty,
            &mut token_args,
            "token_is_cancelled",
        );
        let check_timeout_block = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("await_cancel_timeout_check_timeout")
                    .unwrap()
                    .as_ptr(),
            )
        };
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(
                self.builder,
                cancelled,
                cancel_block,
                check_timeout_block,
            )
        };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, check_timeout_block) };
        let mut now_args: [LLVMValueRef; 0] = [];
        let now = self.call_runtime(
            self.runtime_abi.prime_now_ms,
            self.runtime_abi.prime_now_ms_ty,
            &mut now_args,
            "now_ms",
        );
        let timed_out = unsafe {
            llvm_sys::core::LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                now,
                deadline,
                CString::new("await_cancel_timeout_timed_out")
                    .unwrap()
                    .as_ptr(),
            )
        };
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(self.builder, timed_out, timeout_block, poll_block)
        };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, ready_block) };
        let loaded = unsafe {
            llvm_sys::core::LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                slot,
                CString::new("await_cancel_timeout_value").unwrap().as_ptr(),
            )
        };
        let payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_timeout_ok_payload")
                    .unwrap()
                    .as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, loaded, payload_ptr) };
        let ok_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), ok_tag as u64, 0)
        };
        let ok_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut ok_args = [payload_ptr, ok_len, ok_tag_const];
        let ok_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut ok_args,
            "result_ok",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, ok_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, cancel_block) };
        let mut cancel_task_args = [task_handle];
        let _ = self.call_runtime(
            self.runtime_abi.prime_task_cancel,
            self.runtime_abi.prime_task_cancel_ty,
            &mut cancel_task_args,
            "task_cancel",
        );
        let (ptr, len) =
            self.build_runtime_bytes("cancelled", "await_cancel_timeout_err_cancelled")?;
        let mut str_args = [ptr, len];
        let str_handle = self.call_runtime(
            self.runtime_abi.prime_string_new,
            self.runtime_abi.prime_string_new_ty,
            &mut str_args,
            "string_new",
        );
        let err_payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_timeout_err_payload")
                    .unwrap()
                    .as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, str_handle, err_payload_ptr) };
        let err_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), err_tag as u64, 0)
        };
        let err_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut err_args = [err_payload_ptr, err_len, err_tag_const];
        let err_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut err_args,
            "result_err",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, err_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, timeout_block) };
        let mut cancel_task_args = [task_handle];
        let _ = self.call_runtime(
            self.runtime_abi.prime_task_cancel,
            self.runtime_abi.prime_task_cancel_ty,
            &mut cancel_task_args,
            "task_cancel",
        );
        let (ptr, len) = self.build_runtime_bytes("timeout", "await_cancel_timeout_err_timeout")?;
        let mut str_args = [ptr, len];
        let str_handle = self.call_runtime(
            self.runtime_abi.prime_string_new,
            self.runtime_abi.prime_string_new_ty,
            &mut str_args,
            "string_new",
        );
        let err_payload_ptr = unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.builder,
                self.runtime_abi.handle_type,
                CString::new("await_cancel_timeout_timeout_payload")
                    .unwrap()
                    .as_ptr(),
            )
        };
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, str_handle, err_payload_ptr) };
        let err_tag_const = unsafe {
            llvm_sys::core::LLVMConstInt(LLVMInt32TypeInContext(self.context), err_tag as u64, 0)
        };
        let err_len = unsafe { llvm_sys::core::LLVMConstInt(self.runtime_abi.usize_type, 1, 0) };
        let mut err_args = [err_payload_ptr, err_len, err_tag_const];
        let err_handle = self.call_runtime(
            self.runtime_abi.prime_enum_new,
            self.runtime_abi.prime_enum_new_ty,
            &mut err_args,
            "result_err",
        );
        unsafe { llvm_sys::core::LLVMBuildStore(self.builder, err_handle, result_slot) };
        unsafe { llvm_sys::core::LLVMBuildBr(self.builder, merge_block) };

        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, merge_block) };
        Ok(self.load_guarded_await_result(result_slot, err_tag, c"await_cancel_timeout_result"))
    }
    fn string_or_slice_operand(
        &mut self,
        value: EvaluatedValue,
        temporaries: &mut Vec<LLVMValueRef>,
    ) -> Result<LLVMValueRef, String> {
        if let Value::FormatTemplate(template) = value.value() {
            let mut parts = Vec::new();
            for segment in template.segments.clone() {
                let part = match segment {
                    FormatRuntimeSegment::Literal(text) => self.build_string_constant(text)?.into(),
                    FormatRuntimeSegment::Named(value) => value,
                    FormatRuntimeSegment::Implicit => {
                        self.build_string_constant("{}".into())?.into()
                    }
                };
                parts.push(self.string_or_slice_operand(part, temporaries)?);
            }
            let handle = self.join_string_parts(&parts);
            temporaries.push(handle);
            return Ok(handle);
        }
        let borrowed = match value.value() {
            Value::Slice(slice) => slice.handle,
            Value::Reference(reference) => reference.handle.or(value.runtime_handle()),
            _ => value.runtime_handle(),
        };
        if let Some(handle) = borrowed {
            return Ok(handle);
        }
        let handle = self.build_runtime_handle(value.into_value())?;
        temporaries.push(handle);
        Ok(handle)
    }

    fn join_string_parts(&mut self, parts: &[LLVMValueRef]) -> LLVMValueRef {
        let ptr = self.alloc_handle_array(parts);
        let len = unsafe { LLVMConstInt(self.runtime_abi.usize_type, parts.len() as u64, 0) };
        self.call_runtime(
            self.runtime_abi.prime_string_join,
            self.runtime_abi.prime_string_join_ty,
            &mut [ptr, len],
            "string_join",
        )
    }

    fn emit_host_handle_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        if self.target.is_embedded() || !cfg!(feature = "std-builtins") {
            return Err(format!("{name} requires the host std runtime"));
        }
        self.ensure_runtime_symbols();
        let abi = self.runtime_abi;
        let socket = TcpBuiltin::lookup(name);
        let (function, signature, arity) = match (name, socket) {
            (_, Some(TcpBuiltin::Socket(op))) => (
                abi.prime_net_call,
                abi.prime_net_call_ty,
                op.parameters().len(),
            ),
            ("fs_exists", _) => (abi.prime_fs_exists, abi.prime_fs_exists_ty, 1),
            ("fs_read", _) => (abi.prime_fs_read, abi.prime_fs_read_ty, 1),
            ("fs_write", _) => (abi.prime_fs_write, abi.prime_fs_write_ty, 2),
            ("fs_write_bytes", _) => (abi.prime_fs_write_bytes, abi.prime_fs_write_bytes_ty, 2),
            _ => unreachable!(),
        };
        if args.len() != arity {
            return Err(format!("{name} expects {arity} arguments"));
        }
        let mut operands = Vec::new();
        let mut temporaries = Vec::new();
        for expr in args {
            let value = match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => value,
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            let handle = self.string_or_slice_operand(value, &mut temporaries)?;
            operands.push(handle);
        }
        if let Some(TcpBuiltin::Socket(op)) = socket {
            // Fixed argument slots avoid allocating an argument array inside Prime loops.
            operands.resize(4, unsafe { LLVMConstNull(abi.handle_type) });
            operands.insert(0, unsafe { LLVMConstInt(self.i32_type, op as u64, 0) });
        }
        if name != "fs_exists" {
            for variant in ["Ok", "Err"] {
                let info = self
                    .enum_variants
                    .get(variant)
                    .ok_or_else(|| format!("{name} requires Result::{variant}"))?;
                operands.push(unsafe { LLVMConstInt(self.i32_type, info.variant_index as u64, 0) });
            }
        }
        let handle = self.call_runtime(function, signature, &mut operands, name);
        for temporary in temporaries {
            self.runtime_release(temporary);
        }
        if name == "fs_exists" {
            return Ok(EvalOutcome::Value(
                Value::Bool(BoolValue::new(handle, None)).into(),
            ));
        }
        let success = if let Some(TcpBuiltin::Socket(op)) = socket {
            TcpBuiltin::ty(op.success())
        } else if name == "fs_read" {
            TypeExpr::named("string")
        } else {
            TypeExpr::Unit
        };
        self.handle_types.insert(
            handle,
            TypeExpr::Named("Result".into(), vec![success, TypeExpr::named("string")]),
        );
        Ok(EvalOutcome::Value(
            EvaluatedValue::from_value(Value::runtime_enum("Result")).with_runtime(handle),
        ))
    }

    pub(super) fn try_builtin_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Option<Result<EvalOutcome<EvaluatedValue>, String>> {
        if let Some(parameters) = crate::language::idf_socket::parameters(name) {
            return Some(self.invoke_builtin(args, |this, values| {
                this.builtin_idf_socket(name, parameters, values)
            }));
        }
        if let Some(op) = TcpBuiltin::lookup(name) {
            return Some(match op {
                TcpBuiltin::Socket(_) => self.emit_host_handle_call(name, args),
                _ => self.invoke_builtin(args, |this, values| this.builtin_tcp(name, op, values)),
            });
        }
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
            "slice_get_int" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_slice_get_int(values)))
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
            "panic" if self.in_host_task() => {
                Some(self.invoke_builtin(args, |this, mut values| {
                    if values.len() != 1 {
                        return Err("panic expects a message".into());
                    }
                    let message = Self::expect_string_value(values.remove(0), "panic")?;
                    let never = unsafe { LLVMConstNull(this.runtime_abi.bool_type) };
                    this.host_task_check(never, &message)?;
                    Ok(Value::Unit)
                }))
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
            "cancel_token" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_cancel_token(values)))
            }
            "cancel" => Some(self.invoke_builtin(args, |this, values| this.builtin_cancel(values))),
            "is_cancelled" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_is_cancelled(values)))
            }
            "await_timeout" => Some(self.emit_await_timeout_call(args)),
            "await_cancel" => Some(self.emit_await_cancel_call(args)),
            "await_cancel_timeout" => Some(self.emit_await_cancel_timeout_call(args)),
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
            "fs_exists" | "fs_read" | "fs_write" | "fs_write_bytes" => {
                Some(self.emit_host_handle_call(name, args))
            }
            "gfx_open" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_open(values)))
            }
            "gfx_clear" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_clear(values)))
            }
            "gfx_rect" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_rect(values)))
            }
            "gfx_sprite" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_sprite(values)))
            }
            "gfx_text" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_text(values)))
            }
            "gfx_text_int" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_text_int(values)))
            }
            "gfx_present" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_present(values)))
            }
            "gfx_key_down" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_key_down(values)))
            }
            "gfx_key_pressed" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_key_pressed(values)))
            }
            "gfx_should_close" => Some(
                self.invoke_builtin(args, |this, values| this.builtin_gfx_should_close(values)),
            ),
            "gfx_close" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_gfx_close(values)))
            }
            "audio_play" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_audio_play(values)))
            }
            "audio_stop" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_audio_stop(values)))
            }
            "audio_stop_all" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_audio_stop_all(values)))
            }
            "audio_set_volume" => Some(
                self.invoke_builtin(args, |this, values| this.builtin_audio_set_volume(values)),
            ),
            "audio_is_playing" => Some(
                self.invoke_builtin(args, |this, values| this.builtin_audio_is_playing(values)),
            ),
            "pin_mode" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_pin_mode(values)))
            }
            "digital_write" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_digital_write(values)))
            }
            "digital_read" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_digital_read(values)))
            }
            "reset_reason" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_reset_reason(values)))
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

    pub(super) fn emit_if_expression(
        &mut self,
        if_expr: &IfExpr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let (cond_bool, binding) = match &if_expr.condition {
            IfCondition::Expr(condition) => {
                let value = match self.emit_expression(condition)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                (self.value_to_bool(value)?, None)
            }
            IfCondition::Let { pattern, value } => {
                let candidate = match self.emit_expression(value)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                if let Some(lookup) = self.lookup_pattern(&candidate, pattern)? {
                    (BoolValue::new(lookup.condition, None), lookup.binding)
                } else {
                    self.push_scope();
                    if self.match_pattern(candidate.value(), pattern, false)? {
                        let result = self.execute_block_contents(&if_expr.then_branch)?;
                        self.exit_scope()?;
                        return Ok(match result {
                            BlockEval::Value(value) => EvalOutcome::Value(value),
                            BlockEval::Flow(flow) => EvalOutcome::Flow(flow),
                        });
                    }
                    self.exit_scope()?;
                    (self.const_bool_value(false), None)
                }
            }
        };
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
                self.bind_lookup_payload(binding)?;
                let then_eval = self.execute_block_contents(&if_expr.then_branch)?;
                self.exit_scope()?;
                let then_value = self.finish_if_branch(then_eval, merge_block)?;

                LLVMPositionBuilderAtEnd(self.builder, else_block);
                let else_eval = match &if_expr.else_branch {
                    Some(ElseBranch::Block(block)) => {
                        self.push_scope();
                        let result = self.execute_block_contents(block)?;
                        self.exit_scope()?;
                        result
                    }
                    Some(ElseBranch::ElseIf(nested)) => match self.emit_if_expression(nested)? {
                        EvalOutcome::Value(value) => BlockEval::Value(value),
                        EvalOutcome::Flow(flow) => BlockEval::Flow(flow),
                    },
                    None => BlockEval::Value(Value::Unit.into()),
                };
                let else_value = self.finish_if_branch(else_eval, merge_block)?;
                LLVMPositionBuilderAtEnd(self.builder, merge_block);
                let value = match (then_value, else_value) {
                    (None, None) => {
                        llvm_sys::core::LLVMBuildUnreachable(self.builder);
                        return Ok(EvalOutcome::Flow(FlowSignal::Terminated));
                    }
                    _ if if_expr.else_branch.is_none() => Value::Unit.into(),
                    (Some((value, _)), None) | (None, Some((value, _))) => value,
                    (Some((a, pred_a)), Some((b, pred_b))) => {
                        self.merge_if_values(a, pred_a, b, pred_b)?
                    }
                };
                Ok(EvalOutcome::Value(value))
            }
        }
    }

    fn finish_if_branch(
        &mut self,
        result: BlockEval,
        merge: LLVMBasicBlockRef,
    ) -> Result<Option<(EvaluatedValue, LLVMBasicBlockRef)>, String> {
        match result {
            BlockEval::Value(value) => unsafe {
                let pred = LLVMGetInsertBlock(self.builder);
                LLVMBuildBr(self.builder, merge);
                Ok(Some((value, pred)))
            },
            BlockEval::Flow(flow @ (FlowSignal::Return(_) | FlowSignal::Propagate(_))) => {
                self.branch_to_exit(flow)?;
                Ok(None)
            }
            BlockEval::Flow(_) => {
                if unsafe {
                    LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder)).is_null()
                } {
                    return Err("conditional control flow requires a runtime destination".into());
                }
                Ok(None)
            }
        }
    }

    pub(super) fn merge_if_values(
        &mut self,
        then_val: EvaluatedValue,
        then_block: LLVMBasicBlockRef,
        else_val: EvaluatedValue,
        else_block: LLVMBasicBlockRef,
    ) -> Result<EvaluatedValue, String> {
        let merge_block = unsafe { LLVMGetInsertBlock(self.builder) };
        let is_string = |value: &Value| {
            matches!(value, Value::Str(_))
                || matches!(value, Value::Reference(r) if r.handle.is_some_and(|h| self.is_string_handle(h)))
        };
        if self.target.is_host() && is_string(then_val.value()) && is_string(else_val.value()) {
            let joined = self.merge_match_values(
                &[(then_val, then_block), (else_val, else_block)],
                merge_block,
            )?;
            if let Some(handle) = joined.runtime_handle() {
                self.handle_types.insert(handle, TypeExpr::named("string"));
            }
            return Ok(joined);
        }
        if matches!((then_val.value(), else_val.value()), (Value::Enum(a), Value::Enum(b)) if a.enum_name == b.enum_name)
        {
            return self.merge_match_values(
                &[(then_val, then_block), (else_val, else_block)],
                merge_block,
            );
        }
        let builder = self.builder;
        let build_phi = |ty: LLVMTypeRef, name: &str| unsafe {
            let first_instr = LLVMGetFirstInstruction(merge_block);
            if !first_instr.is_null() {
                LLVMPositionBuilderBefore(builder, first_instr);
            } else {
                LLVMPositionBuilderAtEnd(builder, merge_block);
            }
            let phi = LLVMBuildPhi(builder, ty, CString::new(name).unwrap().as_ptr());
            LLVMPositionBuilderAtEnd(builder, merge_block);
            phi
        };
        let then_value = then_val.into_value();
        let else_value = else_val.into_value();
        match (then_value, else_value) {
            (Value::Int(a), Value::Int(b)) => {
                let mut vals = [a.llvm(), b.llvm()];
                if unsafe { LLVMTypeOf(vals[0]) != LLVMTypeOf(vals[1]) } {
                    for ((block, value), incoming) in [(then_block, &a), (else_block, &b)]
                        .into_iter()
                        .zip(&mut vals)
                    {
                        unsafe {
                            LLVMPositionBuilderBefore(builder, LLVMGetBasicBlockTerminator(block));
                        }
                        *incoming = self.int_to_runtime_int(value, "if_integer")?;
                    }
                    unsafe {
                        LLVMPositionBuilderAtEnd(builder, merge_block);
                    }
                }
                let phi = build_phi(unsafe { LLVMTypeOf(vals[0]) }, "if_int_phi");
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
                let phi = build_phi(ty, "if_float_phi");
                let mut vals = [a.llvm(), b.llvm()];
                let mut blocks = [then_block, else_block];
                unsafe {
                    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                }
                let constant = a.constant().zip(b.constant()).and_then(|(lhs, rhs)| {
                    if (lhs - rhs).abs() < f64::EPSILON {
                        Some(lhs)
                    } else {
                        None
                    }
                });
                Ok(self.evaluated(Value::Float(FloatValue::new(phi, constant))))
            }
            (Value::Bool(a), Value::Bool(b)) => {
                let ty = unsafe { LLVMTypeOf(a.llvm()) };
                let phi = build_phi(ty, "if_bool_phi");
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
            (Value::CancelToken(a), Value::CancelToken(b)) => {
                let phi = build_phi(self.runtime_abi.handle_type, "if_token");
                let mut values = [
                    a.handle.ok_or("missing token handle")?,
                    b.handle.ok_or("missing token handle")?,
                ];
                let mut blocks = [then_block, else_block];
                unsafe {
                    LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                }
                Ok(self.copy_token(phi).into())
            }
            (Value::Reference(a), Value::Reference(b)) => {
                if let (Some(ha), Some(hb)) = (a.handle, b.handle) {
                    let ty = unsafe { LLVMTypeOf(ha) };
                    let phi = build_phi(ty, "if_ref_phi");
                    let mut vals = [ha, hb];
                    let mut blocks = [then_block, else_block];
                    unsafe {
                        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                    }
                    return Ok(self.evaluated(Value::Reference(ReferenceValue {
                        indexed: None,
                        slot: None,
                        cell: a.cell.clone(),
                        mutable: a.mutable || b.mutable,
                        origin: a.origin.clone().or(b.origin.clone()),
                        handle: Some(phi),
                    })));
                }
                if Rc::ptr_eq(&a.cell, &b.cell) {
                    return Ok(self.evaluated(Value::Reference(a)));
                }
                Err(format!(
                    "Dynamic if expression not supported for branch values {} and {}",
                    self.describe_value(&Value::Reference(a)),
                    self.describe_value(&Value::Reference(b))
                ))
            }
            (Value::Unit, Value::Unit) => Ok(self.evaluated(Value::Unit)),
            (lhs, rhs) => Err(format!(
                "Dynamic if expression not supported for branch values {} and {}",
                self.describe_value(&lhs),
                self.describe_value(&rhs)
            )),
        }
    }

    pub(super) fn invoke_builtin<F, V>(
        &mut self,
        args: &[Expr],
        mut f: F,
    ) -> Result<EvalOutcome<EvaluatedValue>, String>
    where
        F: FnMut(&mut Self, Vec<Value>) -> Result<V, String>,
        V: Into<EvaluatedValue>,
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

    pub(super) fn scalar_storage_type(&self, ty: &TypeExpr) -> Option<LLVMTypeRef> {
        match ty {
            TypeExpr::Named(name, _) if name == "bool" => Some(self.runtime_abi.bool_type),
            TypeExpr::Named(name, _) if matches!(name.as_str(), "float32" | "float64") => {
                Some(self.f64_type)
            }
            TypeExpr::Named(name, _) if name == "rune" => Some(self.runtime_abi.int_type),
            _ => IntegerType::from_type(ty, self.target.pointer_width_bits())
                .map(|_| self.runtime_abi.int_type),
        }
    }

    pub(super) fn checked_index(
        &mut self,
        index: &IntValue,
        length: LLVMValueRef,
    ) -> Result<LLVMValueRef, String> {
        let wide = self.int_to_runtime_int(index, "borrow_index")?;
        let valid = unsafe {
            let length = llvm_sys::core::LLVMBuildZExt(
                self.builder,
                length,
                self.runtime_abi.int_type,
                c"borrow_length_wide".as_ptr(),
            );
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                wide,
                length,
                c"borrow_in_bounds".as_ptr(),
            )
        };
        self.runtime_guard(valid, "borrow index out of bounds")?;
        self.int_to_usize(index, "borrow_index_usize")
    }

    pub(super) fn build_reference(
        &mut self,
        expr: &Expr,
        mutable: bool,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        if mutable && let Some((owner, projection)) = projected_place(expr) {
            let binding = self
                .get_binding(&owner.name)
                .ok_or_else(|| format!("unknown owner `{}`", owner.name))?;
            let (cell, owner_slot) = {
                let guard = binding.cell.lock().unwrap();
                match guard.value() {
                    Value::Reference(r) if r.mutable => (r.cell.clone(), r.slot),
                    _ if binding.mutable => (binding.cell.clone(), binding.slot),
                    _ => return Err(format!("owner `{}` is immutable", owner.name)),
                }
            };
            let (target, slot, indexed) = match projection {
                Projection::Field(field) => {
                    let current = cell.lock().unwrap();
                    let Value::Struct(instance) = current.value() else {
                        return Err("field borrow requires a struct".into());
                    };
                    let storage = instance
                        .field_slots
                        .get(field)
                        .ok_or_else(|| format!("field `{field}` has no mutable storage"))?;
                    (storage.cell.clone(), storage.slot, None)
                }
                Projection::Index(id, index) => {
                    let value = match self.emit_expression(index)? {
                        EvalOutcome::Value(v) => v.into_value(),
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    let index = self.expect_int(value)?;
                    let current = match owner_slot {
                        Some(slot) => self.load_slot(slot),
                        None => cell.lock().unwrap().clone(),
                    };
                    let Value::Slice(slice) = current.value() else {
                        return Err("element borrow requires a slice or array".into());
                    };
                    let metadata = Rc::new(Mutex::new(Value::Unit.into()));
                    if let Some(array) = slice.fixed {
                        let length = unsafe {
                            LLVMConstInt(self.runtime_abi.usize_type, array.len as u64, 0)
                        };
                        let i = self.checked_index(&index, length)?;
                        (metadata, Some(array.element(self.builder, i)), None)
                    } else {
                        let handle = slice.handle.ok_or(
                            "indexed borrow requires a runtime slice handle or a fixed array",
                        )?;
                        let element = self
                            .checked
                            .as_ref()
                            .and_then(|c| c.indexed_loans.get(&id))
                            .cloned()
                            .ok_or("missing checked element type for indexed borrow")?;
                        let length = self.call_runtime(
                            self.runtime_abi.prime_slice_len_handle,
                            self.runtime_abi.prime_slice_len_handle_ty,
                            &mut [handle],
                            "borrow_length",
                        );
                        let index = self.checked_index(&index, length)?;
                        (
                            metadata,
                            None,
                            Some(IndexedLoan {
                                slice: handle,
                                index,
                                element,
                            }),
                        )
                    }
                }
            };
            return Ok(EvalOutcome::Value(
                Value::Reference(ReferenceValue {
                    indexed,
                    slot,
                    cell: target,
                    mutable,
                    origin: Some(owner.name.clone()),
                    handle: None,
                })
                .into(),
            ));
        }
        match expr {
            Expr::Identifier(ident) => {
                let Binding {
                    cell,
                    mutable: binding_mut,
                    slot,
                } = self
                    .get_binding(&ident.name)
                    .ok_or_else(|| format!("Unknown variable {}", ident.name))?;
                if mutable && !binding_mut {
                    return Err(format!(
                        "Variable `{}` is immutable and cannot be borrowed as mutable",
                        ident.name
                    ));
                }
                if self.borrows.contains(&cell) {
                    return Err(format!("`{}` is already mutably borrowed", ident.name));
                }
                if matches!(cell.lock().unwrap().value(), Value::Moved) {
                    return Err(format!("Value `{}` has been moved", ident.name));
                }
                Ok(EvalOutcome::Value(
                    Value::Reference(ReferenceValue {
                        indexed: None,
                        slot,
                        cell,
                        mutable,
                        origin: Some(ident.name.clone()),
                        handle: None,
                    })
                    .into(),
                ))
            }
            _ if mutable => Err("mutable borrow requires addressable storage".into()),
            _ => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => Ok(EvalOutcome::Value(
                    Value::Reference(ReferenceValue {
                        indexed: None,
                        slot: None,
                        cell: Rc::new(Mutex::new(value)),
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

    pub(super) fn emit_array_literal(
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

    pub(super) fn emit_map_literal(
        &mut self,
        entries: &[MapLiteralEntry],
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let mut pairs = Vec::new();
        for entry in entries {
            let key_value = match self.emit_expression(&entry.key)? {
                EvalOutcome::Value(value) => value.into_value(),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            let key = Self::expect_string_value(key_value, "map literal key")?;
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

    pub(super) fn emit_move_expression(
        &mut self,
        expr: &Expr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        match expr {
            Expr::Identifier(ident) => {
                let Binding { cell, .. } = self
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
                Ok(EvalOutcome::Value(moved))
            }
            _ => Err("move expressions require identifiers in build mode".into()),
        }
    }

    pub(super) fn is_heap_value(value: &Value) -> bool {
        matches!(value, Value::Boxed(_) | Value::Slice(_) | Value::Map(_))
    }

    pub(super) fn read_reference(
        &mut self,
        reference: &ReferenceValue,
    ) -> Result<EvaluatedValue, String> {
        if let Some(loan) = &reference.indexed {
            let slot = self.build_entry_alloca(self.runtime_abi.handle_type, "element_out");
            let status = self.call_runtime(
                self.runtime_abi.prime_slice_get_handle,
                self.runtime_abi.prime_slice_get_handle_ty,
                &mut [loan.slice, loan.index, slot],
                "element_get",
            );
            self.check_element_status(status)?;
            let payload = unsafe {
                LLVMBuildLoad2(
                    self.builder,
                    self.runtime_abi.handle_type,
                    slot,
                    c"element".as_ptr(),
                )
            };
            self.handle_types.insert(payload, loan.element.clone());
            let value = self.decode_enum_field(payload)?;
            self.runtime_release(payload);
            Ok(value.into())
        } else {
            Ok(self.load_binding_value(&reference.cell, reference.slot))
        }
    }

    pub(super) fn deref_value(&mut self, value: Value) -> Result<EvaluatedValue, String> {
        match value {
            Value::Reference(reference) | Value::Pointer(reference) => {
                self.read_reference(&reference)
            }
            _ => Err("Cannot dereference non-reference value in build mode".into()),
        }
    }

    pub(super) fn emit_match_expression(
        &mut self,
        match_expr: &MatchExpr,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let target = match self.emit_expression(&match_expr.expr)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let checked = self
            .checked
            .as_ref()
            .and_then(|facts| facts.match_expr(match_expr.id))
            .cloned();
        let runtime = self.runtime_enum_match(&target, match_expr, checked.as_ref())?;
        let mut merge = None;
        let mut incoming = Vec::new();
        let mut exhausted = false;
        for (idx, arm) in match_expr.arms.iter().enumerate() {
            let facts = checked.as_ref().map(|m| &m.arms[idx]);
            let mut next = None;
            self.push_scope();
            if let Some(runtime) = &runtime {
                match &runtime.arms[idx] {
                    EnumArm::Variant {
                        tag,
                        fields,
                        bindings,
                    } => {
                        let condition = unsafe {
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                                runtime.tag,
                                LLVMConstInt(LLVMTypeOf(runtime.tag), *tag as u64, 0),
                                c"match_tag_cmp".as_ptr(),
                            )
                        };
                        next = Some(self.branch_match_arm(condition, None));
                        self.bind_enum_fields(runtime.handle, *tag, bindings, fields)?;
                    }
                    EnumArm::CatchAll(Some(name)) => {
                        self.insert_var(name, target.clone(), false)?
                    }
                    EnumArm::CatchAll(None) => {}
                }
            } else if !self.match_pattern(target.value(), &arm.pattern, false)? {
                self.exit_scope()?;
                continue;
            }

            if let Some(guard) = &arm.guard {
                let hint = facts.and_then(|a| a.guard.as_ref()).map(|g| &g.ty);
                let value = match self.emit_expression_with_hint(guard, hint)? {
                    EvalOutcome::Value(v) => v,
                    EvalOutcome::Flow(flow) if next.is_none() && merge.is_none() => {
                        self.exit_scope()?;
                        return Ok(EvalOutcome::Flow(flow));
                    }
                    EvalOutcome::Flow(_) => {
                        return Err(
                            "cannot exit early from a runtime match guard in native code".into(),
                        );
                    }
                };
                let flag = self.value_to_bool(value.into_value())?;
                match flag.constant() {
                    Some(false) => {
                        self.exit_scope()?;
                        if let Some(next) = next {
                            unsafe {
                                LLVMBuildBr(self.builder, next);
                                LLVMPositionBuilderAtEnd(self.builder, next);
                            }
                        }
                        continue;
                    }
                    Some(true) => {}
                    None => {
                        let condition = self.bool_llvm_value(&flag);
                        next = Some(self.branch_match_arm(condition, next));
                    }
                }
            }

            let hint = facts.and_then(|a| a.value.as_ref()).map(|v| &v.ty);
            let outcome = self.emit_expression_with_hint(&arm.value, hint)?;
            self.exit_scope()?;
            if next.is_none() && merge.is_none() {
                return Ok(outcome);
            }
            let merge_block = *merge.get_or_insert_with(|| self.match_block(c"match_merge"));
            match outcome {
                EvalOutcome::Value(value) => unsafe {
                    incoming.push((value, LLVMGetInsertBlock(self.builder)));
                    LLVMBuildBr(self.builder, merge_block);
                },
                EvalOutcome::Flow(flow @ (FlowSignal::Return(_) | FlowSignal::Propagate(_))) => {
                    self.branch_to_exit(flow)?;
                }
                EvalOutcome::Flow(FlowSignal::Terminated) => {}
                EvalOutcome::Flow(FlowSignal::Break | FlowSignal::Continue) => {
                    let block = unsafe { LLVMGetInsertBlock(self.builder) };
                    if unsafe { LLVMGetBasicBlockTerminator(block) }.is_null() {
                        return Err(
                            "break/continue in runtime match arm did not terminate block".into(),
                        );
                    }
                }
            }
            if let Some(next) = next {
                unsafe { LLVMPositionBuilderAtEnd(self.builder, next) };
            } else {
                exhausted = true;
                break;
            }
        }
        let Some(merge_block) = merge else {
            return Err(format!(
                "No match arm matched in build mode (value: {}, span {})",
                self.describe_value(target.value()),
                match_expr.span
            ));
        };
        unsafe {
            if !exhausted {
                llvm_sys::core::LLVMBuildUnreachable(self.builder);
            }
            LLVMPositionBuilderAtEnd(self.builder, merge_block);
            if incoming.is_empty() {
                llvm_sys::core::LLVMBuildUnreachable(self.builder);
                return Ok(EvalOutcome::Flow(FlowSignal::Terminated));
            }
        }
        Ok(EvalOutcome::Value(
            self.merge_match_values(&incoming, merge_block)?,
        ))
    }

    pub(super) fn match_block(&self, name: &CStr) -> LLVMBasicBlockRef {
        unsafe {
            let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            LLVMAppendBasicBlockInContext(self.context, function, name.as_ptr())
        }
    }

    // Both a tag mismatch and a false guard continue at the next source arm.
    fn branch_match_arm(
        &self,
        condition: LLVMValueRef,
        next: Option<LLVMBasicBlockRef>,
    ) -> LLVMBasicBlockRef {
        let success = self.match_block(c"match_arm");
        let next = next.unwrap_or_else(|| self.match_block(c"match_next"));
        unsafe {
            LLVMBuildCondBr(self.builder, condition, success, next);
            LLVMPositionBuilderAtEnd(self.builder, success);
        }
        next
    }

    fn runtime_enum_match<'a>(
        &mut self,
        target: &EvaluatedValue,
        expr: &'a MatchExpr,
        checked: Option<&CheckedMatch>,
    ) -> Result<Option<RuntimeEnumMatch<'a>>, String> {
        let handle = match target.value() {
            Value::Enum(_) => target.runtime_handle(),
            Value::Reference(r) => r.handle,
            _ => None,
        };
        let Some(handle) = handle else {
            return Ok(None);
        };

        // Raw Program callers predate checked facts. Only that entry point
        // resolves a layout from the compiler's enum registry.
        let legacy_owner = if self.checked.is_none() {
            match target.value() {
                Value::Enum(e) => Some(&e.enum_name),
                _ => expr.arms.iter().find_map(|arm| match &arm.pattern {
                    Pattern::EnumVariant { variant, .. } => {
                        self.enum_variants.get(variant).map(|v| &v.enum_name)
                    }
                    _ => None,
                }),
            }
        } else {
            None
        };
        if !checked.is_some_and(|m| m.is_enum) && legacy_owner.is_none() {
            return Ok(None);
        }

        let arms: Option<Vec<_>> = expr
            .arms
            .iter()
            .enumerate()
            .map(|(i, arm)| match &arm.pattern {
                Pattern::EnumVariant {
                    variant, bindings, ..
                } if bindings
                    .iter()
                    .all(|b| matches!(b, Pattern::Identifier(..) | Pattern::Wildcard)) =>
                {
                    let (tag, fields) = if let Some(checked) = checked {
                        let v = checked.arms[i].variant.as_ref()?;
                        (v.tag, v.fields.iter().map(|f| f.ty.clone()).collect())
                    } else {
                        let v = self.enum_variants.get(variant)?;
                        if Some(&v.enum_name) != legacy_owner {
                            return None;
                        }
                        (
                            v.variant_index,
                            v.field_types_for(self.handle_types.get(&handle)),
                        )
                    };
                    Some(EnumArm::Variant {
                        tag,
                        fields,
                        bindings,
                    })
                }
                Pattern::Identifier(name, _) => Some(EnumArm::CatchAll(Some(name))),
                Pattern::Wildcard => Some(EnumArm::CatchAll(None)),
                _ => None,
            })
            .collect();
        let Some(arms) = arms else {
            // Complex patterns can still be matched when the enum value is known.
            if matches!(target.value(), Value::Enum(e) if e.variant != "<runtime>") {
                return Ok(None);
            }
            return Err(format!(
                "unsupported pattern for a runtime enum at {}",
                expr.span
            ));
        };
        let tag = self.call_runtime(
            self.runtime_abi.prime_enum_tag,
            self.runtime_abi.prime_enum_tag_ty,
            &mut [handle],
            "enum_tag",
        );
        Ok(Some(RuntimeEnumMatch { handle, tag, arms }))
    }

    fn bind_enum_fields(
        &mut self,
        handle: LLVMValueRef,
        tag: u32,
        bindings: &[Pattern],
        fields: &[TypeExpr],
    ) -> Result<(), String> {
        for (idx, binding) in bindings.iter().enumerate() {
            let Pattern::Identifier(name, _) = binding else {
                continue;
            };
            let field = self.call_runtime(
                self.runtime_abi.prime_enum_get,
                self.runtime_abi.prime_enum_get_ty,
                &mut [handle, unsafe {
                    LLVMConstInt(self.runtime_abi.usize_type, idx as u64, 0)
                }],
                "enum_get",
            );
            if self
                .enum_string_fields
                .get(&handle)
                .is_some_and(|fs| fs.contains(&(tag, idx)))
            {
                self.handle_types.insert(field, TypeExpr::named("string"));
            } else if let Some(ty) = fields.get(idx) {
                self.handle_types.insert(field, ty.clone());
            }
            let value = self.decode_match_field(field)?;
            self.insert_var(name, value, false)?;
        }
        Ok(())
    }

    pub(super) fn decode_enum_field(&mut self, field: LLVMValueRef) -> Result<Value, String> {
        // Runtime enum fields are boxed. Arithmetic and equality need the
        // actual checked scalar kind, while strings retain their handle.
        let payload = Value::from_runtime_payload(field);
        match self.handle_types.get(&field) {
            Some(TypeExpr::Unit) => Ok(Value::Unit),
            Some(TypeExpr::Named(ty, _)) => match ty.as_str() {
                "bool" => Ok(Value::Bool(self.value_to_bool(payload)?)),
                "float32" | "float64" => Ok(Value::Float(self.expect_float(payload)?)),
                "int8" | "int16" | "int32" | "int64" | "isize" | "uint8" | "uint16" | "uint32"
                | "uint64" | "usize" | "rune" => Ok(Value::Int(self.expect_int(payload)?)),
                _ => Ok(payload),
            },
            _ => Ok(payload),
        }
    }

    pub(super) fn merge_match_values(
        &mut self,
        incoming: &[(EvaluatedValue, LLVMBasicBlockRef)],
        merge_block: LLVMBasicBlockRef,
    ) -> Result<EvaluatedValue, String> {
        let Some((template, _)) = incoming.first() else {
            return Ok(Value::Unit.into());
        };
        let same_kind = incoming.iter().all(|(value, _)| {
            mem::discriminant(value.value()) == mem::discriminant(template.value())
        });
        if same_kind && matches!(template.value(), Value::Unit) {
            return Ok(Value::Unit.into());
        }
        if !same_kind
            && incoming
                .iter()
                .any(|(v, _)| matches!(v.value(), Value::Unit))
        {
            return Err("runtime match arms must agree on whether they produce a value".into());
        }

        // Keep homogeneous scalars in SSA. Embedded targets do not box every scalar type.
        let mut values = Vec::with_capacity(incoming.len());
        let mut blocks = Vec::with_capacity(incoming.len());
        for (value, block) in incoming {
            unsafe { LLVMPositionBuilderBefore(self.builder, LLVMGetBasicBlockTerminator(*block)) };
            let operand = match value.value() {
                Value::Int(v) if same_kind => self.int_to_runtime_int(v, "match_int")?,
                Value::Float(v) if same_kind => unsafe {
                    llvm_sys::core::LLVMBuildFPCast(
                        self.builder,
                        v.llvm(),
                        self.f64_type,
                        c"match_float".as_ptr(),
                    )
                },
                Value::Bool(v) if same_kind => self.bool_llvm_value(v),
                Value::CancelToken(v) if same_kind => v.handle.ok_or("missing token handle")?,
                _ => self.task_value_handle(value)?,
            };
            values.push(operand);
            blocks.push(*block);
        }
        let phi = unsafe {
            LLVMPositionBuilderAtEnd(self.builder, merge_block);
            let phi = LLVMBuildPhi(self.builder, LLVMTypeOf(values[0]), c"match_value".as_ptr());
            LLVMAddIncoming(
                phi,
                values.as_mut_ptr(),
                blocks.as_mut_ptr(),
                blocks.len() as u32,
            );
            phi
        };
        match template.value() {
            Value::Int(_) if same_kind => Ok(Value::Int(IntValue::new(phi, None)).into()),
            Value::Float(_) if same_kind => Ok(Value::Float(FloatValue::new(phi, None)).into()),
            Value::Bool(_) if same_kind => Ok(Value::Bool(BoolValue::new(phi, None)).into()),
            Value::CancelToken(_) if same_kind => Ok(self.copy_token(phi).into()),
            shape => {
                if self.target.is_embedded() && matches!(shape, Value::Enum(_)) {
                    self.record_merged_enum_fields(phi, incoming);
                }
                self.task_value_from_handle(shape, phi, false)
            }
        }
    }

    fn record_merged_enum_fields(
        &mut self,
        handle: LLVMValueRef,
        incoming: &[(EvaluatedValue, LLVMBasicBlockRef)],
    ) {
        let mut known = Vec::new();
        let mut strings = HashSet::new();
        for (value, _) in incoming {
            let Value::Enum(variant) = value.value() else {
                return;
            };
            let (tag, fields) = if variant.variant == "<runtime>" {
                let Some(fields) = value
                    .runtime_handle()
                    .and_then(|h| self.enum_string_fields.get(&h))
                else {
                    return;
                };
                (None, fields.clone())
            } else {
                let mut fields = HashSet::new();
                for (index, field) in variant.values.iter().enumerate() {
                    let is_string = match field {
                        Value::Str(_) => true,
                        Value::Reference(r) => r.handle.is_some_and(|h| self.is_string_handle(h)),
                        _ => false,
                    };
                    if is_string {
                        fields.insert((variant.variant_index, index));
                    }
                }
                (Some(variant.variant_index), fields)
            };
            strings.extend(&fields);
            known.push((tag, fields));
        }
        // Every predecessor that could produce this tag must agree on the field.
        strings.retain(|field: &(u32, usize)| {
            known.iter().all(|(tag, fields)| {
                tag.is_some_and(|tag| tag != field.0) || fields.contains(field)
            })
        });
        if !strings.is_empty() {
            self.enum_string_fields.insert(handle, strings);
        }
    }

    pub(super) fn match_pattern(
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

    pub(super) fn match_literal(&self, value: Value, literal: &Literal) -> Result<bool, String> {
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

    pub(super) fn emit_printf_call(&mut self, fmt: &str, args: &mut [LLVMValueRef]) {
        // Embedded targets skip libc printf; use runtime print for string-only cases.
        if self.target.is_embedded() {
            if args.is_empty() {
                if let Ok((ptr, len)) = self.build_runtime_bytes(fmt, "rt_print") {
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

    pub(super) fn eval_binary(
        &mut self,
        op: BinaryOp,
        left: Value,
        right: Value,
    ) -> Result<Value, String> {
        let dynamic_string = |v: &Value| matches!(v, Value::Reference(r) if r.handle.is_some_and(|h| self.is_string_handle(h)));
        if self.target.is_host()
            && (dynamic_string(&left) || dynamic_string(&right))
            && matches!(op, BinaryOp::Add | BinaryOp::Eq | BinaryOp::NotEq)
        {
            let mut temporaries = Vec::new();
            let lhs = self.string_or_slice_operand(left.into(), &mut temporaries)?;
            let rhs = self.string_or_slice_operand(right.into(), &mut temporaries)?;
            let value = if matches!(op, BinaryOp::Add) {
                let handle = self.join_string_parts(&[lhs, rhs]);
                self.handle_types.insert(handle, TypeExpr::named("string"));
                Value::from_runtime_payload(handle)
            } else {
                let mut equal = self.call_runtime(
                    self.runtime_abi.prime_string_equal,
                    self.runtime_abi.prime_string_equal_ty,
                    &mut [lhs, rhs],
                    "string_equal",
                );
                if matches!(op, BinaryOp::NotEq) {
                    equal = unsafe {
                        llvm_sys::core::LLVMBuildNot(self.builder, equal, c"string_ne".as_ptr())
                    };
                }
                Value::Bool(BoolValue::new(equal, None))
            };
            for temporary in temporaries {
                self.runtime_release(temporary);
            }
            return Ok(value);
        }
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
            (Value::Str(a), Value::Str(b)) if matches!(op, BinaryOp::Add) => {
                self.build_string_constant(format!("{}{}", a.text, b.text))
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

    pub(super) fn eval_unary(&mut self, op: UnaryOp, value: Value) -> Result<Value, String> {
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

    pub(super) fn eval_bool_binary(
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
            BinaryOp::And => unsafe {
                LLVMBuildAnd(self.builder, lhs_llvm, rhs_llvm, name.as_ptr())
            },
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
                ));
            }
        };
        Ok(Value::Bool(BoolValue::new(llvm, None)))
    }

    pub(super) fn eval_int_binary(
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
        let mut lhs_llvm = lhs.llvm();
        let mut rhs_llvm = rhs.llvm();
        unsafe {
            let lhs_ty = LLVMTypeOf(lhs_llvm);
            let rhs_ty = LLVMTypeOf(rhs_llvm);
            if lhs_ty != rhs_ty && !lhs_ty.is_null() && !rhs_ty.is_null() {
                let lhs_bits = LLVMGetIntTypeWidth(lhs_ty);
                let rhs_bits = LLVMGetIntTypeWidth(rhs_ty);
                if lhs_bits < rhs_bits {
                    lhs_llvm = LLVMBuildSExt(
                        self.builder,
                        lhs_llvm,
                        rhs_ty,
                        CString::new("int_widen").unwrap().as_ptr(),
                    );
                } else if rhs_bits < lhs_bits {
                    rhs_llvm = LLVMBuildSExt(
                        self.builder,
                        rhs_llvm,
                        lhs_ty,
                        CString::new("int_widen").unwrap().as_ptr(),
                    );
                } else {
                    rhs_llvm = LLVMBuildIntCast(
                        self.builder,
                        rhs_llvm,
                        lhs_ty,
                        CString::new("int_cast").unwrap().as_ptr(),
                    );
                }
            }
        }
        let name = CString::new("int_bin").unwrap();
        let llvm = match op {
            BinaryOp::Add => unsafe {
                LLVMBuildAdd(self.builder, lhs_llvm, rhs_llvm, name.as_ptr())
            },
            BinaryOp::Sub => unsafe {
                LLVMBuildSub(self.builder, lhs_llvm, rhs_llvm, name.as_ptr())
            },
            BinaryOp::Mul => unsafe {
                LLVMBuildMul(self.builder, lhs_llvm, rhs_llvm, name.as_ptr())
            },
            BinaryOp::Div => unsafe {
                LLVMBuildSDiv(self.builder, lhs_llvm, rhs_llvm, name.as_ptr())
            },
            BinaryOp::Rem => unsafe {
                LLVMBuildSRem(self.builder, lhs_llvm, rhs_llvm, name.as_ptr())
            },
            BinaryOp::Lt => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                    lhs_llvm,
                    rhs_llvm,
                    name.as_ptr(),
                )
            },
            BinaryOp::LtEq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                    lhs_llvm,
                    rhs_llvm,
                    name.as_ptr(),
                )
            },
            BinaryOp::Gt => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                    lhs_llvm,
                    rhs_llvm,
                    name.as_ptr(),
                )
            },
            BinaryOp::GtEq => unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                    lhs_llvm,
                    rhs_llvm,
                    name.as_ptr(),
                )
            },
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
                    "Operation `{op:?}` not supported in build mode for integers (non-constant operands)"
                ));
            }
        };
        if matches!(
            op,
            BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::Eq
                | BinaryOp::NotEq
        ) {
            Ok(Value::Bool(BoolValue::new(llvm, None)))
        } else {
            Ok(Value::Int(IntValue::new(llvm, None)))
        }
    }

    pub(super) fn eval_float_binary(
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
            BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::Eq
                | BinaryOp::NotEq
        ) {
            Ok(Value::Bool(BoolValue::new(llvm, None)))
        } else {
            Ok(Value::Float(FloatValue::new(llvm, None)))
        }
    }

    pub(super) fn int_to_float(&self, value: &IntValue) -> Result<FloatValue, String> {
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

    pub(super) fn deref_if_reference(value: Value) -> Value {
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

    pub(super) fn expect_int(&mut self, value: Value) -> Result<IntValue, String> {
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
                    eprintln!("[prime-debug] expect_int saw {}", describe_value(&other));
                }
                Err(format!(
                    "Expected integer value in build mode, got {}",
                    describe_value(&other)
                ))
            }
        }
    }

    pub(super) fn int_constant_or_llvm(&self, value: &IntValue, ctx: &str) -> Result<i128, String> {
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

    pub(super) fn bool_constant_or_llvm(
        &self,
        value: &BoolValue,
        ctx: &str,
    ) -> Result<bool, String> {
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

    pub(super) fn bool_llvm_value(&mut self, value: &BoolValue) -> LLVMValueRef {
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

    pub(super) fn emit_dynamic_range_for(
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
            let continue_block = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("for_range_continue").unwrap().as_ptr(),
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
            self.loop_controls.push(LoopControl {
                break_block: after_block,
                continue_block,
            });
            let result = (|| {
                self.push_scope();
                self.insert_var(
                    &stmt.binding,
                    Value::Int(IntValue::new(cur, None)).into(),
                    false,
                )?;
                let result = self.execute_block_contents(&stmt.body)?;
                self.exit_scope()?;
                Ok::<_, String>(result)
            })();
            self.loop_controls.pop();
            self.finish_loop_body(result?, after_block, continue_block)?;

            LLVMPositionBuilderAtEnd(self.builder, continue_block);
            let next = LLVMBuildAdd(
                self.builder,
                cur,
                one,
                CString::new("for_range_next").unwrap().as_ptr(),
            );
            LLVMBuildStore(self.builder, next, loop_var);
            LLVMBuildBr(self.builder, cond_block);

            LLVMPositionBuilderAtEnd(self.builder, after_block);
            Ok(None)
        }
    }

    pub(super) fn expect_int_value_from_expr(&mut self, expr: &Expr) -> Result<IntValue, String> {
        match self.emit_expression(expr)? {
            EvalOutcome::Value(value) => self.expect_int(value.into_value()),
            EvalOutcome::Flow(_) => Err("control flow not allowed here".into()),
        }
    }

    pub(super) fn expect_float(&mut self, value: Value) -> Result<FloatValue, String> {
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

    pub(super) fn int_to_usize(
        &mut self,
        value: &IntValue,
        ctx: &str,
    ) -> Result<LLVMValueRef, String> {
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

    pub(super) fn int_to_runtime_int(
        &mut self,
        value: &IntValue,
        ctx: &str,
    ) -> Result<LLVMValueRef, String> {
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

    pub(super) fn build_struct_literal(
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

    pub(super) fn build_enum_literal(
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
        if info.fields.len() != values.len() {
            return Err(format!(
                "Variant `{}` expects {} values, got {}",
                variant,
                info.fields.len(),
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
            runtime: None,
            enum_name: info.enum_name.clone(),
            variant: variant.to_string(),
            values: evaluated,
            variant_index: info.variant_index,
        }))))
    }

    pub(super) fn instantiate_enum_variant(
        &self,
        variant: &str,
        values: Vec<Value>,
    ) -> Result<Value, String> {
        let info = self
            .enum_variants
            .get(variant)
            .ok_or_else(|| format!("Unknown enum variant {}", variant))?;
        self.ensure_item_visible(&info.module, info.visibility, &info.enum_name, "enum")?;
        if info.fields.len() != values.len() {
            return Err(format!(
                "Variant `{}` expects {} values, got {}",
                variant,
                info.fields.len(),
                values.len()
            ));
        }
        Ok(Value::Enum(EnumValue {
            runtime: None,
            enum_name: info.enum_name.clone(),
            variant: variant.to_string(),
            values,
            variant_index: info.variant_index,
        }))
    }

    pub(super) fn eval_expression_statement(
        &mut self,
        expr: &Expr,
    ) -> Result<Option<FlowSignal>, String> {
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
}
