use super::*;

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
            _ => self.emit_expression(expr),
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
                    self.collect_free_in_expr(&arm.value, locals, free);
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
                    let value = if let Some(expr) = &stmt.value {
                        let expected = stmt.ty.as_ref().map(|ann| &ann.ty);
                        match self.emit_expression_with_hint(expr, expected)? {
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
                        LLVMBuildBr(self.builder, control.break_block);
                    }
                }
                return Ok(Some(FlowSignal::Break));
            }
            Statement::Continue => {
                if let Some(control) = self.loop_controls.last().copied() {
                    unsafe {
                        LLVMBuildBr(self.builder, control.continue_block);
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
                            *pointer.cell.lock().unwrap() = value;
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
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
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
                            LLVMBuildBr(self.builder, cond_block);
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
                            let result = result?;
                            match result {
                                BlockEval::Flow(flow @ FlowSignal::Return(_))
                                | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                    LLVMPositionBuilderAtEnd(self.builder, after_block);
                                    return Ok(Some(flow));
                                }
                                _ => {}
                            }

                            let insert_block = LLVMGetInsertBlock(self.builder);
                            let terminated = !insert_block.is_null()
                                && !LLVMGetBasicBlockTerminator(insert_block).is_null();
                            if !terminated {
                                match result {
                                    BlockEval::Flow(FlowSignal::Break) => {
                                        LLVMBuildBr(self.builder, after_block);
                                    }
                                    _ => {
                                        LLVMBuildBr(self.builder, cond_block);
                                    }
                                }
                            }
                            LLVMPositionBuilderAtEnd(self.builder, after_block);
                        }
                        break;
                    }
                },
                WhileCondition::Let { pattern, value } => {
                    if let Pattern::Literal(Literal::Bool(expected, _)) = pattern {
                        unsafe {
                            let current_block = LLVMGetInsertBlock(self.builder);
                            if current_block.is_null() {
                                return Err("no insertion block for while-let loop".into());
                            }
                            let function = LLVMGetBasicBlockParent(current_block);
                            if function.is_null() {
                                return Err("while-let loop outside function".into());
                            }
                            let cond_block = LLVMAppendBasicBlockInContext(
                                self.context,
                                function,
                                CString::new("while_let_cond").unwrap().as_ptr(),
                            );
                            let body_block = LLVMAppendBasicBlockInContext(
                                self.context,
                                function,
                                CString::new("while_let_body").unwrap().as_ptr(),
                            );
                            let after_block = LLVMAppendBasicBlockInContext(
                                self.context,
                                function,
                                CString::new("while_let_after").unwrap().as_ptr(),
                            );
                            LLVMBuildBr(self.builder, cond_block);
                            LLVMPositionBuilderAtEnd(self.builder, cond_block);
                            let cond_value = match self.emit_expression(value)? {
                                EvalOutcome::Value(value) => value.into_value(),
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            };
                            let cond_bool = self.value_to_bool(cond_value)?;
                            let mut cond_llvm = self.bool_llvm_value(&cond_bool);
                            if !*expected {
                                cond_llvm = LLVMBuildNot(
                                    self.builder,
                                    cond_llvm,
                                    CString::new("while_let_not").unwrap().as_ptr(),
                                );
                            }
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
                            let result = result?;
                            match result {
                                BlockEval::Flow(flow @ FlowSignal::Return(_))
                                | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                    LLVMPositionBuilderAtEnd(self.builder, after_block);
                                    return Ok(Some(flow));
                                }
                                _ => {}
                            }

                            let insert_block = LLVMGetInsertBlock(self.builder);
                            let terminated = !insert_block.is_null()
                                && !LLVMGetBasicBlockTerminator(insert_block).is_null();
                            if !terminated {
                                match result {
                                    BlockEval::Flow(FlowSignal::Break) => {
                                        LLVMBuildBr(self.builder, after_block);
                                    }
                                    _ => {
                                        LLVMBuildBr(self.builder, cond_block);
                                    }
                                }
                            }
                            LLVMPositionBuilderAtEnd(self.builder, after_block);
                        }
                    } else {
                        loop {
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
                                BlockEval::Flow(flow @ FlowSignal::Return(_)) => {
                                    return Ok(Some(flow));
                                }
                                BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                    return Ok(Some(flow));
                                }
                            }
                        }
                    }
                }
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
                        let result = result?;

                        let insert_block = LLVMGetInsertBlock(self.builder);
                        let terminated = !insert_block.is_null()
                            && !LLVMGetBasicBlockTerminator(insert_block).is_null();
                        match result {
                            BlockEval::Value(_) | BlockEval::Flow(FlowSignal::Continue) => {
                                if !terminated {
                                    LLVMBuildBr(self.builder, loop_bb);
                                }
                            }
                            BlockEval::Flow(FlowSignal::Break) => {
                                if !terminated {
                                    LLVMBuildBr(self.builder, after_bb);
                                }
                            }
                            BlockEval::Flow(flow @ FlowSignal::Return(_))
                            | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                LLVMPositionBuilderAtEnd(self.builder, after_bb);
                                return Ok(Some(flow));
                            }
                        }

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
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
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
                                    BlockEval::Flow(flow @ FlowSignal::Return(_)) => {
                                        return Ok(Some(flow));
                                    }
                                    BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                        return Ok(Some(flow));
                                    }
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

    pub(super) fn emit_expression(
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
            Expr::Async { block, .. } => {
                self.ensure_async_supported()?;
                self.force_runtime_handles = true;
                let mut locals = HashSet::new();
                let mut free = HashSet::new();
                self.collect_free_in_block(block, &mut locals, &mut free);
                let capture_names: Vec<String> = free.into_iter().collect();
                let task = TaskValue::deferred((**block).clone(), capture_names);
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
                        Value::Reference(reference) => {
                            reference.cell.lock().unwrap().clone().into_value()
                        }
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
        captures: &[(String, EvaluatedValue)],
    ) -> Result<EvaluatedValue, String> {
        self.push_scope();
        for (name, value) in captures {
            self.insert_var(name, value.clone(), true)?;
        }
        let result = self.execute_block_contents(block)?;
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
            BlockEval::Flow(FlowSignal::Propagate(value)) => Err(format!(
                "async block propagated error: {}",
                flow_name(&FlowSignal::Propagate(value.clone()))
            )),
            BlockEval::Flow(_) => Err("control flow cannot exit async block in build mode".into()),
        }
    }

    pub(super) fn emit_call_expression(
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
        let type_code_const =
            unsafe { LLVMConstInt(self.runtime_abi.status_type, type_code as u64, 0) };
        let ok_tag_const = unsafe { LLVMConstInt(self.runtime_abi.status_type, ok_tag as u64, 0) };
        let err_tag_const =
            unsafe { LLVMConstInt(self.runtime_abi.status_type, err_tag as u64, 0) };

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

        // Wrap in EvaluatedValue with runtime handle
        Ok(EvalOutcome::Value(
            self.evaluated(Value::Enum(EnumValue {
                enum_name: "Result".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }))
            .with_runtime(result_handle),
        ))
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

        // Build-mode / deferred tasks: await immediately and wrap in Result::Ok.
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
        let result_handle = unsafe {
            llvm_sys::core::LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                result_slot,
                CString::new("await_timeout_result").unwrap().as_ptr(),
            )
        };

        Ok(EvalOutcome::Value(
            self.evaluated(Value::Enum(EnumValue {
                enum_name: "Result".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }))
            .with_runtime(result_handle),
        ))
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
        let result_handle = unsafe {
            llvm_sys::core::LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                result_slot,
                CString::new("await_cancel_result").unwrap().as_ptr(),
            )
        };

        Ok(EvalOutcome::Value(
            self.evaluated(Value::Enum(EnumValue {
                enum_name: "Result".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }))
            .with_runtime(result_handle),
        ))
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
        let result_handle = unsafe {
            llvm_sys::core::LLVMBuildLoad2(
                self.builder,
                self.runtime_abi.handle_type,
                result_slot,
                CString::new("await_cancel_timeout_result")
                    .unwrap()
                    .as_ptr(),
            )
        };

        Ok(EvalOutcome::Value(
            self.evaluated(Value::Enum(EnumValue {
                enum_name: "Result".into(),
                variant: "<runtime>".into(),
                values: Vec::new(),
                variant_index: 0,
            }))
            .with_runtime(result_handle),
        ))
    }
    pub(super) fn try_builtin_call(
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
            "fs_exists" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_fs_exists(values)))
            }
            "fs_read" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_fs_read(values)))
            }
            "fs_write" => {
                Some(self.invoke_builtin(args, |this, values| this.builtin_fs_write(values)))
            }
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
        match &if_expr.condition {
            IfCondition::Expr(condition) => {
                let cond_value = match self.emit_expression(condition)? {
                    EvalOutcome::Value(value) => value.into_value(),
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let cond_bool = self.value_to_bool(cond_value)?;
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
                        let mut then_value: Option<EvaluatedValue> = None;
                        let then_eval = self.execute_block_contents(&if_expr.then_branch)?;
                        self.exit_scope()?;
                        match then_eval {
                            BlockEval::Flow(flow @ FlowSignal::Return(_))
                            | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                LLVMPositionBuilderAtEnd(self.builder, merge_block);
                                return Ok(EvalOutcome::Flow(flow));
                            }
                            BlockEval::Value(val) => {
                                then_value = Some(val);
                                LLVMBuildBr(self.builder, merge_block);
                            }
                            BlockEval::Flow(FlowSignal::Break)
                            | BlockEval::Flow(FlowSignal::Continue) => {
                                let current_block = LLVMGetInsertBlock(self.builder);
                                if current_block.is_null()
                                    || LLVMGetBasicBlockTerminator(current_block).is_null()
                                {
                                    return Err(
                                        "break/continue in dynamic if branch did not terminate block"
                                            .into(),
                                    );
                                }
                            }
                        };

                        LLVMPositionBuilderAtEnd(self.builder, else_block);
                        let mut else_value: Option<EvaluatedValue> = None;
                        if let Some(else_branch) = &if_expr.else_branch {
                            match else_branch {
                                ElseBranch::Block(block) => {
                                    self.push_scope();
                                    let else_eval = self.execute_block_contents(block)?;
                                    self.exit_scope()?;
                                    match else_eval {
                                        BlockEval::Flow(flow @ FlowSignal::Return(_))
                                        | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                            LLVMPositionBuilderAtEnd(self.builder, merge_block);
                                            return Ok(EvalOutcome::Flow(flow));
                                        }
                                        BlockEval::Value(val) => {
                                            else_value = Some(val);
                                            LLVMBuildBr(self.builder, merge_block);
                                        }
                                        BlockEval::Flow(FlowSignal::Break)
                                        | BlockEval::Flow(FlowSignal::Continue) => {
                                            let current_block = LLVMGetInsertBlock(self.builder);
                                            if current_block.is_null()
                                                || LLVMGetBasicBlockTerminator(current_block)
                                                    .is_null()
                                            {
                                                return Err(
                                                    "break/continue in dynamic if branch did not terminate block"
                                                        .into(),
                                                );
                                            }
                                        }
                                    };
                                }
                                ElseBranch::ElseIf(nested) => {
                                    match self.emit_if_expression(nested)? {
                                        EvalOutcome::Flow(flow) => {
                                            LLVMPositionBuilderAtEnd(self.builder, merge_block);
                                            return Ok(EvalOutcome::Flow(flow));
                                        }
                                        EvalOutcome::Value(val) => {
                                            else_value = Some(val);
                                            LLVMBuildBr(self.builder, merge_block);
                                        }
                                    }
                                }
                            }
                        } else {
                            LLVMBuildBr(self.builder, merge_block);
                        }

                        LLVMPositionBuilderAtEnd(self.builder, merge_block);
                        if let (Some(then_val), Some(else_val)) = (then_value, else_value) {
                            return Ok(EvalOutcome::Value(
                                self.merge_if_values(then_val, then_block, else_val, else_block)?,
                            ));
                        }
                    }
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

    pub(super) fn merge_if_values(
        &mut self,
        then_val: EvaluatedValue,
        then_block: LLVMBasicBlockRef,
        else_val: EvaluatedValue,
        else_block: LLVMBasicBlockRef,
    ) -> Result<EvaluatedValue, String> {
        let merge_block = unsafe { LLVMGetInsertBlock(self.builder) };
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
        let EvaluatedValue {
            value: then_value,
            runtime: then_runtime,
        } = then_val;
        let EvaluatedValue {
            value: else_value,
            runtime: else_runtime,
        } = else_val;
        match (then_value, else_value) {
            (Value::Int(a), Value::Int(b)) => {
                let ty = unsafe { LLVMTypeOf(a.llvm()) };
                let phi = build_phi(ty, "if_int_phi");
                let mut vals = [a.llvm(), b.llvm()];
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
            (Value::Enum(a), Value::Enum(b)) => {
                if a.enum_name != b.enum_name {
                    return Err(format!(
                        "Dynamic if expression not supported for branch values {} and {}",
                        self.describe_value(&Value::Enum(a)),
                        self.describe_value(&Value::Enum(b))
                    ));
                }
                if self.runtime_handles_enabled() {
                    let a_handle = then_runtime
                        .map(|rt| rt.handle)
                        .or_else(|| unsafe {
                            let terminator = LLVMGetLastInstruction(then_block);
                            if terminator.is_null() {
                                LLVMPositionBuilderAtEnd(builder, then_block);
                            } else {
                                LLVMPositionBuilderBefore(builder, terminator);
                            }
                            let handle = self.build_runtime_handle(Value::Enum(a.clone())).ok();
                            LLVMPositionBuilderAtEnd(builder, merge_block);
                            handle
                        })
                        .ok_or_else(|| {
                            "Dynamic if expression not supported for enum without runtime handle"
                                .to_string()
                        })?;
                    let b_handle = else_runtime
                        .map(|rt| rt.handle)
                        .or_else(|| unsafe {
                            let terminator = LLVMGetLastInstruction(else_block);
                            if terminator.is_null() {
                                LLVMPositionBuilderAtEnd(builder, else_block);
                            } else {
                                LLVMPositionBuilderBefore(builder, terminator);
                            }
                            let handle = self.build_runtime_handle(Value::Enum(b.clone())).ok();
                            LLVMPositionBuilderAtEnd(builder, merge_block);
                            handle
                        })
                        .ok_or_else(|| {
                            "Dynamic if expression not supported for enum without runtime handle"
                                .to_string()
                        })?;
                    let phi = build_phi(self.runtime_abi.handle_type, "if_enum_phi");
                    let mut vals = [a_handle, b_handle];
                    let mut blocks = [then_block, else_block];
                    unsafe {
                        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                    }
                    self.pending_runtime = Some(phi);
                    return Ok(self.evaluated(Value::Enum(EnumValue {
                        enum_name: a.enum_name.clone(),
                        variant: "<runtime>".into(),
                        values: Vec::new(),
                        variant_index: a.variant_index,
                    })));
                }
                Err(format!(
                    "Dynamic if expression not supported for branch values {} and {}",
                    self.describe_value(&Value::Enum(a)),
                    self.describe_value(&Value::Enum(b))
                ))
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

    pub(super) fn invoke_builtin<F>(
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
        f(self, evaluated).map(|v| EvalOutcome::Value(self.evaluated(v)))
    }

    pub(super) fn build_reference(
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
                Ok(EvalOutcome::Value(
                    Value::Reference(ReferenceValue {
                        cell,
                        mutable,
                        origin: Some(ident.name.clone()),
                        handle: None,
                    })
                    .into(),
                ))
            }
            _ => match self.emit_expression(expr)? {
                EvalOutcome::Value(value) => Ok(EvalOutcome::Value(
                    Value::Reference(ReferenceValue {
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
                let moved = std::mem::replace(&mut *slot, EvaluatedValue::from_value(Value::Moved));
                self.register_move(&ident.name);
                Ok(EvalOutcome::Value(moved))
            }
            _ => Err("move expressions require identifiers in build mode".into()),
        }
    }

    pub(super) fn is_heap_value(value: &Value) -> bool {
        matches!(value, Value::Boxed(_) | Value::Slice(_) | Value::Map(_))
    }

    pub(super) fn deref_value(&self, value: Value) -> Result<EvaluatedValue, String> {
        match value {
            Value::Reference(reference) => Ok(reference.cell.lock().unwrap().clone()),
            Value::Pointer(pointer) => Ok(pointer.cell.lock().unwrap().clone()),
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
        if let Some(runtime_handle) = target.runtime_handle() {
            if let Value::Enum(enum_value) = target.value() {
                if let Some(result) =
                    self.emit_runtime_enum_match(runtime_handle, enum_value, match_expr)?
                {
                    return Ok(result);
                }
            }
        }
        for arm in &match_expr.arms {
            self.push_scope();
            if self.match_pattern(target.value(), &arm.pattern, false)? {
                let value = self.emit_expression(&arm.value)?;
                self.exit_scope()?;
                return Ok(value);
            }
            self.exit_scope()?;
        }
        let detail = match target.value() {
            Value::Enum(e) => {
                let has_handle = target.runtime_handle().is_some();
                format!(
                    "enum {}::{} (runtime_handle={})",
                    e.enum_name, e.variant, has_handle
                )
            }
            Value::Reference(r) => {
                let inner = r
                    .cell
                    .lock()
                    .ok()
                    .map(|v| self.describe_value(v.value()).to_string());
                format!(
                    "reference (handle={}, inner={})",
                    r.handle.is_some(),
                    inner.unwrap_or_else(|| "unknown".into())
                )
            }
            other => self.describe_value(other).to_string(),
        };
        Err(format!(
            "No match arm matched in build mode (value: {}, span {}..{})",
            detail, match_expr.span.start, match_expr.span.end
        ))
    }

    pub(super) fn emit_runtime_enum_match(
        &mut self,
        handle: LLVMValueRef,
        enum_value: &EnumValue,
        match_expr: &MatchExpr,
    ) -> Result<Option<EvalOutcome<EvaluatedValue>>, String> {
        let mut variant_tags = HashMap::new();
        match enum_value.enum_name.as_str() {
            "Result" => {
                let ok_tag = self
                    .enum_variants
                    .get("Ok")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Result::Ok variant not found in build mode".to_string())?;
                let err_tag = self
                    .enum_variants
                    .get("Err")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Result::Err variant not found in build mode".to_string())?;
                variant_tags.insert("Ok", ok_tag);
                variant_tags.insert("Err", err_tag);
            }
            "Option" => {
                let some_tag = self
                    .enum_variants
                    .get("Some")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Option::Some variant not found in build mode".to_string())?;
                let none_tag = self
                    .enum_variants
                    .get("None")
                    .map(|v| v.variant_index)
                    .ok_or_else(|| "Option::None variant not found in build mode".to_string())?;
                variant_tags.insert("Some", some_tag);
                variant_tags.insert("None", none_tag);
            }
            _ => return Ok(None),
        }

        let supported_patterns = match_expr.arms.iter().all(|arm| match &arm.pattern {
            Pattern::EnumVariant { bindings, .. } => bindings
                .iter()
                .all(|b| matches!(b, Pattern::Identifier(_, _) | Pattern::Wildcard)),
            Pattern::Wildcard => true,
            _ => false,
        });
        if !supported_patterns {
            return Ok(None);
        }

        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        let merge_block = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                function,
                CString::new("match_merge").unwrap().as_ptr(),
            )
        };

        let tag = self.call_runtime(
            self.runtime_abi.prime_enum_tag,
            self.runtime_abi.prime_enum_tag_ty,
            &mut [handle],
            "enum_tag",
        );

        let mut current_block = unsafe { LLVMGetInsertBlock(self.builder) };
        let mut result_value: Option<EvaluatedValue> = None;

        for (idx, arm) in match_expr.arms.iter().enumerate() {
            let (expected_tag, bindings) = match &arm.pattern {
                Pattern::EnumVariant {
                    variant, bindings, ..
                } => {
                    let tag = variant_tags.get(variant.as_str()).copied();
                    if tag.is_none() {
                        return Ok(None);
                    }
                    (tag, Some(bindings))
                }
                Pattern::Wildcard => (None, None),
                _ => return Ok(None),
            };

            let then_block = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    function,
                    CString::new(format!("match_arm_{idx}")).unwrap().as_ptr(),
                )
            };
            let else_block = if idx == match_expr.arms.len() - 1 {
                merge_block
            } else {
                unsafe {
                    LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new(format!("match_else_{idx}")).unwrap().as_ptr(),
                    )
                }
            };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, current_block) };
            if let Some(tag_val) = expected_tag {
                let tag_const = unsafe { LLVMConstInt(LLVMTypeOf(tag), tag_val as u64, 0) };
                let cond = unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        tag,
                        tag_const,
                        CString::new("match_tag_cmp").unwrap().as_ptr(),
                    )
                };
                unsafe { LLVMBuildCondBr(self.builder, cond, then_block, else_block) };
            } else {
                unsafe { LLVMBuildBr(self.builder, then_block) };
            }

            unsafe { LLVMPositionBuilderAtEnd(self.builder, then_block) };
            self.push_scope();
            if let Some(bindings) = bindings {
                for (field_idx, binding) in bindings.iter().enumerate() {
                    let Pattern::Identifier(name, _) = binding else {
                        if matches!(binding, Pattern::Wildcard) {
                            continue;
                        }
                        return Ok(None);
                    };
                    let field_handle = self.call_runtime(
                        self.runtime_abi.prime_enum_get,
                        self.runtime_abi.prime_enum_get_ty,
                        &mut [handle, unsafe {
                            LLVMConstInt(self.runtime_abi.usize_type, field_idx as u64, 0)
                        }],
                        "enum_get",
                    );
                    let reference = ReferenceValue {
                        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Unit))),
                        mutable: false,
                        origin: None,
                        handle: Some(field_handle),
                    };
                    let bound = self.evaluated(Value::Reference(reference));
                    self.insert_var(name, bound, false)?;
                }
            }
            let arm_value = self.emit_expression(&arm.value)?;
            self.exit_scope()?;
            match arm_value {
                EvalOutcome::Value(val) => {
                    result_value = Some(val);
                    unsafe { LLVMBuildBr(self.builder, merge_block) };
                }
                EvalOutcome::Flow(flow @ FlowSignal::Return(_))
                | EvalOutcome::Flow(flow @ FlowSignal::Propagate(_)) => {
                    unsafe {
                        LLVMPositionBuilderAtEnd(self.builder, merge_block);
                    }
                    return Ok(Some(EvalOutcome::Flow(flow)));
                }
                EvalOutcome::Flow(FlowSignal::Break) | EvalOutcome::Flow(FlowSignal::Continue) => {
                    let current_block = unsafe { LLVMGetInsertBlock(self.builder) };
                    if current_block.is_null()
                        || unsafe { LLVMGetBasicBlockTerminator(current_block) }.is_null()
                    {
                        return Err(
                            "break/continue in runtime match arm did not terminate block".into(),
                        );
                    }
                }
            }
            current_block = else_block;
        }

        unsafe { LLVMPositionBuilderAtEnd(self.builder, merge_block) };
        Ok(Some(EvalOutcome::Value(
            result_value.unwrap_or_else(|| self.evaluated(Value::Unit)),
        )))
    }

    pub(super) fn eval_try_operator(
        &mut self,
        value: EvaluatedValue,
    ) -> Result<EvalOutcome<EvaluatedValue>, String> {
        let EvaluatedValue { value, runtime } = value;
        let trace_try = env::var_os("PRIME_DEBUG_TRACE").is_some();
        if trace_try {
            eprintln!(
                "[prime-debug] try operator input {}",
                self.describe_value(&value)
            );
        }
        match value {
            Value::Enum(mut enum_value) => {
                if trace_try {
                    eprintln!(
                        "[prime-debug] try operator enum name={} variant={} idx={} runtime_handle={:?} values_len={} module={}",
                        enum_value.enum_name,
                        enum_value.variant,
                        enum_value.variant_index,
                        runtime.map(|rt| rt.handle),
                        enum_value.values.len(),
                        self.module_stack.last().cloned().unwrap_or_default()
                    );
                }
                if enum_value.enum_name != "Result" {
                    return Err("? operator expects Result value in build mode".into());
                }
                if let Some(runtime_value) = runtime {
                    self.ensure_runtime_symbols();
                    if trace_try {
                        let handle_const =
                            unsafe { !LLVMIsAConstantInt(runtime_value.handle).is_null() };
                        eprintln!(
                            "[prime-debug] try operator runtime handle const={} raw={:?}",
                            handle_const, runtime_value.handle
                        );
                    }
                    let ok_tag = self.enum_variants.get("Ok").cloned().ok_or_else(|| {
                        "? operator expects Result value in build mode".to_string()
                    })?;
                    let err_tag = self.enum_variants.get("Err").cloned().ok_or_else(|| {
                        "? operator expects Result value in build mode".to_string()
                    })?;
                    let mut tag_args = [runtime_value.handle];
                    let tag = self.call_runtime(
                        self.runtime_abi.prime_enum_tag,
                        self.runtime_abi.prime_enum_tag_ty,
                        &mut tag_args,
                        "try_enum_tag",
                    );
                    let tag_value = unsafe {
                        if LLVMIsAConstantInt(tag).is_null() {
                            None
                        } else {
                            Some(LLVMConstIntGetZExtValue(tag) as u32)
                        }
                    }
                    .or(match enum_value.variant.as_str() {
                        "Ok" => Some(ok_tag.variant_index),
                        "Err" => Some(err_tag.variant_index),
                        _ => None,
                    })
                    .unwrap_or(ok_tag.variant_index);
                    if trace_try {
                        let is_const = unsafe { !LLVMIsAConstantInt(tag).is_null() };
                        eprintln!(
                            "[prime-debug] try operator runtime tag const={} raw_handle={:?} resolved_tag={}",
                            is_const, tag, tag_value
                        );
                    }
                    if tag_value == ok_tag.variant_index {
                        if ok_tag.fields == 0 {
                            return Ok(EvalOutcome::Value(self.evaluated(Value::Unit)));
                        }
                        let mut get_args = [runtime_value.handle, unsafe {
                            LLVMConstInt(self.runtime_abi.usize_type, 0, 0)
                        }];
                        let payload_handle = self.call_runtime(
                            self.runtime_abi.prime_enum_get,
                            self.runtime_abi.prime_enum_get_ty,
                            &mut get_args,
                            "try_enum_get",
                        );
                        let payload = if enum_value.values.len() == 1 {
                            enum_value.values.remove(0)
                        } else {
                            // Materialize a concrete payload value; for the blink path this is an int pin.
                            let mut as_int_args = [payload_handle];
                            let llvm = self.call_runtime(
                                self.runtime_abi.prime_value_as_int,
                                self.runtime_abi.prime_value_as_int_ty,
                                &mut as_int_args,
                                "try_enum_payload_as_int",
                            );
                            Value::Int(IntValue::new(llvm, None))
                        };
                        return Ok(EvalOutcome::Value(self.evaluated(payload)));
                    } else if tag_value == err_tag.variant_index {
                        return Ok(EvalOutcome::Flow(FlowSignal::Propagate(EvaluatedValue {
                            value: Value::Enum(EnumValue {
                                enum_name: "Result".into(),
                                variant: "Err".into(),
                                values: Vec::new(),
                                variant_index: err_tag.variant_index,
                            }),
                            runtime: Some(runtime_value),
                        })));
                    } else {
                        if env::var_os("PRIME_DEBUG_TRACE").is_some() {
                            eprintln!(
                                "[prime-debug] try operator saw unexpected Result tag {tag_value}"
                            );
                        }
                        return Err("? operator expects Result value in build mode".into());
                    }
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
                    "Err" => Ok(EvalOutcome::Flow(FlowSignal::Propagate(EvaluatedValue {
                        value: Value::Enum(enum_value),
                        runtime,
                    }))),
                    _ => Err("? operator expects Result value in build mode".into()),
                }
            }
            _ => Err("? operator expects Result value in build mode".into()),
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
                has_spread: _,
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
            let result = result?;

            match result {
                BlockEval::Flow(flow @ FlowSignal::Return(_))
                | BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => return Ok(Some(flow)),
                _ => {}
            }

            let insert_block = LLVMGetInsertBlock(self.builder);
            let terminated =
                !insert_block.is_null() && !LLVMGetBasicBlockTerminator(insert_block).is_null();
            if !terminated {
                match result {
                    BlockEval::Flow(FlowSignal::Break) => {
                        LLVMBuildBr(self.builder, after_block);
                    }
                    _ => {
                        LLVMBuildBr(self.builder, continue_block);
                    }
                }
            }

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
