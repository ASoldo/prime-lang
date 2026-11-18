//! Conditional exits from inlined Prime functions and lexical try blocks.
use super::*;

#[derive(Clone, Copy, PartialEq)]
pub(super) enum ExitKind {
    Function,
    Try,
}

pub(super) struct ExitBoundary {
    kind: ExitKind,
    scope_depth: usize,
    call_depth: usize,
    function: LLVMValueRef,
    pub(super) join: Option<LLVMBasicBlockRef>,
    incoming: Vec<(EvaluatedValue, LLVMBasicBlockRef)>,
}

impl Compiler {
    pub(super) fn begin_exit_boundary(&mut self, kind: ExitKind, scope_depth: usize) {
        self.exits.push(ExitBoundary {
            kind,
            scope_depth,
            call_depth: self.return_type_stack.len(),
            function: unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) },
            join: None,
            incoming: Vec::new(),
        });
    }

    fn exit_index(&self, propagate: bool) -> Option<usize> {
        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        self.exits.iter().rposition(|boundary| {
            boundary.function == function
                && boundary.call_depth == self.return_type_stack.len()
                && (propagate || boundary.kind == ExitKind::Function)
        })
    }

    pub(super) fn can_propagate_result(&self) -> bool {
        self.exit_index(true).is_some_and(|idx| {
            self.exits[idx].kind == ExitKind::Try
                || matches!(self.current_return_types(), Some([TypeExpr::Named(name, _)]) if name == "Result")
        })
    }

    pub(super) fn branch_to_exit(&mut self, flow: FlowSignal) -> Result<(), String> {
        let propagate = matches!(flow, FlowSignal::Propagate(_));
        let idx = self.exit_index(propagate).ok_or(
            "conditional return or propagation requires an enclosing native function or try block",
        )?;
        let value = match flow {
            FlowSignal::Return(mut values) => match values.len() {
                0 => Value::Unit.into(),
                1 => values.pop().unwrap(),
                _ => return Err("conditional return of multiple values is unsupported".into()),
            },
            FlowSignal::Propagate(value) if self.can_propagate_result() => value,
            FlowSignal::Propagate(_) => {
                return Err(
                    "conditional propagation requires a Result return type or try block".into(),
                );
            }
            _ => unreachable!(),
        };
        let depth = self.exits[idx].scope_depth;
        self.emit_exit_cleanups(depth)?;
        let join = self.exits[idx]
            .join
            .unwrap_or_else(|| self.match_block(c"exit_merge"));
        self.exits[idx].join = Some(join);
        unsafe {
            self.exits[idx]
                .incoming
                .push((value, LLVMGetInsertBlock(self.builder)));
            let branch = LLVMBuildBr(self.builder, join);
            for owner in &mut self.token_owners {
                owner.early_exits.push((join, branch));
            }
        }
        Ok(())
    }

    pub(super) fn finish_exit_boundary(&mut self, result: BlockEval) -> Result<BlockEval, String> {
        let mut boundary = self.exits.pop().expect("paired exit boundary");
        let Some(join) = boundary.join else {
            return Ok(result);
        };
        // A try may have caught earlier runtime errors even when its remaining
        // path explicitly returns from the enclosing function.
        let normal = match result {
            BlockEval::Value(_)
                if boundary.kind == ExitKind::Function
                    && self.current_return_types() == Some(&[]) =>
            {
                Some(Value::Unit.into())
            }
            BlockEval::Value(value) | BlockEval::Flow(FlowSignal::Propagate(value)) => Some(value),
            BlockEval::Flow(FlowSignal::Return(mut values))
                if boundary.kind == ExitKind::Function =>
            {
                match values.len() {
                    0 => Some(Value::Unit.into()),
                    1 => values.pop(),
                    _ => return Err("conditional return of multiple values is unsupported".into()),
                }
            }
            BlockEval::Flow(flow @ FlowSignal::Return(_)) => {
                self.branch_to_exit(flow)?;
                None
            }
            BlockEval::Flow(FlowSignal::Terminated) => None,
            BlockEval::Flow(FlowSignal::Break | FlowSignal::Continue) => {
                return Err(
                    "break/continue across a try block with runtime propagation is unsupported"
                        .into(),
                );
            }
        };
        unsafe {
            if let Some(value) = normal {
                boundary
                    .incoming
                    .push((value, LLVMGetInsertBlock(self.builder)));
                LLVMBuildBr(self.builder, join);
            }
            LLVMPositionBuilderAtEnd(self.builder, join);
        }
        let value = self.merge_match_values(&boundary.incoming, join)?;
        if boundary.kind == ExitKind::Function
            && let Some(handle) = value.runtime_handle()
            && let Some([ty]) = self.current_return_types()
        {
            self.handle_types.insert(handle, ty.clone());
        }
        Ok(BlockEval::Value(value))
    }
}
