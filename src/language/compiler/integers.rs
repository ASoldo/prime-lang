use super::*;
use llvm_sys::LLVMIntPredicate::*;
use llvm_sys::core::{LLVMBuildIntCast2, LLVMBuildXor, LLVMConstIntOfArbitraryPrecision};

impl Compiler {
    pub(super) fn wide_integer_constant(&self, value: i128) -> LLVMValueRef {
        let words = [value as u64, (value >> 64) as u64];
        unsafe { LLVMConstIntOfArbitraryPrecision(self.runtime_abi.int_type, 2, words.as_ptr()) }
    }

    fn integer_guard(&mut self, ok: LLVMValueRef, message: &str) -> Result<(), String> {
        if self.in_host_task() {
            return self.host_task_check(ok, message);
        }
        let (ptr, len) = self.build_runtime_bytes(message, "integer_error")?;
        self.call_runtime(
            self.runtime_abi.prime_integer_check,
            self.runtime_abi.prime_integer_check_ty,
            &mut [ok, ptr, len],
            "integer_check",
        );
        Ok(())
    }

    pub(super) fn range_guard(
        &mut self,
        value: LLVMValueRef,
        ty: IntegerType,
    ) -> Result<(), String> {
        let (min, max) = ty.bounds();
        let ok = unsafe {
            let lower = LLVMBuildICmp(
                self.builder,
                LLVMIntSGE,
                value,
                self.wide_integer_constant(min),
                c"int_lower".as_ptr(),
            );
            let upper = LLVMBuildICmp(
                self.builder,
                LLVMIntSLE,
                value,
                self.wide_integer_constant(max),
                c"int_upper".as_ptr(),
            );
            LLVMBuildAnd(self.builder, lower, upper, c"int_fits".as_ptr())
        };
        self.integer_guard(
            ok,
            &format!("integer result is out of range for `{}`", ty.name),
        )
    }

    pub(super) fn emit_integer_op(
        &mut self,
        op: IntegerOp,
        lhs: IntValue,
        rhs: Option<IntValue>,
    ) -> Result<Value, String> {
        if let Some(a) = lhs.constant()
            && rhs.as_ref().is_none_or(|v| v.constant().is_some())
        {
            match op.evaluate(a, rhs.as_ref().and_then(IntValue::constant)) {
                Ok(Scalar::Int(n)) => return Ok(Value::Int(self.const_int_value(n))),
                Ok(Scalar::Bool(v)) => return Ok(Value::Bool(self.const_bool_value(v))),
                // A function argument can become constant only during code generation.
                // Preserve the error on its execution path.
                Err(message) => {
                    let never = unsafe { LLVMConstNull(self.runtime_abi.bool_type) };
                    self.integer_guard(never, &message)?;
                    return Ok(Value::Int(self.const_int_value(0)));
                }
            }
        }
        let a = self.int_to_runtime_int(&lhs, "integer_operand")?;
        let (value, ty) = match op {
            IntegerOp::Cast(ty) => unsafe {
                let narrow = LLVMBuildIntCast2(
                    self.builder,
                    a,
                    LLVMIntTypeInContext(self.context, ty.bits),
                    0,
                    c"cast_low".as_ptr(),
                );
                let wide = LLVMBuildIntCast2(
                    self.builder,
                    narrow,
                    self.runtime_abi.int_type,
                    i32::from(ty.signed),
                    c"cast_value".as_ptr(),
                );
                return Ok(Value::Int(IntValue::new(wide, None)));
            },
            IntegerOp::Neg(ty) => unsafe {
                (
                    LLVMBuildSub(
                        self.builder,
                        self.wide_integer_constant(0),
                        a,
                        c"integer_neg".as_ptr(),
                    ),
                    ty,
                )
            },
            IntegerOp::Binary(op, ty) => {
                let b = self.int_to_runtime_int(
                    &rhs.ok_or("missing right integer operand")?,
                    "integer_operand",
                )?;
                use BinaryOp::*;
                let predicate = match op {
                    Lt => Some(LLVMIntSLT),
                    LtEq => Some(LLVMIntSLE),
                    Gt => Some(LLVMIntSGT),
                    GtEq => Some(LLVMIntSGE),
                    Eq => Some(LLVMIntEQ),
                    NotEq => Some(LLVMIntNE),
                    _ => None,
                };
                if let Some(pred) = predicate {
                    let value =
                        unsafe { LLVMBuildICmp(self.builder, pred, a, b, c"integer_cmp".as_ptr()) };
                    return Ok(Value::Bool(BoolValue::new(value, None)));
                }
                if matches!(op, Div | Rem) {
                    let nonzero = unsafe {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntNE,
                            b,
                            self.wide_integer_constant(0),
                            c"nonzero".as_ptr(),
                        )
                    };
                    self.integer_guard(nonzero, "integer division by zero")?;
                }
                // Every input is a sign- or zero-extended language integer (at most 64 bits).
                // A uint64 product with bit 127 set fails the signed lower-bound check.
                let value = unsafe {
                    match op {
                        Add => LLVMBuildAdd(self.builder, a, b, c"integer_add".as_ptr()),
                        Sub => LLVMBuildSub(self.builder, a, b, c"integer_sub".as_ptr()),
                        Mul => LLVMBuildMul(self.builder, a, b, c"integer_mul".as_ptr()),
                        Div => LLVMBuildSDiv(self.builder, a, b, c"integer_div".as_ptr()),
                        Rem => LLVMBuildSRem(self.builder, a, b, c"integer_rem".as_ptr()),
                        BitAnd => LLVMBuildAnd(self.builder, a, b, c"integer_and".as_ptr()),
                        BitOr => LLVMBuildOr(self.builder, a, b, c"integer_or".as_ptr()),
                        BitXor => LLVMBuildXor(self.builder, a, b, c"integer_xor".as_ptr()),
                        _ => return Err("invalid integer operation".into()),
                    }
                };
                (value, ty)
            }
        };
        self.range_guard(value, ty)?;
        Ok(Value::Int(IntValue::new(value, None)))
    }
}
