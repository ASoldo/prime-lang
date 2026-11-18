//! Embedded ABI constructors return an owned handle. Keep temporary owners
//! in the entry frame so a backedge replaces the old owner in bounded space.
use super::*;
use llvm_sys::core::{LLVMGetNextBasicBlock, LLVMIsAReturnInst};

#[derive(Default)]
pub(super) struct ValueOwners {
    frames: HashMap<LLVMValueRef, Vec<LLVMValueRef>>,
    temporaries: HashMap<LLVMValueRef, LLVMValueRef>,
}

impl Compiler {
    pub(super) fn embedded_owner_slot(&mut self) -> LLVMValueRef {
        let slot = self.null_handle_slot("value_owner");
        let function = unsafe { LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)) };
        self.value_owners
            .frames
            .entry(function)
            .or_default()
            .push(slot);
        slot
    }

    pub(super) fn own_embedded_result(
        &mut self,
        function: LLVMValueRef,
        args: &[LLVMValueRef],
        result: LLVMValueRef,
    ) {
        let abi = self.runtime_abi;
        if [
            abi.prime_enum_new,
            abi.prime_enum_get,
            abi.prime_int_new,
            abi.prime_float_new,
        ]
        .contains(&function)
        {
            let slot = self.embedded_owner_slot();
            self.value_owners.temporaries.insert(result, slot);
            self.call_runtime(
                abi.prime_frame_adopt,
                abi.prime_frame_adopt_ty,
                &mut [slot, result],
                "",
            );
        } else if [
            abi.prime_recv,
            abi.prime_recv_timeout,
            abi.prime_task_poll,
            abi.prime_join,
        ]
        .contains(&function)
        {
            let slot = self.embedded_owner_slot();
            let out = *args.last().expect("ABI result pointer");
            self.call_runtime(
                abi.prime_frame_out,
                abi.prime_frame_out_ty,
                &mut [slot, out, result],
                "",
            );
        }
    }

    pub(super) fn copy_embedded_handle(&mut self, slot: LLVMValueRef, handle: LLVMValueRef) {
        let abi = self.runtime_abi;
        self.call_runtime(
            abi.prime_frame_copy,
            abi.prime_frame_copy_ty,
            &mut [slot, handle],
            "",
        );
    }

    // A freshly boxed enum argument or a field decoded to scalar SSA has
    // finished its transfer. Its producer's slot can be reused immediately.
    pub(super) fn release_consumed_value(&mut self, value: LLVMValueRef) {
        if let Some(slot) = self.value_owners.temporaries.remove(&value) {
            let abi = self.runtime_abi;
            self.call_runtime(
                abi.prime_frame_clear,
                abi.prime_frame_clear_ty,
                &mut [slot],
                "",
            );
        }
    }

    pub(super) fn finish_value_owners(&mut self) {
        let abi = self.runtime_abi;
        for (function, slots) in mem::take(&mut self.value_owners).frames {
            unsafe {
                let mut block = LLVMGetFirstBasicBlock(function);
                while !block.is_null() {
                    let term = LLVMGetBasicBlockTerminator(block);
                    if !term.is_null() && !LLVMIsAReturnInst(term).is_null() {
                        LLVMPositionBuilderBefore(self.builder, term);
                        for slot in &slots {
                            self.call_runtime(
                                abi.prime_frame_clear,
                                abi.prime_frame_clear_ty,
                                &mut [*slot],
                                "",
                            );
                        }
                    }
                    block = LLVMGetNextBasicBlock(block);
                }
            }
        }
    }
}
