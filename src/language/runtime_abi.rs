#![allow(unsafe_op_in_unsafe_fn)]

use llvm_sys::{
    LLVMLinkage,
    core::{
        LLVMAddFunction, LLVMDoubleTypeInContext, LLVMFunctionType, LLVMInt1TypeInContext,
        LLVMInt8TypeInContext, LLVMInt32TypeInContext, LLVMIntTypeInContext, LLVMPointerType,
        LLVMSetLinkage, LLVMVoidTypeInContext,
    },
    prelude::*,
};
use std::ffi::CString;

#[derive(Clone, Copy)]
#[allow(dead_code)]
pub struct RuntimeAbi {
    pub handle_type: LLVMTypeRef,
    pub status_type: LLVMTypeRef,
    pub bool_type: LLVMTypeRef,
    pub int_type: LLVMTypeRef,
    pub float_type: LLVMTypeRef,
    pub usize_type: LLVMTypeRef,
    pub thread_fn_type: LLVMTypeRef,
    pub channel_out_type: LLVMTypeRef,
    pub string_data_type: LLVMTypeRef,
    pub prime_value_retain: LLVMValueRef,
    pub prime_value_retain_ty: LLVMTypeRef,
    pub prime_value_release: LLVMValueRef,
    pub prime_value_release_ty: LLVMTypeRef,
    pub prime_unit_new: LLVMValueRef,
    pub prime_unit_new_ty: LLVMTypeRef,
    pub prime_int_new: LLVMValueRef,
    pub prime_int_new_ty: LLVMTypeRef,
    pub prime_float_new: LLVMValueRef,
    pub prime_float_new_ty: LLVMTypeRef,
    pub prime_bool_new: LLVMValueRef,
    pub prime_bool_new_ty: LLVMTypeRef,
    pub prime_string_new: LLVMValueRef,
    pub prime_string_new_ty: LLVMTypeRef,
    pub prime_slice_new: LLVMValueRef,
    pub prime_slice_new_ty: LLVMTypeRef,
    pub prime_slice_push: LLVMValueRef,
    pub prime_slice_push_ty: LLVMTypeRef,
    pub prime_slice_push_handle: LLVMValueRef,
    pub prime_slice_push_handle_ty: LLVMTypeRef,
    pub prime_slice_len_handle: LLVMValueRef,
    pub prime_slice_len_handle_ty: LLVMTypeRef,
    pub prime_slice_get_handle: LLVMValueRef,
    pub prime_slice_get_handle_ty: LLVMTypeRef,
    pub prime_slice_remove_handle: LLVMValueRef,
    pub prime_slice_remove_handle_ty: LLVMTypeRef,
    pub prime_map_new: LLVMValueRef,
    pub prime_map_new_ty: LLVMTypeRef,
    pub prime_map_len_handle: LLVMValueRef,
    pub prime_map_len_handle_ty: LLVMTypeRef,
    pub prime_map_insert: LLVMValueRef,
    pub prime_map_insert_ty: LLVMTypeRef,
    pub prime_map_insert_handle: LLVMValueRef,
    pub prime_map_insert_handle_ty: LLVMTypeRef,
    pub prime_map_get_handle: LLVMValueRef,
    pub prime_map_get_handle_ty: LLVMTypeRef,
    pub prime_map_remove_handle: LLVMValueRef,
    pub prime_map_remove_handle_ty: LLVMTypeRef,
    pub prime_map_entry_handle: LLVMValueRef,
    pub prime_map_entry_handle_ty: LLVMTypeRef,
    pub prime_value_tag: LLVMValueRef,
    pub prime_value_tag_ty: LLVMTypeRef,
    pub prime_value_as_int: LLVMValueRef,
    pub prime_value_as_int_ty: LLVMTypeRef,
    pub prime_value_as_bool: LLVMValueRef,
    pub prime_value_as_bool_ty: LLVMTypeRef,
    pub prime_value_as_float: LLVMValueRef,
    pub prime_value_as_float_ty: LLVMTypeRef,
    pub prime_value_as_string: LLVMValueRef,
    pub prime_value_as_string_ty: LLVMTypeRef,
    pub prime_enum_new: LLVMValueRef,
    pub prime_enum_new_ty: LLVMTypeRef,
    pub prime_enum_tag: LLVMValueRef,
    pub prime_enum_tag_ty: LLVMTypeRef,
    pub prime_enum_get: LLVMValueRef,
    pub prime_enum_get_ty: LLVMTypeRef,
    pub prime_reference_new: LLVMValueRef,
    pub prime_reference_new_ty: LLVMTypeRef,
    pub prime_reference_read: LLVMValueRef,
    pub prime_reference_read_ty: LLVMTypeRef,
    pub prime_reference_write: LLVMValueRef,
    pub prime_reference_write_ty: LLVMTypeRef,
    pub prime_channel_new: LLVMValueRef,
    pub prime_channel_new_ty: LLVMTypeRef,
    pub prime_send: LLVMValueRef,
    pub prime_send_ty: LLVMTypeRef,
    pub prime_recv: LLVMValueRef,
    pub prime_recv_ty: LLVMTypeRef,
    pub prime_recv_timeout: LLVMValueRef,
    pub prime_recv_timeout_ty: LLVMTypeRef,
    pub prime_close: LLVMValueRef,
    pub prime_close_ty: LLVMTypeRef,
    pub prime_spawn: LLVMValueRef,
    pub prime_spawn_ty: LLVMTypeRef,
    pub prime_join: LLVMValueRef,
    pub prime_join_ty: LLVMTypeRef,
    pub prime_task_new: LLVMValueRef,
    pub prime_task_new_ty: LLVMTypeRef,
    pub prime_task_poll: LLVMValueRef,
    pub prime_task_poll_ty: LLVMTypeRef,
    pub prime_task_wake: LLVMValueRef,
    pub prime_task_wake_ty: LLVMTypeRef,
    pub prime_sleep_task: LLVMValueRef,
    pub prime_sleep_task_ty: LLVMTypeRef,
    pub prime_recv_task: LLVMValueRef,
    pub prime_recv_task_ty: LLVMTypeRef,
    pub prime_print: LLVMValueRef,
    pub prime_print_ty: LLVMTypeRef,
    pub prime_int_to_string: LLVMValueRef,
    pub prime_int_to_string_ty: LLVMTypeRef,
    pub prime_read_value: LLVMValueRef,
    pub prime_read_value_ty: LLVMTypeRef,
    pub prime_struct_new: LLVMValueRef,
    pub prime_struct_new_ty: LLVMTypeRef,
    pub prime_struct_insert: LLVMValueRef,
    pub prime_struct_insert_ty: LLVMTypeRef,
    pub prime_env_free: LLVMValueRef,
    pub prime_env_free_ty: LLVMTypeRef,
    pub prime_delay_ms: LLVMValueRef,
    pub prime_delay_ms_ty: LLVMTypeRef,
    pub prime_pin_mode: LLVMValueRef,
    pub prime_pin_mode_ty: LLVMTypeRef,
    pub prime_digital_write: LLVMValueRef,
    pub prime_digital_write_ty: LLVMTypeRef,
    pub prime_now_ms: LLVMValueRef,
    pub prime_now_ms_ty: LLVMTypeRef,
}

impl RuntimeAbi {
    pub unsafe fn declare(
        context: LLVMContextRef,
        module: LLVMModuleRef,
        pointer_width_bits: u32,
    ) -> Self {
        let handle_type = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
        let status_type = LLVMInt32TypeInContext(context);
        let mut thread_params = [handle_type];
        let thread_fn_type = LLVMPointerType(
            LLVMFunctionType(handle_type, thread_params.as_mut_ptr(), 1, 0),
            0,
        );
        let channel_out_type = LLVMPointerType(handle_type, 0);
        let string_data_type = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
        let void_type = LLVMVoidTypeInContext(context);
        let bool_type = LLVMInt1TypeInContext(context);
        let int_type = LLVMIntTypeInContext(context, 128);
        let float_type = LLVMDoubleTypeInContext(context);
        let usize_type = LLVMIntTypeInContext(context, pointer_width_bits);

        let (prime_value_retain, prime_value_retain_ty) = declare_fn(
            module,
            "prime_value_retain",
            handle_type,
            &mut [handle_type],
        );
        let (prime_value_release, prime_value_release_ty) =
            declare_fn(module, "prime_value_release", void_type, &mut [handle_type]);
        let (prime_unit_new, prime_unit_new_ty) =
            declare_fn(module, "prime_unit_new", handle_type, &mut []);
        let (prime_int_new, prime_int_new_ty) =
            declare_fn(module, "prime_int_new", handle_type, &mut [int_type]);
        let (prime_float_new, prime_float_new_ty) =
            declare_fn(module, "prime_float_new", handle_type, &mut [float_type]);
        let (prime_bool_new, prime_bool_new_ty) =
            declare_fn(module, "prime_bool_new", handle_type, &mut [bool_type]);
        let (prime_string_new, prime_string_new_ty) = declare_fn(
            module,
            "prime_string_new",
            handle_type,
            &mut [string_data_type, int_type],
        );
        let (prime_slice_new, prime_slice_new_ty) =
            declare_fn(module, "prime_slice_new", handle_type, &mut []);
        let (prime_slice_push, prime_slice_push_ty) = declare_fn(
            module,
            "prime_slice_push",
            status_type,
            &mut [handle_type, handle_type],
        );
        let (prime_slice_push_handle, prime_slice_push_handle_ty) = declare_fn(
            module,
            "prime_slice_push_handle",
            status_type,
            &mut [handle_type, handle_type],
        );
        let (prime_slice_len_handle, prime_slice_len_handle_ty) = declare_fn(
            module,
            "prime_slice_len_handle",
            usize_type,
            &mut [handle_type],
        );
        let (prime_slice_get_handle, prime_slice_get_handle_ty) = declare_fn(
            module,
            "prime_slice_get_handle",
            status_type,
            &mut [handle_type, usize_type, LLVMPointerType(handle_type, 0)],
        );
        let (prime_slice_remove_handle, prime_slice_remove_handle_ty) = declare_fn(
            module,
            "prime_slice_remove_handle",
            status_type,
            &mut [handle_type, usize_type, LLVMPointerType(handle_type, 0)],
        );
        let (prime_map_new, prime_map_new_ty) =
            declare_fn(module, "prime_map_new", handle_type, &mut []);
        let (prime_map_len_handle, prime_map_len_handle_ty) = declare_fn(
            module,
            "prime_map_len_handle",
            usize_type,
            &mut [handle_type],
        );
        let (prime_map_insert, prime_map_insert_ty) = declare_fn(
            module,
            "prime_map_insert",
            status_type,
            &mut [handle_type, string_data_type, usize_type, handle_type],
        );
        let (prime_map_insert_handle, prime_map_insert_handle_ty) = declare_fn(
            module,
            "prime_map_insert_handle",
            status_type,
            &mut [handle_type, string_data_type, usize_type, handle_type],
        );
        let (prime_map_get_handle, prime_map_get_handle_ty) = declare_fn(
            module,
            "prime_map_get_handle",
            status_type,
            &mut [
                handle_type,
                string_data_type,
                usize_type,
                LLVMPointerType(handle_type, 0),
            ],
        );
        let (prime_map_remove_handle, prime_map_remove_handle_ty) = declare_fn(
            module,
            "prime_map_remove_handle",
            status_type,
            &mut [
                handle_type,
                string_data_type,
                usize_type,
                LLVMPointerType(handle_type, 0),
            ],
        );
        let (prime_map_entry_handle, prime_map_entry_handle_ty) = declare_fn(
            module,
            "prime_map_entry_handle",
            status_type,
            &mut [
                handle_type,
                usize_type,
                LLVMPointerType(handle_type, 0),
                LLVMPointerType(handle_type, 0),
            ],
        );
        let (prime_value_tag, prime_value_tag_ty) =
            declare_fn(module, "prime_value_tag", status_type, &mut [handle_type]);
        let (prime_value_as_int, prime_value_as_int_ty) =
            declare_fn(module, "prime_value_as_int", int_type, &mut [handle_type]);
        let (prime_value_as_bool, prime_value_as_bool_ty) =
            declare_fn(module, "prime_value_as_bool", bool_type, &mut [handle_type]);
        let (prime_value_as_float, prime_value_as_float_ty) = declare_fn(
            module,
            "prime_value_as_float",
            float_type,
            &mut [handle_type],
        );
        let (prime_value_as_string, prime_value_as_string_ty) = declare_fn(
            module,
            "prime_value_as_string",
            status_type,
            &mut [
                handle_type,
                LLVMPointerType(string_data_type, 0),
                LLVMPointerType(usize_type, 0),
            ],
        );
        let (prime_enum_new, prime_enum_new_ty) = declare_fn(
            module,
            "prime_enum_new",
            handle_type,
            &mut [
                LLVMPointerType(handle_type, 0),
                usize_type,
                LLVMInt32TypeInContext(context),
            ],
        );
        let (prime_enum_tag, prime_enum_tag_ty) = declare_fn(
            module,
            "prime_enum_tag",
            LLVMInt32TypeInContext(context),
            &mut [handle_type],
        );
        let (prime_enum_get, prime_enum_get_ty) = declare_fn(
            module,
            "prime_enum_get",
            handle_type,
            &mut [handle_type, usize_type],
        );
        let (prime_reference_new, prime_reference_new_ty) = declare_fn(
            module,
            "prime_reference_new",
            handle_type,
            &mut [handle_type, bool_type],
        );
        let (prime_reference_read, prime_reference_read_ty) = declare_fn(
            module,
            "prime_reference_read",
            handle_type,
            &mut [handle_type],
        );
        let (prime_reference_write, prime_reference_write_ty) = declare_fn(
            module,
            "prime_reference_write",
            status_type,
            &mut [handle_type, handle_type],
        );
        let (prime_channel_new, prime_channel_new_ty) = declare_fn(
            module,
            "prime_channel_new",
            status_type,
            &mut [channel_out_type, channel_out_type],
        );
        let (prime_send, prime_send_ty) = declare_fn(
            module,
            "prime_send",
            status_type,
            &mut [handle_type, handle_type],
        );
        let (prime_recv, prime_recv_ty) = declare_fn(
            module,
            "prime_recv",
            status_type,
            &mut [handle_type, channel_out_type],
        );
        let (prime_recv_timeout, prime_recv_timeout_ty) = declare_fn(
            module,
            "prime_recv_timeout",
            status_type,
            &mut [handle_type, int_type, channel_out_type],
        );
        let (prime_close, prime_close_ty) =
            declare_fn(module, "prime_close", status_type, &mut [handle_type]);
        let (prime_spawn, prime_spawn_ty) = declare_fn(
            module,
            "prime_spawn",
            status_type,
            &mut [thread_fn_type, handle_type, channel_out_type],
        );
        let (prime_join, prime_join_ty) = declare_fn(
            module,
            "prime_join",
            status_type,
            &mut [handle_type, channel_out_type],
        );
        let (prime_task_new, prime_task_new_ty) = declare_fn(
            module,
            "prime_task_new",
            handle_type,
            &mut [thread_fn_type, handle_type],
        );
        let (prime_task_poll, prime_task_poll_ty) = declare_fn(
            module,
            "prime_task_poll",
            status_type,
            &mut [handle_type, channel_out_type],
        );
        let (prime_task_wake, prime_task_wake_ty) = declare_fn(
            module,
            "prime_task_wake",
            status_type,
            &mut [handle_type, handle_type],
        );
        let (prime_sleep_task, prime_sleep_task_ty) =
            declare_fn(module, "prime_sleep_task", handle_type, &mut [int_type]);
        let (prime_recv_task, prime_recv_task_ty) =
            declare_fn(module, "prime_recv_task", handle_type, &mut [handle_type]);
        let (prime_print, prime_print_ty) =
            declare_fn(module, "prime_print", void_type, &mut [handle_type]);
        let (prime_int_to_string, prime_int_to_string_ty) =
            declare_fn(module, "prime_int_to_string", handle_type, &mut [int_type]);
        let (prime_read_value, prime_read_value_ty) = declare_fn(
            module,
            "prime_read_value",
            handle_type,
            &mut [
                status_type,
                status_type,
                status_type,
                string_data_type,
                usize_type,
                LLVMPointerType(handle_type, 0),
                usize_type,
            ],
        );
        let (prime_struct_new, prime_struct_new_ty) = declare_fn(
            module,
            "prime_struct_new",
            handle_type,
            &mut [string_data_type, usize_type],
        );
        let (prime_struct_insert, prime_struct_insert_ty) = declare_fn(
            module,
            "prime_struct_insert",
            status_type,
            &mut [handle_type, string_data_type, usize_type, handle_type],
        );
        let (prime_env_free, prime_env_free_ty) =
            declare_fn(module, "prime_env_free", void_type, &mut [handle_type]);
        let (prime_delay_ms, prime_delay_ms_ty) = declare_fn(
            module,
            "prime_delay_ms",
            status_type,
            &mut [LLVMInt32TypeInContext(context)],
        );
        let (prime_pin_mode, prime_pin_mode_ty) = declare_fn(
            module,
            "prime_pin_mode",
            status_type,
            &mut [
                LLVMInt32TypeInContext(context),
                LLVMInt32TypeInContext(context),
            ],
        );
        let (prime_digital_write, prime_digital_write_ty) = declare_fn(
            module,
            "prime_digital_write",
            status_type,
            &mut [
                LLVMInt32TypeInContext(context),
                LLVMInt32TypeInContext(context),
            ],
        );
        let (prime_now_ms, prime_now_ms_ty) = declare_fn(module, "prime_now_ms", int_type, &mut []);

        Self {
            handle_type,
            status_type,
            bool_type,
            int_type,
            float_type,
            usize_type,
            thread_fn_type,
            channel_out_type,
            string_data_type,
            prime_value_retain,
            prime_value_retain_ty,
            prime_value_release,
            prime_value_release_ty,
            prime_unit_new,
            prime_unit_new_ty,
            prime_int_new,
            prime_int_new_ty,
            prime_float_new,
            prime_float_new_ty,
            prime_bool_new,
            prime_bool_new_ty,
            prime_string_new,
            prime_string_new_ty,
            prime_slice_new,
            prime_slice_new_ty,
            prime_slice_push,
            prime_slice_push_ty,
            prime_slice_push_handle,
            prime_slice_push_handle_ty,
            prime_slice_len_handle,
            prime_slice_len_handle_ty,
            prime_slice_get_handle,
            prime_slice_get_handle_ty,
            prime_slice_remove_handle,
            prime_slice_remove_handle_ty,
            prime_map_new,
            prime_map_new_ty,
            prime_map_len_handle,
            prime_map_len_handle_ty,
            prime_map_insert,
            prime_map_insert_ty,
            prime_map_insert_handle,
            prime_map_insert_handle_ty,
            prime_map_get_handle,
            prime_map_get_handle_ty,
            prime_map_remove_handle,
            prime_map_remove_handle_ty,
            prime_map_entry_handle,
            prime_map_entry_handle_ty,
            prime_value_tag,
            prime_value_tag_ty,
            prime_value_as_int,
            prime_value_as_int_ty,
            prime_value_as_bool,
            prime_value_as_bool_ty,
            prime_value_as_float,
            prime_value_as_float_ty,
            prime_value_as_string,
            prime_value_as_string_ty,
            prime_enum_new,
            prime_enum_new_ty,
            prime_enum_tag,
            prime_enum_tag_ty,
            prime_enum_get,
            prime_enum_get_ty,
            prime_reference_new,
            prime_reference_new_ty,
            prime_reference_read,
            prime_reference_read_ty,
            prime_reference_write,
            prime_reference_write_ty,
            prime_channel_new,
            prime_channel_new_ty,
            prime_send,
            prime_send_ty,
            prime_recv,
            prime_recv_ty,
            prime_recv_timeout,
            prime_recv_timeout_ty,
            prime_close,
            prime_close_ty,
            prime_spawn,
            prime_spawn_ty,
            prime_join,
            prime_join_ty,
            prime_task_new,
            prime_task_new_ty,
            prime_task_poll,
            prime_task_poll_ty,
            prime_task_wake,
            prime_task_wake_ty,
            prime_sleep_task,
            prime_sleep_task_ty,
            prime_recv_task,
            prime_recv_task_ty,
            prime_print,
            prime_print_ty,
            prime_int_to_string,
            prime_int_to_string_ty,
            prime_read_value,
            prime_read_value_ty,
            prime_struct_new,
            prime_struct_new_ty,
            prime_struct_insert,
            prime_struct_insert_ty,
            prime_env_free,
            prime_env_free_ty,
            prime_delay_ms,
            prime_delay_ms_ty,
            prime_pin_mode,
            prime_pin_mode_ty,
            prime_digital_write,
            prime_digital_write_ty,
            prime_now_ms,
            prime_now_ms_ty,
        }
    }

    pub fn empty() -> Self {
        Self {
            handle_type: std::ptr::null_mut(),
            status_type: std::ptr::null_mut(),
            bool_type: std::ptr::null_mut(),
            int_type: std::ptr::null_mut(),
            float_type: std::ptr::null_mut(),
            usize_type: std::ptr::null_mut(),
            thread_fn_type: std::ptr::null_mut(),
            channel_out_type: std::ptr::null_mut(),
            string_data_type: std::ptr::null_mut(),
            prime_value_retain: std::ptr::null_mut(),
            prime_value_retain_ty: std::ptr::null_mut(),
            prime_value_release: std::ptr::null_mut(),
            prime_value_release_ty: std::ptr::null_mut(),
            prime_unit_new: std::ptr::null_mut(),
            prime_unit_new_ty: std::ptr::null_mut(),
            prime_int_new: std::ptr::null_mut(),
            prime_int_new_ty: std::ptr::null_mut(),
            prime_float_new: std::ptr::null_mut(),
            prime_float_new_ty: std::ptr::null_mut(),
            prime_bool_new: std::ptr::null_mut(),
            prime_bool_new_ty: std::ptr::null_mut(),
            prime_string_new: std::ptr::null_mut(),
            prime_string_new_ty: std::ptr::null_mut(),
            prime_slice_new: std::ptr::null_mut(),
            prime_slice_new_ty: std::ptr::null_mut(),
            prime_slice_push: std::ptr::null_mut(),
            prime_slice_push_ty: std::ptr::null_mut(),
            prime_slice_push_handle: std::ptr::null_mut(),
            prime_slice_push_handle_ty: std::ptr::null_mut(),
            prime_slice_len_handle: std::ptr::null_mut(),
            prime_slice_len_handle_ty: std::ptr::null_mut(),
            prime_slice_get_handle: std::ptr::null_mut(),
            prime_slice_get_handle_ty: std::ptr::null_mut(),
            prime_slice_remove_handle: std::ptr::null_mut(),
            prime_slice_remove_handle_ty: std::ptr::null_mut(),
            prime_map_new: std::ptr::null_mut(),
            prime_map_new_ty: std::ptr::null_mut(),
            prime_map_len_handle: std::ptr::null_mut(),
            prime_map_len_handle_ty: std::ptr::null_mut(),
            prime_map_insert: std::ptr::null_mut(),
            prime_map_insert_ty: std::ptr::null_mut(),
            prime_map_insert_handle: std::ptr::null_mut(),
            prime_map_insert_handle_ty: std::ptr::null_mut(),
            prime_map_get_handle: std::ptr::null_mut(),
            prime_map_get_handle_ty: std::ptr::null_mut(),
            prime_map_remove_handle: std::ptr::null_mut(),
            prime_map_remove_handle_ty: std::ptr::null_mut(),
            prime_map_entry_handle: std::ptr::null_mut(),
            prime_map_entry_handle_ty: std::ptr::null_mut(),
            prime_value_tag: std::ptr::null_mut(),
            prime_value_tag_ty: std::ptr::null_mut(),
            prime_value_as_int: std::ptr::null_mut(),
            prime_value_as_int_ty: std::ptr::null_mut(),
            prime_value_as_bool: std::ptr::null_mut(),
            prime_value_as_bool_ty: std::ptr::null_mut(),
            prime_value_as_float: std::ptr::null_mut(),
            prime_value_as_float_ty: std::ptr::null_mut(),
            prime_value_as_string: std::ptr::null_mut(),
            prime_value_as_string_ty: std::ptr::null_mut(),
            prime_enum_new: std::ptr::null_mut(),
            prime_enum_new_ty: std::ptr::null_mut(),
            prime_enum_tag: std::ptr::null_mut(),
            prime_enum_tag_ty: std::ptr::null_mut(),
            prime_enum_get: std::ptr::null_mut(),
            prime_enum_get_ty: std::ptr::null_mut(),
            prime_reference_new: std::ptr::null_mut(),
            prime_reference_new_ty: std::ptr::null_mut(),
            prime_reference_read: std::ptr::null_mut(),
            prime_reference_read_ty: std::ptr::null_mut(),
            prime_reference_write: std::ptr::null_mut(),
            prime_reference_write_ty: std::ptr::null_mut(),
            prime_channel_new: std::ptr::null_mut(),
            prime_channel_new_ty: std::ptr::null_mut(),
            prime_send: std::ptr::null_mut(),
            prime_send_ty: std::ptr::null_mut(),
            prime_recv: std::ptr::null_mut(),
            prime_recv_ty: std::ptr::null_mut(),
            prime_recv_timeout: std::ptr::null_mut(),
            prime_recv_timeout_ty: std::ptr::null_mut(),
            prime_close: std::ptr::null_mut(),
            prime_close_ty: std::ptr::null_mut(),
            prime_spawn: std::ptr::null_mut(),
            prime_spawn_ty: std::ptr::null_mut(),
            prime_join: std::ptr::null_mut(),
            prime_join_ty: std::ptr::null_mut(),
            prime_task_new: std::ptr::null_mut(),
            prime_task_new_ty: std::ptr::null_mut(),
            prime_task_poll: std::ptr::null_mut(),
            prime_task_poll_ty: std::ptr::null_mut(),
            prime_task_wake: std::ptr::null_mut(),
            prime_task_wake_ty: std::ptr::null_mut(),
            prime_sleep_task: std::ptr::null_mut(),
            prime_sleep_task_ty: std::ptr::null_mut(),
            prime_recv_task: std::ptr::null_mut(),
            prime_recv_task_ty: std::ptr::null_mut(),
            prime_print: std::ptr::null_mut(),
            prime_print_ty: std::ptr::null_mut(),
            prime_int_to_string: std::ptr::null_mut(),
            prime_int_to_string_ty: std::ptr::null_mut(),
            prime_read_value: std::ptr::null_mut(),
            prime_read_value_ty: std::ptr::null_mut(),
            prime_struct_new: std::ptr::null_mut(),
            prime_struct_new_ty: std::ptr::null_mut(),
            prime_struct_insert: std::ptr::null_mut(),
            prime_struct_insert_ty: std::ptr::null_mut(),
            prime_env_free: std::ptr::null_mut(),
            prime_env_free_ty: std::ptr::null_mut(),
            prime_delay_ms: std::ptr::null_mut(),
            prime_delay_ms_ty: std::ptr::null_mut(),
            prime_pin_mode: std::ptr::null_mut(),
            prime_pin_mode_ty: std::ptr::null_mut(),
            prime_digital_write: std::ptr::null_mut(),
            prime_digital_write_ty: std::ptr::null_mut(),
            prime_now_ms: std::ptr::null_mut(),
            prime_now_ms_ty: std::ptr::null_mut(),
        }
    }
}

unsafe fn declare_fn(
    module: LLVMModuleRef,
    name: &str,
    ret: LLVMTypeRef,
    params: &mut [LLVMTypeRef],
) -> (LLVMValueRef, LLVMTypeRef) {
    let name_c = CString::new(name).expect("ffi string");
    let function_type = LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0);
    let func = LLVMAddFunction(module, name_c.as_ptr(), function_type);
    LLVMSetLinkage(func, LLVMLinkage::LLVMExternalLinkage);
    (func, function_type)
}
