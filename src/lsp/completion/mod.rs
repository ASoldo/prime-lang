use crate::{
    language::{
        ast::{
            FunctionDef, FunctionParam, Import, ImportSelector, InterfaceMethod, Item, MacroDef,
            Module, Visibility,
        },
        span::Span,
        types::TypeExpr,
    },
    project::manifest::{ModuleInfo, ModuleVisibility, PackageManifest},
    target::BuildTarget,
};
use std::collections::{HashMap, HashSet};
use tower_lsp_server::ls_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionTextEdit, Range, TextEdit,
};

use super::{
    analysis::{find_local_decl, receiver_type_name, visible_locals},
    text::{
        identifier_prefix_slice, is_ident_char, is_ident_string, offset_to_position, prefix_matches,
    },
};

mod chain;
mod collect;
mod format;
mod items;
mod module_paths;
#[cfg(test)]
mod tests;
mod type_utils;
mod types;

pub use chain::{chain_for_field_token, expression_chain_before_dot, resolve_chain_from_scope};
pub use collect::{
    collect_interface_info, collect_struct_info, select_interface_info, select_struct_info,
};
pub use format::{
    format_function_param, format_function_signature, format_interface_method_signature,
    format_type_arguments, format_type_expr, format_type_params,
};
pub use items::{
    enum_variant_completion_items, general_completion_items, keyword_completion_items,
    member_completion_items,
};
pub use module_paths::{
    completion_prefix, completion_trigger_characters, module_completion_items_from_manifest,
    module_path_completion_context, module_selector_items_from_modules,
};
pub use type_utils::named_type_with_args;
pub use types::{
    ChainResolution, InterfaceInfo, MethodInfo, ModulePathCompletionContext,
    ModulePathCompletionKind, StructFieldInfo, StructInfo, StructInfoMap,
};

use format::format_macro_detail;
use type_utils::{build_type_subst, strip_type_refs, struct_name_from_type};
