#![allow(clippy::too_many_arguments)]

use crate::language::{
    ast::*,
    enum_utils::find_variant,
    macro_expander::ExpandedProgram,
    span::Span,
    types::{Mutability, TypeAnnotation, TypeExpr},
};
use crate::target::{BuildTarget, embedded_target_hint};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

#[derive(Clone, Debug)]
pub struct TypeError {
    pub path: PathBuf,
    pub span: Span,
    pub message: String,
    pub label: String,
    pub code: Option<String>,
    pub help: Option<String>,
}

impl TypeError {
    pub fn new(path: &Path, span: Span, message: impl Into<String>) -> Self {
        let message = message.into();
        Self {
            path: path.to_path_buf(),
            span,
            label: message.clone(),
            message,
            code: None,
            help: None,
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = label.into();
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn display_message(&self) -> String {
        if let Some(code) = &self.code {
            format!("[{code}] {}", self.message)
        } else {
            self.message.clone()
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypecheckOptions {
    pub target: BuildTarget,
}

impl Default for TypecheckOptions {
    fn default() -> Self {
        Self {
            target: BuildTarget::host(),
        }
    }
}

#[derive(Default)]
struct TypeRegistry {
    modules: HashMap<String, ModuleSymbols>,
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    impls: HashSet<ImplKey>,
    pending_impls: Vec<ImplCandidate>,
    drop_impls: Vec<ImplCandidate>,
    errors: Vec<TypeError>,
    module_no_std: HashMap<String, bool>,
}

impl TypeRegistry {
    fn module_symbols_mut(&mut self, module: &str) -> &mut ModuleSymbols {
        self.modules
            .entry(module.to_string())
            .or_insert_with(ModuleSymbols::new)
    }

    fn module_symbols(&self, module: &str) -> Option<&ModuleSymbols> {
        self.modules.get(module)
    }

    fn set_module_no_std(&mut self, module: &str, no_std: bool) {
        self.module_no_std.insert(module.to_string(), no_std);
    }

    fn module_no_std(&self, module: &str) -> bool {
        self.module_no_std.get(module).copied().unwrap_or(false)
    }

    fn find_interface(&self, name: &str) -> Option<InterfaceInfo> {
        self.modules
            .values()
            .find_map(|symbols| symbols.interfaces.get(name).cloned())
    }

    fn find_function_in_module(&self, module: &str, key: &FunctionKey) -> Option<FunctionInfo> {
        self.module_symbols(module)
            .and_then(|symbols| symbols.functions.get(key).cloned())
    }

    fn find_const_in_module(&self, module: &str, name: &str) -> Option<ConstInfo> {
        self.module_symbols(module)
            .and_then(|symbols| symbols.consts.get(name).cloned())
    }

    fn find_struct_in_module(&self, module: &str, name: &str) -> Option<&StructInfo> {
        self.structs.get(name).filter(|info| info._module == module)
    }

    fn find_enum_in_module(&self, module: &str, name: &str) -> Option<&EnumInfo> {
        self.enums.get(name).filter(|info| info.module == module)
    }
}

struct ModuleSymbols {
    functions: HashMap<FunctionKey, FunctionInfo>,
    consts: HashMap<String, ConstInfo>,
    interfaces: HashMap<String, InterfaceInfo>,
    prelude: Vec<ImportSelector>,
}

impl ModuleSymbols {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            consts: HashMap::new(),
            interfaces: HashMap::new(),
            prelude: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct StructInfo {
    def: StructDef,
    _module: String,
}

#[derive(Clone)]
struct EnumInfo {
    def: EnumDef,
    module: String,
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    def: EnumVariant,
    span: Span,
    _module: String,
}

impl EnumVariantInfo {
    fn from_def(enum_name: String, enum_module: String, def: EnumVariant) -> Self {
        Self {
            enum_name,
            span: def.span,
            _module: enum_module,
            def,
        }
    }
}

#[derive(Clone)]
struct ConstInfo {
    ty: Option<TypeAnnotation>,
    visibility: Visibility,
    _span: Span,
    _module: String,
}

#[derive(Clone)]
struct FunctionSignature {
    name: String,
    type_params: Vec<String>,
    params: Vec<TypeAnnotation>,
    returns: Vec<TypeAnnotation>,
    span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionKey {
    name: String,
    receiver: Option<String>,
}

#[derive(Clone)]
struct FunctionInfo {
    signature: FunctionSignature,
    visibility: Visibility,
    _module: String,
}

#[derive(Clone)]
struct InterfaceInfo {
    def: InterfaceDef,
    _module: String,
    _span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ImplKey {
    interface: String,
    type_args: Vec<String>,
    target: String,
}

#[derive(Clone)]
struct ImplCandidate {
    module_path: PathBuf,
    block: ImplBlock,
}

mod checker;

pub use checker::{check_program, check_program_with_options};
