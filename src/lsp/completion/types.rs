use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ModulePathCompletionKind {
    Declaration,
    Import,
    ImportSelectors,
}

pub struct ModulePathCompletionContext {
    pub kind: ModulePathCompletionKind,
    pub prefix: Option<String>,
}

#[derive(Clone)]
pub struct StructFieldInfo {
    pub name: String,
    pub ty: TypeExpr,
    pub declared_in: String,
}

#[derive(Clone)]
pub struct MethodInfo {
    pub name: String,
    pub signature: String,
    pub declared_in: String,
}

#[derive(Clone)]
pub struct StructInfo {
    pub module_name: String,
    pub fields: Vec<StructFieldInfo>,
    pub methods: Vec<MethodInfo>,
}

pub type StructInfoMap = HashMap<String, Vec<StructInfo>>;

pub struct ChainResolution<'a> {
    pub ty: TypeExpr,
    pub last_field: Option<(String, &'a StructFieldInfo)>,
    pub module_name: Option<String>,
}

#[derive(Clone)]
pub struct InterfaceInfo {
    pub module_name: String,
    pub type_params: Vec<String>,
    pub methods: Vec<InterfaceMethod>,
}
