use crate::{
    language::{
        ast::{FunctionDef, FunctionParam, InterfaceMethod, Item, Module},
        span::Span,
        types::TypeExpr,
    },
    project::manifest::{ModuleInfo, ModuleVisibility, PackageManifest},
};
use std::collections::{HashMap, HashSet};
use tower_lsp_server::lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionTextEdit, Range, TextEdit,
};

use super::{
    analysis::{find_local_decl, receiver_type_name, visible_locals},
    text::{
        identifier_prefix_slice, is_ident_char, is_ident_string, offset_to_position, prefix_matches,
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ModulePathCompletionKind {
    Declaration,
    Import,
}

pub struct ModulePathCompletionContext {
    pub kind: ModulePathCompletionKind,
    pub prefix: Option<String>,
}

pub fn module_path_completion_context(
    text: &str,
    offset: usize,
) -> Option<ModulePathCompletionContext> {
    if text.is_empty() || offset == 0 {
        return None;
    }
    let line_start = text[..offset].rfind('\n').map(|idx| idx + 1).unwrap_or(0);
    let line = &text[line_start..offset];
    let mut trimmed = line.trim_start();
    if trimmed.starts_with("module ") {
        let after = trimmed["module ".len()..].trim_start();
        return Some(ModulePathCompletionContext {
            kind: ModulePathCompletionKind::Declaration,
            prefix: sanitize_module_prefix(after),
        });
    }
    if trimmed.starts_with("pub ") {
        trimmed = trimmed["pub ".len()..].trim_start();
    }
    if trimmed.starts_with("import ") {
        let after = trimmed["import ".len()..].trim_start();
        return Some(ModulePathCompletionContext {
            kind: ModulePathCompletionKind::Import,
            prefix: sanitize_module_prefix(after),
        });
    }
    None
}

pub fn completion_trigger_characters() -> Vec<String> {
    const TRIGGER_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.";
    TRIGGER_CHARS.chars().map(|ch| ch.to_string()).collect()
}

fn sanitize_module_prefix(input: &str) -> Option<String> {
    let mut end = input.len();
    for (idx, ch) in input.char_indices() {
        if ch == ';' || ch.is_whitespace() {
            end = idx;
            break;
        }
    }
    let prefix = input[..end].trim().to_string();
    if prefix.is_empty() {
        None
    } else {
        Some(prefix)
    }
}

pub fn module_completion_items_from_manifest(
    manifest: &PackageManifest,
    prefix: Option<&str>,
    expected: Option<&str>,
) -> Vec<CompletionItem> {
    let entries = manifest.module_entries();
    let mut items = Vec::new();
    let mut seen = HashSet::new();
    if let Some(exp) = expected {
        if module_prefix_matches(exp, prefix) && seen.insert(exp.to_string()) {
            let detail = entries
                .iter()
                .find(|entry| entry.name == exp)
                .map(module_detail);
            items.push(CompletionItem {
                label: exp.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                detail,
                ..CompletionItem::default()
            });
        }
    }
    for entry in entries.iter() {
        if !module_prefix_matches(&entry.name, prefix) {
            continue;
        }
        if !seen.insert(entry.name.clone()) {
            continue;
        }
        let detail = module_detail(entry);
        items.push(CompletionItem {
            label: entry.name.clone(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(detail),
            ..CompletionItem::default()
        });
    }
    items
}

fn module_prefix_matches(name: &str, prefix: Option<&str>) -> bool {
    match prefix {
        Some(prefix) => name.starts_with(prefix),
        None => true,
    }
}

fn module_visibility_label(vis: ModuleVisibility) -> &'static str {
    match vis {
        ModuleVisibility::Public => "pub",
        ModuleVisibility::Package => "package",
        ModuleVisibility::Private => "private",
    }
}

fn module_detail(entry: &ModuleInfo) -> String {
    let mut detail = format!(
        "{} [{}]",
        entry.path.display(),
        module_visibility_label(entry.visibility)
    );
    if let Some(package) = &entry.package {
        detail.push_str(&format!(" ({package})"));
    }
    if let Some(doc) = &entry.doc {
        detail.push_str(" - ");
        detail.push_str(doc);
    }
    detail
}

pub fn completion_prefix(
    text: &str,
    offset: usize,
    context: Option<&CompletionContext>,
) -> Option<String> {
    let base = identifier_prefix_slice(text, offset).map(str::to_string);
    let trigger = context
        .and_then(|ctx| ctx.trigger_character.as_ref())
        .filter(|ch| is_ident_string(ch));

    match (base, trigger) {
        (Some(mut prefix), Some(trigger)) => {
            if !prefix.ends_with(trigger) {
                prefix.push_str(trigger);
            }
            Some(prefix)
        }
        (Some(prefix), None) => Some(prefix),
        (None, Some(trigger)) => Some(trigger.to_string()),
        _ => None,
    }
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
    pub fields: Vec<StructFieldInfo>,
    pub methods: Vec<MethodInfo>,
}

#[derive(Clone)]
pub struct InterfaceInfo {
    pub module_name: String,
    pub type_params: Vec<String>,
    pub methods: Vec<InterfaceMethod>,
}

#[derive(Default, Clone)]
struct RawStructInfo {
    fields: Vec<StructFieldInfo>,
    embedded: Vec<String>,
    methods: Vec<MethodInfo>,
}

pub fn collect_struct_info(modules: &[Module]) -> HashMap<String, StructInfo> {
    let mut raw = HashMap::new();
    for module in modules {
        for item in &module.items {
            if let Item::Struct(def) = item {
                let mut struct_fields = Vec::new();
                let mut embedded = Vec::new();
                for field in &def.fields {
                    if let Some(name) = &field.name {
                        struct_fields.push(StructFieldInfo {
                            name: name.clone(),
                            ty: field.ty.ty.clone(),
                            declared_in: def.name.clone(),
                        });
                    } else if field.embedded {
                        if let Some(target) = struct_name_from_type(&field.ty.ty) {
                            embedded.push(target.to_string());
                        }
                    }
                }
                raw.insert(
                    def.name.clone(),
                    RawStructInfo {
                        fields: struct_fields,
                        embedded,
                        methods: Vec::new(),
                    },
                );
            }
        }
    }
    for module in modules {
        for item in &module.items {
            if let Item::Function(func) = item {
                if let Some(first_param) = func.params.first() {
                    if let Some(receiver) = receiver_type_name(&first_param.ty.ty) {
                        if let Some(entry) = raw.get_mut(&receiver) {
                            entry.methods.push(MethodInfo {
                                name: func.name.clone(),
                                signature: format_function_signature(func),
                                declared_in: receiver,
                            });
                        }
                    }
                }
            } else if let Item::Impl(block) = item {
                let target = block.target.clone();
                for method in &block.methods {
                    if let Some(first_param) = method.params.first() {
                        if let Some(receiver) = receiver_type_name(&first_param.ty.ty) {
                            if receiver == target {
                                if let Some(entry) = raw.get_mut(&receiver) {
                                    entry.methods.push(MethodInfo {
                                        name: method.name.clone(),
                                        signature: format_function_signature(method),
                                        declared_in: receiver.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    let mut info = HashMap::new();
    let mut fields_cache = HashMap::new();
    let mut methods_cache = HashMap::new();
    let struct_names: Vec<String> = raw.keys().cloned().collect();
    for name in struct_names {
        let mut field_stack = HashSet::new();
        let fields = flatten_struct_fields(&name, &raw, &mut fields_cache, &mut field_stack);
        let mut method_stack = HashSet::new();
        let methods = flatten_struct_methods(&name, &raw, &mut methods_cache, &mut method_stack);
        info.insert(name.clone(), StructInfo { fields, methods });
    }
    info
}

pub fn collect_interface_info(modules: &[Module]) -> HashMap<String, Vec<InterfaceInfo>> {
    let mut map: HashMap<String, Vec<InterfaceInfo>> = HashMap::new();
    for module in modules {
        for item in &module.items {
            if let Item::Interface(def) = item {
                map.entry(def.name.clone())
                    .or_default()
                    .push(InterfaceInfo {
                        module_name: module.name.clone(),
                        type_params: def.type_params.clone(),
                        methods: def.methods.clone(),
                    });
            }
        }
    }
    map
}

pub fn select_interface_info<'a>(
    interfaces: &'a HashMap<String, Vec<InterfaceInfo>>,
    name: &str,
    module_name: &str,
) -> Option<&'a InterfaceInfo> {
    let list = interfaces.get(name)?;
    list.iter()
        .find(|info| info.module_name == module_name)
        .or_else(|| list.first())
}

fn flatten_struct_fields(
    name: &str,
    raw: &HashMap<String, RawStructInfo>,
    cache: &mut HashMap<String, Vec<StructFieldInfo>>,
    stack: &mut HashSet<String>,
) -> Vec<StructFieldInfo> {
    if let Some(cached) = cache.get(name) {
        return cached.clone();
    }
    if !stack.insert(name.to_string()) {
        return Vec::new();
    }
    let mut fields = Vec::new();
    if let Some(entry) = raw.get(name) {
        fields.extend(entry.fields.clone());
        for embedded in &entry.embedded {
            fields.extend(flatten_struct_fields(embedded, raw, cache, stack));
        }
    }
    stack.remove(name);
    cache.insert(name.to_string(), fields.clone());
    fields
}

fn flatten_struct_methods(
    name: &str,
    raw: &HashMap<String, RawStructInfo>,
    cache: &mut HashMap<String, Vec<MethodInfo>>,
    stack: &mut HashSet<String>,
) -> Vec<MethodInfo> {
    if let Some(cached) = cache.get(name) {
        return cached.clone();
    }
    if !stack.insert(name.to_string()) {
        return Vec::new();
    }
    let mut methods = Vec::new();
    if let Some(entry) = raw.get(name) {
        methods.extend(entry.methods.clone());
        for embedded in &entry.embedded {
            methods.extend(flatten_struct_methods(embedded, raw, cache, stack));
        }
    }
    stack.remove(name);
    cache.insert(name.to_string(), methods.clone());
    methods
}

pub fn member_completion_items(
    text: &str,
    chain: &[String],
    struct_info: &HashMap<String, StructInfo>,
    interfaces: &HashMap<String, Vec<InterfaceInfo>>,
    prefix: Option<&str>,
    module: &Module,
    offset: usize,
) -> Option<Vec<CompletionItem>> {
    let (target_type, _) = resolve_chain_from_scope(chain, module, struct_info, offset)?;
    let qualifier = if chain.is_empty() {
        None
    } else {
        Some(chain.join("."))
    };
    let edit_range = member_completion_edit_range(text, offset, prefix, qualifier.as_deref());
    let mut items = builtin_member_completion_items(
        strip_type_refs(&target_type),
        qualifier.as_deref(),
        prefix,
        &edit_range,
    );
    if let Some((name, args)) = named_type_with_args(strip_type_refs(&target_type)) {
        if let Some(info) = struct_info.get(&name) {
            for field in &info.fields {
                if prefix_matches(&field.name, prefix) {
                    let filter_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", field.name));
                    let new_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", field.name))
                        .unwrap_or_else(|| field.name.clone());
                    items.push(CompletionItem {
                        label: field.name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(format!(
                            "{} (from {})",
                            format_type_expr(&field.ty),
                            field.declared_in
                        )),
                        filter_text,
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text,
                        })),
                        ..Default::default()
                    });
                }
            }
            for method in &info.methods {
                if prefix_matches(&method.name, prefix) {
                    let filter_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name));
                    let new_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name))
                        .unwrap_or_else(|| method.name.clone());
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!(
                            "{} (from {})",
                            method.signature, method.declared_in
                        )),
                        filter_text,
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text,
                        })),
                        ..Default::default()
                    });
                }
            }
        } else if let Some(info) = select_interface_info(interfaces, &name, &module.name) {
            let subst = build_type_subst(&info.type_params, &args);
            let subst_ref = subst.as_ref();
            for method in &info.methods {
                if prefix_matches(&method.name, prefix) {
                    let filter_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name));
                    let new_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name))
                        .unwrap_or_else(|| method.name.clone());
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!(
                            "{} (from {})",
                            format_interface_method_signature(method, subst_ref),
                            name
                        )),
                        filter_text,
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text,
                        })),
                        ..Default::default()
                    });
                }
            }
        }
    }
    if items.is_empty() { None } else { Some(items) }
}

fn member_completion_edit_range(
    text: &str,
    offset: usize,
    prefix: Option<&str>,
    qualifier: Option<&str>,
) -> Range {
    let prefix_len = prefix.map(|p| p.len()).unwrap_or(0);
    let qualifier_len = qualifier.map(|q| q.len()).unwrap_or(0);
    let dot_len = if qualifier.is_some() { 1 } else { 0 };
    let replace_len = prefix_len + qualifier_len + dot_len;
    let start_offset = offset.saturating_sub(replace_len);
    Range {
        start: offset_to_position(text, start_offset),
        end: offset_to_position(text, offset),
    }
}

pub fn named_type_with_args(ty: &TypeExpr) -> Option<(String, Vec<TypeExpr>)> {
    match ty {
        TypeExpr::Named(name, args) => Some((name.clone(), args.clone())),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => named_type_with_args(ty),
        _ => None,
    }
}

fn builtin_member_completion_items(
    ty: &TypeExpr,
    qualifier: Option<&str>,
    prefix: Option<&str>,
    edit_range: &Range,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    match ty {
        TypeExpr::Slice(inner) => {
            let element_ty = inner.as_ref().clone();
            let option_ty = TypeExpr::Named("Option".into(), vec![element_ty.clone()]);
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "slice_len",
                "fn slice_len() -> int32".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "slice_get",
                format!(
                    "fn slice_get(index: int32) -> {}",
                    format_type_expr(&option_ty)
                ),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "slice_push",
                format!(
                    "fn slice_push(value: {}) -> ()",
                    format_type_expr(&element_ty)
                ),
            );
        }
        TypeExpr::Named(name, args) if name == "Box" && args.len() == 1 => {
            let inner = args[0].clone();
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "box_get",
                format!("fn box_get() -> {}", format_type_expr(&inner)),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "box_set",
                format!("fn box_set(value: {}) -> ()", format_type_expr(&inner)),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "box_take",
                format!("fn box_take() -> {}", format_type_expr(&inner)),
            );
        }
        TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
            let value_ty = args[1].clone();
            let option_ty = TypeExpr::Named("Option".into(), vec![value_ty.clone()]);
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "map_get",
                format!(
                    "fn map_get(key: string) -> {}",
                    format_type_expr(&option_ty)
                ),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "map_insert",
                format!(
                    "fn map_insert(key: string, value: {}) -> ()",
                    format_type_expr(&value_ty)
                ),
            );
        }
        _ => {}
    }
    items
}

fn push_builtin_member(
    items: &mut Vec<CompletionItem>,
    qualifier: Option<&str>,
    prefix: Option<&str>,
    edit_range: &Range,
    name: &str,
    detail: String,
) {
    if !prefix_matches(name, prefix) {
        return;
    }
    let filter_text = qualifier.map(|qual| format!("{qual}.{name}"));
    let new_text = filter_text.clone().unwrap_or_else(|| name.to_string());
    items.push(CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::METHOD),
        detail: Some(detail),
        filter_text,
        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: edit_range.clone(),
            new_text,
        })),
        ..Default::default()
    });
}

fn strip_type_refs<'a>(ty: &'a TypeExpr) -> &'a TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => strip_type_refs(ty),
        _ => ty,
    }
}

fn build_type_subst(params: &[String], args: &[TypeExpr]) -> Option<HashMap<String, TypeExpr>> {
    if params.is_empty() || params.len() != args.len() {
        return None;
    }
    let mut map = HashMap::new();
    for (param, arg) in params.iter().zip(args) {
        map.insert(param.clone(), arg.clone());
    }
    Some(map)
}

pub fn general_completion_items(
    module: &Module,
    offset: Option<usize>,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if let Some(offset) = offset {
        for decl in visible_locals(module, offset) {
            items.push(CompletionItem {
                label: decl.name.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: decl.ty.as_ref().map(|ty| format_type_expr(ty)),
                ..Default::default()
            });
        }
    }

    for item in &module.items {
        match item {
            Item::Function(func) => {
                if func.name == "main" {
                    continue;
                }
                items.push(CompletionItem {
                    label: func.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format_function_signature(func)),
                    ..Default::default()
                })
            }
            Item::Struct(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some(format!(
                    "struct {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                ..Default::default()
            }),
            Item::Enum(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::ENUM),
                detail: Some(format!(
                    "enum {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                ..Default::default()
            }),
            Item::Interface(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::INTERFACE),
                detail: Some(format!(
                    "interface {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                ..Default::default()
            }),
            Item::Const(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::CONSTANT),
                detail: def
                    .ty
                    .as_ref()
                    .map(|ty| format_type_expr(&ty.ty))
                    .or(Some("const".into())),
                ..Default::default()
            }),
            Item::Impl(_) => {}
        }
    }

    items.extend(keyword_completion_items(prefix));

    items
        .into_iter()
        .filter(|item| prefix_matches(&item.label, prefix))
        .collect()
}

pub fn keyword_completion_items(prefix: Option<&str>) -> Vec<CompletionItem> {
    const KEYWORDS: &[&str] = &[
        "fn",
        "let",
        "mut",
        "struct",
        "enum",
        "interface",
        "impl",
        "const",
        "match",
        "if",
        "else",
        "for",
        "while",
        "return",
        "defer",
        "import",
        "break",
        "continue",
    ];
    KEYWORDS
        .iter()
        .filter(|kw| prefix_matches(kw, prefix))
        .map(|kw| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        })
        .collect()
}

pub fn format_function_signature(func: &FunctionDef) -> String {
    let mut signature = String::from("fn ");
    signature.push_str(&func.name);
    signature.push_str(&format_type_params(&func.type_params));
    signature.push('(');
    let params = func
        .params
        .iter()
        .map(format_function_param)
        .collect::<Vec<_>>()
        .join(", ");
    signature.push_str(&params);
    signature.push(')');
    if !func.returns.is_empty() {
        signature.push_str(" -> ");
        if func.returns.len() == 1 {
            signature.push_str(&format_type_expr(&func.returns[0].ty));
        } else {
            let returns = func
                .returns
                .iter()
                .map(|ret| format_type_expr(&ret.ty))
                .collect::<Vec<_>>()
                .join(", ");
            signature.push('(');
            signature.push_str(&returns);
            signature.push(')');
        }
    }
    signature
}

pub fn format_function_param(param: &FunctionParam) -> String {
    if param.name == "self" {
        if let Some(shorthand) = format_self_param(&param.ty.ty) {
            let mut text = String::new();
            if param.mutability.is_mutable() {
                text.push_str("mut ");
            }
            text.push_str(&shorthand);
            return text;
        }
    }
    let mut text = String::new();
    if param.mutability.is_mutable() {
        text.push_str("mut ");
    }
    text.push_str(&param.name);
    text.push_str(": ");
    text.push_str(&format_type_expr(&param.ty.ty));
    text
}

pub fn format_interface_method_signature(
    method: &InterfaceMethod,
    subst: Option<&HashMap<String, TypeExpr>>,
) -> String {
    let mut signature = String::from("fn ");
    signature.push_str(&method.name);
    signature.push('(');
    let params = method
        .params
        .iter()
        .map(|param| {
            let mut cloned = param.clone();
            if let Some(map) = subst {
                cloned.ty = cloned.ty.substitute(map);
            }
            format_function_param(&cloned)
        })
        .collect::<Vec<_>>()
        .join(", ");
    signature.push_str(&params);
    signature.push(')');
    if !method.returns.is_empty() {
        signature.push_str(" -> ");
        if method.returns.len() == 1 {
            if let Some(map) = subst {
                let mut ret = method.returns[0].clone();
                ret.ty = ret.ty.substitute(map);
                signature.push_str(&format_type_expr(&ret.ty));
            } else {
                signature.push_str(&format_type_expr(&method.returns[0].ty));
            }
        } else {
            let returns = method
                .returns
                .iter()
                .map(|ret| {
                    if let Some(map) = subst {
                        let mut cloned = ret.clone();
                        cloned.ty = cloned.ty.substitute(map);
                        format_type_expr(&cloned.ty)
                    } else {
                        format_type_expr(&ret.ty)
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            signature.push('(');
            signature.push_str(&returns);
            signature.push(')');
        }
    }
    signature
}

fn format_self_param(ty: &TypeExpr) -> Option<String> {
    match ty {
        TypeExpr::Reference { mutable, ty } => {
            if matches!(**ty, TypeExpr::SelfType) {
                if *mutable {
                    Some("&mut self".into())
                } else {
                    Some("&self".into())
                }
            } else {
                None
            }
        }
        TypeExpr::SelfType => Some("self".into()),
        _ => None,
    }
}

pub fn format_type_expr(expr: &TypeExpr) -> String {
    match expr {
        TypeExpr::Named(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let params = args
                    .iter()
                    .map(format_type_expr)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}[{params}]")
            }
        }
        TypeExpr::Slice(inner) => format!("[]{}", format_type_expr(inner)),
        TypeExpr::Array { size, ty } => format!("[{}]{}", size, format_type_expr(ty)),
        TypeExpr::Reference { mutable, ty } => {
            if *mutable {
                format!("&mut {}", format_type_expr(ty))
            } else {
                format!("&{}", format_type_expr(ty))
            }
        }
        TypeExpr::Pointer { mutable, ty } => {
            if *mutable {
                format!("*mut {}", format_type_expr(ty))
            } else {
                format!("*{}", format_type_expr(ty))
            }
        }
        TypeExpr::Tuple(types) => {
            let inner = types
                .iter()
                .map(format_type_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        TypeExpr::Unit => "()".into(),
        TypeExpr::SelfType => "Self".into(),
    }
}

pub fn format_type_params(params: &[String]) -> String {
    if params.is_empty() {
        String::new()
    } else {
        format!("[{}]", params.join(", "))
    }
}

pub fn format_type_arguments(args: &[TypeExpr]) -> String {
    if args.is_empty() {
        String::new()
    } else {
        let rendered = args
            .iter()
            .map(format_type_expr)
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{}]", rendered)
    }
}

pub fn expression_chain_before_dot(text: &str, offset: usize) -> Option<Vec<String>> {
    if offset == 0 {
        return None;
    }
    let bytes = text.as_bytes();
    let mut idx = skip_ws_back(text, offset);
    if idx > 0 && bytes[idx - 1].is_ascii_alphanumeric() {
        idx = skip_identifier(text, idx);
        idx = skip_ws_back(text, idx);
    }
    if idx == 0 || bytes[idx - 1] != b'.' {
        return None;
    }
    idx -= 1;
    collect_chain_segments(text, idx)
}

pub fn chain_for_field_token(text: &str, span: Span) -> Option<Vec<String>> {
    if span.start >= span.end || span.end > text.len() {
        return None;
    }
    let current = text.get(span.start..span.end)?.to_string();
    let bytes = text.as_bytes();
    let mut idx = skip_ws_back(text, span.start);
    if idx == 0 || bytes[idx - 1] != b'.' {
        return None;
    }
    idx -= 1;
    let mut segments = collect_chain_segments(text, idx)?;
    segments.push(current);
    if segments.len() < 2 {
        return None;
    }
    Some(segments)
}

fn skip_ws_back(text: &str, mut idx: usize) -> usize {
    let bytes = text.as_bytes();
    while idx > 0 {
        if bytes[idx - 1].is_ascii_whitespace() {
            idx -= 1;
        } else {
            break;
        }
    }
    idx
}

fn skip_identifier(text: &str, mut idx: usize) -> usize {
    let bytes = text.as_bytes();
    while idx > 0 {
        let ch = bytes[idx - 1];
        if is_ident_char(ch) {
            idx -= 1;
        } else {
            break;
        }
    }
    idx
}

fn collect_chain_segments(text: &str, mut idx: usize) -> Option<Vec<String>> {
    let bytes = text.as_bytes();
    let mut segments = Vec::new();
    loop {
        idx = skip_ws_back(text, idx);
        if idx == 0 {
            break;
        }
        let end = idx;
        let mut start = end;
        while start > 0 {
            let ch = bytes[start - 1];
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                start -= 1;
            } else {
                break;
            }
        }
        if start == end {
            return None;
        }
        segments.push(text[start..end].to_string());
        idx = start;
        idx = skip_ws_back(text, idx);
        if idx == 0 || bytes[idx - 1] != b'.' {
            break;
        }
        idx -= 1;
    }
    if segments.is_empty() {
        None
    } else {
        segments.reverse();
        Some(segments)
    }
}

fn resolve_chain_from_root<'a>(
    chain: &[String],
    root: TypeExpr,
    structs: &'a HashMap<String, StructInfo>,
) -> Option<(TypeExpr, Option<(String, &'a StructFieldInfo)>)> {
    if chain.is_empty() {
        return None;
    }
    let mut current = root;
    let mut last_field = None;
    for segment in chain.iter().skip(1) {
        let struct_name = struct_name_from_type(&current)?.to_string();
        let info = structs.get(&struct_name)?;
        let field = info.fields.iter().find(|f| f.name == *segment)?;
        current = field.ty.clone();
        last_field = Some((struct_name, field));
    }
    Some((current, last_field))
}

pub fn resolve_chain_from_scope<'a>(
    chain: &[String],
    module: &Module,
    structs: &'a HashMap<String, StructInfo>,
    offset: usize,
) -> Option<(TypeExpr, Option<(String, &'a StructFieldInfo)>)> {
    let root = chain.first()?;
    let ty = identifier_type_from_scope(module, structs, root, offset)?;
    resolve_chain_from_root(chain, ty, structs)
}

fn identifier_type_from_scope(
    module: &Module,
    structs: &HashMap<String, StructInfo>,
    name: &str,
    offset: usize,
) -> Option<TypeExpr> {
    if let Some(decl) = find_local_decl(module, name, offset) {
        if let Some(ty) = decl.ty {
            return Some(ty);
        }
    }
    if structs.contains_key(name) {
        return Some(TypeExpr::Named(name.to_string(), Vec::new()));
    }
    None
}

fn struct_name_from_type<'a>(ty: &'a TypeExpr) -> Option<&'a str> {
    match ty {
        TypeExpr::Named(name, _) => Some(name),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => struct_name_from_type(ty),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::project::manifest::PackageManifest;
    use std::fs;
    use tempfile::tempdir;
    use tower_lsp_server::lsp_types::CompletionTriggerKind;

    struct ManifestEntry<'a> {
        name: &'a str,
        path: &'a str,
        visibility: &'a str,
        package: Option<&'a str>,
        doc: Option<&'a str>,
    }

    fn manifest_with_entries(
        entries: &[ManifestEntry<'_>],
    ) -> (tempfile::TempDir, PackageManifest) {
        let dir = tempdir().expect("tempdir");
        let manifest_path = dir.path().join("prime.toml");
        let mut manifest = String::from(
            r#"manifest_version = "2"

[package]
name = "demo"
version = "0.1.0"
kind = "binary"
entry = "demo::main"
"#,
        );
        for entry in entries {
            let module_path = dir.path().join(entry.path);
            if let Some(parent) = module_path.parent() {
                fs::create_dir_all(parent).expect("create module dir");
            }
            fs::write(&module_path, "module demo;").expect("write module");
            manifest.push_str("\n[[modules]]\n");
            manifest.push_str(&format!("name = \"{}\"\n", entry.name));
            manifest.push_str(&format!("path = \"{}\"\n", entry.path));
            manifest.push_str(&format!("visibility = \"{}\"\n", entry.visibility));
            if let Some(package) = entry.package {
                manifest.push_str(&format!("package = \"{}\"\n", package));
            }
            if let Some(doc) = entry.doc {
                manifest.push_str(&format!("doc = \"{}\"\n", doc));
            }
        }
        fs::write(&manifest_path, manifest).expect("write manifest");
        let manifest = PackageManifest::load(&manifest_path).expect("load manifest");
        (dir, manifest)
    }

    #[test]
    fn detects_module_context_in_declarations() {
        let text = "module demo::core;";
        let ctx = module_path_completion_context(text, text.len()).expect("context");
        assert!(matches!(ctx.kind, ModulePathCompletionKind::Declaration));
        assert_eq!(ctx.prefix.as_deref(), Some("demo::core"));
    }

    #[test]
    fn module_completion_prioritizes_expected_entry() {
        let entries = [
            ManifestEntry {
                name: "app::main",
                path: "src/main.prime",
                visibility: "pub",
                package: Some("app"),
                doc: Some("app entry"),
            },
            ManifestEntry {
                name: "lib::math",
                path: "lib/math.prime",
                visibility: "package",
                package: None,
                doc: None,
            },
        ];
        let (_dir, manifest) = manifest_with_entries(&entries);
        let prioritized =
            module_completion_items_from_manifest(&manifest, Some("app"), Some("app::main"));
        assert!(!prioritized.is_empty());
        assert_eq!(prioritized[0].label, "app::main");

        let all_items = module_completion_items_from_manifest(&manifest, None, None);
        let lib = all_items
            .iter()
            .find(|item| item.label == "lib::math")
            .expect("lib completion item");
        let detail = lib.detail.as_ref().expect("detail");
        assert!(detail.contains("[package]"));
    }

    #[test]
    fn completion_prefix_uses_trigger_when_identifier_missing() {
        let ctx = CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some("a".into()),
        };
        let prefix = completion_prefix("a", 0, Some(&ctx)).expect("prefix");
        assert_eq!(prefix, "a");
    }
}
