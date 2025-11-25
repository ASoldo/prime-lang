use crate::{
    language::{
        ast::{
            FunctionDef, FunctionParam, Import, InterfaceMethod, Item, MacroDef, Module, Visibility,
        },
        span::Span,
        types::TypeExpr,
    },
    project::manifest::{ModuleInfo, ModuleVisibility, PackageManifest},
};
use std::collections::{HashMap, HashSet};
use tower_lsp_server::lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionTextEdit, Range, TextEdit,
};

const BUILTIN_TYPES: &[&str] = &[
    // Integers
    "int8", "int16", "int32", "int64", "isize", "uint8", "uint16", "uint32", "uint64", "usize",
    // Floats
    "float32", "float64",
    // Other primitives
    "bool", "string", "rune",
    // Containers / std types
    "Option", "Result", "Range", "Box", "Map", "Slice", "JoinHandle", "Sender", "Receiver",
];

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

fn macro_visible_to_requester(def: &MacroDef, defining_module: &Module, requester: &Module) -> bool {
    match def.visibility {
        Visibility::Public => true,
        Visibility::Package => match (requester.path.parent(), defining_module.path.parent()) {
            (Some(requester_dir), Some(def_dir)) => requester_dir.starts_with(def_dir),
            _ => false,
        },
        Visibility::Private => requester.path == defining_module.path,
    }
}

fn format_macro_detail(def: &MacroDef) -> String {
    let params = def
        .params
        .iter()
        .map(|p| match &p.ty {
            Some(ty) => format!("{}: {}", p.name, format_type_expr(&ty.ty)),
            None => p.name.clone(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    let ret = def
        .return_ty
        .as_ref()
        .map(|ty| format!(" -> {}", format_type_expr(&ty.ty)))
        .unwrap_or_default();
    let vis = match def.visibility {
        Visibility::Public => "pub",
        Visibility::Package => "pub(package)",
        Visibility::Private => "private",
    };
    format!("{} macro ~{}({}){}", vis, def.name, params, ret)
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
    // Trigger on identifiers, qualification separators, macro sigils, and hygiene escapes.
    // `:` is kept so the LSP can fire on the second colon in `::`.
    const TRIGGER_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_:.~@";
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
        .filter(|ch| is_ident_string(ch) || ch.as_str() == "~");

    let mut base = match (base, trigger) {
        (Some(mut prefix), Some(trigger)) => {
            if !prefix.ends_with(trigger) {
                prefix.push_str(trigger);
            }
            Some(prefix)
        }
        (Some(prefix), None) => Some(prefix),
        (None, Some(trigger)) => Some(trigger.to_string()),
        _ => None,
    };

    // If the cursor is immediately after a '~' or '@', treat it as part of the prefix so macro
    // completions (labels like "~foo" or hygiene escapes) match.
    if let Some(ref prefix) = base {
        let start = offset.saturating_sub(prefix.len());
        if start > 0 {
            let prev = text.as_bytes().get(start - 1).copied();
            if prev == Some(b'~') && !prefix.starts_with('~') {
                base = Some(format!("~{}", prefix));
            } else if prev == Some(b'@') && !prefix.starts_with('@') {
                base = Some(format!("@{}", prefix));
            }
        }
    } else if offset > 0 {
        let prev = text.as_bytes().get(offset - 1).copied();
        if prev == Some(b'~') {
            base = Some("~".to_string());
        } else if prev == Some(b'@') {
            base = Some("@".to_string());
        }
    }

    base
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

#[derive(Default, Clone)]
struct RawStructInfo {
    fields: Vec<StructFieldInfo>,
    embedded: Vec<String>,
    methods: Vec<MethodInfo>,
}

#[derive(Clone)]
struct RawStructEntry {
    module_name: String,
    info: RawStructInfo,
}

pub fn select_struct_info<'a>(
    structs: &'a StructInfoMap,
    name: &str,
    module_name: Option<&str>,
) -> Option<&'a StructInfo> {
    let list = structs.get(name)?;
    if let Some(module_name) = module_name {
        if let Some(info) = list.iter().find(|info| info.module_name == module_name) {
            return Some(info);
        }
    }
    list.first()
}

pub fn collect_struct_info(modules: &[Module]) -> StructInfoMap {
    let mut raw: HashMap<String, Vec<RawStructEntry>> = HashMap::new();
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
                raw.entry(def.name.clone())
                    .or_default()
                    .push(RawStructEntry {
                        module_name: module.name.clone(),
                        info: RawStructInfo {
                            fields: struct_fields,
                            embedded,
                            methods: Vec::new(),
                        },
                    });
            }
        }
    }
    for module in modules {
        for item in &module.items {
            if let Item::Function(func) = item {
                if let Some(first_param) = func.params.first() {
                    if let Some(receiver) = receiver_type_name(&first_param.ty.ty) {
                        if let Some(entry) =
                            select_raw_struct_entry_mut(&mut raw, &receiver, &module.name)
                        {
                            entry.info.methods.push(MethodInfo {
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
                                if let Some(entry) =
                                    select_raw_struct_entry_mut(&mut raw, &receiver, &module.name)
                                {
                                    entry.info.methods.push(MethodInfo {
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
    let mut info: StructInfoMap = HashMap::new();
    let mut fields_cache: HashMap<(String, String), Vec<StructFieldInfo>> = HashMap::new();
    let mut methods_cache: HashMap<(String, String), Vec<MethodInfo>> = HashMap::new();
    let struct_names: Vec<String> = raw.keys().cloned().collect();
    for name in struct_names {
        if let Some(entries) = raw.get(&name) {
            for entry in entries {
                let mut field_stack = HashSet::new();
                let fields = flatten_struct_fields(
                    &name,
                    &entry.module_name,
                    &raw,
                    &mut fields_cache,
                    &mut field_stack,
                );
                let mut method_stack = HashSet::new();
                let methods = flatten_struct_methods(
                    &name,
                    &entry.module_name,
                    &raw,
                    &mut methods_cache,
                    &mut method_stack,
                );
                info.entry(name.clone()).or_default().push(StructInfo {
                    module_name: entry.module_name.clone(),
                    fields,
                    methods,
                });
            }
        }
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

fn select_raw_struct_entry<'a>(
    raw: &'a HashMap<String, Vec<RawStructEntry>>,
    name: &str,
    module_name: &str,
) -> Option<&'a RawStructEntry> {
    raw.get(name).and_then(|entries| {
        entries
            .iter()
            .find(|entry| entry.module_name == module_name)
            .or_else(|| entries.first())
    })
}

fn select_raw_struct_entry_mut<'a>(
    raw: &'a mut HashMap<String, Vec<RawStructEntry>>,
    name: &str,
    module_name: &str,
) -> Option<&'a mut RawStructEntry> {
    let entries = raw.get_mut(name)?;
    if entries.len() == 1 {
        return entries.first_mut();
    }
    let idx = entries
        .iter()
        .position(|entry| entry.module_name == module_name)
        .unwrap_or(0);
    entries.get_mut(idx)
}

fn flatten_struct_fields(
    name: &str,
    module_name: &str,
    raw: &HashMap<String, Vec<RawStructEntry>>,
    cache: &mut HashMap<(String, String), Vec<StructFieldInfo>>,
    stack: &mut HashSet<(String, String)>,
) -> Vec<StructFieldInfo> {
    let key = (module_name.to_string(), name.to_string());
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if !stack.insert(key.clone()) {
        return Vec::new();
    }
    let mut fields = Vec::new();
    if let Some(entry) = select_raw_struct_entry(raw, name, module_name) {
        fields.extend(entry.info.fields.clone());
        for embedded in &entry.info.embedded {
            if let Some(next_entry) = select_raw_struct_entry(raw, embedded, module_name) {
                fields.extend(flatten_struct_fields(
                    embedded,
                    &next_entry.module_name,
                    raw,
                    cache,
                    stack,
                ));
            }
        }
    }
    stack.remove(&key);
    cache.insert(key, fields.clone());
    fields
}

fn flatten_struct_methods(
    name: &str,
    module_name: &str,
    raw: &HashMap<String, Vec<RawStructEntry>>,
    cache: &mut HashMap<(String, String), Vec<MethodInfo>>,
    stack: &mut HashSet<(String, String)>,
) -> Vec<MethodInfo> {
    let key = (module_name.to_string(), name.to_string());
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if !stack.insert(key.clone()) {
        return Vec::new();
    }
    let mut methods = Vec::new();
    if let Some(entry) = select_raw_struct_entry(raw, name, module_name) {
        methods.extend(entry.info.methods.clone());
        for embedded in &entry.info.embedded {
            if let Some(next_entry) = select_raw_struct_entry(raw, embedded, module_name) {
                methods.extend(flatten_struct_methods(
                    embedded,
                    &next_entry.module_name,
                    raw,
                    cache,
                    stack,
                ));
            }
        }
    }
    stack.remove(&key);
    cache.insert(key, methods.clone());
    methods
}

pub fn member_completion_items(
    text: &str,
    chain: &[String],
    struct_info: &StructInfoMap,
    interfaces: &HashMap<String, Vec<InterfaceInfo>>,
    prefix: Option<&str>,
    module: &Module,
    offset: usize,
) -> Option<Vec<CompletionItem>> {
    let ChainResolution {
        ty: target_type,
        module_name: target_module,
        ..
    } = resolve_chain_from_scope(chain, module, struct_info, offset)?;
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
        let module_hint = target_module.as_deref().or(Some(module.name.as_str()));
        if let Some(info) = select_struct_info(struct_info, &name, module_hint) {
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
                "len",
                "fn len() -> int32".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "get",
                format!("fn get(index: int32) -> {}", format_type_expr(&option_ty)),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "push",
                format!("fn push(value: {}) -> ()", format_type_expr(&element_ty)),
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
                "get",
                format!("fn get(key: string) -> {}", format_type_expr(&option_ty)),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "insert",
                format!(
                    "fn insert(key: string, value: {}) -> ()",
                    format_type_expr(&value_ty)
                ),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "len",
                "fn len() -> int32".into(),
            );
        }
        TypeExpr::Named(name, _) if name == "string" => {
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "str_len",
                "fn str_len() -> int32".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "str_contains",
                "fn str_contains(needle: string) -> bool".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "str_trim",
                "fn str_trim() -> string".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "str_split",
                "fn str_split(delim: string) -> []string".into(),
            );
        }
        TypeExpr::Named(name, _) if name.starts_with("int") || name.starts_with("float") => {
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "abs",
                "fn abs()".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "min",
                "fn min(other)".into(),
            );
            push_builtin_member(
                &mut items,
                qualifier,
                prefix,
                edit_range,
                "max",
                "fn max(other)".into(),
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

pub fn enum_variant_completion_items(
    text: &str,
    offset: usize,
    _current_module: Option<&Module>,
    modules: &[Module],
) -> Option<Vec<CompletionItem>> {
    if offset == 0 || offset > text.len() {
        return None;
    }
    let slice = &text[..offset];
    // Require `Enum::` qualifiers for variant completions to avoid noisy suggestions.
    let sep = slice.rfind("::")?;
    // Walk backward from the separator to find the start of the enum identifier (letters, digits, underscore, dot).
    let mut start = sep;
    while start > 0 {
        let ch = slice.chars().nth(start.saturating_sub(1)).unwrap();
        if ch.is_ascii_alphanumeric() || ch == '_' || ch == '.' {
                start -= 1;
            } else {
                break;
            }
        }
        let enum_path = slice[start..sep].trim_end_matches('.');
        if enum_path.is_empty() {
            return None;
        }
    let enum_token = enum_path
        .rsplit('.')
        .next()
        .filter(|name| !name.is_empty())?;
    // Allow `binding::Variant` when binding has an enum type in scope.
    let enum_name = if modules.iter().any(|m| {
        m.items
            .iter()
            .any(|item| matches!(item, Item::Enum(def) if def.name == enum_token))
    }) {
        enum_token.to_string()
    } else if let Some(module) = _current_module {
        find_local_decl(module, enum_token, offset)
            .and_then(|decl| decl.ty)
            .and_then(|ty| match ty {
                TypeExpr::Named(name, _) => Some(name),
                _ => None,
            })?
    } else {
        return None;
    };
    let variant_prefix = slice.get(sep + 2..offset).unwrap_or("");
    let edit_start = sep + 2;

    let edit_range = Range {
        start: offset_to_position(text, edit_start),
        end: offset_to_position(text, offset),
    };
    let mut items = Vec::new();
    for module in modules {
        for item in &module.items {
            if let Item::Enum(def) = item {
                if def.name != enum_name {
                    continue;
                }
                for variant in &def.variants {
                    if !prefix_matches(&variant.name, Some(variant_prefix)) {
                        continue;
                    }
                    items.push(CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{} variant", def.name)),
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text: variant.name.clone(),
                        })),
                        ..Default::default()
                    });
                }
            }
        }
    }
    if items.is_empty() { None } else { Some(items) }
}

pub fn general_completion_items(
    module: &Module,
    all_modules: &[Module],
    offset: Option<usize>,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mut seen = std::collections::HashSet::new();
    let macro_ctx = offset.map(|o| is_macro_context(module, o)).unwrap_or(false);
    let inside_function = offset
        .and_then(|o| is_inside_function(module, o))
        .unwrap_or(false);
    if let Some(offset) = offset {
        for decl in visible_locals(module, offset) {
            if !seen.insert(decl.name.clone()) {
                continue;
            }
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
                if !seen.insert(func.name.clone()) {
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
            Item::Enum(def) => {
                if !seen.insert(def.name.clone()) {
                    continue;
                }
                items.push(CompletionItem {
                    label: def.name.clone(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail: Some(format!(
                        "enum {}{}",
                        def.name,
                        format_type_params(&def.type_params)
                    )),
                    ..Default::default()
                })
            }
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
            Item::Macro(def) => {
                let label = format!("~{}", def.name);
                if !seen.insert(label.clone()) {
                    continue;
                }
                items.push(CompletionItem {
                    label,
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format_macro_detail(def)),
                    ..Default::default()
                });
            }
            Item::Impl(_) => {}
            Item::MacroInvocation(_) => {}
        }
    }

    for import in &module.imports {
        if let Some(imported) = import_module_from_snapshot(all_modules, import) {
            for item in &imported.items {
                match item {
                    Item::Function(func) => {
                        if func.visibility != Visibility::Public || func.name == "main" {
                            continue;
                        }
                        if !seen.insert(func.name.clone()) {
                            continue;
                        }
                        items.push(CompletionItem {
                            label: func.name.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(format_function_signature(func)),
                            ..Default::default()
                        });
                    }
                    Item::Struct(def) => {
                        if def.visibility != Visibility::Public || !seen.insert(def.name.clone()) {
                            continue;
                        }
                        items.push(CompletionItem {
                            label: def.name.clone(),
                            kind: Some(CompletionItemKind::STRUCT),
                            detail: Some(format!(
                                "struct {}{}",
                                def.name,
                                format_type_params(&def.type_params)
                            )),
                            ..Default::default()
                        });
                    }
                    Item::Enum(def) => {
                        if def.visibility != Visibility::Public || !seen.insert(def.name.clone()) {
                            continue;
                        }
                        items.push(CompletionItem {
                            label: def.name.clone(),
                            kind: Some(CompletionItemKind::ENUM),
                            detail: Some(format!(
                                "enum {}{}",
                                def.name,
                                format_type_params(&def.type_params)
                            )),
                            ..Default::default()
                        });
                    }
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
                    Item::Macro(def) => {
                        if !macro_visible_to_requester(def, imported, module) {
                            continue;
                        }
                        let label = format!("~{}", def.name);
                        if !seen.insert(label.clone()) {
                            continue;
                        }
                        items.push(CompletionItem {
                            label,
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(format_macro_detail(def)),
                            ..Default::default()
                        });
                    }
                    Item::Impl(_) => {}
                    Item::MacroInvocation(_) => {}
                }
            }
        }
    }

    items.extend(keyword_completion_items(prefix, macro_ctx, inside_function));

    items
        .into_iter()
        .filter(|item| prefix_matches(&item.label, prefix))
        .collect()
}

fn import_module_from_snapshot<'a>(
    all_modules: &'a [Module],
    import: &Import,
) -> Option<&'a Module> {
    let name = import.path.to_string();
    all_modules.iter().find(|m| m.name == name)
}

pub fn keyword_completion_items(
    prefix: Option<&str>,
    macro_ctx: bool,
    inside_function: bool,
) -> Vec<CompletionItem> {
    const KEYWORDS: &[&str] = &[
        "fn",
        "macro",
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
    let mut items: Vec<CompletionItem> = KEYWORDS
        .iter()
        .filter(|kw| {
            if inside_function && **kw == "macro" {
                return false;
            }
            prefix_matches(kw, prefix)
        })
        .map(|kw| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        })
        .collect();

    const BUILTIN_FUNCS: &[(&str, &str)] = &[
        ("assert", "Built-in test helper"),
        ("expect", "Built-in test helper"),
        ("out", "Built-in output helper"),
        ("channel", "Built-in concurrency helper"),
        ("send", "Built-in concurrency helper"),
        ("recv", "Built-in concurrency helper"),
        ("close", "Built-in concurrency helper"),
        ("join", "Built-in concurrency helper"),
        ("ptr", "Built-in pointer helper"),
        ("ptr_mut", "Built-in pointer helper"),
        ("box_new", "Built-in heap helper"),
        ("box_get", "Built-in heap helper"),
        ("box_set", "Built-in heap helper"),
        ("box_take", "Built-in heap helper"),
        ("slice_new", "Built-in slice helper"),
        ("slice_push", "Built-in slice helper"),
        ("slice_len", "Built-in slice helper"),
        ("slice_get", "Built-in slice helper"),
        ("map_new", "Built-in map helper"),
        ("map_insert", "Built-in map helper"),
        ("map_get", "Built-in map helper"),
        ("str_len", "Built-in string helper"),
        ("str_contains", "Built-in string helper"),
        ("str_trim", "Built-in string helper"),
        ("str_split", "Built-in string helper"),
        ("min", "Built-in math helper"),
        ("max", "Built-in math helper"),
        ("abs", "Built-in math helper"),
    ];
    for (label, detail) in BUILTIN_FUNCS
        .iter()
        .filter(|(name, _)| prefix_matches(name, prefix))
    {
        items.push(CompletionItem {
            label: (*label).to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some((*detail).to_string()),
            ..Default::default()
        });
    }

    // Built-in/primitive types
    for ty in BUILTIN_TYPES.iter().filter(|ty| prefix_matches(ty, prefix)) {
        items.push(CompletionItem {
            label: ty.to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            ..Default::default()
        });
    }

    if macro_ctx {
        const MACRO_HELPERS: &[(&str, &str)] = &[
            ("expr", "macro param kind"),
            ("block", "macro param kind"),
            ("pattern", "macro param kind"),
            ("tokens", "macro param kind"),
        ("repeat", "macro param kind (supports +/*, use with @sep)"),
            ("@sep", "macro repeat separator hint (use `@sep = ,` or `@sep = ;`)"),
            ("@", "hygiene escape for outer bindings"),
        ];
        for (label, detail) in MACRO_HELPERS
            .iter()
            .filter(|(name, _)| prefix_matches(name, prefix))
        {
            items.push(CompletionItem {
                label: (*label).to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some((*detail).to_string()),
                ..Default::default()
            });
        }
    }

    items
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

fn is_macro_context(module: &Module, offset: usize) -> bool {
    fn contains(span: Span, offset: usize) -> bool {
        offset >= span.start && offset < span.end
    }
    for item in &module.items {
        match item {
            Item::Macro(def) if contains(def.span, offset) => return true,
            Item::MacroInvocation(inv) if contains(inv.span, offset) => return true,
            _ => {}
        }
    }
    false
}

fn is_inside_function(module: &Module, offset: usize) -> Option<bool> {
    fn contains(span: Span, offset: usize) -> bool {
        offset >= span.start && offset < span.end
    }
    for item in &module.items {
        if let Item::Function(func) = item {
            let body_span = match &func.body {
                crate::language::ast::FunctionBody::Block(block) => block.span,
                crate::language::ast::FunctionBody::Expr(expr) => expr.span,
            };
            if contains(body_span, offset) {
                return Some(true);
            }
        }
    }
    Some(false)
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
            let ch = bytes.get(end.wrapping_sub(1)).copied();
            if ch == Some(b'`') || ch == Some(b'"') {
                let quote = ch.unwrap();
                start = end.saturating_sub(1);
                while start > 0 {
                    if bytes[start - 1] == quote && bytes.get(start - 2) != Some(&b'\\') {
                        start -= 1;
                        break;
                    }
                    start -= 1;
                }
                if bytes.get(start) != Some(&quote) {
                    return None;
                }
            } else {
                return None;
            }
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
    structs: &'a StructInfoMap,
    preferred_module: Option<&str>,
) -> Option<ChainResolution<'a>> {
    if chain.is_empty() {
        return None;
    }
    let mut current = root;
    let mut last_field = None;
    let mut module_hint = preferred_module.map(|name| name.to_string());
    for segment in chain.iter().skip(1) {
        let struct_name = struct_name_from_type(&current)?.to_string();
        let info = select_struct_info(structs, &struct_name, module_hint.as_deref())?;
        let field = info.fields.iter().find(|f| f.name == *segment)?;
        current = field.ty.clone();
        last_field = Some((struct_name, field));
        module_hint = Some(info.module_name.clone());
    }
    let resolved_module = struct_name_from_type(&current)
        .and_then(|name| select_struct_info(structs, name, module_hint.as_deref()))
        .map(|info| info.module_name.clone())
        .or(module_hint);
    Some(ChainResolution {
        ty: current,
        last_field,
        module_name: resolved_module,
    })
}

pub fn resolve_chain_from_scope<'a>(
    chain: &[String],
    module: &Module,
    structs: &'a StructInfoMap,
    offset: usize,
) -> Option<ChainResolution<'a>> {
    let root = chain.first()?;
    let ty = identifier_type_from_scope(module, structs, root, offset)?;
    resolve_chain_from_root(chain, ty, structs, Some(module.name.as_str()))
}

fn identifier_type_from_scope(
    module: &Module,
    structs: &StructInfoMap,
    name: &str,
    offset: usize,
) -> Option<TypeExpr> {
    if let Some(literal_ty) = literal_type(name) {
        return Some(literal_ty);
    }
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

fn literal_type(name: &str) -> Option<TypeExpr> {
    if (name.starts_with('`') && name.ends_with('`'))
        || (name.starts_with('"') && name.ends_with('"'))
    {
        return Some(TypeExpr::named("string"));
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
    use crate::language::parser::parse_module;
    use crate::project::manifest::PackageManifest;
    use std::fs;
    use std::path::PathBuf;
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

    #[test]
    fn member_completion_uses_destructured_tuple_types() {
        let source = r#"
module demo::lab;

struct Result {
  normalized: int32;
}

fn make() -> (bool, Result) {
  return true, Result{ normalized: 3 };
}

fn main() {
  let (ok, res) = make();
  res.normalized;
}
"#;
        let module =
            parse_module("demo::lab", PathBuf::from("lab.prime"), source).expect("parse module");
        let structs = collect_struct_info(&[module.clone()]);
        let interfaces = collect_interface_info(&[module.clone()]);
        let offset = source
            .find("res.normalized")
            .map(|idx| idx + "res.".len())
            .expect("find member access");
        let chain =
            expression_chain_before_dot(source, offset).expect("expression chain before dot");
        let items =
            member_completion_items(source, &chain, &structs, &interfaces, None, &module, offset)
                .expect("completion items for res");
        assert!(
            items.iter().any(|item| item.label == "normalized"),
            "expected normalized field in completion items, got {:?}",
            items
                .iter()
                .map(|item| item.label.clone())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn macro_completion_respects_visibility() {
        let provider_src = r#"
module pkg::macros;

macro local_only() -> int32 { 1 }
pub(package) macro pkg_only() -> int32 { 2 }
pub macro exported() -> int32 { 3 }
"#;
        let same_pkg_src = r#"
module pkg::user;

import pkg::macros;

fn main() -> int32 { 0 }
"#;
        let other_pkg_src = r#"
module other::user;

import pkg::macros;

fn main() -> int32 { 0 }
"#;

        let provider =
            parse_module("pkg::macros", PathBuf::from("pkg/macros.prime"), provider_src)
                .expect("provider module");
        let same_pkg =
            parse_module("pkg::user", PathBuf::from("pkg/user.prime"), same_pkg_src)
                .expect("same package module");
        let other_pkg =
            parse_module("other::user", PathBuf::from("other/user.prime"), other_pkg_src)
                .expect("other package module");
        let modules = vec![provider.clone(), same_pkg.clone(), other_pkg.clone()];

        let same_pkg_items = general_completion_items(&same_pkg, &modules, None, None);
        assert!(
            same_pkg_items.iter().any(|item| item.label == "~pkg_only"),
            "expected package macro visible inside the package"
        );
        assert!(same_pkg_items.iter().any(|item| item.label == "~exported"));
        assert!(
            !same_pkg_items
                .iter()
                .any(|item| item.label == "~local_only"),
            "private macro should stay local to the defining module"
        );

        let other_pkg_items = general_completion_items(&other_pkg, &modules, None, None);
        assert!(other_pkg_items.iter().any(|item| item.label == "~exported"));
        assert!(
            !other_pkg_items.iter().any(|item| item.label == "~pkg_only"),
            "package macro should be hidden outside the package"
        );
        assert!(
            !other_pkg_items
                .iter()
                .any(|item| item.label == "~local_only"),
            "private macro should be hidden outside the defining module"
        );

        let pkg_only_detail = same_pkg_items
            .iter()
            .find(|item| item.label == "~pkg_only")
            .and_then(|item| item.detail.as_ref())
            .expect("detail for pkg_only macro");
        assert!(
            pkg_only_detail.contains("pub(package) macro"),
            "expected macro visibility in completion detail, got {pkg_only_detail}"
        );
    }
}
