use super::*;

const BUILTIN_TYPES: &[&str] = &[
    // Integers
    "int8",
    "int16",
    "int32",
    "int64",
    "isize",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "usize",
    // Floats
    "float32",
    "float64",
    // Other primitives
    "bool",
    "string",
    "rune",
    // Containers / std types
    "Option",
    "Result",
    "Range",
    "Box",
    "Map",
    "Slice",
    "JoinHandle",
    "Sender",
    "Receiver",
];

fn macro_visible_to_requester(
    def: &MacroDef,
    defining_module: &Module,
    requester: &Module,
) -> bool {
    match def.visibility {
        Visibility::Public => true,
        Visibility::Package => match (requester.path.parent(), defining_module.path.parent()) {
            (Some(requester_dir), Some(def_dir)) => requester_dir.starts_with(def_dir),
            _ => false,
        },
        Visibility::Private => requester.path == defining_module.path,
    }
}

fn builtin_completion_items(prefix: Option<&str>, target: &BuildTarget) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if target.is_embedded() {
        let candidates = [
            ("pin_mode", "fn pin_mode(pin: int32, mode: int32) -> ()"),
            (
                "digital_write",
                "fn digital_write(pin: int32, level: int32) -> ()",
            ),
            ("digital_read", "fn digital_read(pin: int32) -> int32"),
            ("delay_ms", "fn delay_ms(ms: int32) -> ()"),
            ("reset_reason", "fn reset_reason() -> int32"),
        ];
        for (name, detail) in candidates {
            if let Some(pref) = prefix {
                if !name.starts_with(pref) {
                    continue;
                }
            }
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail.to_string()),
                ..Default::default()
            });
        }
    }
    items
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
                            range: edit_range,
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
                            range: edit_range,
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
                            range: edit_range,
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
            range: *edit_range,
            new_text,
        })),
        ..Default::default()
    });
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
                            range: edit_range,
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
    target: &BuildTarget,
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
                detail: decl.ty.as_ref().map(format_type_expr),
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
            Item::Comment { .. } => {}
        }
    }

    items.extend(builtin_completion_items(prefix, target));

    for import in &module.imports {
        if let Some(imported) = import_module_from_snapshot(all_modules, import) {
            let allowed = import_allowed_names(import, imported);

            for item in &imported.items {
                let (label, kind, detail, public) = match item {
                    Item::Function(func) => (
                        func.name.clone(),
                        CompletionItemKind::FUNCTION,
                        Some(format_function_signature(func)),
                        func.visibility == Visibility::Public && func.name != "main",
                    ),
                    Item::Struct(def) => (
                        def.name.clone(),
                        CompletionItemKind::STRUCT,
                        Some(format!(
                            "struct {}{}",
                            def.name,
                            format_type_params(&def.type_params)
                        )),
                        def.visibility == Visibility::Public,
                    ),
                    Item::Enum(def) => (
                        def.name.clone(),
                        CompletionItemKind::ENUM,
                        Some(format!(
                            "enum {}{}",
                            def.name,
                            format_type_params(&def.type_params)
                        )),
                        def.visibility == Visibility::Public,
                    ),
                    Item::Interface(def) => (
                        def.name.clone(),
                        CompletionItemKind::INTERFACE,
                        Some(format!(
                            "interface {}{}",
                            def.name,
                            format_type_params(&def.type_params)
                        )),
                        def.visibility == Visibility::Public,
                    ),
                    Item::Const(def) => (
                        def.name.clone(),
                        CompletionItemKind::CONSTANT,
                        def.ty
                            .as_ref()
                            .map(|ty| format_type_expr(&ty.ty))
                            .or(Some("const".into())),
                        def.visibility == Visibility::Public,
                    ),
                    Item::Macro(def) => (
                        format!("~{}", def.name),
                        CompletionItemKind::FUNCTION,
                        Some(format_macro_detail(def)),
                        def.visibility != Visibility::Private
                            && macro_visible_to_requester(def, imported, module),
                    ),
                    Item::Impl(_) | Item::MacroInvocation(_) => continue,
                    Item::Comment { .. } => continue,
                };

                if !public {
                    continue;
                }
                if let Some(allowed) = &allowed {
                    if !allowed.contains(&label) {
                        continue;
                    }
                }
                if !seen.insert(label.clone()) {
                    continue;
                }
                items.push(CompletionItem {
                    label,
                    kind: Some(kind),
                    detail,
                    ..Default::default()
                });
            }
        }
    }

    items.extend(keyword_completion_items(
        prefix,
        macro_ctx,
        inside_function,
        Some(target),
    ));

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
    if let Some(module) = all_modules.iter().find(|m| m.name == name) {
        return Some(module);
    }
    if import
        .path
        .segments
        .last()
        .map(|s| s == "prelude")
        .unwrap_or(false)
        && import.path.segments.len() > 1
    {
        let base = import.path.segments[..import.path.segments.len() - 1].join("::");
        return all_modules.iter().find(|m| m.name == base);
    }
    None
}

fn import_allowed_names(import: &Import, imported: &Module) -> Option<HashSet<String>> {
    if let Some(selectors) = &import.selectors {
        let mut names = HashSet::new();
        for selector in selectors {
            match selector {
                ImportSelector::Name { name, alias, .. } => {
                    names.insert(alias.clone().unwrap_or_else(|| name.clone()));
                }
                ImportSelector::Glob(_) => {
                    names.clear();
                    break;
                }
            }
        }
        if !names.is_empty() {
            return Some(names);
        }
        return None;
    }
    if import
        .path
        .segments
        .last()
        .map(|s| s == "prelude")
        .unwrap_or(false)
        && import.selectors.is_none()
    {
        let mut names = HashSet::new();
        for item in &imported.prelude {
            match item {
                ImportSelector::Name { name, alias, .. } => {
                    names.insert(alias.clone().unwrap_or_else(|| name.clone()));
                }
                ImportSelector::Glob(_) => return None,
            }
        }
        if !names.is_empty() {
            return Some(names);
        }
    }
    None
}

pub fn keyword_completion_items(
    prefix: Option<&str>,
    macro_ctx: bool,
    inside_function: bool,
    target: Option<&BuildTarget>,
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
        "in",
        "while",
        "loop",
        "return",
        "defer",
        "import",
        "export",
        "module",
        "library",
        "test",
        "break",
        "continue",
        "prelude",
        "pub",
        "async",
        "await",
        "spawn",
        "try",
        "move",
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
        ("in", "Built-in input helper (returns Result[T, string])"),
        ("out", "Built-in output helper"),
        ("assert", "Built-in test helper"),
        ("assert_eq", "Built-in test helper"),
        ("expect", "Built-in test helper"),
        ("panic", "Built-in panic helper (no unwinding; aborts)"),
        ("channel", "Built-in concurrency helper"),
        ("send", "Built-in concurrency helper"),
        ("recv", "Built-in concurrency helper"),
        (
            "recv_timeout",
            "Built-in concurrency helper (millis timeout)",
        ),
        ("recv_task", "Async channel recv (returns Task[Option[T]])"),
        ("close", "Built-in concurrency helper"),
        ("join", "Built-in concurrency helper"),
        ("sleep", "Built-in timer helper (millis)"),
        ("sleep_ms", "Built-in timer helper (alias for sleep)"),
        ("sleep_task", "Async timer helper (returns Task[()])"),
        (
            "cancel_token",
            "Async cancellation helper (returns CancelToken)",
        ),
        ("cancel", "Async cancellation helper"),
        ("is_cancelled", "Async cancellation helper"),
        (
            "await_timeout",
            "Await helper with timeout (returns Result[T, string])",
        ),
        (
            "await_cancel",
            "Await helper with cancellation (returns Result[T, string])",
        ),
        (
            "await_cancel_timeout",
            "Await helper with cancellation+timeout (returns Result[T, string])",
        ),
        ("now_ms", "Built-in clock helper (int64 millis)"),
        ("fs_exists", "Built-in file helper"),
        ("fs_read", "Built-in file helper"),
        ("fs_write", "Built-in file helper"),
        ("ptr", "Built-in pointer helper"),
        ("ptr_mut", "Built-in pointer helper"),
        ("cast", "Built-in cast helper"),
        ("box_new", "Built-in heap helper"),
        ("box_get", "Built-in heap helper"),
        ("box_set", "Built-in heap helper"),
        ("box_take", "Built-in heap helper"),
        ("slice_new", "Built-in slice helper"),
        ("slice_push", "Built-in slice helper"),
        ("slice_len", "Built-in slice helper"),
        ("slice_get", "Built-in slice helper"),
        ("len", "Built-in len helper"),
        ("iter", "Iterator helper for slices/maps"),
        ("next", "Iterator helper"),
        ("map_new", "Built-in map helper"),
        ("map_insert", "Built-in map helper"),
        ("map_get", "Built-in map helper"),
        ("map_keys", "Built-in map helper (string keys)"),
        ("map_values", "Built-in map helper"),
        ("debug_show", "Built-in debug helper"),
        ("str_len", "Built-in string helper"),
        ("str_contains", "Built-in string helper"),
        ("str_trim", "Built-in string helper"),
        ("str_split", "Built-in string helper"),
        ("min", "Built-in math helper"),
        ("max", "Built-in math helper"),
        ("abs", "Built-in math helper"),
    ];
    let embedded_target = target.map(|t| t.is_embedded()).unwrap_or(false);
    const EMBEDDED_HOST_ONLY: &[&str] = &["fs_exists", "fs_read", "fs_write"];
    for (label, detail) in BUILTIN_FUNCS.iter().filter(|(name, _)| {
        prefix_matches(name, prefix)
            && !(embedded_target && EMBEDDED_HOST_ONLY.iter().any(|host_only| host_only == name))
    }) {
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
            (
                "@sep",
                "macro repeat separator hint (use `@sep = ,` or `@sep = ;`)",
            ),
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
