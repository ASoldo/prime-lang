use super::*;

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
    if let Some(after) = trimmed.strip_prefix("module ") {
        let after = after.trim_start();
        return Some(ModulePathCompletionContext {
            kind: ModulePathCompletionKind::Declaration,
            prefix: sanitize_module_prefix(after),
        });
    }
    if let Some(after) = trimmed.strip_prefix("pub ") {
        trimmed = after.trim_start();
    }
    if let Some(after) = trimmed.strip_prefix("import ") {
        let after = after.trim_start();
        if let Some(idx) = after.find('{') {
            let module_part = after[..idx].trim_end().trim_end_matches(':');
            return Some(ModulePathCompletionContext {
                kind: ModulePathCompletionKind::ImportSelectors,
                prefix: sanitize_module_prefix(module_part),
            });
        }
        return Some(ModulePathCompletionContext {
            kind: ModulePathCompletionKind::Import,
            prefix: sanitize_module_prefix(after),
        });
    }
    None
}

pub fn completion_trigger_characters() -> Vec<String> {
    // Trigger on identifiers, qualification separators, macro sigils, and hygiene escapes.
    const TRIGGER_CHARS: &str =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_:/.~@";
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
        Some(prefix.replace('/', "::"))
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
        let exp_norm = exp.replace('/', "::");
        if module_prefix_matches(&exp_norm, prefix) && seen.insert(exp_norm.clone()) {
            let detail = entries
                .iter()
                .find(|entry| entry.name == exp_norm)
                .map(module_detail);
            items.push(CompletionItem {
                label: exp_norm.clone(),
                kind: Some(CompletionItemKind::MODULE),
                detail,
                ..CompletionItem::default()
            });
        }
    }
    for entry in entries.iter() {
        let name_norm = entry.name.replace('/', "::");
        if !module_prefix_matches(&name_norm, prefix) {
            continue;
        }
        if !seen.insert(name_norm.clone()) {
            continue;
        }
        let detail = module_detail(entry);
        items.push(CompletionItem {
            label: name_norm,
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(detail),
            ..CompletionItem::default()
        });
    }
    items
}

pub fn module_selector_items_from_modules(
    module_name: &str,
    modules: &[Module],
) -> Vec<CompletionItem> {
    let target_name = if module_name.ends_with("::prelude") {
        module_name
            .rsplit_once("::")
            .map(|(prefix, _)| prefix)
            .unwrap_or(module_name)
    } else {
        module_name
    };
    let Some(module) = modules.iter().find(|m| m.name == target_name) else {
        return Vec::new();
    };
    let mut items = Vec::new();
    let mut seen = HashSet::new();
    let mut push_name =
        |name: &str, kind: CompletionItemKind, detail: Option<String>, visibility: Visibility| {
            if !matches!(visibility, Visibility::Public | Visibility::Package) {
                return;
            }
            if !seen.insert(name.to_string()) {
                return;
            }
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(kind),
                detail,
                ..CompletionItem::default()
            });
        };

    for item in &module.items {
        match item {
            Item::Function(def) => push_name(
                &def.name,
                CompletionItemKind::FUNCTION,
                Some(format_function_signature(def)),
                def.visibility,
            ),
            Item::Const(def) => push_name(
                &def.name,
                CompletionItemKind::CONSTANT,
                def.ty
                    .as_ref()
                    .map(|ann| format!("const {}: {}", def.name, format_type_expr(&ann.ty))),
                def.visibility,
            ),
            Item::Struct(def) => push_name(
                &def.name,
                CompletionItemKind::STRUCT,
                Some(format!(
                    "struct {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                def.visibility,
            ),
            Item::Enum(def) => push_name(
                &def.name,
                CompletionItemKind::ENUM,
                Some(format!(
                    "enum {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                def.visibility,
            ),
            Item::Interface(def) => push_name(
                &def.name,
                CompletionItemKind::INTERFACE,
                Some(format!(
                    "interface {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                def.visibility,
            ),
            Item::Macro(def) => push_name(
                &def.name,
                CompletionItemKind::FUNCTION,
                Some(format_macro_detail(def)),
                def.visibility,
            ),
            Item::Impl(_) | Item::MacroInvocation(_) | Item::Comment { .. } => {}
        }
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
