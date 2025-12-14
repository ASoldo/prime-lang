use super::*;

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
        || (name.starts_with('\"') && name.ends_with('\"'))
    {
        return Some(TypeExpr::named("string"));
    }
    None
}
