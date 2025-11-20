use crate::language::{
    ast::{ConstDef, EnumDef, EnumVariant, InterfaceDef, Item, Module, StructDef},
    span::Span,
    token::{Token, TokenKind},
    types::{Mutability, TypeExpr},
};
use tower_lsp_server::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind};

use super::{
    analysis::{DeclInfo, DeclKind, find_local_decl},
    completion::{
        ChainResolution, StructInfo, StructInfoMap, chain_for_field_token,
        format_function_signature, format_interface_method_signature, format_type_arguments,
        format_type_expr, format_type_params, resolve_chain_from_scope, select_struct_info,
    },
    text::{extract_text, span_to_range},
};

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub name: String,
    pub ty: Option<String>,
    pub expr_text: Option<String>,
}

pub fn collect_var_infos(text: &str, tokens: &[Token]) -> Vec<VarInfo> {
    let mut vars = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        if matches!(tokens[idx].kind, TokenKind::Let) {
            let mut cursor = idx + 1;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Mut)) {
                cursor += 1;
            }
            let Some(name_token) = tokens.get(cursor) else {
                idx += 1;
                continue;
            };
            let name = if let TokenKind::Identifier(name) = &name_token.kind {
                name.clone()
            } else {
                idx += 1;
                continue;
            };
            cursor += 1;

            let mut ty = None;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Colon)) {
                cursor += 1;
                let type_start = tokens
                    .get(cursor)
                    .map(|t| t.span.start)
                    .unwrap_or(name_token.span.end);
                let mut type_end = type_start;
                while cursor < tokens.len()
                    && !matches!(tokens[cursor].kind, TokenKind::Eq | TokenKind::Semi)
                {
                    type_end = tokens[cursor].span.end;
                    cursor += 1;
                }
                if type_end > type_start {
                    ty = Some(extract_text(text, type_start, type_end));
                }
            }

            let mut expr_text = None;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Eq)) {
                cursor += 1;
                let expr_start = tokens
                    .get(cursor)
                    .map(|t| t.span.start)
                    .unwrap_or(name_token.span.end);
                let mut expr_end = expr_start;
                while cursor < tokens.len() && !matches!(tokens[cursor].kind, TokenKind::Semi) {
                    expr_end = tokens[cursor].span.end;
                    cursor += 1;
                }
                if expr_end > expr_start {
                    expr_text = Some(extract_text(text, expr_start, expr_end));
                }
            }

            vars.push(VarInfo {
                name,
                ty,
                expr_text,
            });
        }
        idx += 1;
    }
    vars
}

pub fn hover_for_token(
    text: &str,
    token: &Token,
    vars: &[VarInfo],
    module: Option<&Module>,
    struct_info: Option<&StructInfoMap>,
) -> Option<Hover> {
    let span = token.span;
    let hover = match &token.kind {
        TokenKind::Identifier(name) => {
            if let Some(struct_info_map) = struct_info {
                let module_hint = module.map(|m| m.name.as_str());
                if let Some(info) = select_struct_info(struct_info_map, name, module_hint) {
                    return Some(markdown_struct_info(text, span, name, info));
                }
            }
            if name == "out" {
                Some(
                    "Built-in output function **out(expr)**\n\nPrints the evaluated expression."
                        .to_string(),
                )
            } else if let Some(doc) = builtin_function_docs(name) {
                Some(doc)
            } else if let Some(module) = module {
                if let Some(field_hover) =
                    hover_for_struct_field_definition(text, span, name, module)
                {
                    return Some(field_hover);
                }
                if let Some(method_hover) =
                    hover_for_interface_method_definition(text, span, name, module)
                {
                    return Some(method_hover);
                }
                if let Some(struct_info) = struct_info {
                    if let Some(hover) =
                        hover_for_field_usage(text, span, struct_info, module, span.start)
                    {
                        return Some(hover);
                    }
                }
                if let Some(decl) = find_local_decl(module, name, span.start) {
                    return Some(hover_for_local_decl(text, span, &decl));
                }
                if let Some(hover) = hover_for_module_symbol(text, span, module, name) {
                    return Some(hover);
                }
                if let Some(info) = vars.iter().rev().find(|var| var.name == *name) {
                    return Some(markdown_var_info(text, span, info));
                }
                let decl_info = find_decl_for_identifier(module, name, span.end);
                let ty = decl_info.ty.as_ref();
                let mut content = identifier_hover(name, ty);
                if let Some(value_span) = decl_info.value_span {
                    content.push_str("\nPattern:\n```prime\n");
                    content.push_str(&extract_text(text, value_span.start, value_span.end));
                    content.push_str("\n```");
                }
                Some(content)
            } else if let Some(info) = vars.iter().rev().find(|var| var.name == *name) {
                return Some(markdown_var_info(text, span, info));
            } else {
                Some(identifier_hover(name, None))
            }
        }
        TokenKind::Let => Some("Keyword **let**\n\nIntroduces a new binding.".to_string()),
        TokenKind::Fn => Some("Keyword **fn**\n\nDefines a function.".to_string()),
        TokenKind::Struct => Some("Keyword **struct**\n\nDeclares a structure.".to_string()),
        TokenKind::Enum => Some("Keyword **enum**\n\nDeclares an enum.".to_string()),
        TokenKind::Interface => {
            Some("Keyword **interface**\n\nDeclares an interface of required methods.".to_string())
        }
        TokenKind::Impl => {
            Some("Keyword **impl**\n\nImplements an interface for a concrete struct.".to_string())
        }
        TokenKind::Pub => Some("Keyword **pub**\n\nMarks an item as public.".to_string()),
        TokenKind::Const => Some("Keyword **const**\n\nDeclares a constant.".to_string()),
        TokenKind::Return => Some("Keyword **return**\n\nExits the current function.".to_string()),
        TokenKind::If => Some("Keyword **if**\n\nConditional execution.".to_string()),
        TokenKind::Else => Some("Keyword **else**\n\nAlternate branch for `if`.".to_string()),
        TokenKind::While => {
            Some("Keyword **while**\n\nLoop while the condition holds.".to_string())
        }
        TokenKind::For => Some("Keyword **for**\n\nRange-based loop.".to_string()),
        TokenKind::Match => Some("Keyword **match**\n\nPattern matching expression.".to_string()),
        TokenKind::Defer => Some("Keyword **defer**\n\nRun code when leaving scope.".to_string()),
        TokenKind::Import => {
            Some("Keyword **import**\n\nBring another module into scope.".to_string())
        }
        TokenKind::Using => {
            Some("Keyword **using**\n\nRe-export or alias imported symbols.".to_string())
        }
        TokenKind::True | TokenKind::False => Some("Boolean literal.".to_string()),
        TokenKind::Integer(value) => Some(format!("Integer literal `{value}`")),
        TokenKind::Float(value) => Some(format!("Float literal `{value}`")),
        TokenKind::String(value) => Some(format!("String literal \"{value}\"")),
        TokenKind::Rune(value) => Some(format!("Rune literal `'{}'`", value)),
        _ => None,
    }?;
    Some(markdown_hover(text, span, hover))
}

fn builtin_function_docs(name: &str) -> Option<String> {
    match name {
        "box_new" => Some("Built-in heap helper\n```prime\nfn box_new[T](value: T) -> Box[T]\n```"
            .into()),
        "box_get" => Some(
            "Built-in heap helper\n```prime\nfn box_get[T](value: Box[T]) -> T\n```".into(),
        ),
        "box_set" => Some(
            "Built-in heap helper\n```prime\nfn box_set[T](value: Box[T], new_value: T) -> ()\n```"
                .into(),
        ),
        "box_take" => Some(
            "Built-in heap helper\n```prime\nfn box_take[T](value: Box[T]) -> T\n```".into(),
        ),
        "slice_new" => Some(
            "Built-in slice helper\n```prime\nfn slice_new[T]() -> []T\n```".into(),
        ),
        "slice_push" => Some(
            "Built-in slice helper\n```prime\nfn slice_push[T](slice: []T, value: T) -> ()\n```"
                .into(),
        ),
        "slice_len" => Some(
            "Built-in slice helper\n```prime\nfn slice_len[T](slice: []T) -> int32\n```".into(),
        ),
        "slice_get" => Some(
            "Built-in slice helper\n```prime\nfn slice_get[T](slice: []T, index: int32) -> Option[T]\n```"
                .into(),
        ),
        "map_new" => Some(
            "Built-in map helper\n```prime\nfn map_new[V]() -> Map[string, V]\n```".into(),
        ),
        "map_insert" => Some(
            "Built-in map helper\n```prime\nfn map_insert[V](map: Map[string, V], key: string, value: V) -> ()\n```"
                .into(),
        ),
        "map_get" => Some(
            "Built-in map helper\n```prime\nfn map_get[V](map: Map[string, V], key: string) -> Option[V]\n```\nEquivalent available via `map.get(key)`"
                .into(),
        ),
        _ => None,
    }
}

fn markdown_var_info(text: &str, span: Span, info: &VarInfo) -> Hover {
    let mut snippet = String::from("```prime\nlet ");
    snippet.push_str(&info.name);
    if let Some(ty) = &info.ty {
        snippet.push_str(": ");
        snippet.push_str(ty);
    }
    if let Some(expr) = &info.expr_text {
        snippet.push_str(" = ");
        snippet.push_str(expr);
    }
    snippet.push_str(";\n```\n");
    if let Some(ty) = &info.ty {
        snippet.push_str(&format!("Type: `{ty}`"));
    } else {
        snippet.push_str("Type: inferred");
    }
    markdown_hover(text, span, snippet)
}

fn hover_for_module_symbol(
    text: &str,
    usage_span: Span,
    module: &Module,
    name: &str,
) -> Option<Hover> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == name => {
                let signature = format_function_signature(func);
                let value = format!("```prime\n{}\n```", signature);
                return Some(markdown_hover(text, usage_span, value));
            }
            Item::Struct(def) if def.name == name => {
                let snippet = format_struct_hover(def);
                return Some(markdown_hover(text, usage_span, snippet));
            }
            Item::Interface(def) if def.name == name => {
                let snippet = format_interface_hover(def);
                return Some(markdown_hover(text, usage_span, snippet));
            }
            Item::Enum(def) => {
                if def.name == name {
                    let snippet = format_enum_hover(def);
                    return Some(markdown_hover(text, usage_span, snippet));
                }
                if let Some(variant) = def.variants.iter().find(|variant| variant.name == name) {
                    let signature = format_enum_variant_signature(variant);
                    let params = format_type_params(&def.type_params);
                    let mut value = String::from("```prime\n");
                    value.push_str(&def.name);
                    value.push_str(&params);
                    value.push_str(" :: ");
                    value.push_str(&signature);
                    value.push_str("\n```");
                    return Some(markdown_hover(text, usage_span, value));
                }
            }
            Item::Const(def) if def.name == name => {
                let snippet = format_const_snippet(text, def);
                return Some(markdown_hover(text, usage_span, snippet));
            }
            Item::Impl(block) => {
                for method in &block.methods {
                    if method.name == name {
                        let signature = format_function_signature(method);
                        let header = format!(
                            "impl {}{} for {}",
                            block.interface,
                            format_type_arguments(&block.type_args),
                            block.target
                        );
                        let value = format!("```prime\n{}\n{}\n```", header, signature);
                        return Some(markdown_hover(text, usage_span, value));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn hover_for_local_decl(text: &str, usage_span: Span, decl: &DeclInfo) -> Hover {
    let mut value = String::new();
    value.push_str("```prime\n");
    value.push_str(&extract_text(text, decl.span.start, decl.span.end));
    value.push_str("\n```\n");
    value.push_str(&format!("Kind: {}", format_decl_kind(decl.kind)));
    if let Some(ty) = &decl.ty {
        value.push_str(&format!("\nType: `{}`", format_type_expr(ty)));
    }
    if decl.mutability.is_mutable() {
        value.push_str("\nMutable binding");
    }
    if decl.kind == DeclKind::Pattern {
        if let Some(span) = decl.value_span {
            value.push_str("\nPattern:\n```prime\n");
            value.push_str(&extract_text(text, span.start, span.end));
            value.push_str("\n```");
        }
    }
    markdown_hover(text, usage_span, value)
}

fn hover_for_field_usage(
    text: &str,
    span: Span,
    struct_info: &StructInfoMap,
    module: &Module,
    offset: usize,
) -> Option<Hover> {
    let chain = chain_for_field_token(text, span)?;
    let name = chain.last()?.clone();
    if let Some(ChainResolution {
        ty: target_type,
        last_field: field_info,
        module_name: target_module,
    }) = resolve_chain_from_scope(&chain, module, struct_info, offset)
    {
        if let Some((struct_name, field)) = field_info {
            let mut value = String::new();
            value.push_str(&format!("Field `{name}`\n\n"));
            value.push_str(&format!("Type: `{}`", format_type_expr(&field.ty)));
            value.push_str(&format!("\nStruct: `{struct_name}`"));
            return Some(markdown_hover(text, span, value));
        }
        if let Some((struct_name, _)) = super::completion::named_type_with_args(&target_type) {
            let module_hint = target_module.as_deref().or(Some(module.name.as_str()));
            if let Some(info) = select_struct_info(struct_info, &struct_name, module_hint) {
                let mut value = String::new();
                value.push_str(&format!("Struct `{struct_name}`\n\n"));
                for method in &info.methods {
                    value.push_str(&format!("- {}\n", method.signature));
                }
                return Some(markdown_hover(text, span, value));
            }
        }
    }
    if chain.len() >= 2 {
        let base_chain = &chain[..chain.len() - 1];
        if let Some(ChainResolution { ty: base_type, .. }) =
            resolve_chain_from_scope(base_chain, module, struct_info, offset)
        {
            if let Some(hover) = hover_for_builtin_method(text, span, &base_type, name.as_str()) {
                return Some(hover);
            }
        }
    }
    None
}

fn hover_for_builtin_method(
    text: &str,
    usage_span: Span,
    ty: &TypeExpr,
    method: &str,
) -> Option<Hover> {
    let stripped = strip_type_refs(ty);
    let (kind, signature) = builtin_method_signature(stripped, method)?;
    let mut value = String::new();
    value.push_str(&format!("Built-in {kind}\n```prime\n{signature}\n```\n"));
    value.push_str(&format!("Receiver: `{}`", format_type_expr(stripped)));
    Some(markdown_hover(text, usage_span, value))
}

fn builtin_method_signature(ty: &TypeExpr, method: &str) -> Option<(&'static str, String)> {
    match ty {
        TypeExpr::Slice(inner) => {
            let element_ty = inner.as_ref().clone();
            let option_ty = TypeExpr::Named("Option".into(), vec![element_ty.clone()]);
            match method {
                "len" => Some(("slice method", "fn len() -> int32".into())),
                "get" => Some((
                    "slice method",
                    format!("fn get(index: int32) -> {}", format_type_expr(&option_ty)),
                )),
                "push" => Some((
                    "slice method",
                    format!("fn push(value: {}) -> ()", format_type_expr(&element_ty)),
                )),
                _ => None,
            }
        }
        TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
            let value_ty = args[1].clone();
            let option_ty = TypeExpr::Named("Option".into(), vec![value_ty.clone()]);
            match method {
                "get" => Some((
                    "map method",
                    format!("fn get(key: string) -> {}", format_type_expr(&option_ty)),
                )),
                "insert" => Some((
                    "map method",
                    format!(
                        "fn insert(key: string, value: {}) -> ()",
                        format_type_expr(&value_ty)
                    ),
                )),
                "len" => Some(("map method", "fn len() -> int32".into())),
                _ => None,
            }
        }
        TypeExpr::Named(name, args) if name == "Box" && args.len() == 1 => {
            let inner = args[0].clone();
            match method {
                "box_get" => Some((
                    "box method",
                    format!("fn box_get() -> {}", format_type_expr(&inner)),
                )),
                "box_set" => Some((
                    "box method",
                    format!("fn box_set(value: {}) -> ()", format_type_expr(&inner)),
                )),
                "box_take" => Some((
                    "box method",
                    format!("fn box_take() -> {}", format_type_expr(&inner)),
                )),
                _ => None,
            }
        }
        _ => None,
    }
}

fn strip_type_refs<'a>(ty: &'a TypeExpr) -> &'a TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => strip_type_refs(ty),
        _ => ty,
    }
}

fn markdown_struct_info(text: &str, usage_span: Span, name: &str, info: &StructInfo) -> Hover {
    let mut value = String::new();
    value.push_str("```prime\nstruct ");
    value.push_str(name);
    value.push_str(" {\n");
    for field in &info.fields {
        value.push_str("  ");
        value.push_str(&field.name);
        value.push_str(": ");
        value.push_str(&format_type_expr(&field.ty));
        value.push_str(";\n");
    }
    value.push_str("}\n```\n");
    markdown_hover(text, usage_span, value)
}

fn hover_for_struct_field_definition(
    text: &str,
    usage_span: Span,
    name: &str,
    module: &Module,
) -> Option<Hover> {
    for item in &module.items {
        if let Item::Struct(def) = item {
            for field in &def.fields {
                if let Some(field_name) = &field.name {
                    if field_name == name && span_contains(field.span, usage_span.start) {
                        let mut value = String::new();
                        value.push_str(&format!("Field `{name}`\n\n"));
                        value.push_str(&format!("Type: `{}`", format_type_expr(&field.ty.ty)));
                        value.push_str(&format!("\nStruct: `{}`", def.name));
                        if field.embedded {
                            value.push_str("\nEmbedded field");
                        }
                        return Some(markdown_hover(text, usage_span, value));
                    }
                }
            }
        }
    }
    None
}

fn hover_for_interface_method_definition(
    text: &str,
    usage_span: Span,
    name: &str,
    module: &Module,
) -> Option<Hover> {
    for item in &module.items {
        if let Item::Interface(def) = item {
            for method in &def.methods {
                if method.name == name && span_contains(method.span, usage_span.start) {
                    let mut value = String::new();
                    value.push_str("```prime\n");
                    value.push_str(&format_interface_method_signature(method, None));
                    value.push_str("\n```\n");
                    value.push_str(&format!(
                        "Interface: `{}`{}",
                        def.name,
                        format_type_params(&def.type_params)
                    ));
                    return Some(markdown_hover(text, usage_span, value));
                }
            }
        }
    }
    None
}

fn format_decl_kind(kind: DeclKind) -> &'static str {
    match kind {
        DeclKind::Param => "parameter",
        DeclKind::Let => "local binding",
        DeclKind::ForBinding => "loop binding",
        DeclKind::Pattern => "pattern binding",
    }
}

fn format_struct_hover(def: &StructDef) -> String {
    let mut value = String::new();
    value.push_str("```prime\n");
    value.push_str(&format!(
        "struct {}{}",
        def.name,
        format_type_params(&def.type_params)
    ));
    value.push_str(" {\n");
    for field in &def.fields {
        if let Some(name) = &field.name {
            value.push_str(&format!(
                "  {}: {},\n",
                name,
                format_type_expr(&field.ty.ty)
            ));
        } else {
            value.push_str(&format!("  {};\n", format_type_expr(&field.ty.ty)));
        }
    }
    value.push_str("}\n```\n");
    value
}

fn format_enum_hover(def: &EnumDef) -> String {
    let mut value = String::new();
    value.push_str("```prime\n");
    value.push_str(&format!(
        "enum {}{}",
        def.name,
        format_type_params(&def.type_params)
    ));
    value.push_str(" {\n");
    for variant in &def.variants {
        value.push_str(&format!("  {},\n", format_enum_variant_signature(variant)));
    }
    value.push_str("}\n```\n");
    value
}

fn format_interface_hover(def: &InterfaceDef) -> String {
    let mut value = String::new();
    value.push_str("```prime\n");
    value.push_str(&format!(
        "interface {}{}",
        def.name,
        format_type_params(&def.type_params)
    ));
    value.push_str(" {\n");
    for method in &def.methods {
        value.push_str(&format!(
            "  {};\n",
            format_interface_method_signature(method, None)
        ));
    }
    value.push_str("}\n```\n");
    value
}

fn format_enum_variant_signature(variant: &EnumVariant) -> String {
    if variant.fields.is_empty() {
        variant.name.clone()
    } else {
        let fields = variant
            .fields
            .iter()
            .map(|field| format_type_expr(&field.ty))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}({})", variant.name, fields)
    }
}

fn format_const_snippet(text: &str, def: &ConstDef) -> String {
    let snippet = extract_text(text, def.span.start, def.span.end)
        .trim()
        .to_string();
    if snippet.is_empty() {
        let mut fallback = format!("const {}", def.name);
        if let Some(ty) = &def.ty {
            fallback.push_str(": ");
            fallback.push_str(&format_type_expr(&ty.ty));
        }
        fallback
    } else {
        snippet
    }
}

fn markdown_hover(text: &str, span: Span, value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(span_to_range(text, span)),
    }
}

fn span_contains(span: Span, offset: usize) -> bool {
    offset >= span.start && offset <= span.end
}

fn identifier_hover(name: &str, ty: Option<&TypeExpr>) -> String {
    let mut content = format!("Identifier `{}`", name);
    if let Some(ty) = ty {
        content.push_str(&format!("\nType: `{}`", format_type_expr(ty)));
    }
    content
}

fn find_decl_for_identifier<'a>(module: &'a Module, name: &str, offset: usize) -> DeclInfo {
    find_local_decl(module, name, offset).unwrap_or_else(|| DeclInfo {
        name: name.to_string(),
        span: Span::new(offset, offset),
        scope: Span::new(0, 0),
        available_from: 0,
        ty: None,
        value_span: None,
        mutability: Mutability::Immutable,
        kind: DeclKind::Pattern,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::{lexer::lex, parser::parse_module};
    use crate::lsp::completion::collect_struct_info;
    use std::path::PathBuf;
    use tower_lsp_server::lsp_types::HoverContents;

    #[test]
    fn collects_var_infos_with_types_and_values() {
        let text = "let score: int32 = 10;\nlet mut label = \"hero\";";
        let tokens = lex(text).expect("lex tokens");
        let vars = collect_var_infos(text, &tokens);
        assert_eq!(vars.len(), 2);
        assert_eq!(vars[0].name, "score");
        assert_eq!(vars[0].ty.as_deref(), Some("int32"));
        assert_eq!(vars[0].expr_text.as_deref(), Some("10"));
        assert_eq!(vars[1].name, "label");
        assert!(vars[1].ty.is_none());
        assert_eq!(vars[1].expr_text.as_deref(), Some("\"hero\""));
    }

    #[test]
    fn hover_returns_struct_definition_for_usage() {
        let text = r#"module demo::main;

struct Player {
  name: string;
}

fn main() {
  let hero = Player { name: "Prime" };
  out(hero.name);
}
"#;
        let tokens = lex(text).expect("lex tokens");
        let module =
            parse_module("demo::main", PathBuf::from("demo.prime"), text).expect("parse module");
        let structs = collect_struct_info(&[module.clone()]);
        let player_usage = tokens
            .iter()
            .filter(|token| matches!(&token.kind, TokenKind::Identifier(name) if name == "Player"))
            .nth(1)
            .expect("second Player token");
        let hover = hover_for_token(text, player_usage, &[], Some(&module), Some(&structs))
            .expect("hover result");
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(content.value.contains("struct Player"));
                assert!(content.value.contains("name: string"));
            }
            _ => panic!("expected markup hover"),
        }
    }

    #[test]
    fn hover_returns_var_info_snippet() {
        let text = "let mut score: int32 = 10;";
        let tokens = lex(text).expect("lex tokens");
        let vars = collect_var_infos(text, &tokens);
        let token = tokens
            .iter()
            .find(|token| matches!(&token.kind, TokenKind::Identifier(name) if name == "score"))
            .expect("score token");
        let hover = hover_for_token(text, token, &vars, None, None).expect("hover result");
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(content.value.contains("let score: int32 = 10;"));
                assert!(content.value.contains("Type: `int32`"));
            }
            _ => panic!("expected markup hover"),
        }
    }

    #[test]
    fn hover_shows_pattern_for_destructured_binding() {
        let text = r#"
module demo::hover;

fn main() {
  let mut (left, right) = (1, 2);
  out(left);
}
"#;
        let tokens = lex(text).expect("lex tokens");
        let module =
            parse_module("demo::hover", PathBuf::from("demo.prime"), text).expect("parse module");
        let token = tokens
            .iter()
            .filter(|token| matches!(&token.kind, TokenKind::Identifier(name) if name == "left"))
            .nth(1)
            .expect("usage of left");
        let hover =
            hover_for_token(text, token, &[], Some(&module), None).expect("hover result for left");
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(
                    content.value.contains("(left, right)"),
                    "expected destructuring snippet in hover, got {}",
                    content.value
                );
            }
            _ => panic!("expected markup hover"),
        }
    }

    #[test]
    fn hover_shows_type_for_tuple_destructuring() {
        let text = r#"
module demo::lab;

struct Sample {
  value: int32;
}

fn build() -> (bool, Sample, string) {
  return true, Sample{ value: 5 }, "ok";
}

fn main() {
  let (ok, sample, message) = build();
  out(sample.value);
}
"#;
        let tokens = lex(text).expect("lex tokens");
        let module =
            parse_module("demo::lab", PathBuf::from("lab.prime"), text).expect("parse module");
        let structs = collect_struct_info(&[module.clone()]);
        let token = tokens
            .iter()
            .filter(|token| matches!(&token.kind, TokenKind::Identifier(name) if name == "sample"))
            .nth(1)
            .expect("usage of sample");
        let hover = hover_for_token(text, token, &[], Some(&module), Some(&structs))
            .expect("hover result for sample");
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(
                    content.value.contains("Type: `Sample`"),
                    "expected hover to include type, got {}",
                    content.value
                );
            }
            _ => panic!("expected markup hover"),
        }
    }
}
