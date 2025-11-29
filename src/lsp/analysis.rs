use crate::language::{
    ast::{
        Block, ClosureBody, ElseBranch, Expr, ForTarget, FormatSegment, FormatStringLiteral,
        FunctionBody, FunctionDef, IfCondition, IfExpr, Item, LetStmt, Literal, MacroBody,
        MacroDef, MacroInvocation, MatchExpr, Module, Pattern, RangeExpr, Statement,
        StructLiteralKind, WhileCondition,
    },
    span::Span,
    token::Token,
    types::{Mutability, TypeAnnotation, TypeExpr},
};
use std::collections::HashSet;
use tower_lsp_server::lsp_types::{Diagnostic, DiagnosticSeverity};

use super::text::{collect_identifier_spans_in_scope, span_to_range};

#[derive(Debug, Clone)]
pub struct DeclInfo {
    pub name: String,
    pub span: Span,
    pub scope: Span,
    pub available_from: usize,
    pub ty: Option<TypeExpr>,
    #[allow(dead_code)]
    pub value_span: Option<Span>,
    pub mutability: Mutability,
    pub kind: DeclKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclKind {
    Param,
    Let,
    ForBinding,
    Pattern,
}

pub fn unused_variable_diagnostics(module: &Module, text: &str) -> Vec<Diagnostic> {
    let decls = collect_decl_spans(module);
    if decls.is_empty() {
        return Vec::new();
    }
    let is_ignored = |name: &str| name.starts_with('_');
    let used = collect_used_identifiers(module);
    decls
        .into_iter()
        .filter(|decl| !is_ignored(&decl.name) && !used.contains(&decl.name))
        .map(|decl| Diagnostic {
            range: span_to_range(text, decl.span),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: format!("Variable `{}` is never used", decl.name),
            ..Default::default()
        })
        .collect()
}

pub fn collect_decl_spans(module: &Module) -> Vec<DeclInfo> {
    let mut decls = Vec::new();
    for item in &module.items {
        collect_decl_from_item(item, module, &mut decls);
    }
    decls
}

fn collect_decl_from_item(item: &Item, module: &Module, decls: &mut Vec<DeclInfo>) {
    match item {
        Item::Function(func) => collect_decl_from_function(func, module, None, decls),
        Item::Impl(block) => {
            let target_ty = Some(TypeExpr::named(block.target.clone()));
            for func in &block.methods {
                collect_decl_from_function(func, module, target_ty.clone(), decls);
            }
        }
        Item::Macro(def) => collect_decl_from_macro(def, module, decls),
        _ => {}
    }
}

fn collect_decl_from_function(
    func: &FunctionDef,
    module: &Module,
    receiver_override: Option<TypeExpr>,
    decls: &mut Vec<DeclInfo>,
) {
    let body_span = match &func.body {
        FunctionBody::Block(block) => block.span,
        FunctionBody::Expr(expr) => expr.span,
    };
    let available_from = body_span.start;
    for param in &func.params {
        let mut param_ty = param.ty.as_ref().map(|ann| ann.ty.clone());
        if param.name == "self" {
            if let Some(override_ty) = receiver_override.clone() {
                param_ty = Some(override_ty);
            } else if let Some(resolved) = param
                .ty
                .as_ref()
                .and_then(|ty| receiver_type_name(&ty.ty).map(TypeExpr::named))
            {
                param_ty = Some(resolved);
            }
        }
        decls.push(DeclInfo {
            name: param.name.clone(),
            span: param.span,
            scope: body_span,
            available_from,
            ty: param_ty.clone(),
            value_span: None,
            mutability: param.mutability,
            kind: DeclKind::Param,
        });
    }
    if let FunctionBody::Block(block) = &func.body {
        collect_decl_from_block(block, module, decls);
    }
}

fn collect_decl_from_macro(def: &MacroDef, module: &Module, decls: &mut Vec<DeclInfo>) {
    let body_span = match &def.body {
        MacroBody::Block(block) => block.span,
        MacroBody::Expr(expr) => expr.span,
        MacroBody::Items(_, span) => *span,
    };
    let available_from = body_span.start;
    for param in &def.params {
        decls.push(DeclInfo {
            name: param.name.clone(),
            span: param.span,
            scope: body_span,
            available_from,
            ty: param.ty.as_ref().map(|annotation| annotation.ty.clone()),
            value_span: None,
            mutability: Mutability::Immutable,
            kind: DeclKind::Param,
        });
    }
    match &def.body {
        MacroBody::Block(block) => collect_decl_from_block(block, module, decls),
        MacroBody::Expr(expr) => collect_decl_from_expr(&expr.node, module, decls),
        MacroBody::Items(items, _) => {
            for item in items {
                collect_decl_from_item(item, module, decls);
            }
        }
    }
}

fn collect_decl_from_block(block: &Block, module: &Module, decls: &mut Vec<DeclInfo>) {
    let scope = block.span;
    for statement in &block.statements {
        match statement {
            Statement::Let(stmt) => {
                if let Some(value) = &stmt.value {
                    collect_decl_from_expr(value, module, decls);
                }
                let inferred_value_ty = stmt
                    .value
                    .as_ref()
                    .and_then(|value| infer_expr_type_with_decls(value, module, decls));
                let mut ty = stmt.ty.as_ref().map(|annotation| annotation.ty.clone());
                if ty.is_none() {
                    ty = infer_type_from_let_value(stmt);
                }
                if ty.is_none() {
                    ty = inferred_value_ty.clone();
                }
                if let Pattern::Identifier(name, _span) = &stmt.pattern {
                    decls.push(DeclInfo {
                        name: name.clone(),
                        span: stmt.span,
                        scope,
                        available_from: stmt.span.end,
                        ty,
                        value_span: stmt.value.as_ref().map(expr_span),
                        mutability: stmt.mutability,
                        kind: DeclKind::Let,
                    });
                } else {
                    collect_pattern_decls(
                        &stmt.pattern,
                        scope,
                        stmt.span.end,
                        decls,
                        pattern_span(&stmt.pattern),
                        ty.as_ref(),
                        module,
                    );
                }
            }
            Statement::MacroSemi(expr) => collect_decl_from_expr(&expr.node, module, decls),
            Statement::Assign(stmt) => {
                collect_decl_from_expr(&stmt.target, module, decls);
                collect_decl_from_expr(&stmt.value, module, decls);
            }
            Statement::Expr(expr_stmt) => collect_decl_from_expr(&expr_stmt.expr, module, decls),
            Statement::Return(ret) => {
                for value in &ret.values {
                    collect_decl_from_expr(value, module, decls);
                }
            }
            Statement::While(while_stmt) => {
                collect_decl_from_while_condition(&while_stmt.condition, module, decls);
                collect_decl_from_block(&while_stmt.body, module, decls);
            }
            Statement::Loop(loop_stmt) => {
                collect_decl_from_block(&loop_stmt.body, module, decls);
            }
            Statement::For(for_stmt) => {
                let mut binding_ty = None;
                match &for_stmt.target {
                    ForTarget::Range(range) => {
                        collect_decl_from_range(range, module, decls);
                        binding_ty = Some(TypeExpr::Named("int32".into(), Vec::new()));
                    }
                    ForTarget::Collection(expr) => {
                        collect_decl_from_expr(expr, module, decls);
                        if let Some(TypeExpr::Slice(inner)) = infer_expr_type(expr, module) {
                            binding_ty = Some(*inner);
                        }
                    }
                }
                let body_span = for_stmt.body.span;
                decls.push(DeclInfo {
                    name: for_stmt.binding.clone(),
                    span: for_stmt.span,
                    scope: body_span,
                    available_from: body_span.start,
                    ty: binding_ty,
                    value_span: None,
                    mutability: Mutability::Immutable,
                    kind: DeclKind::ForBinding,
                });
                collect_decl_from_block(&for_stmt.body, module, decls);
            }
            Statement::Defer(defer_stmt) => collect_decl_from_expr(&defer_stmt.expr, module, decls),
            Statement::Block(inner) => collect_decl_from_block(inner, module, decls),
            Statement::Break | Statement::Continue => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_decl_from_expr(tail, module, decls);
    }
}

fn collect_decl_from_expr(expr: &Expr, module: &Module, decls: &mut Vec<DeclInfo>) {
    match expr {
        Expr::Identifier(_) | Expr::Literal(_) | Expr::FormatString(_) => {}
        Expr::Try { block, .. } => collect_decl_from_block(block, module, decls),
        Expr::TryPropagate { expr: inner, .. } => collect_decl_from_expr(inner, module, decls),
        Expr::Binary { left, right, .. } => {
            collect_decl_from_expr(left, module, decls);
            collect_decl_from_expr(right, module, decls);
        }
        Expr::Unary { expr: inner, .. } => collect_decl_from_expr(inner, module, decls),
        Expr::MacroCall { args, .. } => {
            for arg in args {
                collect_decl_from_expr(&arg.expr, module, decls);
            }
        }
        Expr::Call { callee, args, .. } => {
            collect_decl_from_expr(callee, module, decls);
            for arg in args {
                collect_decl_from_expr(arg, module, decls);
            }
        }
        Expr::Closure { body, .. } => match body {
            ClosureBody::Block(block) => collect_decl_from_block(block, module, decls),
            ClosureBody::Expr(expr) => collect_decl_from_expr(expr.node.as_ref(), module, decls),
        },
        Expr::FieldAccess { base, .. } => collect_decl_from_expr(base, module, decls),
        Expr::Index { base, index, .. } => {
            collect_decl_from_expr(base, module, decls);
            collect_decl_from_expr(index, module, decls);
        }
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    collect_decl_from_expr(&field.value, module, decls);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_decl_from_expr(value, module, decls);
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                collect_decl_from_expr(value, module, decls);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                collect_decl_from_expr(&entry.key, module, decls);
                collect_decl_from_expr(&entry.value, module, decls);
            }
        }
        Expr::Block(block) => collect_decl_from_block(block, module, decls),
        Expr::If(if_expr) => collect_decl_from_if_expr(if_expr, module, decls),
        Expr::Match(match_expr) => {
            collect_decl_from_expr(&match_expr.expr, module, decls);
            for arm in &match_expr.arms {
                let value_span = expr_span(&arm.value);
                let pat_span = pattern_span(&arm.pattern);
                collect_pattern_decls(
                    &arm.pattern,
                    value_span,
                    value_span.start,
                    decls,
                    pat_span,
                    None,
                    module,
                );
                if let Some(guard) = &arm.guard {
                    collect_decl_from_expr(guard, module, decls);
                }
                collect_decl_from_expr(&arm.value, module, decls);
            }
        }
        Expr::Tuple(values, _) | Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_decl_from_expr(value, module, decls);
            }
        }
        Expr::Range(range) => collect_decl_from_range(range, module, decls),
        Expr::Reference { expr: inner, .. } => collect_decl_from_expr(inner, module, decls),
        Expr::Deref { expr: inner, .. } => collect_decl_from_expr(inner, module, decls),
        Expr::Move { expr: inner, .. } => collect_decl_from_expr(inner, module, decls),
        Expr::Async { block, .. } => collect_decl_from_block(block, module, decls),
        Expr::Await { expr, .. } => collect_decl_from_expr(expr, module, decls),
        Expr::Spawn { expr, .. } => collect_decl_from_expr(expr, module, decls),
    }
}

fn collect_decl_from_if_expr(if_expr: &IfExpr, module: &Module, decls: &mut Vec<DeclInfo>) {
    match &if_expr.condition {
        IfCondition::Expr(expr) => collect_decl_from_expr(expr, module, decls),
        IfCondition::Let { pattern, value, .. } => {
            collect_decl_from_expr(value, module, decls);
            let pat_span = pattern_span(pattern);
            collect_pattern_decls(
                pattern,
                if_expr.then_branch.span,
                if_expr.then_branch.span.start,
                decls,
                pat_span,
                infer_expr_type(value, module).as_ref(),
                module,
            );
        }
    }
    collect_decl_from_block(&if_expr.then_branch, module, decls);
    if let Some(else_branch) = &if_expr.else_branch {
        collect_decl_from_else_branch(else_branch, module, decls);
    }
}

fn collect_decl_from_while_condition(
    condition: &WhileCondition,
    module: &Module,
    decls: &mut Vec<DeclInfo>,
) {
    match condition {
        WhileCondition::Expr(expr) => collect_decl_from_expr(expr, module, decls),
        WhileCondition::Let { value, .. } => collect_decl_from_expr(value, module, decls),
    }
}

fn collect_decl_from_else_branch(branch: &ElseBranch, module: &Module, decls: &mut Vec<DeclInfo>) {
    match branch {
        ElseBranch::Block(block) => collect_decl_from_block(block, module, decls),
        ElseBranch::ElseIf(if_expr) => collect_decl_from_if_expr(if_expr, module, decls),
    }
}

fn infer_type_from_let_value(stmt: &LetStmt) -> Option<TypeExpr> {
    let value = stmt.value.as_ref()?;
    if let Expr::StructLiteral { name, .. } = value {
        return Some(TypeExpr::Named(name.clone(), Vec::new()));
    }
    None
}

fn collect_decl_from_range(range: &RangeExpr, module: &Module, decls: &mut Vec<DeclInfo>) {
    collect_decl_from_expr(&range.start, module, decls);
    collect_decl_from_expr(&range.end, module, decls);
}

fn collect_pattern_decls(
    pattern: &Pattern,
    scope: Span,
    available_from: usize,
    decls: &mut Vec<DeclInfo>,
    pattern_span: Span,
    inferred_type: Option<&TypeExpr>,
    module: &Module,
) {
    match pattern {
        Pattern::Identifier(name, span) => decls.push(DeclInfo {
            name: name.clone(),
            span: *span,
            scope,
            available_from,
            ty: inferred_type.cloned(),
            value_span: Some(pattern_span),
            mutability: Mutability::Immutable,
            kind: DeclKind::Pattern,
        }),
        Pattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                collect_pattern_decls(
                    binding,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    None,
                    module,
                );
            }
        }
        Pattern::Tuple(elements, _) => {
            let tuple_types: Vec<Option<&TypeExpr>> = match inferred_type {
                Some(TypeExpr::Tuple(types)) => types.iter().map(Some).collect(),
                _ => Vec::new(),
            };
            for (idx, element) in elements.iter().enumerate() {
                let element_type = tuple_types.get(idx).copied().flatten();
                collect_pattern_decls(
                    element,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    element_type,
                    module,
                );
            }
        }
        Pattern::Map(entries, _) => {
            for entry in entries {
                collect_pattern_decls(
                    &entry.pattern,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    None,
                    module,
                );
            }
        }
        Pattern::Struct {
            struct_name,
            fields,
            ..
        } => {
            for field in fields {
                let field_type = inferred_type
                    .and_then(|ty| struct_field_type(module, ty, &field.name))
                    .or_else(|| {
                        struct_name
                            .as_ref()
                            .and_then(|name| struct_field_type_by_name(module, name, &field.name))
                    });
                collect_pattern_decls(
                    &field.pattern,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    field_type.as_ref(),
                    module,
                );
            }
        }
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for pat in prefix {
                let element_hint = inferred_type.and_then(slice_element_type);
                let element_type = element_hint.as_ref();
                collect_pattern_decls(
                    pat,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    element_type,
                    module,
                );
            }
            if let Some(rest_pattern) = rest {
                let rest_type = inferred_type.map(|ty| match ty {
                    TypeExpr::Slice(inner) => TypeExpr::Slice(inner.clone()),
                    _ => TypeExpr::Slice(Box::new(TypeExpr::Unit)),
                });
                collect_pattern_decls(
                    rest_pattern,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    rest_type.as_ref(),
                    module,
                );
            }
            for pat in suffix {
                let element_hint = inferred_type.and_then(slice_element_type);
                let element_type = element_hint.as_ref();
                collect_pattern_decls(
                    pat,
                    scope,
                    available_from,
                    decls,
                    pattern_span,
                    element_type,
                    module,
                );
            }
        }
        _ => {}
    }
}

fn slice_element_type(ty: &TypeExpr) -> Option<TypeExpr> {
    match ty {
        TypeExpr::Slice(inner) => Some((**inner).clone()),
        _ => None,
    }
}

fn struct_field_type(module: &Module, ty: &TypeExpr, field_name: &str) -> Option<TypeExpr> {
    let struct_name = match ty {
        TypeExpr::Named(name, _) => Some(name.as_str()),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => match ty.as_ref() {
            TypeExpr::Named(name, _) => Some(name.as_str()),
            other => return struct_field_type(module, other, field_name),
        },
        _ => None,
    }?;
    struct_field_type_by_name(module, struct_name, field_name)
}

fn struct_field_type_by_name(
    module: &Module,
    struct_name: &str,
    field_name: &str,
) -> Option<TypeExpr> {
    for item in &module.items {
        if let Item::Struct(def) = item {
            if def.name == struct_name {
                for field in &def.fields {
                    if let Some(name) = &field.name {
                        if name == field_name {
                            return Some(field.ty.ty.clone());
                        }
                    }
                }
            }
        }
    }
    None
}

fn returns_to_type(returns: &[TypeAnnotation]) -> TypeExpr {
    match returns.len() {
        0 => TypeExpr::Unit,
        1 => returns[0].ty.clone(),
        _ => TypeExpr::Tuple(returns.iter().map(|ret| ret.ty.clone()).collect()),
    }
}

fn function_return_type(module: &Module, name: &str) -> Option<TypeExpr> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == name => {
                return Some(returns_to_type(&func.returns));
            }
            Item::Impl(block) => {
                for method in &block.methods {
                    if method.name == name {
                        return Some(returns_to_type(&method.returns));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn infer_expr_type(expr: &Expr, module: &Module) -> Option<TypeExpr> {
    match expr {
        Expr::StructLiteral { name, .. } => Some(TypeExpr::Named(name.clone(), Vec::new())),
        Expr::Call { callee, .. } => match &**callee {
            Expr::Identifier(ident) => function_return_type(module, &ident.name),
            _ => None,
        },
        Expr::Closure { params, ret, .. } => {
            let mut param_tys = Vec::new();
            for param in params {
                if let Some(ann) = &param.ty {
                    param_tys.push(ann.ty.clone());
                } else {
                    return None;
                }
            }
            let returns = ret
                .as_ref()
                .map(|ann| vec![ann.ty.clone()])
                .unwrap_or_else(Vec::new);
            Some(TypeExpr::Function {
                params: param_tys,
                returns,
            })
        }
        Expr::Tuple(values, _) => {
            let mut types = Vec::new();
            for value in values {
                types.push(infer_expr_type(value, module)?);
            }
            Some(TypeExpr::Tuple(types))
        }
        Expr::ArrayLiteral(values, _) => {
            if values.is_empty() {
                None
            } else {
                let element_ty = infer_expr_type(&values[0], module)?;
                Some(TypeExpr::Array {
                    size: values.len(),
                    ty: Box::new(element_ty),
                })
            }
        }
        Expr::MapLiteral { entries, .. } => {
            let value_ty = entries
                .first()
                .and_then(|entry| infer_expr_type(&entry.value, module))?;
            Some(TypeExpr::Named(
                "Map".into(),
                vec![TypeExpr::Named("string".into(), Vec::new()), value_ty],
            ))
        }
        Expr::Reference {
            mutable,
            expr: inner,
            ..
        } => {
            let inner_ty = infer_expr_type(inner, module)?;
            Some(TypeExpr::Reference {
                mutable: *mutable,
                ty: Box::new(inner_ty),
            })
        }
        _ => None,
    }
}

fn infer_expr_type_with_decls(
    expr: &Expr,
    module: &Module,
    decls: &[DeclInfo],
) -> Option<TypeExpr> {
    match expr {
        Expr::Call { callee, .. } => match &**callee {
            Expr::Identifier(ident) => {
                if let Some(decl_ty) = decls
                    .iter()
                    .rev()
                    .find(|decl| decl.name == ident.name)
                    .and_then(|decl| decl.ty.clone())
                {
                    if let TypeExpr::Function { returns, .. } = decl_ty {
                        return match returns.len() {
                            0 => Some(TypeExpr::Unit),
                            1 => Some(returns[0].clone()),
                            _ => Some(TypeExpr::Tuple(returns)),
                        };
                    }
                }
                function_return_type(module, &ident.name)
            }
            _ => infer_expr_type(expr, module),
        },
        _ => infer_expr_type(expr, module),
    }
}

fn collect_used_identifiers(module: &Module) -> HashSet<String> {
    let mut used = HashSet::new();
    for item in &module.items {
        collect_used_in_item(item, &mut used);
    }
    used
}

fn collect_used_in_item(item: &Item, used: &mut HashSet<String>) {
    match item {
        Item::Function(func) => collect_used_in_function(func, used),
        Item::Impl(block) => {
            for method in &block.methods {
                collect_used_in_function(method, used);
            }
        }
        Item::Const(def) => collect_expr_idents(&def.value, used),
        Item::Macro(def) => collect_used_in_macro(def, used),
        Item::MacroInvocation(inv) => {
            for arg in &inv.args {
                collect_expr_idents(&arg.expr, used);
            }
        }
        _ => {}
    }
}

fn collect_used_in_function(func: &FunctionDef, used: &mut HashSet<String>) {
    match &func.body {
        FunctionBody::Block(block) => collect_used_in_block(block, used),
        FunctionBody::Expr(expr) => collect_expr_idents(&expr.node, used),
    }
}

fn collect_used_in_macro(def: &MacroDef, used: &mut HashSet<String>) {
    let params: HashSet<String> = def.params.iter().map(|p| p.name.clone()).collect();
    collect_used_in_macro_body(&def.body, &params, used);
}

fn collect_used_in_macro_body(
    body: &MacroBody,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match body {
        MacroBody::Block(block) => collect_macro_used_in_block(block, params, used),
        MacroBody::Expr(expr) => collect_macro_expr_idents(&expr.node, params, used),
        MacroBody::Items(items, _) => {
            for item in items {
                collect_macro_used_in_item(item, params, used);
            }
        }
    }
}

fn collect_macro_used_in_item(item: &Item, params: &HashSet<String>, used: &mut HashSet<String>) {
    match item {
        Item::Function(func) => collect_macro_used_in_function(func, params, used),
        Item::Impl(block) => {
            for method in &block.methods {
                collect_macro_used_in_function(method, params, used);
            }
        }
        Item::Const(def) => collect_macro_expr_idents(&def.value, params, used),
        Item::MacroInvocation(inv) => {
            for arg in &inv.args {
                collect_macro_expr_idents(&arg.expr, params, used);
            }
        }
        Item::Macro(def) => collect_used_in_macro(def, used),
        _ => {}
    }
}

fn collect_macro_used_in_function(
    func: &FunctionDef,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match &func.body {
        FunctionBody::Block(block) => collect_macro_used_in_block(block, params, used),
        FunctionBody::Expr(expr) => collect_macro_expr_idents(&expr.node, params, used),
    }
}

fn collect_macro_used_in_block(
    block: &Block,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    for statement in &block.statements {
        collect_macro_used_in_statement(statement, params, used);
    }
    if let Some(tail) = &block.tail {
        collect_macro_expr_idents(tail, params, used);
    }
}

fn collect_macro_used_in_statement(
    statement: &Statement,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match statement {
        Statement::Let(stmt) => {
            collect_macro_pattern_usage(&stmt.pattern, params, used);
            if let Some(value) = &stmt.value {
                collect_macro_expr_idents(value, params, used);
            }
        }
        Statement::MacroSemi(expr) => collect_macro_expr_idents(&expr.node, params, used),
        Statement::Assign(stmt) => {
            collect_macro_expr_idents(&stmt.target, params, used);
            collect_macro_expr_idents(&stmt.value, params, used);
        }
        Statement::Expr(expr) => collect_macro_expr_idents(&expr.expr, params, used),
        Statement::Return(ret) => {
            for value in &ret.values {
                collect_macro_expr_idents(value, params, used);
            }
        }
        Statement::While(while_stmt) => {
            collect_macro_expr_idents_from_while_condition(&while_stmt.condition, params, used);
            collect_macro_used_in_block(&while_stmt.body, params, used);
        }
        Statement::Loop(loop_stmt) => {
            collect_macro_used_in_block(&loop_stmt.body, params, used);
        }
        Statement::For(for_stmt) => {
            match &for_stmt.target {
                ForTarget::Range(range) => {
                    collect_macro_expr_idents(&range.start, params, used);
                    collect_macro_expr_idents(&range.end, params, used);
                }
                ForTarget::Collection(expr) => collect_macro_expr_idents(expr, params, used),
            }
            collect_macro_used_in_block(&for_stmt.body, params, used);
        }
        Statement::Defer(defer_stmt) => collect_macro_expr_idents(&defer_stmt.expr, params, used),
        Statement::Block(block) => collect_macro_used_in_block(block, params, used),
        Statement::Break | Statement::Continue => {}
    }
}

fn collect_macro_expr_idents(expr: &Expr, params: &HashSet<String>, used: &mut HashSet<String>) {
    match expr {
        Expr::Identifier(ident) => {
            used.insert(ident.name.clone());
        }
        Expr::Literal(_) => {}
        Expr::MacroCall { args, .. } => {
            for arg in args {
                collect_macro_expr_idents(&arg.expr, params, used);
            }
        }
        Expr::Try { block, .. } => collect_macro_used_in_block(block, params, used),
        Expr::TryPropagate { expr: inner, .. } => collect_macro_expr_idents(inner, params, used),
        Expr::Binary { left, right, .. } => {
            collect_macro_expr_idents(left, params, used);
            collect_macro_expr_idents(right, params, used);
        }
        Expr::Closure { body, .. } => match body {
            ClosureBody::Block(block) => collect_macro_used_in_block(block, params, used),
            ClosureBody::Expr(expr) => collect_macro_expr_idents(expr.node.as_ref(), params, used),
        },
        Expr::Unary { expr: inner, .. } => collect_macro_expr_idents(inner, params, used),
        Expr::Call { callee, args, .. } => {
            collect_macro_expr_idents(callee, params, used);
            for arg in args {
                collect_macro_expr_idents(arg, params, used);
            }
        }
        Expr::FieldAccess { base, .. } => collect_macro_expr_idents(base, params, used),
        Expr::Index { base, index, .. } => {
            collect_macro_expr_idents(base, params, used);
            collect_macro_expr_idents(index, params, used);
        }
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    collect_macro_expr_idents(&field.value, params, used);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_macro_expr_idents(value, params, used);
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                collect_macro_expr_idents(value, params, used);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                collect_macro_expr_idents(&entry.key, params, used);
                collect_macro_expr_idents(&entry.value, params, used);
            }
        }
        Expr::Block(block) => collect_macro_used_in_block(block, params, used),
        Expr::If(if_expr) => collect_macro_used_in_if_expr(if_expr, params, used),
        Expr::Match(match_expr) => {
            collect_macro_expr_idents(&match_expr.expr, params, used);
            for arm in &match_expr.arms {
                collect_macro_pattern_usage(&arm.pattern, params, used);
                if let Some(guard) = &arm.guard {
                    collect_macro_expr_idents(guard, params, used);
                }
                collect_macro_expr_idents(&arm.value, params, used);
            }
        }
        Expr::Tuple(values, _) | Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_macro_expr_idents(value, params, used);
            }
        }
        Expr::Range(range) => collect_macro_range_expr(range, params, used),
        Expr::Reference { expr: inner, .. } => collect_macro_expr_idents(inner, params, used),
        Expr::Deref { expr: inner, .. } => collect_macro_expr_idents(inner, params, used),
        Expr::Move { expr: inner, .. } => collect_macro_expr_idents(inner, params, used),
        Expr::Async { block, .. } => collect_macro_used_in_block(block, params, used),
        Expr::Await { expr, .. } => collect_macro_expr_idents(expr, params, used),
        Expr::Spawn { expr, .. } => collect_macro_expr_idents(expr, params, used),
        Expr::FormatString(literal) => collect_macro_format_string_idents(literal, params, used),
    }
}

fn collect_macro_range_expr(
    range: &RangeExpr,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    collect_macro_expr_idents(&range.start, params, used);
    collect_macro_expr_idents(&range.end, params, used);
}

fn collect_macro_format_string_idents(
    literal: &FormatStringLiteral,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    for segment in &literal.segments {
        match segment {
            FormatSegment::Expr { expr, .. } => collect_macro_expr_idents(expr, params, used),
            FormatSegment::Literal(_) | FormatSegment::Implicit(_) => {}
        }
    }
}

fn collect_macro_used_in_if_expr(
    if_expr: &IfExpr,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match &if_expr.condition {
        IfCondition::Expr(expr) => collect_macro_expr_idents(expr, params, used),
        IfCondition::Let { pattern, value } => {
            collect_macro_pattern_usage(pattern, params, used);
            collect_macro_expr_idents(value, params, used);
        }
    }
    collect_macro_used_in_block(&if_expr.then_branch, params, used);
    if let Some(else_branch) = &if_expr.else_branch {
        collect_macro_used_in_else_branch(else_branch, params, used);
    }
}

fn collect_macro_used_in_else_branch(
    else_branch: &ElseBranch,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match else_branch {
        ElseBranch::Block(block) => collect_macro_used_in_block(block, params, used),
        ElseBranch::ElseIf(if_expr) => collect_macro_used_in_if_expr(if_expr, params, used),
    }
}

fn collect_macro_expr_idents_from_while_condition(
    condition: &WhileCondition,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match condition {
        WhileCondition::Expr(expr) => collect_macro_expr_idents(expr, params, used),
        WhileCondition::Let { pattern, value } => {
            collect_macro_pattern_usage(pattern, params, used);
            collect_macro_expr_idents(value, params, used);
        }
    }
}

fn collect_macro_pattern_usage(
    pattern: &Pattern,
    params: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match pattern {
        Pattern::Identifier(name, _) => {
            if params.contains(name) {
                used.insert(name.clone());
            }
        }
        Pattern::Tuple(bindings, _) => {
            for b in bindings {
                collect_macro_pattern_usage(b, params, used);
            }
        }
        Pattern::Map(entries, _) => {
            for entry in entries {
                collect_macro_pattern_usage(&entry.pattern, params, used);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                collect_macro_pattern_usage(&field.pattern, params, used);
            }
        }
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                collect_macro_pattern_usage(p, params, used);
            }
            if let Some(rest) = rest {
                collect_macro_pattern_usage(rest, params, used);
            }
            for p in suffix {
                collect_macro_pattern_usage(p, params, used);
            }
        }
        Pattern::EnumVariant { bindings, .. } => {
            for b in bindings {
                collect_macro_pattern_usage(b, params, used);
            }
        }
        Pattern::Literal(_) | Pattern::Wildcard => {}
    }
}

fn collect_used_in_block(block: &Block, used: &mut HashSet<String>) {
    for statement in &block.statements {
        collect_used_in_statement(statement, used);
    }
    if let Some(tail) = &block.tail {
        collect_expr_idents(tail, used);
    }
}

fn collect_used_in_statement(statement: &Statement, used: &mut HashSet<String>) {
    match statement {
        Statement::Let(stmt) => {
            if let Some(value) = &stmt.value {
                collect_expr_idents(value, used);
            }
        }
        Statement::MacroSemi(expr) => collect_expr_idents(&expr.node, used),
        Statement::Assign(stmt) => {
            collect_expr_idents(&stmt.target, used);
            collect_expr_idents(&stmt.value, used);
        }
        Statement::Expr(expr) => collect_expr_idents(&expr.expr, used),
        Statement::Return(ret) => {
            for value in &ret.values {
                collect_expr_idents(value, used);
            }
        }
        Statement::While(while_stmt) => {
            collect_expr_idents_from_while_condition(&while_stmt.condition, used);
            collect_used_in_block(&while_stmt.body, used);
        }
        Statement::Loop(loop_stmt) => {
            collect_used_in_block(&loop_stmt.body, used);
        }
        Statement::For(for_stmt) => {
            match &for_stmt.target {
                ForTarget::Range(range) => {
                    collect_expr_idents(&range.start, used);
                    collect_expr_idents(&range.end, used);
                }
                ForTarget::Collection(expr) => collect_expr_idents(expr, used),
            }
            collect_used_in_block(&for_stmt.body, used);
        }
        Statement::Defer(defer_stmt) => collect_expr_idents(&defer_stmt.expr, used),
        Statement::Block(block) => collect_used_in_block(block, used),
        Statement::Break | Statement::Continue => {}
    }
}

fn collect_expr_idents(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Identifier(ident) => {
            used.insert(ident.name.clone());
        }
        Expr::Literal(_) => {}
        Expr::MacroCall { args, .. } => {
            for arg in args {
                collect_expr_idents(&arg.expr, used);
            }
        }
        Expr::Closure { body, .. } => match body {
            ClosureBody::Block(block) => collect_used_in_block(block, used),
            ClosureBody::Expr(expr) => collect_expr_idents(expr.node.as_ref(), used),
        },
        Expr::Try { block, .. } => collect_used_in_block(block, used),
        Expr::TryPropagate { expr: inner, .. } => collect_expr_idents(inner, used),
        Expr::Binary { left, right, .. } => {
            collect_expr_idents(left, used);
            collect_expr_idents(right, used);
        }
        Expr::Unary { expr: inner, .. } => collect_expr_idents(inner, used),
        Expr::Call { callee, args, .. } => {
            collect_expr_idents(callee, used);
            for arg in args {
                collect_expr_idents(arg, used);
            }
        }
        Expr::FieldAccess { base, .. } => collect_expr_idents(base, used),
        Expr::Index { base, index, .. } => {
            collect_expr_idents(base, used);
            collect_expr_idents(index, used);
        }
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    collect_expr_idents(&field.value, used);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_expr_idents(value, used);
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                collect_expr_idents(value, used);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                collect_expr_idents(&entry.key, used);
                collect_expr_idents(&entry.value, used);
            }
        }
        Expr::Block(block) => collect_used_in_block(block, used),
        Expr::If(if_expr) => collect_used_in_if_expr(if_expr, used),
        Expr::Match(match_expr) => {
            collect_expr_idents(&match_expr.expr, used);
            for arm in &match_expr.arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_idents(guard, used);
                }
                collect_expr_idents(&arm.value, used);
            }
        }
        Expr::Tuple(values, _) | Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_expr_idents(value, used);
            }
        }
        Expr::Range(range) => collect_range_expr(range, used),
        Expr::Reference { expr: inner, .. } => collect_expr_idents(inner, used),
        Expr::Deref { expr: inner, .. } => collect_expr_idents(inner, used),
        Expr::Move { expr: inner, .. } => collect_expr_idents(inner, used),
        Expr::Async { block, .. } => collect_used_in_block(block, used),
        Expr::Await { expr, .. } => collect_expr_idents(expr, used),
        Expr::Spawn { expr, .. } => collect_expr_idents(expr, used),
        Expr::FormatString(literal) => collect_format_string_idents(literal, used),
    }
}

fn collect_range_expr(range: &RangeExpr, used: &mut HashSet<String>) {
    collect_expr_idents(&range.start, used);
    collect_expr_idents(&range.end, used);
}

fn collect_format_string_idents(literal: &FormatStringLiteral, used: &mut HashSet<String>) {
    for segment in &literal.segments {
        match segment {
            FormatSegment::Expr { expr, .. } => collect_expr_idents(expr, used),
            FormatSegment::Literal(_) | FormatSegment::Implicit(_) => {}
        }
    }
}

fn span_contains(span: Span, offset: usize) -> bool {
    offset >= span.start && offset < span.end
}

pub fn identifier_at_offset(module: &Module, offset: usize) -> Option<(String, Span)> {
    for item in &module.items {
        if let Some(found) = find_in_item(item, offset) {
            return Some(found);
        }
    }
    None
}

pub fn collect_identifier_spans(module: &Module, name: &str) -> Vec<Span> {
    let mut spans = Vec::new();
    for item in &module.items {
        collect_spans_in_item(item, name, &mut spans);
    }
    spans
}

pub fn collect_identifier_spans_in_scope_ast(
    module: &Module,
    name: &str,
    scope: Span,
) -> Vec<Span> {
    collect_identifier_spans(module, name)
        .into_iter()
        .filter(|span| span.start >= scope.start && span.start < scope.end)
        .collect()
}

pub fn collect_identifier_spans_for_decl(
    module: &Module,
    tokens: &[Token],
    decl: &DeclInfo,
) -> Vec<Span> {
    let mut spans = Vec::new();
    spans.push(decl.span);
    spans.extend(collect_identifier_spans_in_scope(
        tokens, &decl.name, decl.scope,
    ));
    spans.extend(collect_identifier_spans_in_scope_ast(
        module, &decl.name, decl.scope,
    ));
    spans.sort_by(|a, b| a.start.cmp(&b.start).then_with(|| a.end.cmp(&b.end)));
    spans.dedup();
    spans
}

fn collect_spans_in_item(item: &Item, name: &str, spans: &mut Vec<Span>) {
    match item {
        Item::Function(func) => collect_spans_in_function(func, name, spans),
        Item::Const(def) => collect_spans_in_expr(&def.value, name, spans),
        Item::Impl(block) => {
            for method in &block.methods {
                collect_spans_in_function(method, name, spans);
            }
        }
        Item::MacroInvocation(inv) => collect_spans_in_macro_invocation(inv, name, spans),
        Item::Macro(def) => collect_spans_in_macro(def, name, spans),
        Item::Struct(_) | Item::Enum(_) | Item::Interface(_) => {}
    }
}

fn collect_spans_in_function(func: &FunctionDef, name: &str, spans: &mut Vec<Span>) {
    for param in &func.params {
        if param.name == name {
            spans.push(param.span);
        }
    }
    match &func.body {
        FunctionBody::Expr(expr) => collect_spans_in_expr(&expr.node, name, spans),
        FunctionBody::Block(block) => collect_spans_in_block(block, name, spans),
    }
}

fn collect_spans_in_macro_invocation(inv: &MacroInvocation, name: &str, spans: &mut Vec<Span>) {
    if inv.name.name == name {
        spans.push(inv.name.span);
    }
    for arg in &inv.args {
        collect_spans_in_expr(&arg.expr, name, spans);
    }
}

fn collect_spans_in_block(block: &Block, name: &str, spans: &mut Vec<Span>) {
    for stmt in &block.statements {
        collect_spans_in_statement(stmt, name, spans);
    }
    if let Some(tail) = &block.tail {
        collect_spans_in_expr(tail, name, spans);
    }
}

fn collect_spans_in_statement(stmt: &Statement, name: &str, spans: &mut Vec<Span>) {
    match stmt {
        Statement::Let(let_stmt) => {
            collect_spans_in_pattern(&let_stmt.pattern, name, spans);
            if let Some(expr) = &let_stmt.value {
                collect_spans_in_expr(expr, name, spans);
            }
        }
        Statement::Assign(assign) => {
            collect_spans_in_expr(&assign.target, name, spans);
            collect_spans_in_expr(&assign.value, name, spans);
        }
        Statement::Expr(expr) => collect_spans_in_expr(&expr.expr, name, spans),
        Statement::Return(ret) => {
            for value in &ret.values {
                collect_spans_in_expr(value, name, spans);
            }
        }
        Statement::While(while_stmt) => {
            match &while_stmt.condition {
                WhileCondition::Expr(expr) => collect_spans_in_expr(expr, name, spans),
                WhileCondition::Let { pattern, value } => {
                    collect_spans_in_pattern(pattern, name, spans);
                    collect_spans_in_expr(value, name, spans);
                }
            }
            collect_spans_in_block(&while_stmt.body, name, spans);
        }
        Statement::Loop(loop_stmt) => collect_spans_in_block(&loop_stmt.body, name, spans),
        Statement::For(for_stmt) => {
            if for_stmt.binding == name {
                spans.push(for_stmt.span);
            }
            match &for_stmt.target {
                ForTarget::Range(range) => {
                    collect_spans_in_expr(&range.start, name, spans);
                    collect_spans_in_expr(&range.end, name, spans);
                }
                ForTarget::Collection(expr) => collect_spans_in_expr(expr, name, spans),
            }
            collect_spans_in_block(&for_stmt.body, name, spans);
        }
        Statement::Defer(defer_stmt) => collect_spans_in_expr(&defer_stmt.expr, name, spans),
        Statement::Block(block) => collect_spans_in_block(block, name, spans),
        Statement::MacroSemi(expr) => collect_spans_in_expr(&expr.node, name, spans),
        Statement::Break | Statement::Continue => {}
    }
}

fn collect_spans_in_pattern(pattern: &Pattern, name: &str, spans: &mut Vec<Span>) {
    match pattern {
        Pattern::Identifier(ident, span) => {
            if ident == name {
                spans.push(*span);
            }
        }
        Pattern::Tuple(bindings, _) => {
            for b in bindings {
                collect_spans_in_pattern(b, name, spans);
            }
        }
        Pattern::Map(entries, _) => {
            for entry in entries {
                collect_spans_in_pattern(&entry.pattern, name, spans);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                collect_spans_in_pattern(&field.pattern, name, spans);
            }
        }
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                collect_spans_in_pattern(p, name, spans);
            }
            if let Some(rest) = rest {
                collect_spans_in_pattern(rest, name, spans);
            }
            for p in suffix {
                collect_spans_in_pattern(p, name, spans);
            }
        }
        Pattern::EnumVariant { bindings, .. } => {
            for b in bindings {
                collect_spans_in_pattern(b, name, spans);
            }
        }
        Pattern::Literal(_) | Pattern::Wildcard => {}
    }
}

fn collect_spans_in_format_string(
    literal: &FormatStringLiteral,
    name: &str,
    spans: &mut Vec<Span>,
) {
    for segment in &literal.segments {
        if let FormatSegment::Expr { expr, span } = segment {
            if let Some((found, s)) = find_in_expr(expr, span.start) {
                if found == name {
                    spans.push(s);
                }
            }
            collect_spans_in_expr(expr, name, spans);
        }
    }
}

fn collect_spans_in_expr(expr: &Expr, name: &str, spans: &mut Vec<Span>) {
    match expr {
        Expr::Identifier(ident) => {
            if ident.name == name {
                spans.push(ident.span);
            }
        }
        Expr::Literal(_) => {}
        Expr::FormatString(literal) => collect_spans_in_format_string(literal, name, spans),
        Expr::Try { block, .. } => collect_spans_in_block(block, name, spans),
        Expr::TryPropagate { expr, .. }
        | Expr::Reference { expr, .. }
        | Expr::Deref { expr, .. }
        | Expr::Move { expr, .. }
        | Expr::Spawn { expr, .. } => collect_spans_in_expr(expr, name, spans),
        Expr::Async { block, .. } => collect_spans_in_block(block, name, spans),
        Expr::Await { expr, .. } => collect_spans_in_expr(expr, name, spans),
        Expr::Closure { body, .. } => match body {
            ClosureBody::Block(block) => collect_spans_in_block(block, name, spans),
            ClosureBody::Expr(expr) => collect_spans_in_expr(expr.node.as_ref(), name, spans),
        },
        Expr::Binary { left, right, .. } => {
            collect_spans_in_expr(left, name, spans);
            collect_spans_in_expr(right, name, spans);
        }
        Expr::Unary { expr, .. } => collect_spans_in_expr(expr, name, spans),
        Expr::Call { callee, args, .. } => {
            collect_spans_in_expr(callee, name, spans);
            for arg in args {
                collect_spans_in_expr(arg, name, spans);
            }
        }
        Expr::FieldAccess { base, .. } => collect_spans_in_expr(base, name, spans),
        Expr::Index { base, index, .. } => {
            collect_spans_in_expr(base, name, spans);
            collect_spans_in_expr(index, name, spans);
        }
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(entries) => {
                for entry in entries {
                    collect_spans_in_expr(&entry.value, name, spans);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_spans_in_expr(value, name, spans);
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                collect_spans_in_expr(value, name, spans);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                collect_spans_in_expr(&entry.key, name, spans);
                collect_spans_in_expr(&entry.value, name, spans);
            }
        }
        Expr::Block(block) => collect_spans_in_block(block, name, spans),
        Expr::If(if_expr) => {
            match &if_expr.condition {
                IfCondition::Expr(expr) => collect_spans_in_expr(expr, name, spans),
                IfCondition::Let { pattern, value } => {
                    collect_spans_in_pattern(pattern, name, spans);
                    collect_spans_in_expr(value, name, spans);
                }
            }
            collect_spans_in_block(&if_expr.then_branch, name, spans);
            if let Some(else_branch) = &if_expr.else_branch {
                match else_branch {
                    ElseBranch::Block(block) => collect_spans_in_block(block, name, spans),
                    ElseBranch::ElseIf(expr) => {
                        collect_spans_in_expr(&Expr::If(Box::new((**expr).clone())), name, spans)
                    }
                }
            }
        }
        Expr::Match(match_expr) => {
            collect_spans_in_expr(&match_expr.expr, name, spans);
            for arm in &match_expr.arms {
                collect_spans_in_pattern(&arm.pattern, name, spans);
                if let Some(guard) = &arm.guard {
                    collect_spans_in_expr(guard, name, spans);
                }
                collect_spans_in_expr(&arm.value, name, spans);
            }
        }
        Expr::Tuple(values, _) => {
            for value in values {
                collect_spans_in_expr(value, name, spans);
            }
        }
        Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_spans_in_expr(value, name, spans);
            }
        }
        Expr::Range(range) => {
            collect_spans_in_expr(&range.start, name, spans);
            collect_spans_in_expr(&range.end, name, spans);
        }
        Expr::MacroCall { args, .. } => {
            for arg in args {
                collect_spans_in_expr(&arg.expr, name, spans);
            }
        }
    }
}

fn find_in_item(item: &Item, offset: usize) -> Option<(String, Span)> {
    match item {
        Item::Function(func) => find_in_function(func, offset),
        Item::Impl(block) => {
            for method in &block.methods {
                if let Some(found) = find_in_function(method, offset) {
                    return Some(found);
                }
            }
            None
        }
        Item::Const(def) => find_in_expr(&def.value, offset),
        Item::Macro(def) => find_in_macro(def, offset),
        _ => None,
    }
}

fn find_in_macro(def: &MacroDef, offset: usize) -> Option<(String, Span)> {
    match &def.body {
        MacroBody::Block(block) => find_in_block(block, offset),
        MacroBody::Expr(expr) => find_in_expr(&expr.node, offset),
        MacroBody::Items(items, _) => items.iter().find_map(|item| find_in_item(item, offset)),
    }
}

fn collect_spans_in_macro(def: &MacroDef, name: &str, spans: &mut Vec<Span>) {
    match &def.body {
        MacroBody::Block(block) => collect_spans_in_block(block, name, spans),
        MacroBody::Expr(expr) => collect_spans_in_expr(&expr.node, name, spans),
        MacroBody::Items(items, _) => {
            for item in items {
                collect_spans_in_item(item, name, spans);
            }
        }
    }
}

fn find_in_function(func: &FunctionDef, offset: usize) -> Option<(String, Span)> {
    match &func.body {
        FunctionBody::Block(block) => find_in_block(block, offset),
        FunctionBody::Expr(expr) => find_in_expr(&expr.node, offset),
    }
}

fn find_in_block(block: &Block, offset: usize) -> Option<(String, Span)> {
    if !span_contains(block.span, offset) {
        return None;
    }
    for stmt in &block.statements {
        if let Some(found) = find_in_statement(stmt, offset) {
            return Some(found);
        }
    }
    if let Some(tail) = &block.tail {
        return find_in_expr(tail, offset);
    }
    None
}

fn find_in_statement(stmt: &Statement, offset: usize) -> Option<(String, Span)> {
    match stmt {
        Statement::Let(let_stmt) => {
            if span_contains(let_stmt.span, offset) {
                if let Some(value) = &let_stmt.value {
                    if let Some(found) = find_in_expr(value, offset) {
                        return Some(found);
                    }
                }
            }
            None
        }
        Statement::Assign(assign) => {
            find_in_expr(&assign.target, offset).or_else(|| find_in_expr(&assign.value, offset))
        }
        Statement::MacroSemi(expr) => find_in_expr(&expr.node, offset),
        Statement::Expr(expr) => find_in_expr(&expr.expr, offset),
        Statement::Return(ret) => {
            for value in &ret.values {
                if let Some(found) = find_in_expr(value, offset) {
                    return Some(found);
                }
            }
            None
        }
        Statement::While(while_stmt) => {
            match &while_stmt.condition {
                WhileCondition::Expr(expr) => {
                    if let Some(found) = find_in_expr(expr, offset) {
                        return Some(found);
                    }
                }
                WhileCondition::Let { value, .. } => {
                    if let Some(found) = find_in_expr(value, offset) {
                        return Some(found);
                    }
                }
            }
            find_in_block(&while_stmt.body, offset)
        }
        Statement::Loop(loop_stmt) => find_in_block(&loop_stmt.body, offset),
        Statement::For(for_stmt) => {
            match &for_stmt.target {
                ForTarget::Range(range) => {
                    if let Some(found) = find_in_expr(&range.start, offset) {
                        return Some(found);
                    }
                    if let Some(found) = find_in_expr(&range.end, offset) {
                        return Some(found);
                    }
                }
                ForTarget::Collection(expr) => {
                    if let Some(found) = find_in_expr(expr, offset) {
                        return Some(found);
                    }
                }
            }
            find_in_block(&for_stmt.body, offset)
        }
        Statement::Defer(defer_stmt) => find_in_expr(&defer_stmt.expr, offset),
        Statement::Block(block) => find_in_block(block, offset),
        Statement::Break | Statement::Continue => None,
    }
}

fn find_in_if(if_expr: &IfExpr, offset: usize) -> Option<(String, Span)> {
    if !span_contains(if_expr.span, offset) {
        return None;
    }
    match &if_expr.condition {
        IfCondition::Expr(expr) => {
            if let Some(found) = find_in_expr(expr, offset) {
                return Some(found);
            }
        }
        IfCondition::Let { value, .. } => {
            if let Some(found) = find_in_expr(value, offset) {
                return Some(found);
            }
        }
    }
    if let Some(found) = find_in_block(&if_expr.then_branch, offset) {
        return Some(found);
    }
    if let Some(else_branch) = &if_expr.else_branch {
        match else_branch {
            ElseBranch::Block(block) => return find_in_block(block, offset),
            ElseBranch::ElseIf(expr) => return find_in_if(expr, offset),
        }
    }
    None
}

fn find_in_match(match_expr: &MatchExpr, offset: usize) -> Option<(String, Span)> {
    if !span_contains(match_expr.span, offset) {
        return None;
    }
    if let Some(found) = find_in_expr(&match_expr.expr, offset) {
        return Some(found);
    }
    for arm in &match_expr.arms {
        if let Some(guard) = &arm.guard {
            if let Some(found) = find_in_expr(guard, offset) {
                return Some(found);
            }
        }
        if let Some(found) = find_in_expr(&arm.value, offset) {
            return Some(found);
        }
    }
    None
}

fn find_in_format_string(literal: &FormatStringLiteral, offset: usize) -> Option<(String, Span)> {
    for segment in &literal.segments {
        match segment {
            FormatSegment::Expr { expr, span } => {
                if span_contains(*span, offset) {
                    if let Some(found) = find_in_expr(expr, offset) {
                        return Some(found);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_in_expr(expr: &Expr, offset: usize) -> Option<(String, Span)> {
    match expr {
        Expr::Identifier(ident) if span_contains(ident.span, offset) => {
            Some((ident.name.clone(), ident.span))
        }
        Expr::Literal(_) => None,
        Expr::FormatString(literal) => find_in_format_string(literal, offset),
        Expr::Try { block, span } => {
            if span_contains(*span, offset) {
                find_in_block(block, offset)
            } else {
                None
            }
        }
        Expr::TryPropagate { expr: inner, span }
        | Expr::Reference {
            expr: inner, span, ..
        }
        | Expr::Deref {
            expr: inner, span, ..
        }
        | Expr::Move {
            expr: inner, span, ..
        }
        | Expr::Spawn {
            expr: inner, span, ..
        } => {
            if span_contains(*span, offset) {
                find_in_expr(inner, offset)
            } else {
                None
            }
        }
        Expr::Binary {
            left, right, span, ..
        } => {
            if span_contains(*span, offset) {
                find_in_expr(left, offset).or_else(|| find_in_expr(right, offset))
            } else {
                None
            }
        }
        Expr::Unary {
            expr: inner, span, ..
        } => {
            if span_contains(*span, offset) {
                find_in_expr(inner, offset)
            } else {
                None
            }
        }
        Expr::Call {
            callee, args, span, ..
        } => {
            if !span_contains(*span, offset) {
                return None;
            }
            if let Some(found) = find_in_expr(callee, offset) {
                return Some(found);
            }
            for arg in args {
                if let Some(found) = find_in_expr(arg, offset) {
                    return Some(found);
                }
            }
            None
        }
        Expr::FieldAccess {
            base, span, field, ..
        } => {
            if !span_contains(*span, offset) {
                return None;
            }
            let field_len = field.len();
            let field_start = span.end.saturating_sub(field_len);
            let field_span = Span::new(field_start, span.end);
            if span_contains(field_span, offset) {
                return Some((field.clone(), field_span));
            }
            find_in_expr(base, offset)
        }
        Expr::StructLiteral { fields, span, .. } => {
            if !span_contains(*span, offset) {
                return None;
            }
            match fields {
                StructLiteralKind::Named(entries) => {
                    for entry in entries {
                        if let Some(found) = find_in_expr(&entry.value, offset) {
                            return Some(found);
                        }
                    }
                }
                StructLiteralKind::Positional(values) => {
                    for value in values {
                        if let Some(found) = find_in_expr(value, offset) {
                            return Some(found);
                        }
                    }
                }
            }
            None
        }
        Expr::MapLiteral { entries, span } => {
            if !span_contains(*span, offset) {
                return None;
            }
            for entry in entries {
                if let Some(found) = find_in_expr(&entry.key, offset) {
                    return Some(found);
                }
                if let Some(found) = find_in_expr(&entry.value, offset) {
                    return Some(found);
                }
            }
            None
        }
        Expr::Block(block) => find_in_block(block, offset),
        Expr::If(if_expr) => find_in_if(if_expr, offset),
        Expr::Match(match_expr) => find_in_match(match_expr, offset),
        Expr::Tuple(values, span) | Expr::ArrayLiteral(values, span) => {
            if !span_contains(*span, offset) {
                return None;
            }
            for value in values {
                if let Some(found) = find_in_expr(value, offset) {
                    return Some(found);
                }
            }
            None
        }
        Expr::Range(range) => {
            if !span_contains(range.span, offset) {
                return None;
            }
            find_in_expr(&range.start, offset).or_else(|| find_in_expr(&range.end, offset))
        }
        _ => None,
    }
}

fn collect_used_in_if_expr(if_expr: &IfExpr, used: &mut HashSet<String>) {
    match &if_expr.condition {
        IfCondition::Expr(expr) => collect_expr_idents(expr, used),
        IfCondition::Let { value, .. } => collect_expr_idents(value, used),
    }
    collect_used_in_block(&if_expr.then_branch, used);
    if let Some(else_branch) = &if_expr.else_branch {
        collect_used_in_else_branch(else_branch, used);
    }
}

fn collect_expr_idents_from_while_condition(
    condition: &WhileCondition,
    used: &mut HashSet<String>,
) {
    match condition {
        WhileCondition::Expr(expr) => collect_expr_idents(expr, used),
        WhileCondition::Let { value, .. } => collect_expr_idents(value, used),
    }
}

fn collect_used_in_else_branch(else_branch: &ElseBranch, used: &mut HashSet<String>) {
    match else_branch {
        ElseBranch::Block(block) => collect_used_in_block(block, used),
        ElseBranch::ElseIf(if_expr) => collect_used_in_if_expr(if_expr, used),
    }
}

fn pattern_span(pattern: &Pattern) -> Span {
    match pattern {
        Pattern::Wildcard => Span::new(0, 0),
        Pattern::Identifier(_, span) => *span,
        Pattern::Literal(lit) => literal_span(lit),
        Pattern::EnumVariant { bindings, .. } => bindings
            .first()
            .map(pattern_span)
            .unwrap_or_else(|| Span::new(0, 0)),
        Pattern::Tuple(_, span) => *span,
        Pattern::Map(_, span) => *span,
        Pattern::Struct { span, .. } => *span,
        Pattern::Slice { span, .. } => *span,
    }
}

fn literal_span(lit: &Literal) -> Span {
    match lit {
        Literal::Int(_, span)
        | Literal::Float(_, span)
        | Literal::Bool(_, span)
        | Literal::String(_, span)
        | Literal::Rune(_, span) => *span,
    }
}

pub fn visible_locals(module: &Module, offset: usize) -> Vec<DeclInfo> {
    collect_decl_spans(module)
        .into_iter()
        .filter(|decl| scope_contains(decl.scope, offset))
        .filter(|decl| offset >= decl.available_from)
        .collect()
}

pub fn receiver_type_name(expr: &TypeExpr) -> Option<String> {
    struct_name_from_type(expr).map(|name| name.to_string())
}

fn struct_name_from_type<'a>(ty: &'a TypeExpr) -> Option<&'a str> {
    match ty {
        TypeExpr::Named(name, _) => Some(name),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => struct_name_from_type(ty),
        _ => None,
    }
}

pub fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(Literal::Int(_, span))
        | Expr::Literal(Literal::Float(_, span))
        | Expr::Literal(Literal::Bool(_, span))
        | Expr::Literal(Literal::String(_, span))
        | Expr::Literal(Literal::Rune(_, span)) => *span,
        Expr::Try { span, .. } => *span,
        Expr::TryPropagate { span, .. } => *span,
        Expr::Binary { span, .. } => *span,
        Expr::Unary { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::MacroCall { span, .. } => *span,
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::MapLiteral { span, .. } => *span,
        Expr::EnumLiteral { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(if_expr) => if_expr.span,
        Expr::Match(match_expr) => match_expr.span,
        Expr::Tuple(_, span) => *span,
        Expr::ArrayLiteral(_, span) => *span,
        Expr::Range(range) => range.span,
        Expr::Reference { span, .. } => *span,
        Expr::Deref { span, .. } => *span,
        Expr::Move { span, .. } => *span,
        Expr::Index { span, .. } => *span,
        Expr::FormatString(literal) => literal.span,
        Expr::Async { span, .. } => *span,
        Expr::Await { span, .. } => *span,
        Expr::Spawn { span, .. } => *span,
        Expr::Closure { span, .. } => *span,
    }
}

pub fn scope_contains(scope: Span, offset: usize) -> bool {
    offset >= scope.start && offset < scope.end
}

pub fn find_local_decl(module: &Module, name: &str, offset: usize) -> Option<DeclInfo> {
    let decls: Vec<_> = collect_decl_spans(module)
        .into_iter()
        .filter(|decl| decl.name == name)
        .collect();
    decls
        .iter()
        .filter(|decl| scope_contains(decl.scope, offset))
        .filter(|decl| offset >= decl.available_from || offset <= decl.span.end)
        .max_by_key(|decl| decl.span.start)
        .cloned()
}

pub fn find_local_definition_span(module: &Module, name: &str, offset: usize) -> Option<Span> {
    if let Some(decl) = find_local_decl(module, name, offset) {
        return Some(decl.span);
    }
    find_module_item_span(module, name)
}

pub fn find_module_item_span(module: &Module, name: &str) -> Option<Span> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == name => return Some(func.name_span),
            Item::Struct(def) if def.name == name => return Some(def.span),
            Item::Enum(def) if def.name == name => return Some(def.span),
            Item::Interface(def) if def.name == name => return Some(def.span),
            Item::Const(def) if def.name == name => return Some(def.span),
            Item::Macro(def) if def.name == name => return Some(def.name_span),
            Item::Impl(_) => {}
            _ => {}
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::{
        collect_identifier_spans_for_decl, collect_identifier_spans_in_scope_ast, find_local_decl,
        find_local_definition_span, unused_variable_diagnostics,
    };
    use crate::language::{lexer::lex, parser::parse_module};
    use std::path::PathBuf;

    #[test]
    fn format_string_counts_as_usage_for_unused_analysis() {
        let source = r#"
module tests::fmt;

fn main() {
  let int32 hp = 5;
  out(`hp is {hp}`);
}
"#;
        let module = parse_module("tests::fmt", PathBuf::from("fmt.prime"), source).expect("parse");
        let diags = unused_variable_diagnostics(&module, source);
        assert!(
            diags.iter().all(|diag| !diag.message.contains("hp")),
            "expected no unused-variable diagnostic for identifiers referenced in format strings, found {diags:?}"
        );
    }

    #[test]
    fn macro_params_support_definition_lookup() {
        let source = r#"
module tests::macro_defs;

macro dbg(label: string) -> string {
  out(`{label}`);
  label
}
"#;
        let module = parse_module("tests::macro_defs", PathBuf::from("macros.prime"), source)
            .expect("parse");
        let offset = source.find("{label}").expect("placeholder") + 1;
        let span = find_local_definition_span(&module, "label", offset)
            .expect("definition should resolve");
        let macro_def = match &module.items[0] {
            crate::language::ast::Item::Macro(def) => def,
            _ => panic!("expected macro item"),
        };
        assert_eq!(span, macro_def.params[0].span);
    }

    #[test]
    fn format_placeholders_in_macros_are_in_scope() {
        let source = r#"
module tests::macro_fmt;

macro tag(name: string) -> string {
  let string local = name;
  `{local} {name}`
}
"#;
        let module = parse_module("tests::macro_fmt", PathBuf::from("macro_fmt.prime"), source)
            .expect("parse");
        let macro_def = match &module.items[0] {
            crate::language::ast::Item::Macro(def) => def,
            _ => panic!("expected macro item"),
        };
        let block_span = match &macro_def.body {
            crate::language::ast::MacroBody::Block(block) => block.span,
            other => panic!("expected block macro body, found {other:?}"),
        };
        let spans = collect_identifier_spans_in_scope_ast(&module, "name", block_span);
        let placeholder_offset = source.find("{name}").expect("placeholder") + 1;
        assert!(
            spans
                .iter()
                .any(|span| placeholder_offset >= span.start && placeholder_offset < span.end),
            "expected format placeholder to be included in scoped identifier spans, got {spans:?}"
        );
    }

    #[test]
    fn rename_spans_include_macro_params() {
        let source = r#"
module tests::macro_rename;

macro add_twice(a: int32, b: int32) -> int32 {
  a + b + b
}
"#;
        let module = parse_module(
            "tests::macro_rename",
            PathBuf::from("macro_rename.prime"),
            source,
        )
        .expect("parse");
        let tokens = lex(source).expect("lex");
        let usage_offset = source.find("b + b").expect("usage") + 2;
        let decl = find_local_decl(&module, "b", usage_offset).expect("decl should resolve");
        let spans = collect_identifier_spans_for_decl(&module, &tokens, &decl);
        assert!(
            spans.iter().any(|span| span == &decl.span),
            "expected declaration span to be included in rename spans, got {spans:?}"
        );
        assert!(
            spans.len() >= 2,
            "expected both declaration and usage spans, got {spans:?}"
        );
    }

    #[test]
    fn macro_params_count_as_used() {
        let source = r#"
module tests::macro_usage;

macro repeat(expr: tokens) -> int32 {
  expr + expr
}

macro apply_twice(body: block) {
  body;
  body;
}

macro match_literal(pat: pattern, value: int32) -> int32 {
  match value {
    pat => 7,
    _ => 0,
  }
}
"#;
        let module = parse_module(
            "tests::macro_usage",
            PathBuf::from("macro_usage.prime"),
            source,
        )
        .expect("parse");
        let diags = unused_variable_diagnostics(&module, source);
        assert!(
            diags.is_empty(),
            "expected no unused-variable diagnostics for macro params, got {diags:?}"
        );
    }

    #[test]
    fn local_definition_prefers_in_scope_over_earlier_params() {
        let source = r#"
module tests::scope;

fn drink_potion(p: Player, heal: int32) -> Player {
  p
}

fn heal(player: Player, boost: int32) -> Player {
  player
}

fn caller(leveled: Player) -> Player {
  heal(leveled, 8)
}
"#;
        let module =
            parse_module("tests::scope", PathBuf::from("scope.prime"), source).expect("parse");
        let offset = source.find("heal(leveled").expect("call site should exist");
        let span =
            find_local_definition_span(&module, "heal", offset).expect("definition should resolve");
        let fn_pos = source.find("fn heal").expect("fn heal should exist");
        let param_pos = source.find("heal: int32").expect("param heal should exist");
        assert!(
            span.start >= fn_pos && span.end > fn_pos,
            "expected span to cover function `heal`, got {span:?}"
        );
        assert!(
            span.start >= fn_pos && span.start <= fn_pos + "fn heal".len(),
            "resolved to unexpected span (likely a param): {span:?}, param at {param_pos}"
        );
    }
}
