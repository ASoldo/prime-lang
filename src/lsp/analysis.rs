use crate::language::{
    ast::{
        Block, ElseBranch, Expr, ForTarget, FunctionBody, FunctionDef, IfExpr, Item, LetStmt,
        Literal, Module, Pattern, RangeExpr, Statement, StructLiteralKind,
    },
    span::Span,
    types::{Mutability, TypeExpr},
};
use std::collections::HashSet;
use tower_lsp_server::lsp_types::{Diagnostic, DiagnosticSeverity};

use super::text::span_to_range;

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
    let used = collect_used_identifiers(module);
    decls
        .into_iter()
        .filter(|decl| !used.contains(&decl.name))
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
        match item {
            Item::Function(func) => collect_decl_from_function(func, None, &mut decls),
            Item::Impl(block) => {
                let target_ty = Some(TypeExpr::named(block.target.clone()));
                for func in &block.methods {
                    collect_decl_from_function(func, target_ty.clone(), &mut decls);
                }
            }
            _ => {}
        }
    }
    decls
}

fn collect_decl_from_function(
    func: &FunctionDef,
    receiver_override: Option<TypeExpr>,
    decls: &mut Vec<DeclInfo>,
) {
    let body_span = match &func.body {
        FunctionBody::Block(block) => block.span,
        FunctionBody::Expr(expr) => expr.span,
    };
    let available_from = body_span.start;
    for param in &func.params {
        let mut param_ty = param.ty.ty.clone();
        if param.name == "self" {
            if let Some(override_ty) = receiver_override.clone() {
                param_ty = override_ty;
            } else if let Some(resolved) = receiver_type_name(&param.ty.ty).map(TypeExpr::named) {
                param_ty = resolved;
            }
        }
        decls.push(DeclInfo {
            name: param.name.clone(),
            span: param.span,
            scope: body_span,
            available_from,
            ty: Some(param_ty),
            value_span: None,
            mutability: param.mutability,
            kind: DeclKind::Param,
        });
    }
    if let FunctionBody::Block(block) = &func.body {
        collect_decl_from_block(block, decls);
    }
}

fn collect_decl_from_block(block: &Block, decls: &mut Vec<DeclInfo>) {
    let scope = block.span;
    for statement in &block.statements {
        match statement {
            Statement::Let(stmt) => {
                if let Some(value) = &stmt.value {
                    collect_decl_from_expr(value, decls);
                }
                let mut ty = stmt.ty.as_ref().map(|annotation| annotation.ty.clone());
                if ty.is_none() {
                    ty = infer_type_from_let_value(stmt);
                }
                decls.push(DeclInfo {
                    name: stmt.name.clone(),
                    span: stmt.span,
                    scope,
                    available_from: stmt.span.end,
                    ty,
                    value_span: stmt.value.as_ref().map(expr_span),
                    mutability: stmt.mutability,
                    kind: DeclKind::Let,
                });
            }
            Statement::Assign(stmt) => {
                collect_decl_from_expr(&stmt.target, decls);
                collect_decl_from_expr(&stmt.value, decls);
            }
            Statement::Expr(expr_stmt) => collect_decl_from_expr(&expr_stmt.expr, decls),
            Statement::Return(ret) => {
                for value in &ret.values {
                    collect_decl_from_expr(value, decls);
                }
            }
            Statement::While(while_stmt) => {
                collect_decl_from_expr(&while_stmt.condition, decls);
                collect_decl_from_block(&while_stmt.body, decls);
            }
            Statement::For(for_stmt) => {
                match &for_stmt.target {
                    ForTarget::Range(range) => collect_decl_from_range(range, decls),
                    ForTarget::Collection(expr) => collect_decl_from_expr(expr, decls),
                }
                let body_span = for_stmt.body.span;
                decls.push(DeclInfo {
                    name: for_stmt.binding.clone(),
                    span: for_stmt.span,
                    scope: body_span,
                    available_from: body_span.start,
                    ty: None,
                    value_span: None,
                    mutability: Mutability::Immutable,
                    kind: DeclKind::ForBinding,
                });
                collect_decl_from_block(&for_stmt.body, decls);
            }
            Statement::Defer(defer_stmt) => collect_decl_from_expr(&defer_stmt.expr, decls),
            Statement::Block(inner) => collect_decl_from_block(inner, decls),
            Statement::Break | Statement::Continue => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_decl_from_expr(tail, decls);
    }
}

fn collect_decl_from_expr(expr: &Expr, decls: &mut Vec<DeclInfo>) {
    match expr {
        Expr::Identifier(_) | Expr::Literal(_) => {}
        Expr::Try { block, .. } => collect_decl_from_block(block, decls),
        Expr::TryPropagate { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Binary { left, right, .. } => {
            collect_decl_from_expr(left, decls);
            collect_decl_from_expr(right, decls);
        }
        Expr::Unary { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Call { callee, args, .. } => {
            collect_decl_from_expr(callee, decls);
            for arg in args {
                collect_decl_from_expr(arg, decls);
            }
        }
        Expr::FieldAccess { base, .. } => collect_decl_from_expr(base, decls),
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    collect_decl_from_expr(&field.value, decls);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_decl_from_expr(value, decls);
                }
            }
        },
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                collect_decl_from_expr(&entry.key, decls);
                collect_decl_from_expr(&entry.value, decls);
            }
        }
        Expr::Block(block) => collect_decl_from_block(block, decls),
        Expr::If(if_expr) => collect_decl_from_if_expr(if_expr, decls),
        Expr::Match(match_expr) => {
            collect_decl_from_expr(&match_expr.expr, decls);
            for arm in &match_expr.arms {
                let value_span = expr_span(&arm.value);
                collect_pattern_decls(&arm.pattern, value_span, value_span.start, decls);
                if let Some(guard) = &arm.guard {
                    collect_decl_from_expr(guard, decls);
                }
                collect_decl_from_expr(&arm.value, decls);
            }
        }
        Expr::Tuple(values, _) | Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_decl_from_expr(value, decls);
            }
        }
        Expr::Range(range) => collect_decl_from_range(range, decls),
        Expr::Reference { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Deref { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Move { expr: inner, .. } => collect_decl_from_expr(inner, decls),
    }
}

fn collect_decl_from_if_expr(if_expr: &IfExpr, decls: &mut Vec<DeclInfo>) {
    collect_decl_from_expr(&if_expr.condition, decls);
    collect_decl_from_block(&if_expr.then_branch, decls);
    if let Some(else_branch) = &if_expr.else_branch {
        collect_decl_from_else_branch(else_branch, decls);
    }
}

fn collect_decl_from_else_branch(branch: &ElseBranch, decls: &mut Vec<DeclInfo>) {
    match branch {
        ElseBranch::Block(block) => collect_decl_from_block(block, decls),
        ElseBranch::ElseIf(if_expr) => collect_decl_from_if_expr(if_expr, decls),
    }
}

fn infer_type_from_let_value(stmt: &LetStmt) -> Option<TypeExpr> {
    let value = stmt.value.as_ref()?;
    if let Expr::StructLiteral { name, .. } = value {
        return Some(TypeExpr::Named(name.clone(), Vec::new()));
    }
    None
}

fn collect_decl_from_range(range: &RangeExpr, decls: &mut Vec<DeclInfo>) {
    collect_decl_from_expr(&range.start, decls);
    collect_decl_from_expr(&range.end, decls);
}

fn collect_pattern_decls(
    pattern: &Pattern,
    scope: Span,
    available_from: usize,
    decls: &mut Vec<DeclInfo>,
) {
    match pattern {
        Pattern::Identifier(name, span) => decls.push(DeclInfo {
            name: name.clone(),
            span: *span,
            scope,
            available_from,
            ty: None,
            value_span: None,
            mutability: Mutability::Immutable,
            kind: DeclKind::Pattern,
        }),
        Pattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                collect_pattern_decls(binding, scope, available_from, decls);
            }
        }
        _ => {}
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
        _ => {}
    }
}

fn collect_used_in_function(func: &FunctionDef, used: &mut HashSet<String>) {
    match &func.body {
        FunctionBody::Block(block) => collect_used_in_block(block, used),
        FunctionBody::Expr(expr) => collect_expr_idents(&expr.node, used),
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
            collect_expr_idents(&while_stmt.condition, used);
            collect_used_in_block(&while_stmt.body, used);
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
    }
}

fn collect_range_expr(range: &RangeExpr, used: &mut HashSet<String>) {
    collect_expr_idents(&range.start, used);
    collect_expr_idents(&range.end, used);
}

fn collect_used_in_if_expr(if_expr: &IfExpr, used: &mut HashSet<String>) {
    collect_expr_idents(&if_expr.condition, used);
    collect_used_in_block(&if_expr.then_branch, used);
    if let Some(else_branch) = &if_expr.else_branch {
        collect_used_in_else_branch(else_branch, used);
    }
}

fn collect_used_in_else_branch(else_branch: &ElseBranch, used: &mut HashSet<String>) {
    match else_branch {
        ElseBranch::Block(block) => collect_used_in_block(block, used),
        ElseBranch::ElseIf(if_expr) => collect_used_in_if_expr(if_expr, used),
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
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::MapLiteral { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(if_expr) => if_expr.span,
        Expr::Match(match_expr) => match_expr.span,
        Expr::Tuple(_, span) => *span,
        Expr::ArrayLiteral(_, span) => *span,
        Expr::Range(range) => range.span,
        Expr::Reference { span, .. } => *span,
        Expr::Deref { span, .. } => *span,
        Expr::Move { span, .. } => *span,
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
    let strict = decls
        .iter()
        .filter(|decl| scope_contains(decl.scope, offset))
        .filter(|decl| offset >= decl.available_from || offset <= decl.span.end)
        .max_by_key(|decl| decl.span.start)
        .cloned();
    strict.or_else(|| decls.into_iter().max_by_key(|decl| decl.span.start))
}

pub fn find_local_definition_span(module: &Module, name: &str, offset: usize) -> Option<Span> {
    find_local_decl(module, name, offset).map(|decl| decl.span)
}

pub fn find_module_item_span(module: &Module, name: &str) -> Option<Span> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == name => return Some(func.span),
            Item::Struct(def) if def.name == name => return Some(def.span),
            Item::Enum(def) if def.name == name => return Some(def.span),
            Item::Interface(def) if def.name == name => return Some(def.span),
            Item::Const(def) if def.name == name => return Some(def.span),
            Item::Impl(_) => {}
            _ => {}
        }
    }
    None
}
