use crate::language::{
    ast::{
        Block, ElseBranch, Expr, ForTarget, FormatSegment, FormatStringLiteral, FunctionBody,
        FunctionDef, IfCondition, IfExpr, Item, LetStmt, Literal, MatchExpr, Module, Pattern,
        RangeExpr,
        Statement, StructLiteralKind, WhileCondition,
    },
    span::Span,
    types::{Mutability, TypeAnnotation, TypeExpr},
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
        match item {
            Item::Function(func) => collect_decl_from_function(func, module, None, &mut decls),
            Item::Impl(block) => {
                let target_ty = Some(TypeExpr::named(block.target.clone()));
                for func in &block.methods {
                    collect_decl_from_function(func, module, target_ty.clone(), &mut decls);
                }
            }
            _ => {}
        }
    }
    decls
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
        collect_decl_from_block(block, module, decls);
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
                    .and_then(|value| infer_expr_type(value, module));
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
        _ => None,
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
        | Expr::Reference { expr: inner, span, .. }
        | Expr::Deref { expr: inner, span, .. }
        | Expr::Move { expr: inner, span, .. }
        | Expr::Spawn { expr: inner, span, .. } => {
            if span_contains(*span, offset) {
                find_in_expr(inner, offset)
            } else {
                None
            }
        }
        Expr::Binary { left, right, span, .. } => {
            if span_contains(*span, offset) {
                find_in_expr(left, offset).or_else(|| find_in_expr(right, offset))
            } else {
                None
            }
        }
        Expr::Unary { expr: inner, span, .. } => {
            if span_contains(*span, offset) {
                find_in_expr(inner, offset)
            } else {
                None
            }
        }
        Expr::Call { callee, args, span, .. } => {
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
        Expr::FieldAccess { base, span, field, .. } => {
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
        Expr::Spawn { span, .. } => *span,
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

#[cfg(test)]
mod tests {
    use super::{find_local_definition_span, unused_variable_diagnostics};
    use crate::language::parser::parse_module;
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
