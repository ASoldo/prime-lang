use crate::language::{
    ast::*,
    span::Span,
    types::{TypeAnnotation, TypeExpr},
};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Clone, Debug)]
pub struct TypeError {
    pub path: PathBuf,
    pub span: Span,
    pub message: String,
    pub label: String,
}

impl TypeError {
    fn new(path: &PathBuf, span: Span, message: impl Into<String>) -> Self {
        let message = message.into();
        Self {
            path: path.clone(),
            span,
            label: message.clone(),
            message,
        }
    }
}

#[derive(Default)]
struct TypeRegistry {
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    functions: HashMap<FunctionKey, FunctionSignature>,
    consts: HashMap<String, ConstInfo>,
}

#[derive(Clone)]
struct StructInfo {
    def: StructDef,
}

#[derive(Clone)]
struct EnumInfo {
    def: EnumDef,
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    def: EnumVariant,
    span: Span,
}

#[derive(Clone)]
struct ConstInfo {
    ty: Option<TypeAnnotation>,
    _span: Span,
}

#[derive(Clone)]
struct FunctionSignature {
    name: String,
    type_params: Vec<String>,
    params: Vec<TypeAnnotation>,
    returns: Vec<TypeAnnotation>,
    span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionKey {
    name: String,
    receiver: Option<String>,
}

pub fn check_program(program: &Program) -> Result<(), Vec<TypeError>> {
    let mut registry = TypeRegistry::default();
    for module in &program.modules {
        collect_definitions(&mut registry, module);
    }
    let mut checker = Checker {
        registry,
        errors: Vec::new(),
    };
    checker.check_program(program);
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

fn collect_definitions(registry: &mut TypeRegistry, module: &Module) {
    for item in &module.items {
        match item {
            Item::Struct(def) => {
                registry
                    .structs
                    .insert(def.name.clone(), StructInfo { def: def.clone() });
            }
            Item::Enum(def) => {
                for variant in &def.variants {
                    registry.enum_variants.insert(
                        variant.name.clone(),
                        EnumVariantInfo {
                            enum_name: def.name.clone(),
                            def: variant.clone(),
                            span: variant.span,
                        },
                    );
                }
                registry
                    .enums
                    .insert(def.name.clone(), EnumInfo { def: def.clone() });
            }
            Item::Function(def) => {
                register_function(registry, &module.name, def.clone(), None);
            }
            Item::Const(def) => {
                registry.consts.insert(
                    def.name.clone(),
                    ConstInfo {
                        ty: def.ty.clone(),
                        _span: def.span,
                    },
                );
            }
            Item::Impl(block) => {
                for method in &block.methods {
                    let mut method_def = method.clone();
                    substitute_self(&mut method_def, &block.target);
                    register_function(
                        registry,
                        &module.name,
                        method_def,
                        Some(block.target.clone()),
                    );
                }
            }
            Item::Interface(_) => {}
        }
    }
}

fn substitute_self(def: &mut FunctionDef, target: &str) {
    let concrete = TypeExpr::named(target);
    for param in &mut def.params {
        param.ty = param.ty.replace_self(&concrete);
    }
    for ret in &mut def.returns {
        *ret = ret.replace_self(&concrete);
    }
}

fn register_function(
    registry: &mut TypeRegistry,
    module: &str,
    def: FunctionDef,
    receiver: Option<String>,
) {
    let key = FunctionKey {
        name: def.name.clone(),
        receiver: receiver.clone(),
    };
    let params: Vec<TypeAnnotation> = def.params.iter().map(|param| param.ty.clone()).collect();
    let returns = def.returns.clone();
    let base_signature = FunctionSignature {
        name: def.name.clone(),
        type_params: def.type_params.clone(),
        params: params.clone(),
        returns: returns.clone(),
        span: def.span,
    };
    registry.functions.insert(key, base_signature);
    let qualified_name = format!("{}::{}", module, def.name);
    let qualified = FunctionKey {
        name: qualified_name.clone(),
        receiver,
    };
    registry.functions.insert(
        qualified,
        FunctionSignature {
            name: qualified_name,
            type_params: def.type_params,
            params,
            returns,
            span: def.span,
        },
    );
}

struct Checker {
    registry: TypeRegistry,
    errors: Vec<TypeError>,
}

impl Checker {
    fn check_program(&mut self, program: &Program) {
        for module in &program.modules {
            self.check_module(module);
        }
    }

    fn check_module(&mut self, module: &Module) {
        for item in &module.items {
            match item {
                Item::Function(def) => self.check_function(module, def),
                Item::Const(def) => self.check_const(module, def),
                Item::Impl(block) => {
                    for method in &block.methods {
                        self.check_function(module, method);
                    }
                }
                _ => {}
            }
        }
    }

    fn check_function(&mut self, module: &Module, def: &FunctionDef) {
        let mut env = FnEnv::new(module.path.clone(), def.type_params.clone());
        let return_types: Vec<TypeExpr> = def.returns.iter().map(|ann| ann.ty.clone()).collect();
        for param in &def.params {
            env.declare(
                &param.name,
                Some(param.ty.ty.clone()),
                param.span,
                &mut self.errors,
            );
        }
        match &def.body {
            FunctionBody::Block(block) => {
                self.check_block(module, block, &return_types, &mut env);
            }
            FunctionBody::Expr(expr) => {
                let expected = return_types.get(0);
                let ty =
                    self.check_expression(module, &expr.node, expected, &return_types, &mut env);
                if let Some(expected) = expected {
                    self.ensure_type(module, expr.span, expected, ty.as_ref());
                } else if ty.is_some() && !return_types.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        expr.span,
                        "expression form cannot return multiple values",
                    ));
                }
            }
        }
    }

    fn check_const(&mut self, module: &Module, def: &ConstDef) {
        if let Some(annotation) = &def.ty {
            let mut env = FnEnv::new(module.path.clone(), Vec::new());
            let ty = self.check_expression(module, &def.value, Some(&annotation.ty), &[], &mut env);
            self.ensure_type(module, def.span, &annotation.ty, ty.as_ref());
        }
    }

    fn check_block(
        &mut self,
        module: &Module,
        block: &Block,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        env.push_scope();
        for stmt in &block.statements {
            self.check_statement(module, stmt, returns, env);
        }
        let tail_type = if let Some(tail) = &block.tail {
            self.check_expression(module, tail, None, returns, env)
        } else {
            None
        };
        env.pop_scope();
        tail_type
    }

    fn check_statement(
        &mut self,
        module: &Module,
        statement: &Statement,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) {
        match statement {
            Statement::Let(stmt) => {
                let expected = stmt.ty.as_ref().map(|ann| ann.ty.clone());
                let mut pending_borrow: Option<String> = None;
                if let Some(value) = &stmt.value {
                    let ty = self.check_expression(module, value, expected.as_ref(), returns, env);
                    if let Some(expected) = expected.as_ref() {
                        self.ensure_type(module, stmt.span, expected, ty.as_ref());
                    } else if let Some(actual) = ty {
                        env.infer(&stmt.name, Some(actual));
                    }
                    pending_borrow = mutable_reference_target(value);
                }
                env.declare(&stmt.name, expected, stmt.span, &mut self.errors);
                if let Some(target) = pending_borrow {
                    env.register_binding_borrow(&stmt.name, target);
                }
            }
            Statement::Assign(assign) => {
                if let Expr::Identifier(ident) = &assign.target {
                    env.clear_binding_borrows(&ident.name);
                    let expected_type = env.lookup(&ident.name).and_then(|info| info.ty.clone());
                    if let Some(expected) = expected_type {
                        let ty = self.check_expression(
                            module,
                            &assign.value,
                            Some(&expected),
                            returns,
                            env,
                        );
                        self.ensure_type(module, ident.span, &expected, ty.as_ref());
                        if let Some(target) = mutable_reference_target(&assign.value) {
                            env.register_binding_borrow(&ident.name, target);
                        }
                    } else {
                        self.check_expression(module, &assign.value, None, returns, env);
                        if let Some(target) = mutable_reference_target(&assign.value) {
                            env.register_binding_borrow(&ident.name, target);
                        }
                    }
                } else {
                    self.check_expression(module, &assign.value, None, returns, env);
                }
            }
            Statement::Expr(expr) => {
                self.check_expression(module, &expr.expr, None, returns, env);
            }
            Statement::Return(ret) => {
                if returns.is_empty() && !ret.values.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        ret.values
                            .first()
                            .map(expr_span)
                            .unwrap_or(stmt_span(statement)),
                        "function does not return a value",
                    ));
                    return;
                }
                if ret.values.len() != returns.len() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        stmt_span(statement),
                        format!(
                            "function returns {} value(s) but return statement provided {}",
                            returns.len(),
                            ret.values.len()
                        ),
                    ));
                }
                for (idx, value) in ret.values.iter().enumerate() {
                    let expected = returns.get(idx);
                    let ty = self.check_expression(module, value, expected, returns, env);
                    if let Some(expected) = expected {
                        self.ensure_type(module, expr_span(value), expected, ty.as_ref());
                    }
                }
            }
            Statement::While(while_stmt) => {
                self.check_expression(
                    module,
                    &while_stmt.condition,
                    Some(&bool_type()),
                    returns,
                    env,
                );
                self.check_block(module, &while_stmt.body, returns, env);
            }
            Statement::For(for_stmt) => match &for_stmt.target {
                ForTarget::Range(range) => {
                    self.check_expression(module, &range.start, Some(&int_type()), returns, env);
                    self.check_expression(module, &range.end, Some(&int_type()), returns, env);
                    env.push_scope();
                    env.declare(
                        &for_stmt.binding,
                        Some(int_type()),
                        for_stmt.span,
                        &mut self.errors,
                    );
                    self.check_block(module, &for_stmt.body, returns, env);
                    env.pop_scope();
                }
                ForTarget::Collection(expr) => {
                    let collection_ty = self.check_expression(module, expr, None, returns, env);
                    if let Some(coll_ty) = collection_ty {
                        if let Some(element_ty) = self.collection_element_type(&coll_ty) {
                            env.push_scope();
                            env.declare(
                                &for_stmt.binding,
                                Some(element_ty),
                                for_stmt.span,
                                &mut self.errors,
                            );
                            self.check_block(module, &for_stmt.body, returns, env);
                            env.pop_scope();
                        } else {
                            self.errors.push(TypeError::new(
                                &module.path,
                                for_stmt.span,
                                "`for` loops over collections support slices and maps",
                            ));
                        }
                    }
                }
            },
            Statement::Defer(defer_stmt) => {
                self.check_expression(module, &defer_stmt.expr, None, returns, env);
            }
            Statement::Break | Statement::Continue => {}
            Statement::Block(block) => {
                self.check_block(module, block, returns, env);
            }
        }
    }

    fn check_expression(
        &mut self,
        module: &Module,
        expr: &Expr,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match expr {
            Expr::Identifier(ident) => {
                if let Some(entry) = env.lookup(&ident.name) {
                    return entry.ty.clone();
                }
                if let Some(const_info) = self.registry.consts.get(&ident.name) {
                    return const_info.ty.as_ref().map(|ann| ann.ty.clone());
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    ident.span,
                    format!("unknown identifier `{}`", ident.name),
                ));
                None
            }
            Expr::Literal(lit) => Some(literal_type(lit)),
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => self.check_binary(module, *op, left, right, *span, returns, env),
            Expr::Unary { op, expr, span } => {
                self.check_unary(module, *op, expr, *span, returns, env)
            }
            Expr::Call {
                callee,
                args,
                type_args,
                span,
            } => self.check_call(
                module, callee, args, type_args, *span, expected, returns, env,
            ),
            Expr::FieldAccess { base, field, span } => {
                self.check_field_access(module, base, field, *span, returns, env)
            }
            Expr::StructLiteral { name, fields, span } => {
                self.check_struct_literal(module, name, fields, *span, returns, env);
                Some(TypeExpr::Named(name.clone(), Vec::new()))
            }
            Expr::MapLiteral { entries, span } => {
                self.check_map_literal(module, entries, *span, expected, returns, env)
            }
            Expr::Block(block) => self.check_block(module, block, returns, env),
            Expr::If(if_expr) => self.check_if_expression(module, if_expr, expected, returns, env),
            Expr::Match(match_expr) => {
                self.check_match_expression(module, match_expr, expected, returns, env)
            }
            Expr::Tuple(values, _) => {
                let mut types = Vec::new();
                for value in values {
                    types.push(self.check_expression(module, value, None, returns, env));
                }
                if types.iter().all(|ty| ty.is_some()) {
                    Some(TypeExpr::Tuple(
                        types.into_iter().map(|ty| ty.unwrap()).collect(),
                    ))
                } else {
                    None
                }
            }
            Expr::ArrayLiteral(values, span) => match expected {
                Some(TypeExpr::Slice(inner)) => {
                    for value in values {
                        self.check_expression(module, value, Some(inner.as_ref()), returns, env);
                    }
                    Some(TypeExpr::Slice(inner.clone()))
                }
                Some(other) => {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        format!("array literal cannot produce `{}`", other.canonical_name()),
                    ));
                    None
                }
                None => None,
            },
            Expr::Reference {
                mutable,
                expr,
                span,
            } => {
                if *mutable {
                    if let Expr::Identifier(ident) = expr.as_ref() {
                        env.ensure_mut_borrow_allowed(module, *span, &ident.name, &mut self.errors);
                    }
                }
                self.check_expression(module, expr, None, returns, env)
                    .map(|inner| TypeExpr::Reference {
                        mutable: *mutable,
                        ty: Box::new(inner),
                    })
            }
            Expr::Deref { expr, span } => {
                let ty = self.check_expression(module, expr, None, returns, env);
                if let Some(TypeExpr::Reference { ty, .. }) = ty {
                    Some(*ty)
                } else {
                    if let Some(actual) = ty {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            format!("cannot dereference `{}`", actual.canonical_name()),
                        ));
                    }
                    None
                }
            }
            Expr::Try { block, span } => {
                if let Some(expected) = expected {
                    if let TypeExpr::Named(name, args) = expected {
                        if name == "Result" && args.len() == 2 {
                            self.check_block(module, block, returns, env);
                            return Some(expected.clone());
                        }
                    }
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        "`try {}` must be used when the expected type is Result",
                    ));
                }
                None
            }
            Expr::TryPropagate { expr, span } => {
                let ty = self.check_expression(module, expr, None, returns, env);
                if let Some(TypeExpr::Named(name, args)) = ty {
                    if name == "Result" && !args.is_empty() {
                        return Some(args[0].clone());
                    }
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    *span,
                    "`?` operator requires a Result value",
                ));
                None
            }
            Expr::Move { expr, span } => {
                if !matches!(expr.as_ref(), Expr::Identifier(_)) {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        "`move` expressions require identifiers",
                    ));
                }
                let ty = self.check_expression(module, expr, None, returns, env);
                if let Some(actual) = ty.as_ref() {
                    if !is_heap_type(actual) {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            "`move` only supports Box, slice, or Map values",
                        ));
                    }
                }
                ty
            }
            Expr::Range(_) => None,
        }
    }

    fn check_if_expression(
        &mut self,
        module: &Module,
        expr: &IfExpr,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let then_ty = match &expr.condition {
            IfCondition::Expr(condition) => {
                self.check_expression(module, condition, Some(&bool_type()), returns, env);
                self.check_block(module, &expr.then_branch, returns, env)
            }
            IfCondition::Let { pattern, value, .. } => {
                let value_ty = self.check_expression(module, value, None, returns, env);
                env.push_scope();
                self.bind_pattern(module, pattern, value_ty.as_ref(), env);
                let ty = self.check_block(module, &expr.then_branch, returns, env);
                env.pop_scope();
                ty
            }
        };
        if let Some(else_branch) = &expr.else_branch {
            let else_ty = match else_branch {
                ElseBranch::Block(block) => self.check_block(module, block, returns, env),
                ElseBranch::ElseIf(nested) => {
                    self.check_if_expression(module, nested, expected, returns, env)
                }
            };
            if let (Some(expected), Some(actual)) = (expected, else_ty.as_ref()) {
                self.ensure_type(module, expr.span, expected, Some(actual));
                return Some(actual.clone());
            }
            else_ty
        } else {
            then_ty
        }
    }

    fn check_match_expression(
        &mut self,
        module: &Module,
        expr: &MatchExpr,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let scrutinee_ty = self.check_expression(module, &expr.expr, None, returns, env);
        let mut arm_type: Option<TypeExpr> = None;
        let mut covered_variants: HashSet<String> = HashSet::new();
        let mut has_wildcard = false;
        for arm in &expr.arms {
            env.push_scope();
            self.bind_pattern(module, &arm.pattern, scrutinee_ty.as_ref(), env);
            if let Some(guard) = &arm.guard {
                self.check_expression(module, guard, Some(&bool_type()), returns, env);
            }
            let value_ty = self.check_expression(module, &arm.value, expected, returns, env);
            if arm_type.is_none() {
                arm_type = value_ty.clone();
            } else if let (Some(a), Some(b)) = (arm_type.as_ref(), value_ty.as_ref()) {
                self.ensure_type(module, pattern_span(&arm.pattern), a, Some(b));
            }
            if let Some(enum_variant) = self.pattern_variant(&arm.pattern) {
                covered_variants.insert(enum_variant);
            } else if matches!(arm.pattern, Pattern::Wildcard) {
                has_wildcard = true;
            }
            env.pop_scope();
        }
        if let Some(TypeExpr::Named(enum_name, _)) = &scrutinee_ty {
            if let Some(enum_info) = self.registry.enums.get(enum_name) {
                if !has_wildcard {
                    let all_variants: HashSet<String> = enum_info
                        .def
                        .variants
                        .iter()
                        .map(|variant| variant.name.clone())
                        .collect();
                    if !covered_variants.is_superset(&all_variants) {
                        self.errors.push(TypeError::new(
                            &module.path,
                            expr.span,
                            format!(
                                "`match` is not exhaustive; missing variants: {}",
                                all_variants
                                    .difference(&covered_variants)
                                    .cloned()
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        ));
                    }
                }
            }
        }
        arm_type
    }

    fn check_field_access(
        &mut self,
        module: &Module,
        base: &Expr,
        field: &str,
        span: Span,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let base_ty = self.check_expression(module, base, None, returns, env)?;
        if let TypeExpr::Named(name, _) = &base_ty {
            if let Some(struct_info) = self.registry.structs.get(name) {
                return lookup_struct_field(&self.registry, &struct_info.def, field);
            }
        }
        self.errors.push(TypeError::new(
            &module.path,
            span,
            format!("`{}` has no field `{}`", base_ty.canonical_name(), field),
        ));
        None
    }

    fn check_struct_literal(
        &mut self,
        module: &Module,
        name: &str,
        fields: &StructLiteralKind,
        span: Span,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) {
        let Some(struct_info) = self.registry.structs.get(name) else {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!("Unknown struct `{}`", name),
            ));
            return;
        };
        let struct_fields = struct_info.def.fields.clone();
        match fields {
            StructLiteralKind::Named(named_fields) => {
                for field in named_fields {
                    let expected = struct_fields
                        .iter()
                        .find(|f| f.name.as_deref() == Some(&field.name))
                        .map(|f| f.ty.ty.clone());
                    if let Some(expected) = expected {
                        let ty = self.check_expression(
                            module,
                            &field.value,
                            Some(&expected),
                            returns,
                            env,
                        );
                        self.ensure_type(module, expr_span(&field.value), &expected, ty.as_ref());
                    } else {
                        self.errors.push(TypeError::new(
                            &module.path,
                            expr_span(&field.value),
                            format!("`{}` has no field `{}`", name, field.name),
                        ));
                    }
                }
            }
            StructLiteralKind::Positional(values) => {
                if values.len() != struct_fields.len() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!(
                            "`{}` expects {} field(s), found {}",
                            name,
                            struct_fields.len(),
                            values.len()
                        ),
                    ));
                }
                for (value, def) in values.iter().zip(struct_fields.iter()) {
                    let expected = def.ty.ty.clone();
                    let ty = self.check_expression(module, value, Some(&expected), returns, env);
                    self.ensure_type(module, expr_span(value), &expected, ty.as_ref());
                }
            }
        }
    }

    fn check_call(
        &mut self,
        module: &Module,
        callee: &Expr,
        args: &[Expr],
        type_args: &[TypeExpr],
        span: Span,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match callee {
            Expr::Identifier(ident) => {
                if self.is_builtin_name(&ident.name) {
                    return self.check_builtin_call(
                        module,
                        &ident.name,
                        args,
                        type_args,
                        expected,
                        returns,
                        env,
                        span,
                    );
                }
                if let Some(sig) = self.lookup_function(&ident.name, None) {
                    return self.check_function_call(
                        module, sig, args, type_args, expected, returns, env, span,
                    );
                }
                if let Some(variant) = self.registry.enum_variants.get(&ident.name).cloned() {
                    return self
                        .check_variant_call(module, variant, args, expected, returns, env, span);
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    ident.span,
                    format!("Unknown function `{}`", ident.name),
                ));
                None
            }
            Expr::FieldAccess { base, field, .. } => {
                if let Expr::Identifier(module_ident) = base.as_ref() {
                    let qualified = format!("{}::{}", module_ident.name, field);
                    if let Some(sig) = self.lookup_function(&qualified, None) {
                        return self.check_function_call(
                            module, sig, args, type_args, expected, returns, env, span,
                        );
                    }
                }
                let receiver = match self.check_expression(module, base, None, returns, env) {
                    Some(ty) => ty,
                    None => return None,
                };
                if let Some(name) = named_type_name(&receiver) {
                    if let Some(sig) = self.lookup_function(field, Some(name)) {
                        return self.check_method_call(
                            module, sig, &receiver, args, type_args, expected, returns, env, span,
                        );
                    }
                }
                if let Some(result) = self
                    .check_builtin_method_call(module, &receiver, field, args, returns, env, span)
                {
                    return Some(result);
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "Unsupported call target",
                ));
                None
            }
            _ => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "Unsupported call target",
                ));
                None
            }
        }
    }

    fn check_builtin_call(
        &mut self,
        module: &Module,
        name: &str,
        args: &[Expr],
        type_args: &[TypeExpr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        if !type_args.is_empty() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!("`{}` does not accept type arguments", name),
            ));
        }
        match name {
            "out" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`out` expects 1 argument, got {}", args.len()),
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(module, &args[0], None, returns, env);
                Some(TypeExpr::Unit)
            }
            "box_new" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`box_new` expects 1 argument, got {}", args.len()),
                    ));
                    return None;
                }
                let value_ty = self.check_expression(module, &args[0], None, returns, env)?;
                Some(make_box_type(value_ty))
            }
            "box_get" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`box_get` expects 1 argument, got {}", args.len()),
                    ));
                    return None;
                }
                let ty = self.check_expression(module, &args[0], None, returns, env);
                self.expect_box_inner(module, span, ty.as_ref())
            }
            "box_set" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`box_set` expects 2 arguments, got {}", args.len()),
                    ));
                    return Some(TypeExpr::Unit);
                }
                let box_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(inner) = self.expect_box_inner(module, span, box_ty.as_ref()) {
                    let value_ty =
                        self.check_expression(module, &args[1], Some(&inner), returns, env);
                    self.ensure_type(module, expr_span(&args[1]), &inner, value_ty.as_ref());
                }
                Some(TypeExpr::Unit)
            }
            "box_take" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`box_take` expects 1 argument, got {}", args.len()),
                    ));
                    return None;
                }
                let ty = self.check_expression(module, &args[0], None, returns, env);
                self.expect_box_inner(module, span, ty.as_ref())
            }
            "slice_new" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`slice_new` expects 0 arguments, got {}", args.len()),
                    ));
                }
                let slice_ty = expected.and_then(|ty| match ty {
                    TypeExpr::Slice(inner) => Some(TypeExpr::Slice(inner.clone())),
                    _ => None,
                });
                if let Some(slice_ty) = slice_ty {
                    Some(slice_ty)
                } else {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`slice_new` requires a contextual slice type",
                    ));
                    None
                }
            }
            "slice_push" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`slice_push` expects 2 arguments, got {}", args.len()),
                    ));
                    return Some(TypeExpr::Unit);
                }
                let slice_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(elem_ty) = self.expect_slice_type(module, span, slice_ty.as_ref()) {
                    let value_ty =
                        self.check_expression(module, &args[1], Some(&elem_ty), returns, env);
                    self.ensure_type(module, expr_span(&args[1]), &elem_ty, value_ty.as_ref());
                }
                Some(TypeExpr::Unit)
            }
            "slice_len" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`slice_len` expects 1 argument, got {}", args.len()),
                    ));
                    return Some(int_type());
                }
                self.check_expression(module, &args[0], None, returns, env);
                Some(int_type())
            }
            "slice_get" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`slice_get` expects 2 arguments, got {}", args.len()),
                    ));
                    return None;
                }
                let slice_ty = self.check_expression(module, &args[0], None, returns, env);
                self.check_expression(module, &args[1], Some(&int_type()), returns, env);
                self.expect_slice_type(module, span, slice_ty.as_ref())
                    .map(|elem| make_option_type(elem))
            }
            "map_new" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`map_new` expects 0 arguments, got {}", args.len()),
                    ));
                }
                if let Some(TypeExpr::Named(name, args)) = expected {
                    if name == "Map" && args.len() == 2 {
                        if !is_string_type(&args[0]) {
                            self.errors.push(TypeError::new(
                                &module.path,
                                span,
                                "Map keys must be `string`",
                            ));
                        }
                        return Some(TypeExpr::Named(name.clone(), args.clone()));
                    }
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "`map_new` requires a contextual Map type",
                ));
                None
            }
            "map_insert" => {
                if args.len() != 3 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`map_insert` expects 3 arguments, got {}", args.len()),
                    ));
                    return Some(TypeExpr::Unit);
                }
                let map_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some((key_ty, value_ty)) =
                    self.expect_map_type(module, span, map_ty.as_ref())
                {
                    self.check_expression(module, &args[1], Some(&key_ty), returns, env);
                    let value_expr_ty =
                        self.check_expression(module, &args[2], Some(&value_ty), returns, env);
                    self.ensure_type(
                        module,
                        expr_span(&args[2]),
                        &value_ty,
                        value_expr_ty.as_ref(),
                    );
                }
                Some(TypeExpr::Unit)
            }
            "map_get" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`map_get` expects 2 arguments, got {}", args.len()),
                    ));
                    return None;
                }
                let map_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some((key_ty, value_ty)) =
                    self.expect_map_type(module, span, map_ty.as_ref())
                {
                    self.check_expression(module, &args[1], Some(&key_ty), returns, env);
                    return Some(make_option_type(value_ty));
                }
                None
            }
            _ => None,
        }
    }

    fn check_function_call(
        &mut self,
        module: &Module,
        sig: FunctionSignature,
        args: &[Expr],
        type_args: &[TypeExpr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        let instantiated =
            instantiate_function(&sig, type_args, &module.path, span, &mut self.errors);
        if args.len() != instantiated.params.len() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!(
                    "`{}` expects {} argument(s), got {}",
                    sig.name,
                    instantiated.params.len(),
                    args.len()
                ),
            ));
        }
        for (expr, param) in args.iter().zip(instantiated.params.iter()) {
            self.check_expression(module, expr, Some(&param.ty), returns, env);
        }
        self.select_call_result(module, span, &instantiated.returns, expected)
    }

    fn check_method_call(
        &mut self,
        module: &Module,
        sig: FunctionSignature,
        receiver_type: &TypeExpr,
        args: &[Expr],
        type_args: &[TypeExpr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        let instantiated =
            instantiate_function(&sig, type_args, &module.path, span, &mut self.errors);
        if instantiated.params.is_empty() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                "method is missing receiver parameter",
            ));
            return None;
        }
        self.ensure_type(
            module,
            span,
            &instantiated.params[0].ty,
            Some(receiver_type),
        );
        if instantiated.params.len() - 1 != args.len() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!(
                    "method expects {} argument(s) after `self`, got {}",
                    instantiated.params.len() - 1,
                    args.len()
                ),
            ));
        }
        for (expr, param) in args.iter().zip(instantiated.params.iter().skip(1)) {
            self.check_expression(module, expr, Some(&param.ty), returns, env);
        }
        self.select_call_result(module, span, &instantiated.returns, expected)
    }

    fn check_builtin_method_call(
        &mut self,
        module: &Module,
        receiver: &TypeExpr,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        let target = strip_references(receiver);
        match target {
            TypeExpr::Slice(inner) => {
                self.check_slice_method(module, inner.as_ref(), method, args, returns, env, span)
            }
            TypeExpr::Named(name, generics) if name == "Box" && generics.len() == 1 => {
                self.check_box_method(module, &generics[0], method, args, returns, env, span)
            }
            TypeExpr::Named(name, _) if name == "Map" => {
                self.check_map_method(module, target, method, args, returns, env, span)
            }
            _ => None,
        }
    }

    fn check_box_method(
        &mut self,
        module: &Module,
        inner: &TypeExpr,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        match method {
            "box_get" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`box_get` expects 0 argument(s) after receiver",
                    ));
                }
                Some(inner.clone())
            }
            "box_set" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`box_set` expects 1 argument after receiver",
                    ));
                    return Some(TypeExpr::Unit);
                }
                let value_ty = self.check_expression(module, &args[0], Some(inner), returns, env);
                self.ensure_type(module, expr_span(&args[0]), inner, value_ty.as_ref());
                Some(TypeExpr::Unit)
            }
            "box_take" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`box_take` expects 0 argument(s) after receiver",
                    ));
                }
                Some(inner.clone())
            }
            _ => None,
        }
    }

    fn check_slice_method(
        &mut self,
        module: &Module,
        inner: &TypeExpr,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        match method {
            "slice_len" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`slice_len` expects 0 argument(s) after receiver",
                    ));
                }
                Some(int_type())
            }
            "slice_get" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`slice_get` expects 1 argument after receiver",
                    ));
                    return Some(make_option_type(inner.clone()));
                }
                self.check_expression(module, &args[0], Some(&int_type()), returns, env);
                Some(make_option_type(inner.clone()))
            }
            "slice_push" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`slice_push` expects 1 argument after receiver",
                    ));
                    return Some(TypeExpr::Unit);
                }
                let value_ty = self.check_expression(module, &args[0], Some(inner), returns, env);
                self.ensure_type(module, expr_span(&args[0]), inner, value_ty.as_ref());
                Some(TypeExpr::Unit)
            }
            _ => None,
        }
    }

    fn check_map_method(
        &mut self,
        module: &Module,
        map_ty: &TypeExpr,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        let (key_ty, value_ty) = self.expect_map_type(module, span, Some(map_ty))?;
        match method {
            "map_get" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_get` expects 1 argument after receiver",
                    ));
                } else {
                    self.check_expression(module, &args[0], Some(&key_ty), returns, env);
                }
                Some(make_option_type(value_ty))
            }
            "map_insert" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_insert` expects 2 argument(s) after receiver",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(module, &args[0], Some(&key_ty), returns, env);
                let value_ty_expr =
                    self.check_expression(module, &args[1], Some(&value_ty), returns, env);
                self.ensure_type(
                    module,
                    expr_span(&args[1]),
                    &value_ty,
                    value_ty_expr.as_ref(),
                );
                Some(TypeExpr::Unit)
            }
            _ => None,
        }
    }

    fn check_variant_call(
        &mut self,
        module: &Module,
        variant: EnumVariantInfo,
        args: &[Expr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        if args.len() != variant.def.fields.len() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!(
                    "`{}` expects {} argument(s), got {}",
                    variant.def.name,
                    variant.def.fields.len(),
                    args.len()
                ),
            ));
        }
        if let Some(expected) = expected {
            if let TypeExpr::Named(name, _) = expected {
                if name != &variant.enum_name {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!(
                            "`{}` variant belongs to `{}` but expression expected `{}`",
                            variant.def.name, variant.enum_name, name
                        ),
                    ));
                }
            }
        }
        for (expr, field) in args.iter().zip(variant.def.fields.iter()) {
            self.check_expression(module, expr, Some(&field.ty), returns, env);
        }
        expected.cloned().or_else(|| {
            Some(TypeExpr::Named(
                variant.enum_name.clone(),
                variant.def.fields.iter().map(|f| f.ty.clone()).collect(),
            ))
        })
    }

    fn select_call_result(
        &mut self,
        module: &Module,
        span: Span,
        returns: &[TypeAnnotation],
        expected: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        let result = match returns.len() {
            0 => Some(TypeExpr::Unit),
            1 => Some(returns[0].ty.clone()),
            _ => Some(TypeExpr::Tuple(
                returns.iter().map(|ret| ret.ty.clone()).collect(),
            )),
        };
        if let (Some(expected), Some(actual)) = (expected, result.as_ref()) {
            self.ensure_type(module, span, expected, Some(actual));
        }
        result
    }

    fn ensure_type(
        &mut self,
        module: &Module,
        span: Span,
        expected: &TypeExpr,
        actual: Option<&TypeExpr>,
    ) {
        if let Some(actual) = actual {
            if expected != actual {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "expected `{}`, found `{}`",
                        expected.canonical_name(),
                        actual.canonical_name()
                    ),
                ));
            }
        }
    }

    fn lookup_function(&self, name: &str, receiver: Option<&str>) -> Option<FunctionSignature> {
        let key = FunctionKey {
            name: name.to_string(),
            receiver: receiver.map(|s| s.to_string()),
        };
        self.registry.functions.get(&key).cloned()
    }

    fn pattern_variant(&self, pattern: &Pattern) -> Option<String> {
        match pattern {
            Pattern::EnumVariant { variant, .. } => Some(variant.clone()),
            _ => None,
        }
    }

    fn bind_pattern(
        &mut self,
        module: &Module,
        pattern: &Pattern,
        ty: Option<&TypeExpr>,
        env: &mut FnEnv,
    ) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Identifier(name, span) => {
                env.declare(name, ty.cloned(), *span, &mut self.errors);
            }
            Pattern::Literal(lit) => {
                if let Some(actual) = ty {
                    let literal_ty = literal_type(lit);
                    if &literal_ty != actual {
                        self.errors.push(TypeError::new(
                            &module.path,
                            literal_span(lit),
                            format!(
                                "pattern expects `{}`, found `{}`",
                                literal_ty.canonical_name(),
                                actual.canonical_name()
                            ),
                        ));
                    }
                }
            }
            Pattern::EnumVariant {
                enum_name,
                variant,
                bindings,
            } => {
                if let Some(info) = self.registry.enum_variants.get(variant).cloned() {
                    if let Some(actual_enum) = enum_name {
                        if actual_enum != &info.enum_name {
                            self.errors.push(TypeError::new(
                                &module.path,
                                info.span,
                                format!("`{}` does not belong to enum `{}`", variant, actual_enum),
                            ));
                            return;
                        }
                    } else if let Some(TypeExpr::Named(actual_enum, _)) = ty {
                        if actual_enum != &info.enum_name {
                            self.errors.push(TypeError::new(
                                &module.path,
                                pattern_span(pattern),
                                format!("`{}` does not belong to enum `{}`", variant, actual_enum),
                            ));
                            return;
                        }
                    }
                    if bindings.len() != info.def.fields.len() {
                        self.errors.push(TypeError::new(
                            &module.path,
                            pattern_span(pattern),
                            format!(
                                "`{}` expects {} field(s), found {}",
                                variant,
                                info.def.fields.len(),
                                bindings.len()
                            ),
                        ));
                        return;
                    }
                    for (binding, field) in bindings.iter().zip(info.def.fields.iter()) {
                        self.bind_pattern(module, binding, Some(&field.ty), env);
                    }
                } else {
                    self.errors.push(TypeError::new(
                        &module.path,
                        pattern_span(pattern),
                        format!("Unknown variant `{}`", variant),
                    ));
                }
            }
            Pattern::Tuple(elements, span) => {
                if let Some(TypeExpr::Tuple(types)) = ty {
                    if elements.len() != types.len() {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            format!(
                                "tuple pattern expects {} element(s), found {}",
                                types.len(),
                                elements.len()
                            ),
                        ));
                        return;
                    }
                    for (pat, elem_ty) in elements.iter().zip(types.iter()) {
                        self.bind_pattern(module, pat, Some(elem_ty), env);
                    }
                } else if let Some(actual) = ty {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        format!("expected tuple type, found `{}`", actual.canonical_name()),
                    ));
                } else {
                    for pat in elements {
                        self.bind_pattern(module, pat, None, env);
                    }
                }
            }
            Pattern::Map(entries, span) => {
                let value_ty = match ty {
                    Some(actual) => match self.expect_map_type(module, *span, Some(actual)) {
                        Some((_key, value)) => Some(value),
                        None => return,
                    },
                    None => None,
                };
                for entry in entries {
                    self.bind_pattern(module, &entry.pattern, value_ty.as_ref(), env);
                }
            }
            Pattern::Struct {
                struct_name,
                fields,
                has_spread: _,
                span,
            } => {
                let mut resolved = struct_name.clone();
                if let Some(TypeExpr::Named(name, _)) = ty {
                    if let Some(pattern_name) = struct_name {
                        if pattern_name != name {
                            self.errors.push(TypeError::new(
                                &module.path,
                                *span,
                                format!(
                                    "struct pattern `{}` does not match value of type `{}`",
                                    pattern_name, name
                                ),
                            ));
                        }
                    }
                    resolved = Some(name.clone());
                } else if ty.is_some() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        "struct pattern requires struct type",
                    ));
                }
                let struct_def = resolved
                    .as_ref()
                    .and_then(|name| self.registry.structs.get(name).cloned());
                if resolved.is_some() && struct_def.is_none() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        "unknown struct in pattern",
                    ));
                }
                for field in fields {
                    let field_ty = struct_def.as_ref().and_then(|info| {
                        lookup_struct_field(&self.registry, &info.def, &field.name)
                    });
                    if struct_def.is_some() && field_ty.is_none() {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            format!("unknown field `{}` in struct pattern", field.name),
                        ));
                    }
                    self.bind_pattern(module, &field.pattern, field_ty.as_ref(), env);
                }
            }
            Pattern::Slice {
                prefix,
                rest,
                suffix,
                span,
            } => {
                let element_ty = match ty {
                    Some(TypeExpr::Slice(inner)) => Some((**inner).clone()),
                    Some(actual) => {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            format!(
                                "slice pattern requires slice type, found `{}`",
                                actual.canonical_name()
                            ),
                        ));
                        None
                    }
                    None => None,
                };
                for pat in prefix {
                    self.bind_pattern(module, pat, element_ty.as_ref(), env);
                }
                if let Some(rest_pattern) = rest {
                    let rest_ty = element_ty
                        .as_ref()
                        .map(|inner| TypeExpr::Slice(Box::new(inner.clone())));
                    self.bind_pattern(module, rest_pattern, rest_ty.as_ref(), env);
                }
                for pat in suffix {
                    self.bind_pattern(module, pat, element_ty.as_ref(), env);
                }
            }
        }
    }

    fn collection_element_type(&self, ty: &TypeExpr) -> Option<TypeExpr> {
        match ty {
            TypeExpr::Slice(inner) => Some((**inner).clone()),
            TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
                Some(TypeExpr::Tuple(vec![args[0].clone(), args[1].clone()]))
            }
            TypeExpr::Reference { ty: inner, .. } => self.collection_element_type(inner.as_ref()),
            _ => None,
        }
    }

    fn is_builtin_name(&self, name: &str) -> bool {
        matches!(
            name,
            "out"
                | "box_new"
                | "box_get"
                | "box_set"
                | "box_take"
                | "slice_new"
                | "slice_push"
                | "slice_len"
                | "slice_get"
                | "map_new"
                | "map_insert"
                | "map_get"
        )
    }

    fn expect_box_inner(
        &mut self,
        module: &Module,
        span: Span,
        ty: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        match ty {
            Some(TypeExpr::Reference { ty: inner, .. }) => {
                self.expect_box_inner(module, span, Some(inner.as_ref()))
            }
            Some(TypeExpr::Named(name, args)) if name == "Box" && args.len() == 1 => {
                Some(args[0].clone())
            }
            Some(other) => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!("expected `Box[T]`, found `{}`", other.canonical_name()),
                ));
                None
            }
            None => None,
        }
    }

    fn expect_slice_type(
        &mut self,
        module: &Module,
        span: Span,
        ty: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        match ty {
            Some(TypeExpr::Reference { ty: inner, .. }) => {
                self.expect_slice_type(module, span, Some(inner.as_ref()))
            }
            Some(TypeExpr::Slice(inner)) => Some((**inner).clone()),
            Some(other) => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!("expected slice type, found `{}`", other.canonical_name()),
                ));
                None
            }
            None => None,
        }
    }

    fn expect_map_type(
        &mut self,
        module: &Module,
        span: Span,
        ty: Option<&TypeExpr>,
    ) -> Option<(TypeExpr, TypeExpr)> {
        match ty {
            Some(TypeExpr::Reference { ty: inner, .. }) => {
                self.expect_map_type(module, span, Some(inner.as_ref()))
            }
            Some(TypeExpr::Named(name, args)) if name == "Map" && args.len() == 2 => {
                if !is_string_type(&args[0]) {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "Map keys must be of type `string`",
                    ));
                }
                Some((args[0].clone(), args[1].clone()))
            }
            Some(other) => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "expected `Map[string, T]`, found `{}`",
                        other.canonical_name()
                    ),
                ));
                None
            }
            None => None,
        }
    }

    fn check_map_literal(
        &mut self,
        module: &Module,
        entries: &[MapLiteralEntry],
        span: Span,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        if let Some((key_ty, value_ty)) = self.expect_map_type(module, span, expected) {
            for entry in entries {
                let key = self.check_expression(module, &entry.key, Some(&key_ty), returns, env);
                self.ensure_type(module, expr_span(&entry.key), &key_ty, key.as_ref());
                let value =
                    self.check_expression(module, &entry.value, Some(&value_ty), returns, env);
                self.ensure_type(module, expr_span(&entry.value), &value_ty, value.as_ref());
            }
            return expected.cloned();
        }

        if entries.is_empty() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                "cannot infer type of empty map literal; add a `Map[string, T]` annotation",
            ));
            return None;
        }

        let key_ty = string_type();
        let mut inferred_value: Option<TypeExpr> = None;
        for entry in entries {
            let key = self.check_expression(module, &entry.key, Some(&key_ty), returns, env);
            self.ensure_type(module, expr_span(&entry.key), &key_ty, key.as_ref());

            let value =
                self.check_expression(module, &entry.value, inferred_value.as_ref(), returns, env);
            match (inferred_value.as_ref(), value.as_ref()) {
                (None, Some(actual)) => inferred_value = Some(actual.clone()),
                (Some(expected), Some(actual)) => {
                    self.ensure_type(module, expr_span(&entry.value), expected, Some(actual));
                }
                _ => {}
            }
        }
        inferred_value.map(|value_ty| TypeExpr::Named("Map".into(), vec![key_ty.clone(), value_ty]))
    }

    fn resolve_numeric_type(
        &mut self,
        module: &Module,
        span: Span,
        left: Option<&TypeExpr>,
        right: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        let left_kind = left.and_then(numeric_kind);
        let right_kind = right.and_then(numeric_kind);
        match (left_kind, right_kind) {
            (Some(NumericKind::Float), _) | (_, Some(NumericKind::Float)) => Some(float_type()),
            (Some(NumericKind::Int), Some(NumericKind::Int)) => Some(int_type()),
            (Some(kind), None) | (None, Some(kind)) => match kind {
                NumericKind::Float => Some(float_type()),
                NumericKind::Int => Some(int_type()),
            },
            _ => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "numeric operations require int or float operands",
                ));
                None
            }
        }
    }

    fn check_binary(
        &mut self,
        module: &Module,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: Span,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                let left_ty = self.check_expression(module, left, None, returns, env);
                let right_ty = self.check_expression(module, right, None, returns, env);
                self.resolve_numeric_type(module, span, left_ty.as_ref(), right_ty.as_ref())
            }
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                self.check_expression(module, left, Some(&int_type()), returns, env);
                self.check_expression(module, right, Some(&int_type()), returns, env);
                Some(int_type())
            }
            BinaryOp::And | BinaryOp::Or => {
                self.check_expression(module, left, Some(&bool_type()), returns, env);
                self.check_expression(module, right, Some(&bool_type()), returns, env);
                Some(bool_type())
            }
            BinaryOp::Eq | BinaryOp::NotEq => {
                let left_ty = self.check_expression(module, left, None, returns, env);
                let right_ty = self.check_expression(module, right, left_ty.as_ref(), returns, env);
                if let Some(left_ty) = left_ty.as_ref() {
                    self.ensure_type(module, span, left_ty, right_ty.as_ref());
                }
                Some(bool_type())
            }
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                let left_ty = self.check_expression(module, left, None, returns, env);
                let right_ty = self.check_expression(module, right, None, returns, env);
                self.resolve_numeric_type(module, span, left_ty.as_ref(), right_ty.as_ref());
                Some(bool_type())
            }
        }
    }

    fn check_unary(
        &mut self,
        module: &Module,
        op: UnaryOp,
        expr: &Expr,
        _span: Span,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match op {
            UnaryOp::Neg => {
                self.check_expression(module, expr, Some(&int_type()), returns, env);
                Some(int_type())
            }
            UnaryOp::Not => {
                self.check_expression(module, expr, Some(&bool_type()), returns, env);
                Some(bool_type())
            }
        }
    }
}

#[derive(Clone)]
struct BindingInfo {
    ty: Option<TypeExpr>,
    mut_borrows: Vec<String>,
}

impl BindingInfo {
    fn new(ty: Option<TypeExpr>) -> Self {
        Self {
            ty,
            mut_borrows: Vec::new(),
        }
    }
}

struct FnEnv {
    path: PathBuf,
    _type_params: HashSet<String>,
    scopes: Vec<HashMap<String, BindingInfo>>,
    active_mut_borrows: HashMap<String, usize>,
}

impl FnEnv {
    fn new(path: PathBuf, type_params: Vec<String>) -> Self {
        Self {
            path,
            _type_params: type_params.into_iter().collect(),
            scopes: vec![HashMap::new()],
            active_mut_borrows: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            for info in scope.values() {
                for target in &info.mut_borrows {
                    self.end_mut_borrow(target);
                }
            }
        }
        if self.scopes.is_empty() {
            self.scopes.push(HashMap::new());
        }
    }

    fn declare(
        &mut self,
        name: &str,
        ty: Option<TypeExpr>,
        span: Span,
        errors: &mut Vec<TypeError>,
    ) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), BindingInfo::new(ty));
        } else {
            errors.push(TypeError::new(
                &self.path,
                span,
                format!("Failed to declare `{}`", name),
            ));
        }
    }

    fn infer(&mut self, name: &str, ty: Option<TypeExpr>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope
                .entry(name.to_string())
                .or_insert_with(|| BindingInfo::new(None))
                .ty = ty;
        }
    }

    fn lookup(&self, name: &str) -> Option<&BindingInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    fn clear_binding_borrows(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                let targets: Vec<String> = info.mut_borrows.drain(..).collect();
                for target in targets {
                    self.end_mut_borrow(&target);
                }
                return;
            }
        }
    }

    fn register_binding_borrow(&mut self, name: &str, target: String) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.mut_borrows.push(target.clone());
                *self.active_mut_borrows.entry(target).or_default() += 1;
                return;
            }
        }
    }

    fn ensure_mut_borrow_allowed(
        &self,
        module: &Module,
        span: Span,
        target: &str,
        errors: &mut Vec<TypeError>,
    ) {
        if self.active_mut_borrows.get(target).copied().unwrap_or(0) > 0 {
            errors.push(TypeError::new(
                &module.path,
                span,
                format!("`{}` is already mutably borrowed", target),
            ));
        }
    }

    fn end_mut_borrow(&mut self, target: &str) {
        if let Some(entry) = self.active_mut_borrows.get_mut(target) {
            if *entry > 1 {
                *entry -= 1;
            } else {
                self.active_mut_borrows.remove(target);
            }
        }
    }
}

fn instantiate_function(
    sig: &FunctionSignature,
    type_args: &[TypeExpr],
    path: &PathBuf,
    span: Span,
    errors: &mut Vec<TypeError>,
) -> FunctionSignature {
    if sig.type_params.is_empty() {
        if !type_args.is_empty() {
            errors.push(TypeError::new(
                path,
                span,
                format!(
                    "`{}` is not generic but {} type argument(s) were provided",
                    sig.name,
                    type_args.len()
                ),
            ));
        }
    } else if sig.type_params.len() != type_args.len() {
        errors.push(TypeError::new(
            path,
            span,
            format!(
                "`{}` expects {} type argument(s), got {}",
                sig.name,
                sig.type_params.len(),
                type_args.len()
            ),
        ));
        return sig.clone();
    }
    let mut map = HashMap::new();
    for (param, arg) in sig.type_params.iter().zip(type_args.iter()) {
        map.insert(param.clone(), arg.clone());
    }
    FunctionSignature {
        name: sig.name.clone(),
        type_params: sig.type_params.clone(),
        params: sig
            .params
            .iter()
            .map(|ann| TypeAnnotation {
                ty: ann.ty.substitute(&map),
                span: ann.span,
            })
            .collect(),
        returns: sig
            .returns
            .iter()
            .map(|ann| TypeAnnotation {
                ty: ann.ty.substitute(&map),
                span: ann.span,
            })
            .collect(),
        span: sig.span,
    }
}

fn lookup_struct_field(registry: &TypeRegistry, def: &StructDef, field: &str) -> Option<TypeExpr> {
    for struct_field in &def.fields {
        if let Some(name) = &struct_field.name {
            if name == field {
                return Some(struct_field.ty.ty.clone());
            }
        }
    }
    for struct_field in &def.fields {
        if struct_field.embedded {
            if let TypeExpr::Named(embed, _) = &struct_field.ty.ty {
                if let Some(embed_info) = registry.structs.get(embed) {
                    if let Some(found) = lookup_struct_field(registry, &embed_info.def, field) {
                        return Some(found);
                    }
                }
            }
        }
    }
    None
}

fn literal_type(lit: &Literal) -> TypeExpr {
    match lit {
        Literal::Int(_, _) => int_type(),
        Literal::Float(_, _) => float_type(),
        Literal::Bool(_, _) => bool_type(),
        Literal::String(_, _) => string_type(),
        Literal::Rune(_, _) => TypeExpr::Named("rune".into(), Vec::new()),
    }
}

fn int_type() -> TypeExpr {
    TypeExpr::Named("int32".into(), Vec::new())
}

fn float_type() -> TypeExpr {
    TypeExpr::Named("float32".into(), Vec::new())
}

fn bool_type() -> TypeExpr {
    TypeExpr::Named("bool".into(), Vec::new())
}

fn string_type() -> TypeExpr {
    TypeExpr::Named("string".into(), Vec::new())
}

fn make_box_type(inner: TypeExpr) -> TypeExpr {
    TypeExpr::Named("Box".into(), vec![inner])
}

fn make_option_type(inner: TypeExpr) -> TypeExpr {
    TypeExpr::Named("Option".into(), vec![inner])
}

fn is_string_type(ty: &TypeExpr) -> bool {
    matches!(ty, TypeExpr::Named(name, args) if name == "string" && args.is_empty())
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(lit) => literal_span(lit),
        Expr::Binary { span, .. } => *span,
        Expr::Unary { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::MapLiteral { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(if_expr) => if_expr.span,
        Expr::Match(m) => m.span,
        Expr::Tuple(_, span) => *span,
        Expr::ArrayLiteral(_, span) => *span,
        Expr::Range(range) => range.span,
        Expr::Reference { span, .. } => *span,
        Expr::Deref { span, .. } => *span,
        Expr::Try { span, .. } => *span,
        Expr::TryPropagate { span, .. } => *span,
        Expr::Move { span, .. } => *span,
    }
}

fn stmt_span(statement: &Statement) -> Span {
    match statement {
        Statement::Let(stmt) => stmt.span,
        Statement::Assign(AssignStmt { target, .. }) => expr_span(target),
        Statement::Expr(expr) => expr_span(&expr.expr),
        Statement::Return(_) => Span::new(0, 0),
        Statement::While(while_stmt) => while_stmt.body.span,
        Statement::For(stmt) => stmt.span,
        Statement::Defer(_) => Span::new(0, 0),
        Statement::Break | Statement::Continue => Span::new(0, 0),
        Statement::Block(block) => block.span,
    }
}

enum NumericKind {
    Int,
    Float,
}

fn numeric_kind(ty: &TypeExpr) -> Option<NumericKind> {
    match ty {
        TypeExpr::Named(name, _) if name == "int32" => Some(NumericKind::Int),
        TypeExpr::Named(name, _) if name == "float32" => Some(NumericKind::Float),
        _ => None,
    }
}

fn is_heap_type(ty: &TypeExpr) -> bool {
    matches!(ty, TypeExpr::Slice(_))
        || matches!(ty, TypeExpr::Named(name, _) if name == "Box" || name == "Map")
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

fn mutable_reference_target(expr: &Expr) -> Option<String> {
    if let Expr::Reference {
        mutable: true,
        expr,
        ..
    } = expr
    {
        if let Expr::Identifier(ident) = expr.as_ref() {
            return Some(ident.name.clone());
        }
    }
    None
}

fn strip_references<'a>(ty: &'a TypeExpr) -> &'a TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } => strip_references(ty),
        _ => ty,
    }
}

fn named_type_name<'a>(ty: &'a TypeExpr) -> Option<&'a str> {
    match ty {
        TypeExpr::Named(name, _) => Some(name),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => named_type_name(ty),
        _ => None,
    }
}
