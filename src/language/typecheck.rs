use crate::language::{
    ast::*,
    enum_utils::find_variant,
    span::Span,
    types::{TypeAnnotation, TypeExpr},
};
use std::{
    collections::{HashMap, HashSet},
    mem,
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
    pub fn new(path: &PathBuf, span: Span, message: impl Into<String>) -> Self {
        let message = message.into();
        Self {
            path: path.clone(),
            span,
            label: message.clone(),
            message,
        }
    }
}

struct TypeRegistry {
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    functions: HashMap<FunctionKey, FunctionSignature>,
    consts: HashMap<String, ConstInfo>,
    interfaces: HashMap<String, InterfaceInfo>,
    impls: HashSet<ImplKey>,
    pending_impls: Vec<ImplCandidate>,
    errors: Vec<TypeError>,
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self {
            structs: HashMap::new(),
            enums: HashMap::new(),
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            consts: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashSet::new(),
            pending_impls: Vec::new(),
            errors: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct StructInfo {
    def: StructDef,
}

#[derive(Clone)]
struct EnumInfo {
    def: EnumDef,
    module: String,
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    def: EnumVariant,
    span: Span,
    _module: String,
}

impl EnumVariantInfo {
    fn from_def(enum_name: String, enum_module: String, def: EnumVariant) -> Self {
        Self {
            enum_name,
            span: def.span,
            _module: enum_module,
            def,
        }
    }
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

#[derive(Clone)]
struct InterfaceInfo {
    def: InterfaceDef,
    #[allow(dead_code)]
    module: String,
    #[allow(dead_code)]
    span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ImplKey {
    interface: String,
    type_args: Vec<String>,
    target: String,
}

#[derive(Clone)]
struct ImplCandidate {
    module_path: PathBuf,
    block: ImplBlock,
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
    checker.errors.extend(checker.registry.errors.drain(..));
    checker.validate_impls();
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
                        EnumVariantInfo::from_def(
                            def.name.clone(),
                            module.name.clone(),
                            variant.clone(),
                        ),
                    );
                }
                registry
                    .enums
                    .insert(
                        def.name.clone(),
                        EnumInfo {
                            def: def.clone(),
                            module: module.name.clone(),
                        },
                    );
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
                registry.pending_impls.push(ImplCandidate {
                    module_path: module.path.clone(),
                    block: block.clone(),
                });
            }
            Item::Interface(def) => {
                if registry.interfaces.contains_key(&def.name) {
                    registry.errors.push(TypeError::new(
                        &module.path,
                        def.span,
                        format!("duplicate interface `{}`", def.name),
                    ));
                } else {
                    registry.interfaces.insert(
                        def.name.clone(),
                        InterfaceInfo {
                            def: def.clone(),
                            module: module.name.clone(),
                            span: def.span,
                        },
                    );
                }
            }
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
    fn resolve_enum_literal(
        &self,
        module: &Module,
        enum_name: Option<&str>,
        variant: &str,
        span: Span,
    ) -> Result<EnumVariantInfo, TypeError> {
        if let Some(name) = enum_name {
            let enum_info = self.registry.enums.get(name).ok_or_else(|| {
                TypeError::new(&module.path, span, format!("Unknown enum `{}`", name))
            })?;
            let variant_def =
                find_variant(&enum_info.def, &enum_info.module, variant, module, span)?;
            Ok(EnumVariantInfo::from_def(
                name.to_string(),
                enum_info.module.clone(),
                variant_def.clone(),
            ))
        } else if let Some(info) = self.registry.enum_variants.get(variant) {
            Ok(info.clone())
        } else {
            Err(TypeError::new(
                &module.path,
                span,
                format!("Unknown enum variant `{}`", variant),
            ))
        }
    }

    fn validate_impls(&mut self) {
        for candidate in self.registry.pending_impls.clone() {
            let block = candidate.block.clone();
            let span = block
                .methods
                .first()
                .map(|m| m.span)
                .unwrap_or_else(|| Span::new(0, 0));
            let Some(iface) = self.registry.interfaces.get(&block.interface).cloned() else {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("unknown interface `{}`", block.interface),
                ));
                continue;
            };
            if !self.registry.structs.contains_key(&block.target) {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("unknown target type `{}`", block.target),
                ));
                continue;
            }
            if iface.def.type_params.len() != block.type_args.len() {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!(
                        "`{}` expects {} type argument(s), got {}",
                        iface.def.name,
                        iface.def.type_params.len(),
                        block.type_args.len()
                    ),
                ));
                continue;
            }
            let key = ImplKey {
                interface: block.interface.clone(),
                type_args: block
                    .type_args
                    .iter()
                    .map(|ty| ty.canonical_name())
                    .collect(),
                target: block.target.clone(),
            };
            if self.registry.impls.contains(&key) {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!(
                        "`{}` is already implemented for `{}` with these type arguments",
                        block.interface, block.target
                    ),
                ));
                continue;
            }
            self.validate_impl_methods(&candidate, &iface, &block);
            self.registry.impls.insert(key);
        }
    }

    fn validate_impl_methods(
        &mut self,
        candidate: &ImplCandidate,
        iface: &InterfaceInfo,
        block: &ImplBlock,
    ) {
        let mut provided: HashMap<String, &FunctionDef> = HashMap::new();
        for method in &block.methods {
            provided.insert(method.name.clone(), method);
        }
        let type_arg_map: HashMap<String, TypeExpr> = iface
            .def
            .type_params
            .iter()
            .cloned()
            .zip(block.type_args.iter().cloned())
            .collect();
        let self_ty = TypeExpr::named(&block.target);
        for iface_method in &iface.def.methods {
            let expected_params: Vec<TypeAnnotation> = iface_method
                .params
                .iter()
                .map(|param| param.ty.substitute(&type_arg_map).replace_self(&self_ty))
                .collect();
            let expected_returns: Vec<TypeAnnotation> = iface_method
                .returns
                .iter()
                .map(|ret| ret.substitute(&type_arg_map).replace_self(&self_ty))
                .collect();
            let Some(impl_method) = provided.get(&iface_method.name) else {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    iface_method.span,
                    format!(
                        "`{}` missing method `{}` required by interface `{}`",
                        block.target, iface_method.name, iface.def.name
                    ),
                ));
                continue;
            };
            if impl_method.params.len() != expected_params.len() {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    impl_method.span,
                    format!(
                        "`{}` expects {} parameter(s) in `{}` (found {})",
                        iface.def.name,
                        expected_params.len(),
                        iface_method.name,
                        impl_method.params.len()
                    ),
                ));
                continue;
            }
            if impl_method.returns.len() != expected_returns.len() {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    impl_method.span,
                    format!(
                        "`{}` expects {} return value(s) in `{}` (found {})",
                        iface.def.name,
                        expected_returns.len(),
                        iface_method.name,
                        impl_method.returns.len()
                    ),
                ));
                continue;
            }
            for (param, expected) in impl_method.params.iter().zip(expected_params.iter()) {
                if param.ty.ty != expected.ty {
                    self.errors.push(TypeError::new(
                        &candidate.module_path,
                        param.span,
                        format!(
                            "parameter `{}` in `{}` should be `{}` to match interface `{}`",
                            param.name,
                            iface_method.name,
                            expected.ty.canonical_name(),
                            iface.def.name
                        ),
                    ));
                }
            }
            for (ret, expected) in impl_method.returns.iter().zip(expected_returns.iter()) {
                if ret.ty != expected.ty {
                    self.errors.push(TypeError::new(
                        &candidate.module_path,
                        ret.span,
                        format!(
                            "return type `{}` in `{}` should be `{}` to match interface `{}`",
                            ret.ty.canonical_name(),
                            iface_method.name,
                            expected.ty.canonical_name(),
                            iface.def.name
                        ),
                    ));
                }
            }
        }
    }

    fn check_program(&mut self, program: &Program) {
        for module in &program.modules {
            self.check_module(module);
        }
    }

    fn validate_format_string(
        &mut self,
        module: &Module,
        literal: &FormatStringLiteral,
        env: &mut FnEnv,
    ) {
        for segment in &literal.segments {
            if let FormatSegment::Expr { expr, .. } = segment {
                self.check_expression(module, expr, None, &[], env);
            }
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
                match &stmt.pattern {
                    Pattern::Identifier(name, _span) => {
                        let mut pending_borrows: Vec<String> = Vec::new();
                        if let Some(value) = &stmt.value {
                            let ty = self.check_expression(
                                module,
                                value,
                                expected.as_ref(),
                                returns,
                                env,
                            );
                            if let Some(expected) = expected.as_ref() {
                                self.ensure_type(module, stmt.span, expected, ty.as_ref());
                            } else if let Some(actual) = ty {
                                env.infer(name, Some(actual));
                            }
                            pending_borrows = expression_borrow_targets(value, env);
                        }
                        env.declare(name, expected, stmt.span, &mut self.errors);
                        for target in pending_borrows {
                            env.register_binding_borrow(name, target);
                        }
                    }
                    pattern => {
                        if let Some(value_expr) = &stmt.value {
                            let ty = self.check_expression(
                                module,
                                value_expr,
                                expected.as_ref(),
                                returns,
                                env,
                            );
                            self.bind_pattern(module, pattern, ty.as_ref(), env);
                        } else {
                            self.errors.push(TypeError::new(
                                &module.path,
                                stmt.span,
                                "destructuring bindings require an initializer",
                            ));
                        }
                    }
                }
            }
            Statement::Assign(assign) => {
                match &assign.target {
                    Expr::Identifier(ident) => {
                        env.clear_binding_borrows(&ident.name);
                        env.reset_moved(&ident.name);
                        let expected_type =
                            env.lookup(&ident.name).and_then(|info| info.ty.clone());
                        if let Some(expected) = expected_type {
                            let ty = self.check_expression(
                                module,
                                &assign.value,
                                Some(&expected),
                                returns,
                                env,
                            );
                            self.ensure_type(module, ident.span, &expected, ty.as_ref());
                            for target in expression_borrow_targets(&assign.value, env) {
                                env.register_binding_borrow(&ident.name, target);
                            }
                        } else {
                            self.check_expression(module, &assign.value, None, returns, env);
                            for target in expression_borrow_targets(&assign.value, env) {
                                env.register_binding_borrow(&ident.name, target);
                            }
                        }
                    }
                    Expr::Index { base, index, span } => {
                        let base_ty = self.check_expression(module, base, None, returns, env);
                        let Some(container_ty) = base_ty.as_ref() else {
                            self.check_expression(module, &assign.value, None, returns, env);
                            return;
                        };
                        let target_ty = strip_refs_and_pointers(container_ty);
                        match target_ty {
                            TypeExpr::Slice(inner) => {
                                self.check_expression(
                                    module,
                                    index,
                                    Some(&int_type()),
                                    returns,
                                    env,
                                );
                                let value_ty = self.check_expression(
                                    module,
                                    &assign.value,
                                    Some(inner.as_ref()),
                                    returns,
                                    env,
                                );
                                self.ensure_type(
                                    module,
                                    expr_span(&assign.value),
                                    inner.as_ref(),
                                    value_ty.as_ref(),
                                );
                            }
                            TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
                                self.check_expression(
                                    module,
                                    index,
                                    Some(&string_type()),
                                    returns,
                                    env,
                                );
                                let expected = &args[1];
                                let value_ty = self.check_expression(
                                    module,
                                    &assign.value,
                                    Some(expected),
                                    returns,
                                    env,
                                );
                                self.ensure_type(
                                    module,
                                    expr_span(&assign.value),
                                    expected,
                                    value_ty.as_ref(),
                                );
                            }
                            TypeExpr::Array { ty, .. } => {
                                self.check_expression(
                                    module,
                                    index,
                                    Some(&int_type()),
                                    returns,
                                    env,
                                );
                                let value_ty = self.check_expression(
                                    module,
                                    &assign.value,
                                    Some(ty.as_ref()),
                                    returns,
                                    env,
                                );
                                self.ensure_type(
                                    module,
                                    expr_span(&assign.value),
                                    ty.as_ref(),
                                    value_ty.as_ref(),
                                );
                            }
                            other => {
                                self.errors.push(TypeError::new(
                                    &module.path,
                                    *span,
                                    format!("`{}` cannot be indexed", other.canonical_name()),
                                ));
                                self.check_expression(module, &assign.value, None, returns, env);
                            }
                        }
                    }
                    _ => {
                        self.check_expression(module, &assign.value, None, returns, env);
                    }
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
            Statement::While(while_stmt) => match &while_stmt.condition {
                WhileCondition::Expr(condition) => {
                    self.check_expression(module, condition, Some(&bool_type()), returns, env);
                    let entry_state = env.snapshot_borrows();
                    let (_, body_state) = env.run_branch(|env| {
                        self.check_block(module, &while_stmt.body, returns, env);
                    });
                    env.merge_branch_borrows(&[entry_state, body_state]);
                }
                WhileCondition::Let { pattern, value } => {
                    let value_ty = self.check_expression(module, value, None, returns, env);
                    let pattern_value_ty = value_ty.clone();
                    let entry_state = env.snapshot_borrows();
                    let (_, body_state) = env.run_branch(move |env| {
                        env.push_scope();
                        self.bind_pattern(module, pattern, pattern_value_ty.as_ref(), env);
                        self.check_block(module, &while_stmt.body, returns, env);
                        env.pop_scope();
                    });
                    env.merge_branch_borrows(&[entry_state, body_state]);
                }
            },
            Statement::Loop(loop_stmt) => {
                let entry_state = env.snapshot_borrows();
                let (_, body_state) = env.run_branch(|env| {
                    self.check_block(module, &loop_stmt.body, returns, env);
                });
                env.merge_branch_borrows(&[entry_state, body_state]);
            }
            Statement::For(for_stmt) => match &for_stmt.target {
                ForTarget::Range(range) => {
                    let element_ty =
                        self.check_range_bounds(module, range, None, returns, env);
                    let entry_state = env.snapshot_borrows();
                    let (_, body_state) = env.run_branch(|env| {
                        env.push_scope();
                        env.declare(
                            &for_stmt.binding,
                            element_ty.clone(),
                            for_stmt.span,
                            &mut self.errors,
                        );
                        self.check_block(module, &for_stmt.body, returns, env);
                        env.pop_scope();
                    });
                    env.merge_branch_borrows(&[entry_state, body_state]);
                }
                ForTarget::Collection(expr) => {
                    let collection_ty = self.check_expression(module, expr, None, returns, env);
                    if let Some(coll_ty) = collection_ty {
                        if let Some(element_ty) = self.collection_element_type(&coll_ty) {
                            let entry_state = env.snapshot_borrows();
                            let element_ty_clone = element_ty.clone();
                            let (_, body_state) = env.run_branch(move |env| {
                                env.push_scope();
                                env.declare(
                                    &for_stmt.binding,
                                    Some(element_ty_clone.clone()),
                                    for_stmt.span,
                                    &mut self.errors,
                                );
                                self.check_block(module, &for_stmt.body, returns, env);
                                env.pop_scope();
                            });
                            env.merge_branch_borrows(&[entry_state, body_state]);
                        } else {
                            self.errors.push(TypeError::new(
                                &module.path,
                                for_stmt.span,
                                "`for` loops require a range, slice, map, or value with an `iter()` method",
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
                    env.ensure_not_moved(module, ident.span, &ident.name, &mut self.errors);
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
            Expr::Literal(lit) => Some(literal_type(lit, expected)),
            Expr::FormatString(literal) => {
                self.validate_format_string(module, literal, env);
                Some(string_type())
            }
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => self.check_binary(module, *op, left, right, *span, expected, returns, env),
            Expr::Unary { op, expr, span } => {
                self.check_unary(module, *op, expr, *span, expected, returns, env)
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
            Expr::EnumLiteral {
                enum_name,
                variant,
                values,
                span,
            } => {
        let info = match self.resolve_enum_literal(
            module,
            enum_name.as_deref(),
            variant,
            *span,
        ) {
            Ok(info) => info,
            Err(err) => {
                self.errors.push(err);
                return None;
            }
                };
                for (field, expr) in info.def.fields.iter().zip(values.iter()) {
                    self.check_expression(module, expr, Some(&field.ty), returns, env);
                }
                Some(TypeExpr::Named(info.enum_name.clone(), Vec::new()))
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
            Expr::Range(range) => {
                let expected_inner = match expected {
                    Some(TypeExpr::Named(name, args)) if name == "Range" && args.len() == 1 => {
                        Some(args[0].clone())
                    }
                    _ => None,
                };
                let inner =
                    self.check_range_bounds(module, range, expected_inner.as_ref(), returns, env);
                inner.map(|ty| TypeExpr::Named("Range".into(), vec![ty]))
            }
            Expr::Reference {
                mutable,
                expr,
                span,
            } => {
                if *mutable {
                    let context = HashMap::new();
                    if let Some(target) = borrow_target_from_expression(expr, env, &context) {
                        env.ensure_mut_borrow_allowed(module, *span, &target, &mut self.errors);
                    }
                }
                if reference_may_dangle(expr) {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        "cannot take a reference to a temporary value; assign it to a binding first",
                    ));
                }
                self.check_expression(module, expr, None, returns, env)
                    .map(|inner| TypeExpr::Reference {
                        mutable: *mutable,
                        ty: Box::new(inner),
                    })
            }
            Expr::Deref { expr, span } => {
                let ty = self.check_expression(module, expr, None, returns, env);
                match ty {
                    Some(TypeExpr::Reference { ty, .. }) | Some(TypeExpr::Pointer { ty, .. }) => {
                        Some(*ty)
                    }
                    other => {
                        if let Some(actual) = other {
                            self.errors.push(TypeError::new(
                                &module.path,
                                *span,
                                format!("cannot dereference `{}`", actual.canonical_name()),
                            ));
                        }
                        None
                    }
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
                if let Expr::Identifier(ident) = expr.as_ref() {
                    env.ensure_not_moved(module, *span, &ident.name, &mut self.errors);
                    env.mark_moved(&ident.name);
                }
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
            Expr::Spawn { expr, span } => {
                let inner = self.check_expression(module, expr, None, returns, env);
                let handle_ty = TypeExpr::Named(
                    "JoinHandle".into(),
                    vec![inner.clone().unwrap_or(TypeExpr::Unit)],
                );
                if inner.is_none() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        "`spawn` requires an expression",
                    ));
                }
                Some(handle_ty)
            }
            Expr::Index { base, index, span } => {
                let base_ty = self.check_expression(module, base, None, returns, env);
                let Some(container_ty) = base_ty.as_ref() else {
                    self.check_expression(module, index, None, returns, env);
                    return None;
                };
                let target_ty = strip_refs_and_pointers(container_ty);
                match target_ty {
                    TypeExpr::Slice(inner) => {
                        self.check_expression(module, index, Some(&int_type()), returns, env);
                        Some(make_option_type((**inner).clone()))
                    }
                    TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
                        self.check_expression(module, index, Some(&string_type()), returns, env);
                        Some(make_option_type(args[1].clone()))
                    }
                    TypeExpr::Array { ty, .. } => {
                        self.check_expression(module, index, Some(&int_type()), returns, env);
                        Some(make_option_type((**ty).clone()))
                    }
                    other => {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            format!("`{}` cannot be indexed", other.canonical_name()),
                        ));
                        self.check_expression(module, index, None, returns, env);
                        None
                    }
                }
            }
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
        let (then_ty, then_state) = env.run_branch(|env| match &expr.condition {
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
        });
        if let Some(else_branch) = &expr.else_branch {
            let (else_ty, else_state) = env.run_branch(|env| match else_branch {
                ElseBranch::Block(block) => self.check_block(module, block, returns, env),
                ElseBranch::ElseIf(nested) => {
                    self.check_if_expression(module, nested, expected, returns, env)
                }
            });
            env.merge_branch_borrows(&[then_state, else_state]);
            if let (Some(expected), Some(actual)) = (expected, else_ty.as_ref()) {
                self.ensure_type(module, expr.span, expected, Some(actual));
                return Some(actual.clone());
            }
            else_ty
        } else {
            let inactive_state = env.snapshot_borrows();
            env.merge_branch_borrows(&[then_state, inactive_state]);
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
        let mut borrow_states: Vec<BorrowState> = Vec::new();
        for arm in &expr.arms {
            let (value_ty, state) = env.run_branch(|env| {
                env.push_scope();
                self.bind_pattern(module, &arm.pattern, scrutinee_ty.as_ref(), env);
                if let Some(guard) = &arm.guard {
                    self.check_expression(module, guard, Some(&bool_type()), returns, env);
                }
                let ty = self.check_expression(module, &arm.value, expected, returns, env);
                env.pop_scope();
                ty
            });
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
            borrow_states.push(state);
        }
        env.merge_branch_borrows(&borrow_states);
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
        let Some(mut inner_ty) = self.check_expression(module, base, None, returns, env) else {
            return None;
        };
        loop {
            match inner_ty {
                TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
                    inner_ty = *ty;
                }
                _ => break,
            }
        }
        if let TypeExpr::Named(name, _) = &inner_ty {
            if let Some(struct_info) = self.registry.structs.get(&name.to_string()) {
                return lookup_struct_field(&self.registry, &struct_info.def, field);
            }
        }
        self.errors.push(TypeError::new(
            &module.path,
            span,
            format!("`{}` has no field `{}`", inner_ty.canonical_name(), field),
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
        let mut seen_mut_refs = HashSet::new();
        for arg in args {
            if let Some(target) = mutable_reference_target(arg) {
                if !seen_mut_refs.insert(target.clone()) {
                    self.errors.push(TypeError::new(
                        &module.path,
                        expr_span(arg),
                        format!("`{}` is already mutably borrowed in this call", target),
                    ));
                }
            }
        }

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
        let allows_type_args = matches!(name, "channel");
        if !allows_type_args && !type_args.is_empty() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!("`{}` does not accept type arguments", name),
            ));
        }
        match name {
            "out" => {
                if args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`out` expects at least 1 argument",
                    ));
                    return Some(TypeExpr::Unit);
                }
                if let Expr::FormatString(literal) = &args[0] {
                    let implicit_spans: Vec<Span> = literal
                        .segments
                        .iter()
                        .filter_map(|seg| {
                            if let FormatSegment::Implicit(span) = seg {
                                Some(*span)
                            } else {
                                None
                            }
                        })
                        .collect();
                    let implicit = implicit_spans.len();
                    if args.len() - 1 != implicit {
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            format!(
                                "`out` expects {} argument(s) to fill format placeholders, got {}",
                                implicit,
                                args.len().saturating_sub(1)
                            ),
                        ));
                    }
                    self.check_expression(module, &args[0], None, returns, env);
                    for arg in args.iter().skip(1) {
                        self.check_expression(module, arg, None, returns, env);
                    }
                } else {
                    if args.len() != 1 {
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            format!(
                                "`out` expects 1 argument or a format literal, got {}",
                                args.len()
                            ),
                        ));
                    }
                    self.check_expression(module, &args[0], None, returns, env);
                }
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
            "channel" => {
                if args.len() != 0 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`channel` expects 0 arguments",
                    ));
                }
                let element_ty = type_args.first().cloned().unwrap_or(TypeExpr::Unit);
                Some(TypeExpr::Tuple(vec![
                    TypeExpr::Named("Sender".into(), vec![element_ty.clone()]),
                    TypeExpr::Named("Receiver".into(), vec![element_ty]),
                ]))
            }
            "send" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`send` expects 2 arguments",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                let sender_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(TypeExpr::Named(name, params)) = sender_ty {
                    if name != "Sender" || params.len() != 1 {
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            "`send` expects Sender[T] as first argument",
                        ));
                    } else {
                        self.check_expression(module, &args[1], Some(&params[0]), returns, env);
                    }
                } else {
                    self.check_expression(module, &args[1], None, returns, env);
                }
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "recv" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`recv` expects 1 argument",
                    ));
                    return None;
                }
                let receiver_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(TypeExpr::Named(name, params)) = receiver_ty {
                    if name == "Receiver" && params.len() == 1 {
                        return Some(make_option_type(params[0].clone()));
                    }
                }
                Some(TypeExpr::Named("Option".into(), vec![TypeExpr::Unit]))
            }
            "close" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`close` expects 1 argument",
                    ));
                } else {
                    self.check_expression(module, &args[0], None, returns, env);
                }
                Some(TypeExpr::Unit)
            }
            "join" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`join` expects 1 argument",
                    ));
                    return None;
                }
                let handle_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(TypeExpr::Named(name, params)) = handle_ty {
                    if name == "JoinHandle" && params.len() == 1 {
                        return Some(params[0].clone());
                    }
                }
                Some(TypeExpr::Unit)
            }
            "ptr" | "ptr_mut" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`{}` expects 1 argument", name),
                    ));
                    return None;
                }
                let arg_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(TypeExpr::Reference { mutable, ty }) = arg_ty {
                    Some(TypeExpr::Pointer {
                        mutable: name == "ptr_mut" || mutable,
                        ty,
                    })
                } else if let Some(TypeExpr::Pointer { mutable, ty }) = arg_ty {
                    Some(TypeExpr::Pointer {
                        mutable: name == "ptr_mut" || mutable,
                        ty,
                    })
                } else {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`{}` requires a reference or pointer argument", name),
                    ));
                    None
                }
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
            TypeExpr::Named(name, _) if name == "string" => {
                self.check_string_method(module, method, args, returns, env, span)
            }
            TypeExpr::Named(name, _) if name.starts_with("int") => {
                self.check_int_method(module, method, args, returns, env, span)
            }
            TypeExpr::Named(name, _) if name.starts_with("float") => {
                self.check_float_method(module, method, args, returns, env, span)
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
            "slice_len" | "len" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`slice len` expects 0 argument(s) after receiver",
                    ));
                }
                Some(int_type())
            }
            "slice_get" | "get" => {
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
            "slice_push" | "push" => {
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
            "map_get" | "get" => {
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
            "map_insert" | "insert" => {
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
            "map_len" | "len" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`len` expects 0 argument(s) after receiver",
                    ));
                }
                Some(int_type())
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
            if expected == actual {
                return;
            }
            if self.interface_compatible(expected, actual) {
                return;
            }
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

    fn interface_compatible(&self, expected: &TypeExpr, actual: &TypeExpr) -> bool {
        let expected_iface = match expected {
            TypeExpr::Reference { mutable, ty } => match actual {
                TypeExpr::Reference {
                    mutable: other_mutable,
                    ty: inner_actual,
                } if mutable == other_mutable => {
                    return self.interface_compatible(ty, inner_actual);
                }
                _ => return false,
            },
            TypeExpr::Pointer { mutable, ty } => match actual {
                TypeExpr::Pointer {
                    mutable: other_mutable,
                    ty: inner_actual,
                } if mutable == other_mutable => {
                    return self.interface_compatible(ty, inner_actual);
                }
                _ => return false,
            },
            TypeExpr::Named(name, args) if self.registry.interfaces.contains_key(name) => {
                Some((name, args))
            }
            _ => None,
        };
        let Some((iface, args)) = expected_iface else {
            return false;
        };
        let Some(target) = named_type_name(actual) else {
            return false;
        };
        let key = ImplKey {
            interface: iface.clone(),
            type_args: args.iter().map(|ty| ty.canonical_name()).collect(),
            target: target.to_string(),
        };
        self.registry.impls.contains(&key)
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
                    let literal_ty = literal_type(lit, ty);
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
                if let Some(name) = enum_name {
                    if let Some(enum_info) = self.registry.enums.get(name) {
                        let Ok(variant_def) = find_variant(
                            &enum_info.def,
                            &enum_info.module,
                            variant,
                            module,
                            pattern_span(pattern),
                        ) else {
                            self.errors.push(TypeError::new(
                                &module.path,
                                pattern_span(pattern),
                                format!("`{}` does not belong to enum `{}`", variant, name),
                            ));
                            return;
                        };
                        if bindings.len() != variant_def.fields.len() {
                            self.errors.push(TypeError::new(
                                &module.path,
                                pattern_span(pattern),
                                format!(
                                    "`{}` expects {} field(s), found {}",
                                    variant,
                                    variant_def.fields.len(),
                                    bindings.len()
                                ),
                            ));
                            return;
                        }
                        let info = EnumVariantInfo::from_def(
                            enum_info.def.name.clone(),
                            enum_info.module.clone(),
                            variant_def.clone(),
                        );
                        let fields = instantiate_variant_fields(&info, ty, &self.registry);
                        for (binding, field) in bindings.iter().zip(fields.iter()) {
                            self.bind_pattern(module, binding, Some(&field.ty), env);
                        }
                        return;
                    } else {
                        self.errors.push(TypeError::new(
                            &module.path,
                            pattern_span(pattern),
                            format!("Unknown enum `{}`", name),
                        ));
                        return;
                    }
                }
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
                    let fields = instantiate_variant_fields(&info, ty, &self.registry);
                    for (binding, field) in bindings.iter().zip(fields.iter()) {
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

    fn check_range_bounds(
        &mut self,
        module: &Module,
        range: &RangeExpr,
        expected_inner: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let start_ty =
            self.check_expression(module, &range.start, expected_inner, returns, env);
        let end_ty = self.check_expression(module, &range.end, expected_inner, returns, env);
        self.resolve_range_element_type(module, range.span, start_ty, end_ty, expected_inner)
    }

    fn resolve_range_element_type(
        &mut self,
        module: &Module,
        span: Span,
        start_ty: Option<TypeExpr>,
        end_ty: Option<TypeExpr>,
        expected_inner: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        let mut candidate = match (start_ty.clone(), end_ty.clone()) {
            (Some(start), Some(end)) => {
                if start != end {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!(
                            "range bounds must share a type (found `{}` and `{}`)",
                            start.canonical_name(),
                            end.canonical_name()
                        ),
                    ));
                    None
                } else {
                    Some(start)
                }
            }
            (Some(start), None) => Some(start),
            (None, Some(end)) => Some(end),
            (None, None) => None,
        };

        if let Some(expected) = expected_inner {
            if let Some(actual) = &candidate {
                if actual != expected {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!(
                            "range expects `{}`, found `{}`",
                            expected.canonical_name(),
                            actual.canonical_name()
                        ),
                    ));
                }
            } else {
                candidate = Some(expected.clone());
            }
        }

        if let Some(ref ty) = candidate {
            match numeric_kind(ty) {
                Some(NumericKind::Signed(_, _)) | Some(NumericKind::Unsigned(_, _)) => {}
                Some(NumericKind::Float(_, _)) => {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "ranges only support integer bounds",
                    ));
                    return None;
                }
                None => {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "range bounds must be numeric",
                    ));
                    return None;
                }
            }
        }
        candidate
    }

    fn collection_element_type(&self, ty: &TypeExpr) -> Option<TypeExpr> {
        if let Some(inner) = self.direct_collection_element_type(ty) {
            return Some(inner);
        }
        if let Some(return_ty) = self.iter_method_return_type(ty) {
            return self.collection_element_type(&return_ty);
        }
        None
    }

    fn direct_collection_element_type(&self, ty: &TypeExpr) -> Option<TypeExpr> {
        match ty {
            TypeExpr::Slice(inner) => Some((**inner).clone()),
            TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
                Some(TypeExpr::Tuple(vec![args[0].clone(), args[1].clone()]))
            }
            TypeExpr::Named(name, args) if name == "Range" && args.len() == 1 => {
                Some(args[0].clone())
            }
            TypeExpr::Reference { ty: inner, .. } | TypeExpr::Pointer { ty: inner, .. } => {
                self.direct_collection_element_type(inner.as_ref())
            }
            _ => None,
        }
    }

    fn iter_method_return_type(&self, receiver: &TypeExpr) -> Option<TypeExpr> {
        let Some(name) = named_type_name(receiver) else {
            return None;
        };
        let sig = self.lookup_function("iter", Some(name))?;
        if sig.params.len() != 1 || sig.returns.len() != 1 {
            return None;
        }
        if named_type_name(&sig.params[0].ty) != named_type_name(receiver) {
            return None;
        }
        let return_ty = sig.returns[0].ty.clone();
        if &return_ty == receiver {
            return None;
        }
        Some(return_ty)
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
                | "assert"
                | "expect"
                | "str_len"
                | "str_contains"
                | "str_trim"
                | "str_split"
                | "min"
                | "max"
                | "abs"
                | "channel"
                | "send"
                | "recv"
                | "close"
                | "join"
                | "ptr"
                | "ptr_mut"
        )
    }

    fn check_string_method(
        &mut self,
        module: &Module,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        match method {
            "str_len" => {
                self.ensure_arguments(module, span, args, 0);
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("int32".into(), Vec::new()),
                    span,
                };
                self.select_call_result(
                    module,
                    span,
                    &[ret],
                    Some(&TypeExpr::Named("int32".into(), Vec::new())),
                )
            }
            "str_contains" => {
                self.ensure_arguments(module, span, args, 1);
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("string".into(), Vec::new())),
                    returns,
                    env,
                );
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("bool".into(), Vec::new()),
                    span,
                };
                self.select_call_result(
                    module,
                    span,
                    &[ret],
                    Some(&TypeExpr::Named("bool".into(), Vec::new())),
                )
            }
            "str_trim" => {
                self.ensure_arguments(module, span, args, 0);
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("string".into(), Vec::new()),
                    span,
                };
                self.select_call_result(
                    module,
                    span,
                    &[ret],
                    Some(&TypeExpr::Named("string".into(), Vec::new())),
                )
            }
            "str_split" => {
                self.ensure_arguments(module, span, args, 1);
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("string".into(), Vec::new())),
                    returns,
                    env,
                );
                let ret = TypeAnnotation {
                    ty: TypeExpr::Slice(Box::new(TypeExpr::Named("string".into(), Vec::new()))),
                    span,
                };
                self.select_call_result(
                    module,
                    span,
                    &[ret],
                    Some(&TypeExpr::Slice(Box::new(TypeExpr::Named(
                        "string".into(),
                        Vec::new(),
                    )))),
                )
            }
            _ => None,
        }
    }

    fn check_int_method(
        &mut self,
        module: &Module,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        match method {
            "abs" => {
                self.ensure_arguments(module, span, args, 0);
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("int32".into(), Vec::new()),
                    span,
                };
                let ret_ty = ret.ty.clone();
                self.select_call_result(module, span, &[ret.clone()], Some(&ret_ty))
            }
            "min" | "max" => {
                self.ensure_arguments(module, span, args, 1);
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("int32".into(), Vec::new())),
                    returns,
                    env,
                );
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("int32".into(), Vec::new()),
                    span,
                };
                let ret_ty = ret.ty.clone();
                self.select_call_result(module, span, &[ret.clone()], Some(&ret_ty))
            }
            _ => None,
        }
    }

    fn check_float_method(
        &mut self,
        module: &Module,
        method: &str,
        args: &[Expr],
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        match method {
            "abs" => {
                self.ensure_arguments(module, span, args, 0);
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("float64".into(), Vec::new()),
                    span,
                };
                let ret_ty = ret.ty.clone();
                self.select_call_result(module, span, &[ret.clone()], Some(&ret_ty))
            }
            "min" | "max" => {
                self.ensure_arguments(module, span, args, 1);
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("float64".into(), Vec::new())),
                    returns,
                    env,
                );
                let ret = TypeAnnotation {
                    ty: TypeExpr::Named("float64".into(), Vec::new()),
                    span,
                };
                let ret_ty = ret.ty.clone();
                self.select_call_result(module, span, &[ret.clone()], Some(&ret_ty))
            }
            _ => None,
        }
    }

    fn ensure_arguments(&mut self, module: &Module, span: Span, args: &[Expr], expected: usize) {
        if args.len() != expected {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!("method expects {} argument(s) after receiver", expected),
            ));
        }
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
        expected: Option<&TypeExpr>,
        left: Option<&TypeExpr>,
        right: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        let expected_kind = expected.and_then(numeric_kind);
        let left_kind = left.and_then(numeric_kind);
        let right_kind = right.and_then(numeric_kind);

        if let Some(target) = expected_kind {
            let left_ok = self.numeric_operand_matches(module, span, left, left_kind, target);
            let right_ok = self.numeric_operand_matches(module, span, right, right_kind, target);
            if left_ok && right_ok {
                return Some(numeric_type_from_kind(target));
            }
            return None;
        }

        let Some(kind) =
            self.unify_numeric_kinds(module, span, left, right, left_kind, right_kind)
        else {
            return None;
        };
        Some(numeric_type_from_kind(kind))
    }

    fn resolve_integer_type(
        &mut self,
        module: &Module,
        span: Span,
        expected: Option<&TypeExpr>,
        left: Option<&TypeExpr>,
        right: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        if matches!(expected.and_then(numeric_kind), Some(NumericKind::Float(_, _))) {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                "bitwise operations require integer operands",
            ));
            return None;
        }

        let left_kind = left.and_then(numeric_kind);
        let right_kind = right.and_then(numeric_kind);

        if matches!(left_kind, Some(NumericKind::Float(_, _)))
            || matches!(right_kind, Some(NumericKind::Float(_, _)))
        {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                "bitwise operations require integer operands",
            ));
            return None;
        }

        let kind = if let Some(target) = expected.and_then(numeric_kind) {
            let left_ok = self.numeric_operand_matches(module, span, left, left_kind, target);
            let right_ok = self.numeric_operand_matches(module, span, right, right_kind, target);
            if left_ok && right_ok {
                Some(target)
            } else {
                None
            }
        } else {
            self.unify_numeric_kinds(module, span, left, right, left_kind, right_kind)
        };

        match kind {
            Some(NumericKind::Float(_, _)) => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "bitwise operations require integer operands",
                ));
                None
            }
            Some(other) => Some(numeric_type_from_kind(other)),
            None => None,
        }
    }

    fn numeric_operand_matches(
        &mut self,
        module: &Module,
        span: Span,
        operand_ty: Option<&TypeExpr>,
        operand_kind: Option<NumericKind>,
        expected_kind: NumericKind,
    ) -> bool {
        match operand_kind {
            Some(kind) if self.numeric_kinds_compatible(expected_kind, kind) => true,
            Some(_) => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "expected `{}`, found `{}`",
                        numeric_type_from_kind(expected_kind).canonical_name(),
                        describe_type(operand_ty)
                    ),
                ));
                false
            }
            None => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "numeric operations require int or float operands",
                ));
                false
            }
        }
    }

    fn numeric_kinds_compatible(&self, expected: NumericKind, actual: NumericKind) -> bool {
        match (expected, actual) {
            (NumericKind::Float(_, _), NumericKind::Float(_, _))
            | (NumericKind::Float(_, _), NumericKind::Signed(_, _))
            | (NumericKind::Float(_, _), NumericKind::Unsigned(_, _)) => true,
            (NumericKind::Signed(_, exp_bits), NumericKind::Signed(_, act_bits)) => {
                exp_bits == act_bits
            }
            (NumericKind::Unsigned(_, exp_bits), NumericKind::Unsigned(_, act_bits)) => {
                exp_bits == act_bits
            }
            _ => false,
        }
    }

    fn unify_numeric_kinds(
        &mut self,
        module: &Module,
        span: Span,
        left_ty: Option<&TypeExpr>,
        right_ty: Option<&TypeExpr>,
        left_kind: Option<NumericKind>,
        right_kind: Option<NumericKind>,
    ) -> Option<NumericKind> {
        let mut had_error = false;
        if let (Some(ty), None) = (left_ty, left_kind) {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!(
                    "numeric operations require numeric operands (found `{}`)",
                    ty.canonical_name()
                ),
            ));
            had_error = true;
        }
        if let (Some(ty), None) = (right_ty, right_kind) {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!(
                    "numeric operations require numeric operands (found `{}`)",
                    ty.canonical_name()
                ),
            ));
            had_error = true;
        }
        if had_error {
            return None;
        }

        match (left_kind, right_kind) {
            (Some(kind), None) | (None, Some(kind)) => Some(kind),
            (None, None) => {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "numeric operations require int or float operands",
                ));
                None
            }
            (Some(NumericKind::Float(_, left_bits)), Some(NumericKind::Float(_, right_bits))) => {
                Some(float_kind(left_bits.max(right_bits)))
            }
            (Some(NumericKind::Float(_, bits)), Some(_))
            | (Some(_), Some(NumericKind::Float(_, bits))) => Some(float_kind(bits)),
            (Some(NumericKind::Signed(left_name, left_bits)), Some(NumericKind::Signed(_, right_bits))) => {
                if left_bits == right_bits {
                    Some(NumericKind::Signed(left_name, left_bits))
                } else {
                    self.emit_numeric_mismatch(module, span, left_ty, right_ty);
                    None
                }
            }
            (Some(NumericKind::Unsigned(left_name, left_bits)), Some(NumericKind::Unsigned(_, right_bits))) => {
                if left_bits == right_bits {
                    Some(NumericKind::Unsigned(left_name, left_bits))
                } else {
                    self.emit_numeric_mismatch(module, span, left_ty, right_ty);
                    None
                }
            }
            _ => {
                self.emit_numeric_mismatch(module, span, left_ty, right_ty);
                None
            }
        }
    }

    fn emit_numeric_mismatch(
        &mut self,
        module: &Module,
        span: Span,
        left_ty: Option<&TypeExpr>,
        right_ty: Option<&TypeExpr>,
    ) {
        self.errors.push(TypeError::new(
            &module.path,
            span,
            format!(
                "numeric operands must share a type (found `{}` and `{}`)",
                describe_type(left_ty),
                describe_type(right_ty)
            ),
        ));
    }

    fn check_binary(
        &mut self,
        module: &Module,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: Span,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                let numeric_expected = expected.and_then(|ty| numeric_kind(ty).map(|_| ty));
                let left_ty = self.check_expression(module, left, numeric_expected, returns, env);
                let right_ty = self.check_expression(
                    module,
                    right,
                    numeric_expected.or(left_ty.as_ref()),
                    returns,
                    env,
                );
                self.resolve_numeric_type(
                    module,
                    span,
                    numeric_expected,
                    left_ty.as_ref(),
                    right_ty.as_ref(),
                )
            }
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                let numeric_expected = expected.and_then(|ty| numeric_kind(ty).map(|_| ty));
                let left_ty = self.check_expression(module, left, numeric_expected, returns, env);
                let right_ty = self.check_expression(
                    module,
                    right,
                    numeric_expected.or(left_ty.as_ref()),
                    returns,
                    env,
                );
                self.resolve_integer_type(
                    module,
                    span,
                    numeric_expected,
                    left_ty.as_ref(),
                    right_ty.as_ref(),
                )
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
                let numeric_expected = expected.and_then(|ty| numeric_kind(ty).map(|_| ty));
                let left_ty = self.check_expression(module, left, numeric_expected, returns, env);
                let right_ty = self.check_expression(
                    module,
                    right,
                    numeric_expected.or(left_ty.as_ref()),
                    returns,
                    env,
                );
                self.resolve_numeric_type(
                    module,
                    span,
                    numeric_expected,
                    left_ty.as_ref(),
                    right_ty.as_ref(),
                );
                Some(bool_type())
            }
        }
    }

    fn check_unary(
        &mut self,
        module: &Module,
        op: UnaryOp,
        expr: &Expr,
        span: Span,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match op {
            UnaryOp::Neg => {
                let numeric_expected = expected.and_then(|ty| numeric_kind(ty).map(|_| ty));
                let ty = self.check_expression(module, expr, numeric_expected, returns, env);
                match ty.as_ref().and_then(numeric_kind) {
                    Some(NumericKind::Unsigned(_, _)) | None => {
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            "unary `-` requires a signed or floating-point type",
                        ));
                        None
                    }
                    Some(kind) => Some(numeric_type_from_kind(kind)),
                }
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
    moved: bool,
}

impl BindingInfo {
    fn new(ty: Option<TypeExpr>) -> Self {
        Self {
            ty,
            mut_borrows: Vec::new(),
            moved: false,
        }
    }
}

struct FnEnv {
    path: PathBuf,
    _type_params: HashSet<String>,
    scopes: Vec<HashMap<String, BindingInfo>>,
    active_mut_borrows: HashMap<String, usize>,
}

#[derive(Clone)]
struct BorrowState {
    active_mut_borrows: HashMap<String, usize>,
    scope_borrows: Vec<HashMap<String, HashMap<String, usize>>>,
    moved_scopes: Vec<HashSet<String>>,
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

    fn snapshot_borrows(&self) -> BorrowState {
        let mut scope_borrows = Vec::new();
        let mut moved_scopes = Vec::new();
        for scope in &self.scopes {
            let mut binding_map = HashMap::new();
            let mut moved = HashSet::new();
            for (name, info) in scope {
                binding_map.insert(name.clone(), Self::borrow_counts(&info.mut_borrows));
                if info.moved {
                    moved.insert(name.clone());
                }
            }
            scope_borrows.push(binding_map);
            moved_scopes.push(moved);
        }
        BorrowState {
            active_mut_borrows: self.active_mut_borrows.clone(),
            scope_borrows,
            moved_scopes,
        }
    }

    fn restore_borrow_state(&mut self, state: &BorrowState) {
        self.active_mut_borrows = state.active_mut_borrows.clone();
        self.apply_scope_borrows(&state.scope_borrows);
        self.apply_scope_moves(&state.moved_scopes);
    }

    fn run_branch<T, F>(&mut self, branch: F) -> (T, BorrowState)
    where
        F: FnOnce(&mut Self) -> T,
    {
        let snapshot = self.snapshot_borrows();
        let result = branch(self);
        let branch_state = self.snapshot_borrows();
        self.restore_borrow_state(&snapshot);
        (result, branch_state)
    }

    fn merge_branch_borrows(&mut self, states: &[BorrowState]) {
        if states.is_empty() {
            self.active_mut_borrows.clear();
            for scope in &mut self.scopes {
                for info in scope.values_mut() {
                    info.mut_borrows.clear();
                }
            }
            return;
        }
        let mut merged = states[0].clone();
        for state in &states[1..] {
            Self::intersect_counts(&mut merged.active_mut_borrows, &state.active_mut_borrows);
            for (idx, scope) in merged.scope_borrows.iter_mut().enumerate() {
                if let Some(other_scope) = state.scope_borrows.get(idx) {
                    Self::intersect_binding_borrows(scope, other_scope);
                } else {
                    scope.clear();
                }
            }
            for (idx, moved) in merged.moved_scopes.iter_mut().enumerate() {
                if let Some(other_moved) = state.moved_scopes.get(idx) {
                    moved.extend(other_moved.iter().cloned());
                }
            }
        }
        self.active_mut_borrows = merged.active_mut_borrows;
        self.apply_scope_borrows(&merged.scope_borrows);
        self.apply_scope_moves(&merged.moved_scopes);
    }

    fn intersect_counts(counts: &mut HashMap<String, usize>, other: &HashMap<String, usize>) {
        counts.retain(|target, count| {
            if let Some(other_count) = other.get(target) {
                *count = (*count).min(*other_count);
                *count > 0
            } else {
                false
            }
        });
    }

    fn intersect_binding_borrows(
        scope: &mut HashMap<String, HashMap<String, usize>>,
        other: &HashMap<String, HashMap<String, usize>>,
    ) {
        scope.retain(|name, counts| {
            if let Some(other_counts) = other.get(name) {
                Self::intersect_counts(counts, other_counts);
                !counts.is_empty()
            } else {
                false
            }
        });
    }

    fn apply_scope_borrows(&mut self, scope_borrows: &[HashMap<String, HashMap<String, usize>>]) {
        for (idx, scope) in self.scopes.iter_mut().enumerate() {
            if let Some(saved_scope) = scope_borrows.get(idx) {
                for (name, info) in scope.iter_mut() {
                    if let Some(counts) = saved_scope.get(name) {
                        info.mut_borrows = Self::counts_to_borrows(counts);
                    } else {
                        info.mut_borrows.clear();
                    }
                }
            } else {
                for info in scope.values_mut() {
                    info.mut_borrows.clear();
                }
            }
        }
    }

    fn apply_scope_moves(&mut self, moved_scopes: &[HashSet<String>]) {
        for (idx, scope) in self.scopes.iter_mut().enumerate() {
            if let Some(saved_scope) = moved_scopes.get(idx) {
                for (name, info) in scope.iter_mut() {
                    info.moved = saved_scope.contains(name);
                }
            } else {
                for info in scope.values_mut() {
                    info.moved = false;
                }
            }
        }
    }

    fn borrow_counts(borrows: &[String]) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        for target in borrows {
            *counts.entry(target.clone()).or_default() += 1;
        }
        counts
    }

    fn counts_to_borrows(counts: &HashMap<String, usize>) -> Vec<String> {
        let mut borrows = Vec::new();
        for (target, count) in counts {
            for _ in 0..*count {
                borrows.push(target.clone());
            }
        }
        borrows
    }

    fn binding_borrow_target(&self, name: &str) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return info.mut_borrows.first().cloned();
            }
        }
        None
    }

    fn ensure_not_moved(
        &self,
        module: &Module,
        span: Span,
        name: &str,
        errors: &mut Vec<TypeError>,
    ) {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                if info.moved {
                    errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`{}` was moved and can no longer be used here", name),
                    ));
                }
                return;
            }
        }
    }

    fn mark_moved(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.moved = true;
                return;
            }
        }
    }

    fn reset_moved(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.moved = false;
                return;
            }
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

fn literal_type(lit: &Literal, expected: Option<&TypeExpr>) -> TypeExpr {
    match lit {
        Literal::Int(_, _) => {
            if let Some(exp) = expected {
                if numeric_kind(exp).is_some() {
                    return exp.clone();
                }
            }
            int_type()
        }
        Literal::Float(_, _) => {
            if let Some(exp) = expected {
                if let Some(NumericKind::Float(_, _)) = numeric_kind(exp) {
                    return exp.clone();
                }
            }
            float_type()
        }
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

fn instantiate_variant_fields(
    info: &EnumVariantInfo,
    scrutinee: Option<&TypeExpr>,
    registry: &TypeRegistry,
) -> Vec<TypeAnnotation> {
    let mut fields = info.def.fields.clone();
    let Some(TypeExpr::Named(enum_name, args)) = scrutinee else {
        return fields;
    };
    if enum_name != &info.enum_name {
        return fields;
    }
    let Some(enum_info) = registry.enums.get(enum_name) else {
        return fields;
    };
    if enum_info.def.type_params.len() != args.len() {
        return fields;
    }
    let map: HashMap<String, TypeExpr> = enum_info
        .def
        .type_params
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect();
    for field in &mut fields {
        field.ty = field.ty.substitute(&map);
    }
    fields
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(lit) => literal_span(lit),
        Expr::FormatString(literal) => literal.span,
        Expr::Binary { span, .. } => *span,
        Expr::Unary { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::MapLiteral { span, .. } => *span,
        Expr::EnumLiteral { span, .. } => *span,
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
        Expr::Index { span, .. } => *span,
        Expr::Spawn { span, .. } => *span,
    }
}

fn stmt_span(statement: &Statement) -> Span {
    match statement {
        Statement::Let(stmt) => stmt.span,
        Statement::Assign(AssignStmt { target, .. }) => expr_span(target),
        Statement::Expr(expr) => expr_span(&expr.expr),
        Statement::Return(_) => Span::new(0, 0),
        Statement::While(while_stmt) => while_stmt.body.span,
        Statement::Loop(stmt) => stmt.span,
        Statement::For(stmt) => stmt.span,
        Statement::Defer(_) => Span::new(0, 0),
        Statement::Break | Statement::Continue => Span::new(0, 0),
        Statement::Block(block) => block.span,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumericKind {
    Signed(&'static str, u32),
    Unsigned(&'static str, u32),
    Float(&'static str, u32),
}

fn pointer_width_bits() -> u32 {
    (mem::size_of::<usize>() * 8) as u32
}

fn numeric_kind_from_name(name: &str) -> Option<NumericKind> {
    match name {
        "int8" => Some(NumericKind::Signed("int8", 8)),
        "int16" => Some(NumericKind::Signed("int16", 16)),
        "int32" => Some(NumericKind::Signed("int32", 32)),
        "int64" => Some(NumericKind::Signed("int64", 64)),
        "isize" => Some(NumericKind::Signed("isize", pointer_width_bits())),
        "uint8" => Some(NumericKind::Unsigned("uint8", 8)),
        "uint16" => Some(NumericKind::Unsigned("uint16", 16)),
        "uint32" => Some(NumericKind::Unsigned("uint32", 32)),
        "uint64" => Some(NumericKind::Unsigned("uint64", 64)),
        "usize" => Some(NumericKind::Unsigned("usize", pointer_width_bits())),
        "float32" => Some(NumericKind::Float("float32", 32)),
        "float64" => Some(NumericKind::Float("float64", 64)),
        _ => None,
    }
}

fn numeric_kind(ty: &TypeExpr) -> Option<NumericKind> {
    match ty {
        TypeExpr::Named(name, _) => numeric_kind_from_name(name),
        _ => None,
    }
}

fn numeric_type_from_kind(kind: NumericKind) -> TypeExpr {
    match kind {
        NumericKind::Signed(name, _) | NumericKind::Unsigned(name, _) | NumericKind::Float(name, _) => {
            TypeExpr::Named(name.into(), Vec::new())
        }
    }
}

fn float_kind(bits: u32) -> NumericKind {
    if bits >= 64 {
        NumericKind::Float("float64", 64)
    } else {
        NumericKind::Float("float32", 32)
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

fn describe_type(ty: Option<&TypeExpr>) -> String {
    ty.map(|t| t.canonical_name())
        .unwrap_or_else(|| "unknown".into())
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

fn expression_borrow_targets(expr: &Expr, env: &FnEnv) -> Vec<String> {
    let context = HashMap::new();
    expression_borrow_targets_with_context(expr, env, &context)
}

fn mutable_reference_target(expr: &Expr) -> Option<String> {
    if let Expr::Reference {
        mutable: true,
        expr: inner,
        ..
    } = expr
    {
        if let Expr::Identifier(ident) = inner.as_ref() {
            return Some(ident.name.clone());
        }
    }
    None
}

fn expression_borrow_targets_with_context(
    expr: &Expr,
    env: &FnEnv,
    context: &HashMap<String, String>,
) -> Vec<String> {
    match expr {
        Expr::Reference {
            mutable: true,
            expr,
            ..
        } => borrow_target_from_expression(expr, env, context)
            .into_iter()
            .collect(),
        Expr::Identifier(ident) => context.get(&ident.name).cloned().into_iter().collect(),
        Expr::Block(block) => block_borrow_targets(block, env, context),
        _ => Vec::new(),
    }
}

fn borrow_target_from_expression(
    expr: &Expr,
    env: &FnEnv,
    context: &HashMap<String, String>,
) -> Option<String> {
    match expr {
        Expr::Identifier(ident) => context
            .get(&ident.name)
            .cloned()
            .or_else(|| env.binding_borrow_target(&ident.name))
            .or_else(|| Some(ident.name.clone())),
        Expr::Deref { expr, .. } | Expr::Reference { expr, .. } => {
            borrow_target_from_expression(expr, env, context)
        }
        Expr::FieldAccess { base, .. } => borrow_target_from_expression(base, env, context),
        _ => None,
    }
}

fn block_borrow_targets(
    block: &Block,
    env: &FnEnv,
    parent: &HashMap<String, String>,
) -> Vec<String> {
    let mut map = parent.clone();
    for statement in &block.statements {
        if let Statement::Let(stmt) = statement {
            if let Some(value) = &stmt.value {
                if let Pattern::Identifier(name, _) = &stmt.pattern {
                    if let Some(target) = expression_borrow_targets_with_context(value, env, &map)
                        .into_iter()
                        .next()
                    {
                        map.insert(name.clone(), target);
                    }
                }
            }
        }
    }
    if let Some(tail) = &block.tail {
        expression_borrow_targets_with_context(tail, env, &map)
    } else {
        Vec::new()
    }
}

fn reference_may_dangle(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_) => false,
        Expr::Literal(_) | Expr::FormatString(_) => false,
        Expr::FieldAccess { base, .. } | Expr::Deref { expr: base, .. } => {
            reference_may_dangle(base)
        }
        Expr::Reference { expr, .. } => reference_may_dangle(expr),
        Expr::Block(_)
        | Expr::If(_)
        | Expr::Match(_)
        | Expr::Call { .. }
        | Expr::StructLiteral { .. }
        | Expr::MapLiteral { .. }
        | Expr::ArrayLiteral { .. }
        | Expr::Tuple(..)
        | Expr::Move { .. }
        | Expr::Unary { .. }
        | Expr::Binary { .. }
        | Expr::Try { .. }
        | Expr::TryPropagate { .. }
        | Expr::Index { .. }
        | Expr::Range(_) => true,
        Expr::Spawn { .. } => true,
        Expr::EnumLiteral { .. } => true,
    }
}

fn strip_refs_and_pointers<'a>(ty: &'a TypeExpr) -> &'a TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
            strip_refs_and_pointers(ty)
        }
        _ => ty,
    }
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

#[cfg(test)]
mod tests {
    use super::check_program;
    use crate::language::{ast::Program, parser::parse_module};
    use std::path::PathBuf;

    fn typecheck_source(source: &str) -> Result<(), Vec<super::TypeError>> {
        let module =
            parse_module("tests::module", PathBuf::from("test.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        check_program(&program)
    }

    #[test]
    fn borrows_release_after_control_flow() {
        let cases = [
            (
                "if",
                r#"
module tests::borrow_if;

fn release_after_if(flag: bool) {
  let mut int32 value = 0;
  if flag {
    let &mut int32 alias = &mut value;
    *alias = 1;
  } else {
    let &mut int32 alias = &mut value;
    *alias = 2;
  }
  let &mut int32 again = &mut value;
  *again = 3;
}
"#,
            ),
            (
                "match",
                r#"
module tests::borrow_match;

fn release_after_match(flag: bool) {
  let mut int32 value = 0;
  match flag {
    true => {
      let &mut int32 alias = &mut value;
      *alias = 1;
    },
    false => {
      let &mut int32 alias = &mut value;
      *alias = 2;
    },
  }
  let &mut int32 next = &mut value;
  *next = 4;
}
"#,
            ),
            (
                "while",
                r#"
module tests::borrow_while;

fn release_after_while() {
  let mut int32 value = 0;
  let mut int32 idx = 0;
  while idx < 1 {
    let &mut int32 alias = &mut value;
    *alias = idx;
    idx = idx + 1;
  }
  let &mut int32 after = &mut value;
  *after = 5;
}
"#,
            ),
            (
                "while_let",
                r#"
module tests::borrow_while_let;

fn release_after_while_let() {
  let mut int32 value = 0;
  let mut int32 idx = 0;
  while let true = idx == 0 {
    let &mut int32 alias = &mut value;
    *alias = idx;
    idx = idx + 1;
  }
  let &mut int32 after = &mut value;
  *after = 6;
}
"#,
            ),
            (
                "for_range",
                r#"
module tests::borrow_for_range;

fn release_after_for_range() {
  let mut int32 value = 0;
  for count in 0..1 {
    let &mut int32 alias = &mut value;
    *alias = count;
  }
  let &mut int32 after = &mut value;
  *after = 9;
}
"#,
            ),
            (
                "for_collection",
                r#"
module tests::borrow_for_collection;

fn release_after_for_collection() {
  let []int32 entries = [1, 2];
  let mut int32 value = 0;
  for entry in entries {
    let &mut int32 alias = &mut value;
    *alias = entry;
  }
  let &mut int32 after = &mut value;
  *after = 7;
}
"#,
            ),
        ];

        for (name, source) in cases {
            if let Err(errors) = typecheck_source(source) {
                panic!(
                    "borrow merging failed for case `{}`: {:?}",
                    name,
                    errors
                        .iter()
                        .map(|err| format!("{} @{}", err.message, err.span.start))
                        .collect::<Vec<_>>()
                );
            }
        }
    }

    #[test]
    fn borrow_remains_live_without_branching() {
        let source = r#"
module tests::borrow;

fn alias_survives_scope() {
  let mut int32 value = 0;
  let &mut int32 alias = &mut value;
  let &mut int32 second = &mut value;
  *second = 1;
}
"#;
        let errors = typecheck_source(source).expect_err("expected borrow error");
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("already mutably borrowed")),
            "expected active borrow diagnostic, got {:?}",
            errors
        );
    }

    #[test]
    fn reborrow_through_alias_is_rejected() {
        let source = r#"
module tests::reborrow;

fn reject_reborrow() {
  let mut int32 value = 0;
  let &mut int32 first = &mut value;
  let &mut int32 second = &mut *first;
  *second = 1;
}
"#;
        let errors = typecheck_source(source).expect_err("expected reborrow error");
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("already mutably borrowed")),
            "expected reborrow diagnostic, got {:?}",
            errors
        );
    }

    #[test]
    fn moved_bindings_cannot_be_reused() {
        let source = r#"
module tests::moves;

fn move_then_use() {
  let []string squad = ["alpha"];
  let []string redeployed = move squad;
  let int32 leftover = squad.len();
}
"#;
        let errors = typecheck_source(source).expect_err("expected move error");
        assert!(
            errors.iter().any(|err| err.message.contains("was moved")),
            "expected move diagnostic, got {:?}",
            errors
        );
    }

    #[test]
    fn dangling_reference_to_temporary_is_flagged() {
        let source = r#"
module tests::dangling;

fn compute() -> int32 { 4 }

fn borrow_temporary() {
  let &int32 alias = &compute();
}
"#;
        let errors = typecheck_source(source).expect_err("expected dangling reference error");
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("temporary value")),
            "expected dangling diagnostic, got {:?}",
            errors
        );
    }

    #[test]
    fn mutable_destructuring_typechecks() {
        let source = r#"
module tests::patterns;

struct Telemetry {
  hp: int32;
  mp: int32;
  notes: []string;
}

fn mutate_destructuring() {
  let mut (left, right) = (10, 5);
  left = left + right;

  let mut #{ "hp": hp_score, "mp": mp_score } = #{
    "hp": 80,
    "mp": 40,
  };
  hp_score = hp_score + mp_score;

  let mut Telemetry{ hp, mp, .. } = Telemetry{
    hp: 70,
    mp: 35,
    notes: ["alpha"],
  };
  hp = hp + mp;

  let mut [first, ..rest] = ["steady", "ready"];
  first = "start";
  rest = rest;
}
"#;
        match typecheck_source(source) {
            Ok(()) => {}
            Err(errors) => panic!(
                "expected mutable destructuring patterns to typecheck, got {:?}",
                errors
                    .iter()
                    .map(|err| format!("{} @{}", err.message, err.span.start))
                    .collect::<Vec<_>>()
            ),
        }
    }

    #[test]
    fn format_string_requires_known_identifiers() {
        let source = r#"
module tests::format_lookup;

fn check() {
  let int32 hp = 10;
  out(`hp is {hp}`);
  out(`missing {mp}`);
}
"#;
        let errors =
            typecheck_source(source).expect_err("expected error for unknown format identifier");
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("unknown identifier `mp`")),
            "expected unknown identifier error, got {:?}",
            errors
        );
    }

    #[test]
    fn format_string_argument_mismatch_is_rejected() {
        let source = r#"
module tests::format_args;

fn report() {
  out(`value {} {}`, 1);
}
"#;
        let errors = typecheck_source(source).expect_err("expected format arg count error");
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("fill format placeholders")),
            "expected placeholder count error, got {:?}",
            errors
        );
    }
}
