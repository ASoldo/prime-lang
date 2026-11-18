use super::*;
use crate::language::integer::{IntegerOp, IntegerType, Scalar};
use crate::language::tcp_builtin::TcpBuiltin;

#[path = "match_coverage.rs"]
mod match_coverage;

pub fn check_program(expanded: &ExpandedProgram) -> Result<(), Vec<TypeError>> {
    check_program_with_options(expanded, &TypecheckOptions::default())
}

pub fn check_program_with_options(
    expanded: &ExpandedProgram,
    options: &TypecheckOptions,
) -> Result<(), Vec<TypeError>> {
    check_program_typed(expanded, options).map(|_| ())
}

pub fn check_program_typed<'a>(
    expanded: &'a ExpandedProgram,
    options: &TypecheckOptions,
) -> Result<CheckedProgram<'a>, Vec<TypeError>> {
    let program = &expanded.program;
    let mut registry = TypeRegistry::default();
    for module in &program.modules {
        collect_definitions(&mut registry, module);
    }
    for module in &program.modules {
        collect_public_type_imports(&mut registry, module);
    }
    let mut checker = Checker {
        registry,
        folded_integers: HashMap::new(),
        facts: CheckedFacts::default(),
        errors: Vec::new(),
        imports: HashMap::new(),
        current_module: None,
        current_no_std: false,
        target: options.target.clone(),
    };
    checker.errors.append(&mut checker.registry.errors);
    checker.validate_drop_impls();
    checker.validate_impls();
    checker.check_program(program);
    if checker.errors.is_empty() {
        return Ok(CheckedProgram {
            program,
            target: checker.target,
            facts: checker.facts,
        });
    }

    let mut errors = checker.errors;
    for err in &mut errors {
        if err.help.is_none() {
            err.help = expanded.traces.help_for(&err.path, err.span);
        }
    }
    Err(errors)
}

fn collect_public_type_imports(registry: &mut TypeRegistry, module: &Module) {
    for import in &module.imports {
        let (Visibility::Public, Some(selectors)) = (import.visibility, &import.selectors) else {
            continue;
        };
        let path = import.canonical_path();
        let owner = path.strip_suffix("::prelude").unwrap_or(&path);
        for selector in selectors {
            let ImportSelector::Name { name, alias, .. } = selector else {
                continue;
            };
            let visibility = registry
                .structs
                .get(name)
                .filter(|info| info._module == owner)
                .map(|info| info.def.visibility)
                .or_else(|| {
                    registry
                        .enums
                        .get(name)
                        .filter(|info| info.module == owner)
                        .map(|info| info.def.visibility)
                })
                .or_else(|| {
                    registry
                        .module_symbols(owner)?
                        .interfaces
                        .get(name)
                        .map(|info| info.def.visibility)
                });
            if visibility == Some(Visibility::Public) {
                registry
                    .module_symbols_mut(&module.name)
                    .public_types
                    .insert(
                        alias.as_ref().unwrap_or(name).clone(),
                        (owner.to_owned(), name.clone()),
                    );
            }
        }
    }
}

fn collect_definitions(registry: &mut TypeRegistry, module: &Module) {
    {
        let symbols = registry.module_symbols_mut(&module.name);
        symbols.prelude = module.prelude.clone();
    }
    registry.set_module_no_std(&module.name, module.no_std);
    for item in &module.items {
        match item {
            Item::Struct(def) => {
                registry.structs.insert(
                    def.name.clone(),
                    StructInfo {
                        def: def.clone(),
                        _module: module.name.clone(),
                    },
                );
            }
            Item::Enum(def) => {
                for (tag, variant) in def.variants.iter().enumerate() {
                    registry.enum_variants.insert(
                        variant.name.clone(),
                        EnumVariantInfo::from_def(
                            def.name.clone(),
                            module.name.clone(),
                            variant.clone(),
                            tag as u32,
                        ),
                    );
                }
                registry.enums.insert(
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
                let symbols = registry.module_symbols_mut(&module.name);
                symbols.consts.insert(
                    def.name.clone(),
                    ConstInfo {
                        ty: def.ty.clone(),
                        visibility: def.visibility,
                        _span: def.span,
                        _module: module.name.clone(),
                    },
                );
            }
            Item::Macro(_) => {}
            Item::MacroInvocation(_) => {}
            Item::Comment { .. } => {}
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
                let candidate = ImplCandidate {
                    module_path: module.path.clone(),
                    block: block.clone(),
                };
                if block.inherent || block.interface == "Drop" {
                    registry.drop_impls.push(candidate);
                } else {
                    registry.pending_impls.push(candidate);
                }
            }
            Item::Interface(def) => {
                let symbols = registry.module_symbols_mut(&module.name);
                if symbols.interfaces.contains_key(&def.name) {
                    registry.errors.push(TypeError::new(
                        &module.path,
                        def.span,
                        format!("duplicate interface `{}`", def.name),
                    ));
                } else {
                    symbols.interfaces.insert(
                        def.name.clone(),
                        InterfaceInfo {
                            def: def.clone(),
                            _module: module.name.clone(),
                            _span: def.span,
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
        if let Some(ty) = param.ty.as_mut() {
            *ty = ty.replace_self(&concrete);
        }
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
    let params: Vec<TypeAnnotation> = def
        .params
        .iter()
        .filter_map(|param| param.ty.clone())
        .collect();
    let returns = def.returns.clone();
    let base_signature = FunctionSignature {
        name: def.name.clone(),
        type_params: def.type_params.clone(),
        params: params.clone(),
        returns: returns.clone(),
        span: def.span,
    };
    let symbols = registry.module_symbols_mut(module);
    symbols.functions.insert(
        FunctionKey {
            name: def.name.clone(),
            receiver: receiver.clone(),
        },
        FunctionInfo {
            signature: base_signature,
            visibility: def.visibility,
            _module: module.to_string(),
        },
    );
}

struct Checker {
    folded_integers: HashMap<ExprId, i128>,
    facts: CheckedFacts,
    registry: TypeRegistry,
    errors: Vec<TypeError>,
    imports: HashMap<String, ImportScope>,
    current_module: Option<String>,
    current_no_std: bool,
    target: BuildTarget,
}

impl Checker {
    fn integer_constant(&self, expr: &Expr) -> Option<i128> {
        match expr {
            Expr::Literal(Literal::Int(n, _)) => Some(*n),
            Expr::Identifier(ident) => self.folded_integers.get(&ident.id).copied(),
            Expr::Unary {
                op: UnaryOp::Neg,
                expr,
                ..
            } if matches!(expr.as_ref(), Expr::Literal(Literal::Int(..))) => {
                self.integer_constant(expr)?.checked_neg()
            }
            Expr::Binary { id, .. } | Expr::Unary { id, .. } | Expr::Call { id, .. } => {
                self.folded_integers.get(id).copied()
            }
            _ => None,
        }
    }

    fn record_integer_op(
        &mut self,
        module: &Module,
        span: Span,
        id: ExprId,
        op: IntegerOp,
        a: &Expr,
        b: Option<&Expr>,
    ) {
        self.facts.integers.operations.insert(id, op);
        let Some(a) = self.integer_constant(a) else {
            return;
        };
        let b = match b {
            Some(expr) => match self.integer_constant(expr) {
                Some(n) => Some(n),
                None => return,
            },
            None => None,
        };
        match op.evaluate(a, b) {
            Ok(Scalar::Int(n)) => {
                self.folded_integers.insert(id, n);
            }
            Ok(Scalar::Bool(_)) => {}
            Err(message) => self
                .errors
                .push(TypeError::new(&module.path, span, message)),
        }
    }

    fn resolve_enum_literal(
        &self,
        module: &Module,
        env: &FnEnv,
        enum_name: Option<&str>,
        variant: &str,
        span: Span,
    ) -> Result<EnumVariantInfo, Box<TypeError>> {
        if let Some(name) = enum_name {
            let mut private = false;
            let enum_info = self.lookup_enum(env, name, &mut private).ok_or_else(|| {
                Box::new(if private {
                    TypeError::new(&module.path, span, format!("enum `{}` is not public", name))
                } else {
                    TypeError::new(&module.path, span, format!("Unknown enum `{}`", name))
                })
            })?;
            let (tag, variant_def) =
                find_variant_with_tag(&enum_info.def, &enum_info.module, variant, module, span)?;
            Ok(EnumVariantInfo::from_def(
                name.to_string(),
                enum_info.module.clone(),
                variant_def.clone(),
                tag,
            ))
        } else if let Some(info) = self.registry.enum_variants.get(variant) {
            Ok(info.clone())
        } else {
            Err(Box::new(TypeError::new(
                &module.path,
                span,
                format!("Unknown enum variant `{}`", variant),
            )))
        }
    }

    fn can_access(&self, module: &str, visibility: Visibility) -> bool {
        matches!(visibility, Visibility::Public) || self.current_module.as_deref() == Some(module)
    }

    fn pointer_bits(&self) -> u32 {
        self.target.pointer_width_bits()
    }

    fn build_import_scope(&mut self, module: &Module) -> ImportScope {
        let mut scope = ImportScope::default();
        self.apply_default_prelude(module, &mut scope);
        for import in &module.imports {
            let (module_name, selectors_override) = if import
                .path
                .segments
                .last()
                .map(|s| s == "prelude")
                .unwrap_or(false)
            {
                if import.path.segments.len() < 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        import.span,
                        "prelude import requires a parent module",
                    ));
                    continue;
                }
                let base_segments = &import.path.segments[..import.path.segments.len() - 1];
                let base_module = base_segments.join("::");
                let Some(symbols) = self.registry.module_symbols(&base_module) else {
                    self.errors.push(TypeError::new(
                        &module.path,
                        import.span,
                        format!("unknown module `{}`", base_module),
                    ));
                    continue;
                };
                let selectors = if let Some(sel) = &import.selectors {
                    sel.clone()
                } else {
                    symbols.prelude.clone()
                };
                (base_module, Some(selectors))
            } else {
                (import.canonical_path(), import.selectors.clone())
            };
            if self.registry.module_symbols(&module_name).is_none() {
                self.errors.push(TypeError::new(
                    &module.path,
                    import.span,
                    format!("unknown module `{}`", module_name),
                ));
                continue;
            }
            if self.current_no_std && !self.registry.module_no_std(&module_name) {
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        import.span,
                        format!(
                            "module `{}` is not marked `no_std` but `{}` requires no-std",
                            module_name, module.name
                        ),
                    )
                    .with_code("E0NS")
                    .with_help(
                        "mark the imported module as `no_std = true` in its prime.toml or avoid importing it from a no-std module",
                    ),
                );
                continue;
            }
            if let Some(selectors) = selectors_override.as_ref() {
                for selector in selectors {
                    match selector {
                        ImportSelector::Name { name, alias, span } => {
                            let local_name = alias.clone().unwrap_or_else(|| name.clone());
                            if let Some(existing) = scope.selected.get(&local_name) {
                                if self.registry.type_origin(&existing.module, &existing.name)
                                    == self.registry.type_origin(&module_name, name)
                                {
                                    continue;
                                }
                                self.errors.push(TypeError::new(
                                    &module.path,
                                    *span,
                                    format!("duplicate import `{}`", local_name),
                                ));
                                continue;
                            }
                            if !self.validate_public_symbol(&module_name, name, &module.path, *span)
                            {
                                continue;
                            }
                            scope.selected.insert(
                                local_name,
                                ImportedItem {
                                    module: module_name.clone(),
                                    name: name.clone(),
                                },
                            );
                        }
                        ImportSelector::Glob(span) => {
                            if scope.globs.contains(&module_name) {
                                self.errors.push(TypeError::new(
                                    &module.path,
                                    *span,
                                    format!("duplicate glob import from `{}`", module_name),
                                ));
                                continue;
                            }
                            scope.globs.push(module_name.clone());
                        }
                    }
                }
            } else {
                let alias = import
                    .alias
                    .clone()
                    .or_else(|| import.path.segments.last().cloned())
                    .unwrap_or_else(|| module_name.clone());
                if scope.modules.contains_key(&alias) {
                    self.errors.push(TypeError::new(
                        &module.path,
                        import.span,
                        format!("duplicate module import alias `{}`", alias),
                    ));
                    continue;
                }
                scope.modules.insert(alias, module_name.clone());
                if let Some(symbols) = self.registry.module_symbols(&module_name) {
                    let prelude = symbols.prelude.clone();
                    for selector in prelude {
                        match selector {
                            ImportSelector::Name { name, alias, span } => {
                                let local_name = alias.clone().unwrap_or_else(|| name.clone());
                                if let Some(existing) = scope.selected.get(&local_name) {
                                    if self.registry.type_origin(&existing.module, &existing.name)
                                        == self.registry.type_origin(&module_name, &name)
                                    {
                                        continue;
                                    }
                                    self.errors.push(TypeError::new(
                                        &module.path,
                                        span,
                                        format!("duplicate import `{}`", local_name),
                                    ));
                                    continue;
                                }
                                if !self.validate_public_symbol(
                                    &module_name,
                                    &name,
                                    &module.path,
                                    span,
                                ) {
                                    continue;
                                }
                                scope.selected.insert(
                                    local_name,
                                    ImportedItem {
                                        module: module_name.clone(),
                                        name: name.clone(),
                                    },
                                );
                            }
                            ImportSelector::Glob(span) => {
                                scope.globs.push(module_name.clone());
                                let _ = span;
                            }
                        }
                    }
                }
            }
        }
        scope
    }

    fn apply_default_prelude(&mut self, module: &Module, scope: &mut ImportScope) {
        let default = "core::types";
        if module.name == default {
            return;
        }
        let Some(symbols) = self.registry.module_symbols(default) else {
            return;
        };
        let prelude = symbols.prelude.clone();
        for selector in prelude {
            match selector {
                ImportSelector::Name { name, alias, span } => {
                    let local_name = alias.clone().unwrap_or_else(|| name.clone());
                    if scope.selected.contains_key(&local_name) {
                        continue;
                    }
                    if !self.validate_public_symbol(default, &name, &module.path, span) {
                        continue;
                    }
                    scope.selected.insert(
                        local_name,
                        ImportedItem {
                            module: default.to_string(),
                            name,
                        },
                    );
                }
                ImportSelector::Glob(_) => {
                    if !scope.globs.contains(&default.to_string()) {
                        scope.globs.push(default.to_string());
                    }
                }
            }
        }
    }

    fn validate_public_symbol(
        &mut self,
        module_name: &str,
        name: &str,
        path: &Path,
        span: Span,
    ) -> bool {
        let Some(symbols) = self.registry.module_symbols(module_name) else {
            self.errors.push(TypeError::new(
                path,
                span,
                format!("unknown module `{}`", module_name),
            ));
            return false;
        };
        let key = FunctionKey {
            name: name.to_string(),
            receiver: None,
        };
        if let Some(func) = symbols.functions.get(&key) {
            if self.can_access(module_name, func.visibility) {
                return true;
            }
            self.errors.push(TypeError::new(
                path,
                span,
                format!("`{}` is not public in `{}`", name, module_name),
            ));
            return false;
        }
        if let Some(const_info) = symbols.consts.get(name) {
            if self.can_access(module_name, const_info.visibility) {
                return true;
            }
            self.errors.push(TypeError::new(
                path,
                span,
                format!("`{}` is not public in `{}`", name, module_name),
            ));
            return false;
        }
        let (owner, type_name) = self.registry.type_origin(module_name, name);
        if let Some(interface) = self
            .registry
            .module_symbols(owner)
            .and_then(|symbols| symbols.interfaces.get(type_name))
        {
            if self.can_access(module_name, interface.def.visibility) {
                return true;
            }
            self.errors.push(TypeError::new(
                path,
                span,
                format!("`{}` is not public in `{}`", name, module_name),
            ));
            return false;
        }
        if let Some(struct_info) = self.registry.find_struct_in_module(module_name, name) {
            if self.can_access(module_name, struct_info.def.visibility) {
                return true;
            }
            self.errors.push(TypeError::new(
                path,
                span,
                format!("`{}` is not public in `{}`", name, module_name),
            ));
            return false;
        }
        if let Some(enum_info) = self.registry.find_enum_in_module(module_name, name) {
            if self.can_access(module_name, enum_info.def.visibility) {
                return true;
            }
            self.errors.push(TypeError::new(
                path,
                span,
                format!("`{}` is not public in `{}`", name, module_name),
            ));
            return false;
        }
        self.errors.push(TypeError::new(
            path,
            span,
            format!("`{}` not found in `{}`", name, module_name),
        ));
        false
    }

    fn lookup_function_in_module(
        &self,
        module_name: &str,
        func_name: &str,
        receiver: Option<&str>,
    ) -> Option<FunctionSignature> {
        let key = FunctionKey {
            name: func_name.to_string(),
            receiver: receiver.map(|s| s.to_string()),
        };
        let info = self.registry.find_function_in_module(module_name, &key)?;
        if self.can_access(module_name, info.visibility) {
            Some(info.signature)
        } else {
            None
        }
    }

    fn resolve_module_name<'a>(&self, env: &'a FnEnv, name: &'a str) -> Option<&'a str> {
        if let Some(found) = env.module_alias(name) {
            Some(found.as_str())
        } else if self.registry.module_symbols(name).is_some() {
            Some(name)
        } else {
            None
        }
    }

    fn lookup_const_in_module(&self, module_name: &str, name: &str) -> Option<TypeExpr> {
        let info = self.registry.find_const_in_module(module_name, name)?;
        if self.can_access(module_name, info.visibility) {
            info.ty.as_ref().map(|ann| ann.ty.clone())
        } else {
            None
        }
    }

    fn lookup_module_const_type(
        &mut self,
        module_path: &Path,
        module_name: &str,
        name: &str,
        span: Span,
    ) -> Option<TypeExpr> {
        let Some(info) = self.registry.find_const_in_module(module_name, name) else {
            self.errors.push(TypeError::new(
                module_path,
                span,
                format!("`{}` not found in module `{}`", name, module_name),
            ));
            return None;
        };
        if self.can_access(module_name, info.visibility) {
            return info.ty.as_ref().map(|ann| ann.ty.clone());
        }
        self.errors.push(TypeError::new(
            module_path,
            span,
            format!("`{}` is not public in `{}`", name, module_name),
        ));
        None
    }

    fn lookup_struct(
        &self,
        env: &FnEnv,
        name: &str,
        found_private: &mut bool,
    ) -> Option<StructInfo> {
        if let Some(import) = env.imported_item(name) {
            if let Some(info) = self
                .registry
                .find_struct_in_module(&import.module, &import.name)
            {
                if self.can_access(&import.module, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        if let Some(module) = self.current_module.as_deref() {
            if let Some(info) = self.registry.find_struct_in_module(module, name) {
                if self.can_access(module, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        for module_name in env.glob_modules() {
            if let Some(info) = self.registry.find_struct_in_module(module_name, name) {
                if self.can_access(module_name, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        if let Some((module_name, type_name)) = name.rsplit_once("::") {
            if let Some(info) = self.registry.find_struct_in_module(module_name, type_name) {
                if self.can_access(module_name, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        None
    }

    fn lookup_enum(&self, env: &FnEnv, name: &str, found_private: &mut bool) -> Option<EnumInfo> {
        if let Some(import) = env.imported_item(name) {
            if let Some(info) = self
                .registry
                .find_enum_in_module(&import.module, &import.name)
            {
                if self.can_access(&import.module, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        if let Some(module) = self.current_module.as_deref() {
            if let Some(info) = self.registry.find_enum_in_module(module, name) {
                if self.can_access(module, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        for module_name in env.glob_modules() {
            if let Some(info) = self.registry.find_enum_in_module(module_name, name) {
                if self.can_access(module_name, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        if let Some((module_name, type_name)) = name.rsplit_once("::") {
            if let Some(info) = self.registry.find_enum_in_module(module_name, type_name) {
                if self.can_access(module_name, info.def.visibility) {
                    return Some(info.clone());
                }
                *found_private = true;
            }
        }
        None
    }

    fn validate_drop_impls(&mut self) {
        let mut seen: HashSet<String> = HashSet::new();
        for candidate in self.registry.drop_impls.clone() {
            let block = candidate.block.clone();
            let span = block
                .methods
                .first()
                .map(|m| m.span)
                .unwrap_or_else(|| Span::new(0, 0));
            if !self.registry.structs.contains_key(&block.target)
                && !self.registry.enums.contains_key(&block.target)
            {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("unknown target type `{}` for drop impl", block.target),
                ));
                continue;
            }
            if !block.type_args.is_empty() {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    "drop implementations do not support type arguments",
                ));
            }
            if !seen.insert(block.target.clone()) {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("`{}` already has a drop implementation", block.target),
                ));
                continue;
            }
            let Some(drop_method) = block.methods.iter().find(|m| m.name == "drop") else {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("`{}` is missing required method `drop`", block.target),
                ));
                continue;
            };
            if block.methods.len() > 1 {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    "drop impls may only contain `drop`",
                ));
            }
            let mut method = drop_method.clone();
            substitute_self(&mut method, &block.target);
            if !method.returns.is_empty() {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    method.span,
                    "`drop` cannot return values",
                ));
            }
            if method.params.len() != 1 {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    method.span,
                    "`drop` must take exactly `&mut self`",
                ));
                continue;
            }
            let param = &method.params[0];
            let Some(param_ty) = param.ty.as_ref() else {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    param.span,
                    "`drop` parameter requires type annotation",
                ));
                continue;
            };
            match &param_ty.ty {
                TypeExpr::Reference { mutable: true, ty } => match &**ty {
                    TypeExpr::Named(name, _) if name == &block.target => {}
                    _ => {
                        self.errors.push(TypeError::new(
                            &candidate.module_path,
                            param_ty.span,
                            format!("`drop` expects type `&mut {}`", block.target),
                        ));
                    }
                },
                _ => {
                    self.errors.push(TypeError::new(
                        &candidate.module_path,
                        param_ty.span,
                        format!("`drop` expects type `&mut {}`", block.target),
                    ));
                }
            }
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
            let Some(iface) = self.registry.find_interface(&block.interface) else {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("unknown interface `{}`", block.interface),
                ));
                continue;
            };
            if !self.interface_accessible(&iface) {
                self.errors.push(TypeError::new(
                    &candidate.module_path,
                    span,
                    format!("interface `{}` is not public", block.interface),
                ));
                continue;
            }
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
                .filter_map(|param| {
                    param
                        .ty
                        .as_ref()
                        .map(|ty| ty.substitute(&type_arg_map).replace_self(&self_ty))
                })
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
                let Some(actual) = param.ty.as_ref() else {
                    self.errors.push(TypeError::new(
                        &candidate.module_path,
                        param.span,
                        format!(
                            "parameter `{}` in `{}` requires a type annotation",
                            param.name, iface_method.name
                        ),
                    ));
                    continue;
                };
                if actual.ty != expected.ty {
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
            self.current_module = Some(module.name.clone());
            self.current_no_std = module.no_std;
            let imports = self.build_import_scope(module);
            self.imports.insert(module.name.clone(), imports);
        }
        for module in &program.modules {
            self.check_module(module);
        }
        self.current_module = None;
        self.current_no_std = false;
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
        self.current_module = Some(module.name.clone());
        self.current_no_std = module.no_std;
        let import_scope = self.imports[&module.name].clone();
        for item in &module.items {
            match item {
                Item::Function(def) => self.check_function(module, def, &import_scope),
                Item::Const(def) => self.check_const(module, def, &import_scope),
                Item::Impl(block) => {
                    for method in &block.methods {
                        self.check_function(module, method, &import_scope);
                    }
                }
                _ => {}
            }
        }
        self.current_module = None;
    }

    fn check_function(&mut self, module: &Module, def: &FunctionDef, imports: &ImportScope) {
        let mut env = FnEnv::new(
            module.path.clone(),
            def.type_params.clone(),
            imports.clone(),
        );
        let return_types: Vec<TypeExpr> = def.returns.iter().map(|ann| ann.ty.clone()).collect();
        for param in &def.params {
            if let Some(ty) = &param.ty {
                env.declare(
                    &param.name,
                    Some(ty.ty.clone()),
                    param.mutability,
                    param.span,
                    &mut self.errors,
                );
            } else {
                self.errors.push(TypeError::new(
                    &module.path,
                    param.span,
                    format!("parameter `{}` requires a type annotation", param.name),
                ));
                env.declare(
                    &param.name,
                    None,
                    param.mutability,
                    param.span,
                    &mut self.errors,
                );
            }
        }
        match &def.body {
            FunctionBody::Block(block) => {
                let expected = match return_types.as_slice() {
                    [single] => Some(single),
                    _ => None,
                };
                let tail = self.check_block(module, block, expected, &return_types, &mut env);
                if let Some(expected) = expected {
                    self.check_ownership_coercion(
                        module,
                        &env,
                        block.span,
                        expected,
                        tail.as_ref(),
                    );
                }
            }
            FunctionBody::Expr(expr) => {
                let expected = return_types.first();
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

    fn check_const(&mut self, module: &Module, def: &ConstDef, imports: &ImportScope) {
        let expected = def.ty.as_ref().map(|annotation| &annotation.ty);
        // The registry has no inferred types for unannotated constant dependencies.
        if expected.is_none() && !is_literal_initializer(&def.value) {
            return;
        }
        let mut env = FnEnv::new(module.path.clone(), Vec::new(), imports.clone());
        let ty = self.check_expression(module, &def.value, expected, &[], &mut env);
        if let Some(expected) = expected {
            self.ensure_type(module, def.span, expected, ty.as_ref());
        }
    }

    fn check_block(
        &mut self,
        module: &Module,
        block: &Block,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        env.push_scope();
        for stmt in &block.statements {
            self.check_statement(module, stmt, returns, env);
        }
        let tail_type = if let Some(tail) = &block.tail {
            self.check_expression(module, tail, expected, returns, env)
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
                            } else if let Some(actual) = &ty {
                                env.infer(name, Some(actual.clone()));
                            }
                            pending_borrows = expression_borrow_targets(value, env);
                            let binding_ty = expected.clone().or(ty);
                            env.declare(
                                name,
                                binding_ty,
                                stmt.mutability,
                                stmt.span,
                                &mut self.errors,
                            );
                            if !stmt.mutability.is_mutable()
                                && let Some(n) = self.integer_constant(value)
                                && let Some(binding) =
                                    env.scopes.last_mut().and_then(|scope| scope.get_mut(name))
                            {
                                binding.integer_constant = Some(n);
                            }
                        } else {
                            env.declare(
                                name,
                                expected.clone(),
                                stmt.mutability,
                                stmt.span,
                                &mut self.errors,
                            );
                        }
                        for target in pending_borrows {
                            env.register_binding_borrow(name, target, stmt.span);
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
                            self.bind_pattern(module, pattern, ty.as_ref(), env, stmt.mutability);
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
            Statement::Assign(assign) => match &assign.target {
                Expr::Identifier(ident) => {
                    env.clear_binding_borrows(&ident.name);
                    env.reset_moved(&ident.name);
                    let expected_type = env
                        .lookup_with_captures(&ident.name)
                        .and_then(|info| info.ty.clone());
                    if let Some(expected) = expected_type {
                        let ty = self.check_expression(
                            module,
                            &assign.value,
                            Some(&expected),
                            returns,
                            env,
                        );
                        self.ensure_type(module, ident.span, &expected, ty.as_ref());
                        if let Some(actual) = ty.as_ref()
                            && actual != &expected
                            && let Some(refined) = merge_variant_types(&expected, actual)
                            && let Some(binding) = env
                                .scopes
                                .iter_mut()
                                .rev()
                                .find_map(|scope| scope.get_mut(&ident.name))
                        {
                            binding.ty = Some(refined);
                        }
                        for target in expression_borrow_targets(&assign.value, env) {
                            env.register_binding_borrow(&ident.name, target, ident.span);
                        }
                    } else {
                        self.check_expression(module, &assign.value, None, returns, env);
                        for target in expression_borrow_targets(&assign.value, env) {
                            env.register_binding_borrow(&ident.name, target, ident.span);
                        }
                    }
                }
                Expr::Index {
                    base, index, span, ..
                } => {
                    let base_ty = self.check_expression(module, base, None, returns, env);
                    let Some(container_ty) = base_ty.as_ref() else {
                        self.check_expression(module, &assign.value, None, returns, env);
                        return;
                    };
                    let target_ty = strip_refs_and_pointers(container_ty);
                    match target_ty {
                        TypeExpr::Slice(inner) => {
                            self.check_expression(module, index, Some(&int_type()), returns, env);
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
                            self.check_expression(module, index, Some(&int_type()), returns, env);
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
            },
            Statement::Expr(expr) => {
                self.check_expression(module, &expr.expr, None, returns, env);
            }
            Statement::MacroSemi(expr) => {
                self.check_expression(module, &expr.node, None, returns, env);
            }
            Statement::Return(ret) => {
                if env.task_returns.is_some() {
                    let values: Vec<_> = ret
                        .values
                        .iter()
                        .filter_map(|v| self.check_expression(module, v, None, returns, env))
                        .collect();
                    let ty = match values.as_slice() {
                        [] => TypeExpr::Unit,
                        [ty] => ty.clone(),
                        _ => TypeExpr::Tuple(values),
                    };
                    env.task_returns
                        .as_mut()
                        .unwrap()
                        .push((stmt_span(statement), ty));
                    return;
                }
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
                        self.check_block(module, &while_stmt.body, None, returns, env);
                    });
                    env.merge_branch_borrows(&[entry_state, body_state]);
                }
                WhileCondition::Let { pattern, value } => {
                    let value_ty = self.check_expression(module, value, None, returns, env);
                    let pattern_value_ty = value_ty.clone();
                    let entry_state = env.snapshot_borrows();
                    let (_, body_state) = env.run_branch(move |env| {
                        env.push_scope();
                        self.bind_pattern(
                            module,
                            pattern,
                            pattern_value_ty.as_ref(),
                            env,
                            Mutability::Immutable,
                        );
                        self.check_block(module, &while_stmt.body, None, returns, env);
                        env.pop_scope();
                    });
                    env.merge_branch_borrows(&[entry_state, body_state]);
                }
            },
            Statement::Loop(loop_stmt) => {
                let entry_state = env.snapshot_borrows();
                let (_, body_state) = env.run_branch(|env| {
                    self.check_block(module, &loop_stmt.body, None, returns, env);
                });
                env.merge_branch_borrows(&[entry_state, body_state]);
            }
            Statement::For(for_stmt) => match &for_stmt.target {
                ForTarget::Range(range) => {
                    let element_ty = self.check_range_bounds(module, range, None, returns, env);
                    let entry_state = env.snapshot_borrows();
                    let (_, body_state) = env.run_branch(|env| {
                        env.push_scope();
                        env.declare(
                            &for_stmt.binding,
                            element_ty.clone(),
                            Mutability::Immutable,
                            for_stmt.span,
                            &mut self.errors,
                        );
                        self.check_block(module, &for_stmt.body, None, returns, env);
                        env.pop_scope();
                    });
                    env.merge_branch_borrows(&[entry_state, body_state]);
                }
                ForTarget::Collection(expr) => {
                    let collection_ty = self.check_expression(module, expr, None, returns, env);
                    if let Some(coll_ty) = collection_ty {
                        if let Some(element_ty) = self.collection_element_type(&coll_ty, env) {
                            let entry_state = env.snapshot_borrows();
                            let element_ty_clone = element_ty.clone();
                            let (_, body_state) = env.run_branch(move |env| {
                                env.push_scope();
                                env.declare(
                                    &for_stmt.binding,
                                    Some(element_ty_clone.clone()),
                                    Mutability::Immutable,
                                    for_stmt.span,
                                    &mut self.errors,
                                );
                                self.check_block(module, &for_stmt.body, None, returns, env);
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
            Statement::Comment { .. } => {}
            Statement::Block(block) => {
                self.check_block(module, block, None, returns, env);
            }
        }
    }

    // Some call sites accept numeric coercions. A borrowed value cannot be
    // retyped as owned at those sites and later captured under the owned type.
    fn check_ownership_coercion(
        &mut self,
        module: &Module,
        env: &FnEnv,
        span: Span,
        expected: &TypeExpr,
        actual: Option<&TypeExpr>,
    ) {
        if let Some(actual) = actual
            && expected != actual
            && self
                .task_transfer_error(actual, env, &mut HashSet::new())
                .is_some()
            && self
                .task_transfer_error(expected, env, &mut HashSet::new())
                .is_none()
        {
            self.ensure_type(module, span, expected, Some(actual));
        }
    }

    fn check_task_transfer(
        &mut self,
        module: &Module,
        env: &FnEnv,
        span: Span,
        ty: &TypeExpr,
        context: &str,
    ) {
        if let Some(reason) = self.task_transfer_error(ty, env, &mut HashSet::new()) {
            self.errors.push(
                TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "{context} of type `{}` cannot cross a task boundary: {reason}",
                        ty.canonical_name()
                    ),
                )
                .with_code("E0ASYNCSEND")
                .with_help("transfer an owned value, such as the value read through the reference"),
            );
        }
    }

    fn task_transfer_error(
        &self,
        ty: &TypeExpr,
        env: &FnEnv,
        visiting: &mut HashSet<String>,
    ) -> Option<String> {
        self.transfer_error_in(ty, env, &env.imports, &[], visiting)
    }

    fn transfer_error_in(
        &self,
        ty: &TypeExpr,
        env: &FnEnv,
        imports: &ImportScope,
        checked_params: &[String],
        visiting: &mut HashSet<String>,
    ) -> Option<String> {
        let mut check =
            |t: &TypeExpr| self.transfer_error_in(t, env, imports, checked_params, visiting);
        match ty {
            TypeExpr::Reference { .. } | TypeExpr::Pointer { .. } => {
                Some(format!("contains `{}`", ty.canonical_name()))
            }
            TypeExpr::Function { .. } => {
                Some("the function type does not describe its captures".into())
            }
            TypeExpr::Slice(inner) | TypeExpr::Array { ty: inner, .. } => check(inner),
            TypeExpr::Tuple(items) => items.iter().find_map(&mut check),
            TypeExpr::Named(name, args) => {
                // Check arguments even on recursive edges (e.g. List[&T]).
                if let Some(reason) = args.iter().find_map(&mut check) {
                    return Some(reason);
                }
                // The enclosing application already checked these arguments.
                if checked_params.contains(name) {
                    return None;
                }
                if env.type_params.contains(name) {
                    return Some(format!(
                        "type parameter `{name}` has no ownership constraint"
                    ));
                }
                let name = imports
                    .selected
                    .get(name)
                    .map(|i| i.name.as_str())
                    .unwrap_or(name)
                    .rsplit("::")
                    .next()
                    .unwrap_or(name);
                if self.registry.find_interface(name).is_some() {
                    return Some(
                        "the interface does not describe the concrete value's fields".into(),
                    );
                }
                if !visiting.insert(name.to_string()) {
                    return None;
                }
                let mut check_field = |module: &str, params: &[String], field: &TypeAnnotation| {
                    self.transfer_error_in(&field.ty, env, &self.imports[module], params, visiting)
                };
                let reason = if let Some(info) = self.registry.structs.get(name) {
                    info.def
                        .fields
                        .iter()
                        .find_map(|f| check_field(&info._module, &info.def.type_params, &f.ty))
                } else if let Some(info) = self.registry.enums.get(name) {
                    info.def
                        .variants
                        .iter()
                        .flat_map(|v| &v.fields)
                        .find_map(|f| check_field(&info.module, &info.def.type_params, f))
                } else {
                    None
                };
                visiting.remove(name);
                reason
            }
            TypeExpr::Unit | TypeExpr::SelfType => None,
        }
    }

    fn visible_constant(&self, env: &FnEnv, name: &str) -> Option<TypeExpr> {
        env.imported_item(name)
            .and_then(|i| self.lookup_const_in_module(&i.module, &i.name))
            .or_else(|| self.lookup_const_in_module(self.current_module.as_deref()?, name))
            .or_else(|| {
                env.glob_modules()
                    .find_map(|m| self.lookup_const_in_module(m, name))
            })
            .or_else(|| {
                let (m, n) = name.rsplit_once("::")?;
                self.lookup_const_in_module(m, n)
            })
    }

    fn check_task_captures(&mut self, module: &Module, span: Span, env: &mut FnEnv) -> Vec<String> {
        let mut names = env.end_closure_capture();
        names.sort();
        for name in &names {
            if let Some(binding) = env.lookup(name)
                && binding.mutable
                && binding.ty.as_ref().is_some_and(has_inferred_variant_param)
            {
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        span,
                        format!("mutable capture `{name}` has unresolved payload types"),
                    )
                    .with_code("E0ASYNCSEND")
                    .with_help("annotate the binding with its concrete type"),
                );
                continue;
            }
            if let Some(ty) = env
                .lookup(name)
                .and_then(|binding| binding.ty.clone())
                .or_else(|| self.visible_constant(env, name))
            {
                self.check_task_transfer(module, env, span, &ty, &format!("capture `{name}`"));
            }
        }
        names
    }

    fn check_expression(
        &mut self,
        module: &Module,
        expr: &Expr,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let actual = self.check_expression_hint(module, expr, expected, returns, env)?;
        if let Some(dest_ty) = expected
            && &actual != dest_ty
            && let Some(dest) = IntegerType::from_type(dest_ty, self.pointer_bits())
            && IntegerType::from_type(&actual, self.pointer_bits()).is_some()
            && let Some(site) = crate::language::integer::value_site(expr)
        {
            self.facts.integers.narrowing.insert(site, dest);
            if let Some(value) = self.integer_constant(expr)
                && let Err(message) = dest.checked(value)
            {
                self.errors
                    .push(TypeError::new(&module.path, expr_span(expr), message));
            }
            return Some(dest_ty.clone());
        }
        Some(actual)
    }

    fn check_expression_hint(
        &mut self,
        module: &Module,
        expr: &Expr,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        match expr {
            Expr::Identifier(ident) => {
                if let Some(n) = env.lookup(&ident.name).and_then(|b| b.integer_constant) {
                    self.folded_integers.insert(ident.id, n);
                }
                if let Some(found_ty) = env
                    .lookup_with_captures(&ident.name)
                    .and_then(|entry| entry.ty.clone())
                {
                    env.ensure_not_moved(module, ident.span, &ident.name, &mut self.errors);
                    return Some(found_ty);
                }
                if let Some(ty) = self.visible_constant(env, &ident.name) {
                    env.capture_global(&ident.name);
                    return Some(ty);
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    ident.span,
                    format!("unknown identifier `{}`", ident.name),
                ));
                None
            }
            Expr::Literal(lit) => self.check_literal(module, lit, expected),
            Expr::FormatString(literal) => {
                self.validate_format_string(module, literal, env);
                Some(string_type())
            }
            Expr::MacroCall { name, span, .. } => {
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        *span,
                        format!(
                            "macro `{}` requires expansion before type checking",
                            name.name
                        ),
                    )
                    .with_code("Emacro"),
                );
                None
            }
            Expr::Binary {
                op,
                left,
                right,
                span,
                id,
            } => self.check_binary(module, *id, *op, left, right, *span, expected, returns, env),
            Expr::Unary { op, expr, span, id } => {
                let ty = self.check_unary(module, *op, expr, *span, expected, returns, env);
                if let Some(int) = ty
                    .as_ref()
                    .and_then(|t| IntegerType::from_type(t, self.pointer_bits()))
                {
                    self.record_integer_op(module, *span, *id, IntegerOp::Neg(int), expr, None);
                }
                ty
            }
            Expr::Call {
                callee,
                args,
                type_args,
                span,
                id,
            } => self.check_call(
                module, *id, callee, args, type_args, *span, expected, returns, env,
            ),
            Expr::FieldAccess {
                base, field, span, ..
            } => {
                if let Expr::Identifier(module_ident) = base.as_ref() {
                    if let Some(module_name) = self.resolve_module_name(env, &module_ident.name) {
                        return self.lookup_module_const_type(
                            &module.path,
                            module_name,
                            field,
                            *span,
                        );
                    }
                }
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
                // Support `value::Variant` when the value has an enum type.
                let mut resolved_name = enum_name.as_deref();
                let mut owner_type = None;
                if let Some(name) = enum_name
                    && !self.registry.enums.contains_key(name)
                    && let Some(binding) = env.lookup(name)
                    && let Some(ty @ TypeExpr::Named(actual, _)) = &binding.ty
                    && self.registry.enums.contains_key(actual)
                {
                    resolved_name = Some(actual.as_str());
                    owner_type = Some(ty.clone());
                }
                let info =
                    match self.resolve_enum_literal(module, env, resolved_name, variant, *span) {
                        Ok(info) => info,
                        Err(err) => {
                            self.errors.push(*err);
                            return None;
                        }
                    };
                let owner = owner_type.as_ref().or(expected);
                Some(self.check_variant_fields(module, &info, values, owner, returns, env))
            }
            Expr::MapLiteral { entries, span } => {
                self.check_map_literal(module, entries, *span, expected, returns, env)
            }
            Expr::Block(block) => {
                let first_error = self.errors.len();
                let ty = self.check_block(module, block, expected, returns, env);
                // Expanded expressions can carry offsets from a macro definition preceding
                // the call. Anchor those diagnostics at the expansion for source and trace lookup.
                for error in &mut self.errors[first_error..] {
                    if error.span.end <= block.span.start {
                        error.span = block.span;
                    }
                }
                ty
            }
            Expr::If(if_expr) => self.check_if_expression(module, if_expr, expected, returns, env),
            Expr::Match(match_expr) => {
                self.check_match_expression(module, match_expr, expected, returns, env)
            }
            Expr::Tuple(values, _) if values.is_empty() => Some(TypeExpr::Unit),
            Expr::Tuple(values, _) => {
                let mut types = Vec::new();
                for (idx, value) in values.iter().enumerate() {
                    let element = match expected {
                        Some(TypeExpr::Tuple(elements)) => elements.get(idx),
                        _ => None,
                    };
                    types.push(self.check_expression(module, value, element, returns, env));
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
                        let actual = self.check_expression(
                            module,
                            value,
                            Some(inner.as_ref()),
                            returns,
                            env,
                        );
                        self.check_ownership_coercion(
                            module,
                            env,
                            expr_span(value),
                            inner,
                            actual.as_ref(),
                        );
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
                None => {
                    let mut element = None;
                    for value in values {
                        let ty =
                            self.check_expression(module, value, element.as_ref(), returns, env);
                        if let Some(first) = &element {
                            self.ensure_type(module, expr_span(value), first, ty.as_ref());
                        } else {
                            element = ty;
                        }
                    }
                    element.map(|ty| TypeExpr::Slice(Box::new(ty)))
                }
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
            Expr::Deref { expr, span, .. } => {
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
                            self.check_block(module, block, Some(&args[0]), returns, env);
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
            Expr::TryPropagate { id, expr, span } => {
                let ty = self.check_expression(module, expr, None, returns, env);
                if let Some(TypeExpr::Named(name, args)) = ty {
                    if name == "Result" && !args.is_empty() {
                        self.facts.try_payloads.insert(*id, args[0].clone());
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
                    env.ensure_not_borrowed_for_move(module, *span, &ident.name, &mut self.errors);
                    env.mark_moved(&ident.name, *span);
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
            Expr::Spawn {
                expr,
                span,
                captures,
            } => {
                if self.current_no_std && !self.target.is_embedded() {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            *span,
                            "`spawn` is unavailable in no-std modules",
                        )
                        .with_code("E0NS")
                        .with_help(
                            "remove spawn/channel usage or enable `std-builtins` when compiling prime-lang",
                        ),
                    );
                    return None;
                }
                env.begin_capture(true);
                let inner = self.check_expression(module, expr, None, returns, env);
                *captures.write().unwrap() = Some(self.check_task_captures(module, *span, env));
                if let Some(ty) = &inner {
                    self.check_task_transfer(module, env, *span, ty, "spawn result");
                }
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
            Expr::Async {
                block,
                span,
                captures,
            } => {
                let expected_inner = match expected {
                    Some(TypeExpr::Named(name, args)) if name == "Task" => args.first(),
                    _ => None,
                };
                env.begin_capture(true);
                let outer_returns = env.task_returns.replace(Vec::new());
                let tail = self.check_block(module, block, expected_inner, returns, env);
                let explicit = std::mem::replace(&mut env.task_returns, outer_returns).unwrap();
                *captures.write().unwrap() = Some(self.check_task_captures(module, *span, env));
                let inner = tail
                    .or_else(|| explicit.first().map(|(_, ty)| ty.clone()))
                    .unwrap_or(TypeExpr::Unit);
                self.check_task_transfer(module, env, *span, &inner, "async result");
                for (span, ty) in explicit {
                    self.ensure_type(module, span, &inner, Some(&ty));
                    if ty != inner {
                        self.check_task_transfer(module, env, span, &ty, "async return");
                    }
                }
                Some(make_task_type(inner))
            }
            Expr::Await { expr, span, .. } => {
                let awaited = self.check_expression(module, expr, None, returns, env);
                // Check borrow/send rules at suspension.
                if env.has_active_mut_borrows() {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            *span,
                            "mutable borrows cannot be held across `await`",
                        )
                        .with_code("E0ASYNCBORROW")
                        .with_help("end mutable borrows before `await` or clone the data"),
                    );
                }
                match awaited {
                    Some(TypeExpr::Named(name, args)) if name == "Task" => {
                        if let Some(inner) = args.first() {
                            self.check_task_transfer(module, env, *span, inner, "await result");
                            Some(inner.clone())
                        } else {
                            None
                        }
                    }
                    Some(other) => {
                        self.errors.push(
                            TypeError::new(&module.path, *span, "`await` expects a Task value")
                                .with_help(format!("found `{}`", other.canonical_name())),
                        );
                        None
                    }
                    None => None,
                }
            }
            Expr::Closure {
                params,
                body,
                ret,
                captures,
                span,
            } => {
                let expected_fn = match expected {
                    Some(TypeExpr::Function { params, returns }) => {
                        Some((params.clone(), returns.clone()))
                    }
                    _ => None,
                };
                env.begin_capture(false);
                env.push_scope();
                let mut param_types = Vec::new();
                let closure_span = *span;
                for (idx, param) in params.iter().enumerate() {
                    let annotated = param.ty.as_ref().map(|ann| ann.ty.clone());
                    let inferred = expected_fn
                        .as_ref()
                        .and_then(|(params, _)| params.get(idx).cloned());
                    let final_ty = annotated.or(inferred);
                    if let Some(ty) = final_ty {
                        env.declare(
                            &param.name,
                            Some(ty.clone()),
                            param.mutability,
                            param.span,
                            &mut self.errors,
                        );
                        param_types.push(ty);
                    } else {
                        self.errors.push(TypeError::new(
                            &module.path,
                            param.span,
                            format!(
                                "parameter `{}` in closure requires a type annotation or an expected function type",
                                param.name
                            ),
                        ));
                        env.declare(
                            &param.name,
                            None,
                            param.mutability,
                            param.span,
                            &mut self.errors,
                        );
                        param_types.push(TypeExpr::Unit);
                    }
                }
                let expected_ret = ret.as_ref().map(|ann| &ann.ty).or_else(|| {
                    expected_fn
                        .as_ref()
                        .and_then(|(_, returns)| returns.first())
                });
                let task_returns = env.task_returns.take();
                let body_ty = match body {
                    ClosureBody::Block(block) => {
                        self.check_block(module, block, expected_ret, returns, env)
                    }
                    ClosureBody::Expr(expr) => self.check_expression(
                        module,
                        expr.node.as_ref(),
                        expected_ret,
                        returns,
                        env,
                    ),
                };
                env.task_returns = task_returns;
                if let (Some(annotation), Some(actual)) = (ret, body_ty.as_ref()) {
                    self.ensure_type(module, annotation.span, &annotation.ty, Some(actual));
                }
                env.pop_scope();
                let captured_names = env.end_closure_capture();
                let mut captured_vars = Vec::new();
                for name in captured_names {
                    // Constructing this closure reads each capture in the enclosing scope.
                    if let Some(info) = env.lookup_with_captures(&name) {
                        let binding_ty = info.ty.clone();
                        let binding_mutable = info.mutable;
                        let capture_mode = match &binding_ty {
                            Some(TypeExpr::Reference { mutable, .. }) => {
                                if binding_mutable && *mutable {
                                    env.ensure_mut_borrow_allowed(
                                        module,
                                        *span,
                                        &name,
                                        &mut self.errors,
                                    );
                                    env.register_binding_borrow(&name, name.clone(), *span);
                                }
                                CaptureMode::Reference {
                                    mutable: binding_mutable && *mutable,
                                }
                            }
                            _ => {
                                env.mark_moved(&name, *span);
                                CaptureMode::Move
                            }
                        };
                        captured_vars.push(CapturedVar {
                            name: name.clone(),
                            mutable: binding_mutable,
                            ty: binding_ty,
                            mode: capture_mode,
                            span: closure_span,
                        });
                    } else {
                        captured_vars.push(CapturedVar {
                            name: name.clone(),
                            mutable: false,
                            ty: None,
                            mode: CaptureMode::Move,
                            span: closure_span,
                        });
                    }
                }
                if let Ok(mut guard) = captures.write() {
                    guard.clear();
                    guard.extend(captured_vars);
                }
                let returns_vec = if let Some(annotation) = ret {
                    vec![annotation.ty.clone()]
                } else if let Some(actual) = body_ty.clone() {
                    vec![actual]
                } else {
                    Vec::new()
                };
                let closure_ty = TypeExpr::Function {
                    params: param_types,
                    returns: returns_vec,
                };
                if let Some(expected) = expected {
                    self.ensure_type(module, *span, expected, Some(&closure_ty));
                }
                Some(closure_ty)
            }
            Expr::Index {
                base, index, span, ..
            } => {
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
                self.check_block(module, &expr.then_branch, expected, returns, env)
            }
            IfCondition::Let { pattern, value, .. } => {
                let value_ty = self.check_expression(module, value, None, returns, env);
                env.push_scope();
                self.bind_pattern(
                    module,
                    pattern,
                    value_ty.as_ref(),
                    env,
                    Mutability::Immutable,
                );
                let ty = self.check_block(module, &expr.then_branch, expected, returns, env);
                env.pop_scope();
                ty
            }
        });
        if let Some(else_branch) = &expr.else_branch {
            let (else_ty, else_state) = env.run_branch(|env| match else_branch {
                ElseBranch::Block(block) => self.check_block(module, block, expected, returns, env),
                ElseBranch::ElseIf(nested) => {
                    self.check_if_expression(module, nested, expected, returns, env)
                }
            });
            env.merge_branch_borrows(&[then_state, else_state]);
            if let (Some(left), Some(right)) = (&then_ty, &else_ty) {
                if let Some(merged) = merge_variant_types(left, right) {
                    return Some(merged);
                }
                self.ensure_type(module, expr.span, left, Some(right));
            }
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
        let mut arms = Vec::with_capacity(expr.arms.len());
        let mut patterns = Vec::new();
        let mut borrow_states: Vec<BorrowState> = Vec::new();
        for arm in &expr.arms {
            let (value_ty, state) = env.run_branch(|env| {
                env.push_scope();
                let errors_before_pattern = self.errors.len();
                let variant = self.bind_pattern(
                    module,
                    &arm.pattern,
                    scrutinee_ty.as_ref(),
                    env,
                    Mutability::Immutable,
                );
                if arm.guard.is_none() && self.errors.len() == errors_before_pattern {
                    patterns.push(&arm.pattern);
                }
                let guard = arm.guard.as_ref().and_then(|guard| {
                    let span = expr_span(guard);
                    let before = self.errors.len();
                    let ty =
                        self.check_expression(module, guard, Some(&bool_type()), returns, env)?;
                    if self.errors.len() == before {
                        self.ensure_type(module, span, &bool_type(), Some(&ty));
                    }
                    Some(TypeAnnotation { ty, span })
                });
                let ty = self.check_expression(module, &arm.value, expected, returns, env);
                arms.push(CheckedMatchArm {
                    variant,
                    guard,
                    value: ty.clone().map(|ty| TypeAnnotation {
                        ty,
                        span: expr_span(&arm.value),
                    }),
                });
                env.pop_scope();
                ty
            });
            if arm_type.is_none() {
                arm_type = value_ty.clone();
            } else if let (Some(a), Some(b)) = (arm_type.as_ref(), value_ty.as_ref()) {
                if let Some(merged) = merge_variant_types(a, b) {
                    arm_type = Some(merged);
                } else {
                    self.ensure_type(module, pattern_span(&arm.pattern), a, Some(b));
                }
            }
            borrow_states.push(state);
        }
        env.merge_branch_borrows(&borrow_states);
        let mut is_enum = arms.iter().any(|arm| arm.variant.is_some());
        if let Some(TypeExpr::Named(enum_name, args)) = &scrutinee_ty {
            let mut private = false;
            if let Some(enum_info) = self.lookup_enum(env, enum_name, &mut private) {
                is_enum = true;
                let missing =
                    match_coverage::missing_variants(self, env, &enum_info.def, args, &patterns);
                if !missing.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        expr.span,
                        format!(
                            "`match` is not exhaustive; missing variants: {}",
                            missing.join(", ")
                        ),
                    ));
                }
            } else if private {
                self.errors.push(TypeError::new(
                    &module.path,
                    expr.span,
                    format!("enum `{}` is not public", enum_name),
                ));
            }
        }
        if let Some(ty) = scrutinee_ty {
            // A builtin can return a core enum even when its name is not imported.
            is_enum |=
                named_type_name(&ty).is_some_and(|name| self.registry.enums.contains_key(name));
            self.facts.matches.insert(
                expr.id,
                CheckedMatch {
                    is_enum,
                    path: module.path.clone(),
                    span: expr.span,
                    scrutinee: TypeAnnotation {
                        ty,
                        span: expr_span(&expr.expr),
                    },
                    arms,
                },
            );
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
        let mut inner_ty = self.check_expression(module, base, None, returns, env)?;
        while let TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } = inner_ty {
            inner_ty = *ty;
        }
        if let TypeExpr::Named(name, _) = &inner_ty {
            let mut private = false;
            if let Some(struct_info) = self.lookup_struct(env, name, &mut private) {
                return lookup_struct_field(&self.registry, &struct_info.def, field);
            }
            if private {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!("struct `{}` is not public", name),
                ));
                return None;
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
        let mut private = false;
        let Some(struct_info) = self.lookup_struct(env, name, &mut private) else {
            if private {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!("struct `{}` is not public", name),
                ));
            } else {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!("Unknown struct `{}`", name),
                ));
            }
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
        id: ExprId,
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
                if self.is_embedded_builtin(&ident.name) && !self.embedded_builtins_enabled() {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            span,
                            format!(
                                "`{}` is an embedded-only built-in; compile with --target {}",
                                ident.name,
                                embedded_target_hint()
                            ),
                        )
                        .with_code("E0EM")
                        .with_help("select an ESP32 embedded target or remove embedded-only calls"),
                    );
                    return None;
                }
                if matches!(
                    ident.name.as_str(),
                    "fs_exists" | "fs_read" | "fs_write" | "fs_write_bytes"
                ) && (self.current_no_std
                    || self.target.is_embedded()
                    || !cfg!(feature = "std-builtins"))
                {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            span,
                            format!("`{}` requires the host std filesystem", ident.name),
                        )
                        .with_code("E0NS")
                        .with_help(
                            "use a std module on a host target with the std-builtins feature",
                        ),
                    );
                    return None;
                }
                if self.current_no_std
                    && self.is_std_only_builtin(&ident.name)
                    && !self.embedded_std_compatible_builtin(&ident.name)
                {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            span,
                            format!("`{}` is unavailable in no-std modules", ident.name),
                        )
                        .with_code("E0NS")
                        .with_help(
                            "remove std-builtins usage or enable `std-builtins` when compiling prime-lang",
                        ),
                    );
                    return None;
                }
                if self.is_builtin_name(&ident.name) {
                    return self.check_builtin_call(
                        module,
                        id,
                        &ident.name,
                        args,
                        type_args,
                        expected,
                        returns,
                        env,
                        span,
                    );
                }
                if let Some(sig) = self.lookup_function(env, &ident.name, None) {
                    return self.check_function_call(
                        module, sig, args, type_args, expected, returns, env, span,
                    );
                }
                if let Some(binding) = env.lookup_with_captures(&ident.name) {
                    if let Some(TypeExpr::Function {
                        params,
                        returns: fn_returns,
                    }) = binding.ty.clone()
                    {
                        return self.check_function_type_call(
                            module,
                            &params,
                            &fn_returns,
                            args,
                            expected,
                            returns,
                            env,
                            span,
                        );
                    }
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
                    if let Some(module_name) = self.resolve_module_name(env, &module_ident.name) {
                        if let Some(sig) = self.lookup_function_in_module(module_name, field, None)
                        {
                            return self.check_function_call(
                                module, sig, args, type_args, expected, returns, env, span,
                            );
                        }
                        if let Some(const_ty) =
                            self.lookup_module_const_type(&module.path, module_name, field, span)
                        {
                            if !args.is_empty() {
                                self.errors.push(TypeError::new(
                                    &module.path,
                                    span,
                                    "const values are not callable",
                                ));
                            }
                            return Some(const_ty);
                        }
                    }
                }
                let receiver = self.check_expression(module, base, None, returns, env)?;
                if let Some(name) = named_type_name(&receiver) {
                    if let Some(sig) = self.lookup_function(env, field, Some(name)) {
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
                if let Some(TypeExpr::Function {
                    params,
                    returns: fn_returns,
                }) = self.check_expression(module, callee, None, returns, env)
                {
                    return self.check_function_type_call(
                        module,
                        &params,
                        &fn_returns,
                        args,
                        expected,
                        returns,
                        env,
                        span,
                    );
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "Unsupported call target",
                ));
                None
            }
            _ => {
                if let Some(TypeExpr::Function {
                    params,
                    returns: fn_returns,
                }) = self.check_expression(module, callee, None, returns, env)
                {
                    return self.check_function_type_call(
                        module,
                        &params,
                        &fn_returns,
                        args,
                        expected,
                        returns,
                        env,
                        span,
                    );
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "Unsupported call target",
                ));
                None
            }
        }
    }

    fn check_function_type_call(
        &mut self,
        module: &Module,
        params: &[TypeExpr],
        returns_types: &[TypeExpr],
        args: &[Expr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        if params.len() != args.len() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!(
                    "function expects {} argument(s), got {}",
                    params.len(),
                    args.len()
                ),
            ));
        }
        for (expr, param_ty) in args.iter().zip(params.iter()) {
            let actual = self.check_expression(module, expr, Some(param_ty), returns, env);
            self.check_ownership_coercion(module, env, expr_span(expr), param_ty, actual.as_ref());
        }
        let result_ty = match returns_types.len() {
            0 => TypeExpr::Unit,
            1 => returns_types[0].clone(),
            _ => TypeExpr::Tuple(returns_types.to_vec()),
        };
        if let Some(expected) = expected
            && !(IntegerType::from_type(expected, self.pointer_bits()).is_some()
                && IntegerType::from_type(&result_ty, self.pointer_bits()).is_some())
        {
            self.ensure_type(module, span, expected, Some(&result_ty));
        }
        Some(result_ty)
    }

    fn check_builtin_call(
        &mut self,
        module: &Module,
        id: ExprId,
        name: &str,
        args: &[Expr],
        type_args: &[TypeExpr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        let allows_type_args = matches!(name, "channel" | "in" | "recv_task" | "cast");
        if !allows_type_args && !type_args.is_empty() {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                format!("`{}` does not accept type arguments", name),
            ));
        }
        if self.is_embedded_builtin(name) && !self.embedded_builtins_enabled() {
            self.errors.push(
                TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "`{}` is an embedded-only built-in; compile with --target {}",
                        name,
                        embedded_target_hint()
                    ),
                )
                .with_code("E0EM")
                .with_help("select an ESP32 embedded target or remove embedded-only calls"),
            );
            return None;
        }
        if let Some(op) = TcpBuiltin::lookup(name) {
            if !TcpBuiltin::available(&self.target, self.current_no_std) {
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        span,
                        format!("`{name}` requires the host std runtime"),
                    )
                    .with_code("E0NS"),
                );
                return None;
            }
            let parameter = op.parameter();
            let count = usize::from(parameter.is_some());
            if args.len() != count {
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    format!("`{name}` expects {count} arguments"),
                ));
            } else if let Some((_, ty)) = parameter {
                let actual = self.check_expression(module, &args[0], Some(&ty), returns, env);
                self.ensure_type(module, expr_span(&args[0]), &ty, actual.as_ref());
            }
            return Some(op.result());
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
            "debug_show" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`debug_show` expects exactly 1 argument",
                    ));
                }
                if let Some(arg) = args.first() {
                    self.check_expression(module, arg, None, returns, env);
                }
                Some(TypeExpr::Unit)
            }
            "in" => {
                if type_args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`in` expects exactly one type argument, e.g. in[int32](\"prompt\")",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, TypeExpr::Named("string".into(), Vec::new())],
                    ));
                }
                if args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`in` expects at least 1 argument (a prompt, optionally with format placeholders)",
                    ));
                }
                if let Some(ty) = type_args.first() {
                    if args.len() == 1 {
                        self.check_expression(module, &args[0], None, returns, env);
                    } else if let Expr::FormatString(literal) = &args[0] {
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
                                    "`in` expects {} argument(s) to fill format placeholders, got {}",
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
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            "`in` with multiple arguments requires a format string literal",
                        ));
                        for arg in args {
                            self.check_expression(module, arg, None, returns, env);
                        }
                    }
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![ty.clone(), TypeExpr::Named("string".into(), Vec::new())],
                    ));
                }
                None
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
                    .map(make_option_type)
            }
            "slice_get_int" => {
                if args.len() != 3 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`slice_get_int` expects 3 arguments, got {}", args.len()),
                    ));
                    return Some(int_type());
                }
                let slice_ty = self.check_expression(module, &args[0], None, returns, env);
                self.check_expression(module, &args[1], Some(&int_type()), returns, env);
                let fallback_ty =
                    self.check_expression(module, &args[2], Some(&int_type()), returns, env);
                self.ensure_type(
                    module,
                    expr_span(&args[2]),
                    &int_type(),
                    fallback_ty.as_ref(),
                );
                if let Some(elem_ty) = self.expect_slice_type(module, span, slice_ty.as_ref()) {
                    self.ensure_type(module, expr_span(&args[0]), &int_type(), Some(&elem_ty));
                }
                Some(int_type())
            }
            "slice_remove" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`slice_remove` expects 2 arguments, got {}", args.len()),
                    ));
                    return None;
                }
                let slice_ty = self.check_expression(module, &args[0], None, returns, env);
                self.check_expression(module, &args[1], Some(&int_type()), returns, env);
                self.expect_slice_type(module, span, slice_ty.as_ref())
                    .map(make_option_type)
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
            "map_remove" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`map_remove` expects 2 arguments, got {}", args.len()),
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
            "len" => {
                if args.len() != 1 {
                    self.errors
                        .push(TypeError::new(&module.path, span, "len expects 1 argument"));
                }
                self.check_expression(module, &args[0], None, returns, env);
                Some(TypeExpr::named("int32"))
            }
            "iter" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`iter` expects 1 argument",
                    ));
                }
                let target_ty = self.check_expression(module, &args[0], None, returns, env);
                match target_ty {
                    Some(TypeExpr::Slice(inner)) => {
                        Some(TypeExpr::Named("Iterator".into(), vec![*inner]))
                    }
                    Some(TypeExpr::Named(name, params)) if name == "Map" && params.len() == 2 => {
                        let tuple = TypeExpr::Tuple(vec![
                            TypeExpr::Named("string".into(), Vec::new()),
                            params[1].clone(),
                        ]);
                        Some(TypeExpr::Named("Iterator".into(), vec![tuple]))
                    }
                    Some(other) => {
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            format!("`iter` not supported for {}", other.canonical_name()),
                        ));
                        None
                    }
                    None => None,
                }
            }
            "channel" => {
                if !args.is_empty() {
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
                        let actual =
                            self.check_expression(module, &args[1], Some(&params[0]), returns, env);
                        self.check_ownership_coercion(
                            module,
                            env,
                            expr_span(&args[1]),
                            &params[0],
                            actual.as_ref(),
                        );
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
            "recv_task" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`recv_task` expects 1 argument",
                    ));
                    return None;
                }
                if type_args.len() > 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`recv_task` accepts at most one type argument (payload type)",
                    ));
                }
                let receiver_ty = self.check_expression(module, &args[0], None, returns, env);
                let payload_from_type_arg = type_args.first().cloned();
                if let Some(TypeExpr::Named(name, params)) = receiver_ty {
                    if name == "Receiver" && params.len() == 1 {
                        let payload = payload_from_type_arg.unwrap_or_else(|| params[0].clone());
                        return Some(make_task_type(make_option_type(payload)));
                    }
                }
                let payload = payload_from_type_arg.unwrap_or(TypeExpr::Unit);
                Some(make_task_type(make_option_type(payload)))
            }
            "recv_timeout" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`recv_timeout` expects 2 arguments",
                    ));
                    return None;
                }
                let receiver_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(TypeExpr::Named(name, params)) = receiver_ty {
                    if name == "Receiver" && params.len() == 1 {
                        self.check_expression(
                            module,
                            &args[1],
                            Some(&TypeExpr::Named("int64".into(), Vec::new())),
                            returns,
                            env,
                        );
                        return Some(make_option_type(params[0].clone()));
                    }
                }
                self.check_expression(module, &args[1], None, returns, env);
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
            "sleep" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`sleep` expects 1 argument (millis)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("int64".into(), Vec::new())),
                    returns,
                    env,
                );
                Some(TypeExpr::Unit)
            }
            "sleep_ms" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`sleep_ms` expects 1 argument (millis)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("int64".into(), Vec::new())),
                    returns,
                    env,
                );
                Some(TypeExpr::Unit)
            }
            "sleep_task" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`sleep_task` expects 1 argument (millis)",
                    ));
                    return Some(make_task_type(TypeExpr::Unit));
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("int64".into(), Vec::new())),
                    returns,
                    env,
                );
                Some(make_task_type(TypeExpr::Unit))
            }
            "cancel_token" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`cancel_token` expects 0 arguments",
                    ));
                }
                Some(TypeExpr::Named("CancelToken".into(), Vec::new()))
            }
            "cancel" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`cancel` expects 1 argument (CancelToken)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("CancelToken".into(), Vec::new())),
                    returns,
                    env,
                );
                Some(TypeExpr::Unit)
            }
            "is_cancelled" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`is_cancelled` expects 1 argument (CancelToken)",
                    ));
                    return Some(bool_type());
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("CancelToken".into(), Vec::new())),
                    returns,
                    env,
                );
                Some(bool_type())
            }
            "await_timeout" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`await_timeout` expects 2 arguments (Task[T], millis)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                let task_ty = self.check_expression(module, &args[0], None, returns, env);
                self.check_expression(
                    module,
                    &args[1],
                    Some(&TypeExpr::Named("int64".into(), Vec::new())),
                    returns,
                    env,
                );
                if let Some(TypeExpr::Named(name, params)) = task_ty {
                    if name == "Task" && params.len() == 1 {
                        self.check_task_transfer(
                            module,
                            env,
                            span,
                            &params[0],
                            "guarded await result",
                        );
                        return Some(TypeExpr::Named(
                            "Result".into(),
                            vec![params[0].clone(), string_type()],
                        ));
                    }
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "`await_timeout` expects a Task as the first argument",
                ));
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "await_cancel" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`await_cancel` expects 2 arguments (Task[T], CancelToken)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                let task_ty = self.check_expression(module, &args[0], None, returns, env);
                self.check_expression(
                    module,
                    &args[1],
                    Some(&TypeExpr::Named("CancelToken".into(), Vec::new())),
                    returns,
                    env,
                );
                if let Some(TypeExpr::Named(name, params)) = task_ty {
                    if name == "Task" && params.len() == 1 {
                        self.check_task_transfer(
                            module,
                            env,
                            span,
                            &params[0],
                            "guarded await result",
                        );
                        return Some(TypeExpr::Named(
                            "Result".into(),
                            vec![params[0].clone(), string_type()],
                        ));
                    }
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "`await_cancel` expects a Task as the first argument",
                ));
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "await_cancel_timeout" => {
                if args.len() != 3 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`await_cancel_timeout` expects 3 arguments (Task[T], CancelToken, millis)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                let task_ty = self.check_expression(module, &args[0], None, returns, env);
                self.check_expression(
                    module,
                    &args[1],
                    Some(&TypeExpr::Named("CancelToken".into(), Vec::new())),
                    returns,
                    env,
                );
                self.check_expression(
                    module,
                    &args[2],
                    Some(&TypeExpr::Named("int64".into(), Vec::new())),
                    returns,
                    env,
                );
                if let Some(TypeExpr::Named(name, params)) = task_ty {
                    if name == "Task" && params.len() == 1 {
                        self.check_task_transfer(
                            module,
                            env,
                            span,
                            &params[0],
                            "guarded await result",
                        );
                        return Some(TypeExpr::Named(
                            "Result".into(),
                            vec![params[0].clone(), string_type()],
                        ));
                    }
                }
                self.errors.push(TypeError::new(
                    &module.path,
                    span,
                    "`await_cancel_timeout` expects a Task as the first argument",
                ));
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "delay_ms" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`delay_ms` expects 1 argument (millis)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("int32".into(), Vec::new())),
                    returns,
                    env,
                );
                Some(TypeExpr::Unit)
            }
            "now_ms" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`now_ms` expects 0 arguments",
                    ));
                }
                Some(TypeExpr::Named("int64".into(), Vec::new()))
            }
            "reset_reason" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`reset_reason` expects 0 arguments",
                    ));
                }
                Some(TypeExpr::Named("int32".into(), Vec::new()))
            }
            "digital_read" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`digital_read` expects 1 argument (pin)",
                    ));
                }
                if let Some(pin) = args.first() {
                    self.check_expression(
                        module,
                        pin,
                        Some(&TypeExpr::Named("int32".into(), Vec::new())),
                        returns,
                        env,
                    );
                }
                Some(TypeExpr::Named("int32".into(), Vec::new()))
            }
            "fs_exists" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`fs_exists` expects 1 argument (path)",
                    ));
                }
                if let Some(path) = args.first() {
                    self.check_expression(module, path, Some(&string_type()), returns, env);
                }
                Some(bool_type())
            }
            "fs_read" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`fs_read` expects 1 argument (path)",
                    ));
                }
                if let Some(path) = args.first() {
                    self.check_expression(module, path, Some(&string_type()), returns, env);
                }
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![string_type(), string_type()],
                ))
            }
            "fs_write" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`fs_write` expects 2 arguments (path, contents)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                self.check_expression(module, &args[1], Some(&string_type()), returns, env);
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "fs_write_bytes" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`fs_write_bytes` expects 2 arguments (path, contents)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                let bytes_type =
                    TypeExpr::Slice(Box::new(TypeExpr::Named("uint8".into(), Vec::new())));
                self.check_expression(module, &args[1], Some(&bytes_type), returns, env);
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "gfx_open" => {
                if args.len() != 3 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_open` expects 3 arguments (title, width, height)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                self.check_expression(module, &args[1], Some(&int_type()), returns, env);
                self.check_expression(module, &args[2], Some(&int_type()), returns, env);
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "gfx_clear" => {
                if args.len() != 3 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_clear` expects 3 arguments (r, g, b)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                for arg in args {
                    self.check_expression(module, arg, Some(&int_type()), returns, env);
                }
                Some(TypeExpr::Unit)
            }
            "gfx_rect" => {
                if args.len() != 7 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_rect` expects 7 arguments (x, y, width, height, r, g, b)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                for arg in args {
                    self.check_expression(module, arg, Some(&int_type()), returns, env);
                }
                Some(TypeExpr::Unit)
            }
            "gfx_sprite" => {
                if args.len() != 8 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_sprite` expects 8 arguments (path, x, y, width, height, r, g, b)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![TypeExpr::Unit, string_type()],
                    ));
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                for arg in args.iter().skip(1) {
                    self.check_expression(module, arg, Some(&int_type()), returns, env);
                }
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![TypeExpr::Unit, string_type()],
                ))
            }
            "gfx_text" => {
                if args.len() != 7 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_text` expects 7 arguments (text, x, y, scale, r, g, b)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                for arg in args.iter().skip(1) {
                    self.check_expression(module, arg, Some(&int_type()), returns, env);
                }
                Some(TypeExpr::Unit)
            }
            "gfx_text_int" => {
                if args.len() != 8 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_text_int` expects 8 arguments (label, value, x, y, scale, r, g, b)",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                for arg in args.iter().skip(1) {
                    self.check_expression(module, arg, Some(&int_type()), returns, env);
                }
                Some(TypeExpr::Unit)
            }
            "gfx_present" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_present` expects 0 arguments",
                    ));
                }
                Some(bool_type())
            }
            "gfx_key_down" | "gfx_key_pressed" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        format!("`{}` expects 1 argument (key)", name),
                    ));
                    return Some(bool_type());
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                Some(bool_type())
            }
            "gfx_should_close" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_should_close` expects 0 arguments",
                    ));
                }
                Some(bool_type())
            }
            "gfx_close" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`gfx_close` expects 0 arguments",
                    ));
                }
                Some(TypeExpr::Unit)
            }
            "audio_play" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`audio_play` expects 2 arguments (path, looped)",
                    ));
                    return Some(TypeExpr::Named(
                        "Result".into(),
                        vec![int_type(), string_type()],
                    ));
                }
                self.check_expression(module, &args[0], Some(&string_type()), returns, env);
                self.check_expression(module, &args[1], Some(&bool_type()), returns, env);
                Some(TypeExpr::Named(
                    "Result".into(),
                    vec![int_type(), string_type()],
                ))
            }
            "audio_stop" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`audio_stop` expects 1 argument (handle)",
                    ));
                    return Some(bool_type());
                }
                self.check_expression(module, &args[0], Some(&int_type()), returns, env);
                Some(bool_type())
            }
            "audio_stop_all" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`audio_stop_all` expects 0 arguments",
                    ));
                }
                Some(TypeExpr::Unit)
            }
            "audio_set_volume" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`audio_set_volume` expects 2 arguments (handle, volume_percent)",
                    ));
                    return Some(bool_type());
                }
                self.check_expression(module, &args[0], Some(&int_type()), returns, env);
                self.check_expression(module, &args[1], Some(&int_type()), returns, env);
                Some(bool_type())
            }
            "audio_is_playing" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`audio_is_playing` expects 1 argument (handle)",
                    ));
                    return Some(bool_type());
                }
                self.check_expression(module, &args[0], Some(&int_type()), returns, env);
                Some(bool_type())
            }
            "pin_mode" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`pin_mode` expects 2 arguments (pin, mode)",
                    ));
                }
                if let Some(pin) = args.first() {
                    self.check_expression(
                        module,
                        pin,
                        Some(&TypeExpr::Named("int32".into(), Vec::new())),
                        returns,
                        env,
                    );
                }
                if let Some(mode) = args.get(1) {
                    self.check_expression(
                        module,
                        mode,
                        Some(&TypeExpr::Named("int32".into(), Vec::new())),
                        returns,
                        env,
                    );
                }
                Some(TypeExpr::Unit)
            }
            "digital_write" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`digital_write` expects 2 arguments (pin, level)",
                    ));
                }
                if let Some(pin) = args.first() {
                    self.check_expression(
                        module,
                        pin,
                        Some(&TypeExpr::Named("int32".into(), Vec::new())),
                        returns,
                        env,
                    );
                }
                if let Some(level) = args.get(1) {
                    self.check_expression(
                        module,
                        level,
                        Some(&TypeExpr::Named("int32".into(), Vec::new())),
                        returns,
                        env,
                    );
                }
                Some(TypeExpr::Unit)
            }
            "map_keys" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_keys` expects 1 argument",
                    ));
                    return Some(TypeExpr::Slice(Box::new(string_type())));
                }
                let map_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some((key_ty, _)) = self.expect_map_type(module, span, map_ty.as_ref()) {
                    if !is_string_type(&key_ty) {
                        self.errors.push(TypeError::new(
                            &module.path,
                            span,
                            "map_keys requires map with string keys",
                        ));
                    }
                    return Some(TypeExpr::Slice(Box::new(string_type())));
                }
                Some(TypeExpr::Slice(Box::new(string_type())))
            }
            "map_values" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_values` expects 1 argument",
                    ));
                    return None;
                }
                let map_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some((_, value_ty)) = self.expect_map_type(module, span, map_ty.as_ref()) {
                    return Some(TypeExpr::Slice(Box::new(value_ty)));
                }
                None
            }
            "next" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`next` expects 1 argument",
                    ));
                    return None;
                }
                let iter_ty = self.check_expression(module, &args[0], None, returns, env);
                if let Some(TypeExpr::Named(name, params)) = iter_ty {
                    if name == "Iterator" && params.len() == 1 {
                        return Some(make_option_type(params[0].clone()));
                    }
                }
                None
            }
            "assert_eq" => {
                if args.len() != 2 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`assert_eq` expects 2 arguments",
                    ));
                    return Some(TypeExpr::Unit);
                }
                let left = self.check_expression(module, &args[0], None, returns, env);
                let right = self.check_expression(module, &args[1], left.as_ref(), returns, env);
                if let Some(expected) = left.as_ref() {
                    self.ensure_type(module, expr_span(&args[1]), expected, right.as_ref());
                }
                Some(TypeExpr::Unit)
            }
            "panic" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`panic` expects 1 argument",
                    ));
                    return Some(TypeExpr::Unit);
                }
                self.check_expression(
                    module,
                    &args[0],
                    Some(&TypeExpr::Named("string".into(), Vec::new())),
                    returns,
                    env,
                );
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
                        self.check_task_transfer(module, env, span, &params[0], "join result");
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
            "cast" => {
                if type_args.len() != 1 {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            span,
                            "`cast` expects exactly one type argument (target type)",
                        )
                        .with_code("E0300"),
                    );
                    return None;
                }
                if args.len() != 1 {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            span,
                            format!("`cast` expects 1 argument, got {}", args.len()),
                        )
                        .with_code("E0300"),
                    );
                    return Some(type_args[0].clone());
                }
                let target = &type_args[0];
                let target_kind = numeric_kind(target, self.pointer_bits());
                if target_kind.is_none() {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            span,
                            "`cast` only supports numeric target types",
                        )
                        .with_code("E0301"),
                    );
                    return None;
                }
                // An explicit conversion accepts an integer literal of any parsed magnitude.
                // Its operand has no implicit integer destination, including the default int32.
                let int_target = IntegerType::from_type(target, self.pointer_bits());
                if is_integer_literal(&args[0]) {
                    if let Some(int) = int_target {
                        self.record_integer_op(
                            module,
                            span,
                            id,
                            IntegerOp::Cast(int),
                            &args[0],
                            None,
                        );
                    }
                    return Some(target.clone());
                }
                let value_ty = self.check_expression(module, &args[0], None, returns, env);
                let value_kind = value_ty
                    .as_ref()
                    .and_then(|ty| numeric_kind(ty, self.pointer_bits()));
                if value_kind.is_none() {
                    self.errors.push(
                        TypeError::new(
                            &module.path,
                            expr_span(&args[0]),
                            "`cast` only supports numeric values",
                        )
                        .with_code("E0302"),
                    );
                    return Some(target.clone());
                }
                if let Some(int) = int_target
                    && matches!(
                        value_kind,
                        Some(NumericKind::Signed(..) | NumericKind::Unsigned(..))
                    )
                {
                    self.record_integer_op(module, span, id, IntegerOp::Cast(int), &args[0], None);
                }
                Some(target.clone())
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
            let actual = self.check_expression(module, expr, Some(&param.ty), returns, env);
            self.check_ownership_coercion(module, env, expr_span(expr), &param.ty, actual.as_ref());
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
            let actual = self.check_expression(module, expr, Some(&param.ty), returns, env);
            self.check_ownership_coercion(module, env, expr_span(expr), &param.ty, actual.as_ref());
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
            TypeExpr::Named(name, generics) if name == "Iterator" => {
                self.check_iterator_method(module, generics, method, args, returns, env, span)
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
            "iter" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`iter` expects 0 argument(s) after receiver",
                    ));
                }
                Some(TypeExpr::Named("Iterator".into(), vec![inner.clone()]))
            }
            "slice_remove" | "remove" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`slice_remove` expects 1 argument after receiver",
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
            "map_remove" | "remove" => {
                if args.len() != 1 {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_remove` expects 1 argument after receiver",
                    ));
                    return Some(make_option_type(value_ty));
                }
                self.check_expression(module, &args[0], Some(&key_ty), returns, env);
                Some(make_option_type(value_ty))
            }
            "map_keys" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_keys` expects 0 argument(s) after receiver",
                    ));
                }
                Some(TypeExpr::Slice(Box::new(string_type())))
            }
            "map_values" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`map_values` expects 0 argument(s) after receiver",
                    ));
                }
                Some(TypeExpr::Slice(Box::new(value_ty)))
            }
            "iter" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`iter` expects 0 argument(s) after receiver",
                    ));
                }
                let tuple = TypeExpr::Tuple(vec![string_type(), value_ty.clone()]);
                Some(TypeExpr::Named("Iterator".into(), vec![tuple]))
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

    fn check_iterator_method(
        &mut self,
        module: &Module,
        generics: &[TypeExpr],
        method: &str,
        args: &[Expr],
        _returns: &[TypeExpr],
        _env: &mut FnEnv,
        span: Span,
    ) -> Option<TypeExpr> {
        if generics.len() != 1 {
            self.errors.push(TypeError::new(
                &module.path,
                span,
                "`Iterator` expects 1 type argument",
            ));
            return None;
        }
        let elem = &generics[0];
        match method {
            "next" => {
                if !args.is_empty() {
                    self.errors.push(TypeError::new(
                        &module.path,
                        span,
                        "`next` expects 0 argument(s) after receiver",
                    ));
                }
                Some(make_option_type(elem.clone()))
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
        if let Some(TypeExpr::Named(name, _)) = expected {
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
        Some(self.check_variant_fields(module, &variant, args, expected, returns, env))
    }

    fn check_variant_fields(
        &mut self,
        module: &Module,
        variant: &EnumVariantInfo,
        args: &[Expr],
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> TypeExpr {
        let canonical = env
            .imported_item(&variant.enum_name)
            .map(|i| i.name.as_str())
            .unwrap_or(&variant.enum_name);
        let canonical = canonical.rsplit("::").next().unwrap_or(canonical);
        let params = self.registry.enums[canonical].def.type_params.clone();
        let mut subst: HashMap<_, _> = match expected {
            Some(TypeExpr::Named(name, args)) if name == &variant.enum_name => {
                params.iter().cloned().zip(args.iter().cloned()).collect()
            }
            _ => HashMap::new(),
        };
        for (expr, field) in args.iter().zip(&variant.def.fields) {
            let hint = field.ty.substitute(&subst);
            if let Some(actual) = self.check_expression(module, expr, Some(&hint), returns, env) {
                infer_variant_params(&field.ty, &actual, &params, &mut subst);
                self.ensure_type(
                    module,
                    expr_span(expr),
                    &field.ty.substitute(&subst),
                    Some(&actual),
                );
            }
        }
        TypeExpr::Named(
            variant.enum_name.clone(),
            params
                .iter()
                .map(|p| {
                    subst
                        .remove(p)
                        .unwrap_or_else(|| TypeExpr::named(INFERRED_VARIANT_PARAM))
                })
                .collect(),
        )
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
        if let (Some(expected), Some(actual)) = (expected, result.as_ref())
            && !(IntegerType::from_type(expected, self.pointer_bits()).is_some()
                && IntegerType::from_type(actual, self.pointer_bits()).is_some())
        {
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
            if expected == actual || merge_variant_types(expected, actual).is_some() {
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
            TypeExpr::Named(name, args) => {
                if let Some(iface) = self.registry.find_interface(name) {
                    if self.interface_accessible(&iface) {
                        Some((name, args))
                    } else {
                        return false;
                    }
                } else {
                    None
                }
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

    fn interface_accessible(&self, iface: &InterfaceInfo) -> bool {
        self.can_access(&iface._module, iface.def.visibility)
    }

    fn lookup_function(
        &self,
        env: &FnEnv,
        name: &str,
        receiver: Option<&str>,
    ) -> Option<FunctionSignature> {
        if let Some(import) = env.imported_item(name) {
            if let Some(sig) =
                self.lookup_function_in_module(&import.module, &import.name, receiver)
            {
                return Some(sig);
            }
        }
        if let Some(module) = self.current_module.as_deref() {
            if let Some(sig) = self.lookup_function_in_module(module, name, receiver) {
                return Some(sig);
            }
        }
        for module_name in env.glob_modules() {
            if let Some(sig) = self.lookup_function_in_module(module_name, name, receiver) {
                return Some(sig);
            }
        }
        if let Some((module_path, func_name)) = name.rsplit_once("::") {
            if let Some(sig) = self.lookup_function_in_module(module_path, func_name, receiver) {
                return Some(sig);
            }
        }
        None
    }

    fn bind_pattern(
        &mut self,
        module: &Module,
        pattern: &Pattern,
        ty: Option<&TypeExpr>,
        env: &mut FnEnv,
        mutability: Mutability,
    ) -> Option<CheckedVariant> {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Identifier(name, span) => {
                env.declare(name, ty.cloned(), mutability, *span, &mut self.errors);
            }
            Pattern::Literal(lit) => {
                if let Some(actual) = ty {
                    let literal_ty = self.check_literal(module, lit, ty)?;
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
                let expected_ty = ty.map(strip_refs_and_pointers);
                let expected_enum = expected_ty.and_then(named_type_name).and_then(|name| {
                    self.lookup_enum(env, name, &mut false)
                        .or_else(|| self.registry.enums.get(name).cloned())
                });
                let owner = if let Some(name) = enum_name {
                    let mut private = false;
                    let Some(owner) = self.lookup_enum(env, name, &mut private) else {
                        let msg = if private {
                            format!("enum `{name}` is not public")
                        } else {
                            format!("Unknown enum `{name}`")
                        };
                        self.errors
                            .push(TypeError::new(&module.path, pattern_span(pattern), msg));
                        return None;
                    };
                    if let Some(expected) = &expected_enum
                        && (expected.def.name != owner.def.name || expected.module != owner.module)
                    {
                        self.errors.push(TypeError::new(
                            &module.path, pattern_span(pattern),
                            format!("enum pattern `{name}::{variant}` does not match value of type `{}`", expected.def.name),
                        ));
                        return None;
                    }
                    Some(owner)
                } else {
                    expected_enum
                };
                if let Some(enum_info) = owner {
                    let Ok((tag, variant_def)) = find_variant_with_tag(
                        &enum_info.def,
                        &enum_info.module,
                        variant,
                        module,
                        pattern_span(pattern),
                    ) else {
                        self.errors.push(TypeError::new(
                            &module.path,
                            pattern_span(pattern),
                            format!(
                                "`{}` does not belong to enum `{}`",
                                variant, enum_info.def.name
                            ),
                        ));
                        return None;
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
                        return None;
                    }
                    let args: &[TypeExpr] = match expected_ty {
                        Some(TypeExpr::Named(_, args)) => args,
                        _ => &[],
                    };
                    let fields = variant_fields(&enum_info.def, args, variant_def);
                    for (binding, field) in bindings.iter().zip(fields.iter()) {
                        self.bind_pattern(module, binding, Some(&field.ty), env, mutability);
                    }
                    return Some(CheckedVariant {
                        enum_name: enum_info.def.name,
                        name: variant.clone(),
                        tag,
                        fields,
                    });
                }
                if let Some(info) = self.registry.enum_variants.get(variant).cloned() {
                    if let Some(actual_enum) = enum_name {
                        if actual_enum != &info.enum_name {
                            self.errors.push(TypeError::new(
                                &module.path,
                                info.span,
                                format!("`{}` does not belong to enum `{}`", variant, actual_enum),
                            ));
                            return None;
                        }
                    } else if let Some(TypeExpr::Named(actual_enum, _)) = ty {
                        if actual_enum != &info.enum_name {
                            self.errors.push(TypeError::new(
                                &module.path,
                                pattern_span(pattern),
                                format!("`{}` does not belong to enum `{}`", variant, actual_enum),
                            ));
                            return None;
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
                        return None;
                    }
                    let fields = instantiate_variant_fields(&info, ty, &self.registry);
                    for (binding, field) in bindings.iter().zip(fields.iter()) {
                        self.bind_pattern(module, binding, Some(&field.ty), env, mutability);
                    }
                    return Some(CheckedVariant {
                        enum_name: info.enum_name,
                        name: variant.clone(),
                        tag: info.tag,
                        fields,
                    });
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
                        return None;
                    }
                    for (pat, elem_ty) in elements.iter().zip(types.iter()) {
                        self.bind_pattern(module, pat, Some(elem_ty), env, mutability);
                    }
                } else if let Some(actual) = ty {
                    self.errors.push(TypeError::new(
                        &module.path,
                        *span,
                        format!("expected tuple type, found `{}`", actual.canonical_name()),
                    ));
                } else {
                    for pat in elements {
                        self.bind_pattern(module, pat, None, env, mutability);
                    }
                }
            }
            Pattern::Map(entries, span) => {
                let value_ty = match ty {
                    Some(actual) => match self.expect_map_type(module, *span, Some(actual)) {
                        Some((_key, value)) => Some(value),
                        None => return None,
                    },
                    None => None,
                };
                for entry in entries {
                    self.bind_pattern(module, &entry.pattern, value_ty.as_ref(), env, mutability);
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
                let args: &[TypeExpr] = match ty {
                    Some(TypeExpr::Named(_, args)) => args,
                    _ => &[],
                };
                for field in fields {
                    let field_ty = struct_def.as_ref().and_then(|info| {
                        struct_pattern_field(&self.registry, &info.def, args, &field.name)
                    });
                    if struct_def.is_some() && field_ty.is_none() {
                        self.errors.push(TypeError::new(
                            &module.path,
                            *span,
                            format!("unknown field `{}` in struct pattern", field.name),
                        ));
                    }
                    self.bind_pattern(module, &field.pattern, field_ty.as_ref(), env, mutability);
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
                    self.bind_pattern(module, pat, element_ty.as_ref(), env, mutability);
                }
                if let Some(rest_pattern) = rest {
                    let rest_ty = element_ty
                        .as_ref()
                        .map(|inner| TypeExpr::Slice(Box::new(inner.clone())));
                    self.bind_pattern(module, rest_pattern, rest_ty.as_ref(), env, mutability);
                }
                for pat in suffix {
                    self.bind_pattern(module, pat, element_ty.as_ref(), env, mutability);
                }
            }
        }
        None
    }

    fn check_range_bounds(
        &mut self,
        module: &Module,
        range: &RangeExpr,
        expected_inner: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let start_ty = self.check_expression(module, &range.start, expected_inner, returns, env);
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
            match numeric_kind(ty, self.pointer_bits()) {
                Some(NumericKind::Signed(_, _)) | Some(NumericKind::Unsigned(_, _)) => {}
                Some(NumericKind::Float(_, _)) => {
                    self.errors.push(
                        TypeError::new(&module.path, span, "ranges only support integer bounds")
                            .with_code("E0204"),
                    );
                    return None;
                }
                None => {
                    self.errors.push(
                        TypeError::new(&module.path, span, "range bounds must be numeric")
                            .with_code("E0205"),
                    );
                    return None;
                }
            }
        }
        candidate
    }

    fn collection_element_type(&self, ty: &TypeExpr, env: &FnEnv) -> Option<TypeExpr> {
        if let Some(inner) = Self::direct_collection_element_type(ty) {
            return Some(inner);
        }
        if let Some(return_ty) = self.iter_method_return_type(ty, env) {
            return self.collection_element_type(&return_ty, env);
        }
        None
    }

    fn direct_collection_element_type(ty: &TypeExpr) -> Option<TypeExpr> {
        match ty {
            TypeExpr::Slice(inner) => Some((**inner).clone()),
            TypeExpr::Named(name, args) if name == "Map" && args.len() == 2 => {
                Some(TypeExpr::Tuple(vec![args[0].clone(), args[1].clone()]))
            }
            TypeExpr::Named(name, args) if name == "Range" && args.len() == 1 => {
                Some(args[0].clone())
            }
            TypeExpr::Reference { ty: inner, .. } | TypeExpr::Pointer { ty: inner, .. } => {
                Self::direct_collection_element_type(inner.as_ref())
            }
            _ => None,
        }
    }

    fn iter_method_return_type(&self, receiver: &TypeExpr, env: &FnEnv) -> Option<TypeExpr> {
        let name = named_type_name(receiver)?;
        let sig = self.lookup_function(env, "iter", Some(name))?;
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

    fn embedded_builtins_enabled(&self) -> bool {
        self.target.is_embedded()
    }

    fn embedded_std_compatible_builtin(&self, name: &str) -> bool {
        self.target.is_embedded()
            && matches!(
                name,
                "spawn"
                    | "join"
                    | "channel"
                    | "send"
                    | "recv"
                    | "recv_timeout"
                    | "recv_task"
                    | "close"
                    | "sleep"
                    | "sleep_ms"
                    | "sleep_task"
                    | "now_ms"
                    | "cancel_token"
                    | "cancel"
                    | "is_cancelled"
                    | "await_timeout"
                    | "await_cancel"
                    | "await_cancel_timeout"
                    | "digital_read"
                    | "reset_reason"
            )
    }

    fn is_embedded_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "pin_mode" | "digital_write" | "digital_read" | "delay_ms" | "reset_reason"
        )
    }

    fn is_builtin_name(&self, name: &str) -> bool {
        if TcpBuiltin::lookup(name).is_some() {
            return true;
        }
        matches!(
            name,
            "out"
                | "in"
                | "box_new"
                | "box_get"
                | "box_set"
                | "box_take"
                | "slice_new"
                | "slice_push"
                | "slice_len"
                | "slice_get"
                | "slice_get_int"
                | "map_new"
                | "map_insert"
                | "map_get"
                | "map_keys"
                | "map_values"
                | "assert"
                | "expect"
                | "len"
                | "iter"
                | "next"
                | "str_len"
                | "str_contains"
                | "str_trim"
                | "str_split"
                | "min"
                | "max"
                | "abs"
                | "channel"
                | "debug_show"
                | "send"
                | "recv"
                | "recv_timeout"
                | "recv_task"
                | "cancel_token"
                | "cancel"
                | "is_cancelled"
                | "await_timeout"
                | "await_cancel"
                | "await_cancel_timeout"
                | "close"
                | "join"
                | "sleep"
                | "sleep_ms"
                | "sleep_task"
                | "now_ms"
                | "fs_exists"
                | "fs_read"
                | "fs_write"
                | "fs_write_bytes"
                | "gfx_open"
                | "gfx_clear"
                | "gfx_rect"
                | "gfx_sprite"
                | "gfx_text"
                | "gfx_text_int"
                | "gfx_present"
                | "gfx_key_down"
                | "gfx_key_pressed"
                | "gfx_should_close"
                | "gfx_close"
                | "audio_play"
                | "audio_stop"
                | "audio_stop_all"
                | "audio_set_volume"
                | "audio_is_playing"
                | "ptr"
                | "ptr_mut"
                | "cast"
                | "assert_eq"
                | "panic"
        ) || self.is_embedded_builtin(name)
    }

    fn is_std_only_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "channel"
                | "send"
                | "recv"
                | "recv_timeout"
                | "recv_task"
                | "cancel_token"
                | "cancel"
                | "is_cancelled"
                | "await_timeout"
                | "await_cancel"
                | "await_cancel_timeout"
                | "close"
                | "join"
                | "sleep"
                | "sleep_ms"
                | "sleep_task"
                | "now_ms"
                | "fs_exists"
                | "fs_read"
                | "fs_write"
                | "fs_write_bytes"
                | "gfx_open"
                | "gfx_clear"
                | "gfx_rect"
                | "gfx_sprite"
                | "gfx_text"
                | "gfx_text_int"
                | "gfx_present"
                | "gfx_key_down"
                | "gfx_key_pressed"
                | "gfx_should_close"
                | "gfx_close"
                | "audio_play"
                | "audio_stop"
                | "audio_stop_all"
                | "audio_set_volume"
                | "audio_is_playing"
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
                self.select_call_result(module, span, std::slice::from_ref(&ret), Some(&ret.ty))
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
                self.select_call_result(module, span, std::slice::from_ref(&ret), Some(&ret.ty))
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
                self.select_call_result(module, span, std::slice::from_ref(&ret), Some(&ret.ty))
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
                self.select_call_result(module, span, std::slice::from_ref(&ret), Some(&ret.ty))
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
        let pointer_bits = self.pointer_bits();
        let expected_kind = expected.and_then(|ty| numeric_kind(ty, pointer_bits));
        let left_kind = left.and_then(|ty| numeric_kind(ty, pointer_bits));
        let right_kind = right.and_then(|ty| numeric_kind(ty, pointer_bits));

        if expected.is_some()
            && (left != expected || right != expected)
            && matches!(
                expected_kind,
                Some(NumericKind::Signed(..) | NumericKind::Unsigned(..))
            )
            && matches!(
                left_kind,
                Some(NumericKind::Signed(..) | NumericKind::Unsigned(..))
            )
            && matches!(
                right_kind,
                Some(NumericKind::Signed(..) | NumericKind::Unsigned(..))
            )
        {
            return self
                .unify_numeric_kinds(module, span, left, right, left_kind, right_kind)
                .map(numeric_type_from_kind);
        }
        if let Some(target) = expected_kind {
            let left_ok = self.numeric_operand_matches(module, span, left, left_kind, target);
            let right_ok = self.numeric_operand_matches(module, span, right, right_kind, target);
            if left_ok && right_ok {
                return Some(numeric_type_from_kind(target));
            }
            return None;
        }

        let kind = self.unify_numeric_kinds(module, span, left, right, left_kind, right_kind)?;
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
        let pointer_bits = self.pointer_bits();
        if [expected, left, right]
            .into_iter()
            .flatten()
            .any(|ty| matches!(numeric_kind(ty, pointer_bits), Some(NumericKind::Float(..))))
        {
            self.errors.push(
                TypeError::new(
                    &module.path,
                    span,
                    "bitwise operations require integer operands",
                )
                .with_code("E0206"),
            );
            return None;
        }
        self.resolve_numeric_type(module, span, expected, left, right)
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
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        span,
                        format!(
                            "expected `{}`, found `{}`",
                            numeric_type_from_kind(expected_kind).canonical_name(),
                            describe_type(operand_ty)
                        ),
                    )
                    .with_code("E0201"),
                );
                false
            }
            None => {
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        span,
                        "numeric operations require int or float operands",
                    )
                    .with_code("E0200"),
                );
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
        let pointer_bits = self.pointer_bits();
        let mut had_error = false;
        if let (Some(ty), None) = (left_ty, left_kind) {
            self.errors.push(
                TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "numeric operations require numeric operands (found `{}`)",
                        ty.canonical_name()
                    ),
                )
                .with_code("E0200"),
            );
            had_error = true;
        }
        if let (Some(ty), None) = (right_ty, right_kind) {
            self.errors.push(
                TypeError::new(
                    &module.path,
                    span,
                    format!(
                        "numeric operations require numeric operands (found `{}`)",
                        ty.canonical_name()
                    ),
                )
                .with_code("E0200"),
            );
            had_error = true;
        }
        if had_error {
            return None;
        }

        match (left_kind, right_kind) {
            (Some(kind), None) | (None, Some(kind)) => Some(kind),
            (None, None) => {
                self.errors.push(
                    TypeError::new(
                        &module.path,
                        span,
                        "numeric operations require int or float operands",
                    )
                    .with_code("E0200"),
                );
                None
            }
            (Some(NumericKind::Float(_, left_bits)), Some(NumericKind::Float(_, right_bits))) => {
                Some(float_kind(left_bits.max(right_bits)))
            }
            (Some(NumericKind::Float(_, bits)), Some(_))
            | (Some(_), Some(NumericKind::Float(_, bits))) => Some(float_kind(bits)),
            (Some(NumericKind::Signed(_, left_bits)), Some(NumericKind::Signed(_, right_bits))) => {
                Some(signed_kind_for_bits(
                    left_bits.max(right_bits),
                    pointer_bits,
                ))
            }
            (
                Some(NumericKind::Unsigned(_, left_bits)),
                Some(NumericKind::Unsigned(_, right_bits)),
            ) => Some(unsigned_kind_for_bits(
                left_bits.max(right_bits),
                pointer_bits,
            )),
            _ => {
                self.emit_signedness_mismatch(module, span, left_ty, right_ty);
                None
            }
        }
    }

    #[allow(dead_code)]
    fn emit_numeric_mismatch(
        &mut self,
        module: &Module,
        span: Span,
        left_ty: Option<&TypeExpr>,
        right_ty: Option<&TypeExpr>,
    ) {
        self.errors.push(
            TypeError::new(
                &module.path,
                span,
                format!(
                    "numeric operands must share a type (found `{}` and `{}`)",
                    describe_type(left_ty),
                    describe_type(right_ty)
                ),
            )
            .with_code("E0201"),
        );
    }

    fn emit_signedness_mismatch(
        &mut self,
        module: &Module,
        span: Span,
        left_ty: Option<&TypeExpr>,
        right_ty: Option<&TypeExpr>,
    ) {
        self.errors.push(
            TypeError::new(
                &module.path,
                span,
                format!(
                    "signed and unsigned integers cannot be combined implicitly (found `{}` and `{}`)",
                    describe_type(left_ty),
                    describe_type(right_ty)
                ),
            )
            .with_code("E0202"),
        );
    }

    fn check_binary(
        &mut self,
        module: &Module,
        id: ExprId,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: Span,
        expected: Option<&TypeExpr>,
        returns: &[TypeExpr],
        env: &mut FnEnv,
    ) -> Option<TypeExpr> {
        let pointer_bits = self.pointer_bits();
        let mut comparison = false;
        let ty = match op {
            BinaryOp::Add => {
                let string_expected = expected.filter(|ty| is_string_type(ty));
                let numeric_expected =
                    expected.and_then(|ty| numeric_kind(ty, pointer_bits).map(|_| ty));
                let left_ty = self.check_expression_hint(
                    module,
                    left,
                    string_expected.or(numeric_expected),
                    returns,
                    env,
                );
                let string_ty = string_type();
                let right_expected = if left_ty.as_ref().is_some_and(is_string_type) {
                    Some(&string_ty)
                } else {
                    left_ty.as_ref().or(numeric_expected)
                };
                let right_ty =
                    self.check_expression_hint(module, right, right_expected, returns, env);
                if left_ty.as_ref().is_some_and(is_string_type)
                    && right_ty.as_ref().is_some_and(is_string_type)
                {
                    Some(string_type())
                } else {
                    self.resolve_numeric_type(
                        module,
                        span,
                        numeric_expected,
                        left_ty.as_ref(),
                        right_ty.as_ref(),
                    )
                }
            }
            BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                let numeric_expected =
                    expected.and_then(|ty| numeric_kind(ty, pointer_bits).map(|_| ty));
                let left_ty =
                    self.check_expression_hint(module, left, numeric_expected, returns, env);
                let right_ty = self.check_expression_hint(
                    module,
                    right,
                    left_ty.as_ref().or(numeric_expected),
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
                let numeric_expected =
                    expected.and_then(|ty| numeric_kind(ty, pointer_bits).map(|_| ty));
                let left_ty =
                    self.check_expression_hint(module, left, numeric_expected, returns, env);
                let right_ty = self.check_expression_hint(
                    module,
                    right,
                    left_ty.as_ref().or(numeric_expected),
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
                self.check_expression_hint(module, left, Some(&bool_type()), returns, env);
                self.check_expression_hint(module, right, Some(&bool_type()), returns, env);
                Some(bool_type())
            }
            BinaryOp::Eq | BinaryOp::NotEq => {
                let left_ty = self.check_expression_hint(module, left, None, returns, env);
                let right_ty =
                    self.check_expression_hint(module, right, left_ty.as_ref(), returns, env);
                if let Some(left_ty) = left_ty.as_ref() {
                    self.ensure_type(module, span, left_ty, right_ty.as_ref());
                }
                comparison = true;
                left_ty
            }
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                let numeric_expected =
                    expected.and_then(|ty| numeric_kind(ty, pointer_bits).map(|_| ty));
                let left_ty =
                    self.check_expression_hint(module, left, numeric_expected, returns, env);
                let right_ty = self.check_expression_hint(
                    module,
                    right,
                    left_ty.as_ref().or(numeric_expected),
                    returns,
                    env,
                );
                comparison = true;
                self.resolve_numeric_type(
                    module,
                    span,
                    numeric_expected,
                    left_ty.as_ref(),
                    right_ty.as_ref(),
                )
            }
        };
        if let Some(int) = ty
            .as_ref()
            .and_then(|t| IntegerType::from_type(t, pointer_bits))
        {
            self.record_integer_op(
                module,
                span,
                id,
                IntegerOp::Binary(op, int),
                left,
                Some(right),
            );
        }
        if comparison { Some(bool_type()) } else { ty }
    }

    fn check_literal(
        &mut self,
        module: &Module,
        lit: &Literal,
        expected: Option<&TypeExpr>,
    ) -> Option<TypeExpr> {
        let pointer_bits = self.pointer_bits();
        let ty = literal_type(lit, expected, pointer_bits);
        if let Some(expected) = expected
            && ((matches!(lit, Literal::Int(..)) && expected == &bool_type())
                || (matches!(lit, Literal::Bool(..))
                    && numeric_kind(expected, pointer_bits).is_some()))
        {
            self.ensure_type(module, literal_span(lit), expected, Some(&ty));
            return None;
        }
        if let Literal::Int(value, span) = lit {
            if let Some(int) = IntegerType::from_type(&ty, pointer_bits)
                && int.checked(*value).is_err()
            {
                self.errors.push(TypeError::new(
                    &module.path,
                    *span,
                    format!(
                        "integer literal `{value}` is out of range for `{}`",
                        ty.canonical_name()
                    ),
                ));
                return None;
            }
        }
        Some(ty)
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
        let pointer_bits = self.pointer_bits();
        match op {
            UnaryOp::Neg => {
                let numeric_expected =
                    expected.and_then(|ty| numeric_kind(ty, pointer_bits).map(|_| ty));
                // The minimum signed value fits even though its positive magnitude does not.
                if let Expr::Literal(Literal::Int(magnitude, _)) = expr
                    && !matches!(
                        numeric_expected.and_then(|ty| numeric_kind(ty, pointer_bits)),
                        Some(NumericKind::Unsigned(..))
                    )
                {
                    return self.check_literal(module, &Literal::Int(-magnitude, span), expected);
                }
                let ty = self.check_expression_hint(module, expr, numeric_expected, returns, env);
                match ty.as_ref().and_then(|ty| numeric_kind(ty, pointer_bits)) {
                    Some(NumericKind::Unsigned(_, _)) | None => {
                        self.errors.push(
                            TypeError::new(
                                &module.path,
                                span,
                                "unary `-` requires a signed or floating-point type",
                            )
                            .with_code("E0203"),
                        );
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
    integer_constant: Option<i128>,
    ty: Option<TypeExpr>,
    mut_borrows: Vec<String>,
    moved: bool,
    mutable: bool,
    origin: Span,
    last_move: Option<Span>,
}

impl BindingInfo {
    fn new(ty: Option<TypeExpr>, mutable: Mutability, origin: Span) -> Self {
        Self {
            integer_constant: None,
            ty,
            mut_borrows: Vec::new(),
            moved: false,
            mutable: mutable.is_mutable(),
            origin,
            last_move: None,
        }
    }
}

#[derive(Clone)]
struct ImportedItem {
    module: String,
    name: String,
}

#[derive(Clone, Default)]
struct ImportScope {
    modules: HashMap<String, String>,
    selected: HashMap<String, ImportedItem>,
    globs: Vec<String>,
}

struct FnEnv {
    path: PathBuf,
    type_params: HashSet<String>,
    scopes: Vec<HashMap<String, BindingInfo>>,
    active_mut_borrows: HashMap<String, Vec<BorrowRecord>>,
    imports: ImportScope,
    capture_stack: Vec<ClosureCaptureContext>,
    task_returns: Option<Vec<(Span, TypeExpr)>>,
}

#[derive(Clone)]
struct BorrowState {
    active_mut_borrows: HashMap<String, usize>,
    scope_borrows: Vec<HashMap<String, HashMap<String, usize>>>,
    moved_scopes: Vec<HashSet<String>>,
}

#[derive(Clone, Debug)]
struct BorrowRecord {
    borrower: String,
    span: Span,
}

struct ClosureCaptureContext {
    task: bool,
    base_depth: usize,
    captured: HashSet<String>,
}

impl FnEnv {
    fn new(path: PathBuf, type_params: Vec<String>, imports: ImportScope) -> Self {
        Self {
            path,
            type_params: type_params.into_iter().collect(),
            scopes: vec![HashMap::new()],
            active_mut_borrows: HashMap::new(),
            imports,
            capture_stack: Vec::new(),
            task_returns: None,
        }
    }

    fn capture_global(&mut self, name: &str) {
        for ctx in &mut self.capture_stack {
            if ctx.task {
                ctx.captured.insert(name.to_owned());
            }
        }
    }

    fn begin_capture(&mut self, task: bool) {
        let base_depth = self.scopes.len();
        self.capture_stack.push(ClosureCaptureContext {
            task,
            base_depth,
            captured: HashSet::new(),
        });
    }

    fn end_closure_capture(&mut self) -> Vec<String> {
        if let Some(ctx) = self.capture_stack.pop() {
            ctx.captured.into_iter().collect()
        } else {
            Vec::new()
        }
    }

    fn has_active_mut_borrows(&self) -> bool {
        !self.active_mut_borrows.is_empty()
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
        let mut active_counts = HashMap::new();
        for (name, spans) in &self.active_mut_borrows {
            active_counts.insert(name.clone(), spans.len());
        }
        BorrowState {
            active_mut_borrows: active_counts,
            scope_borrows,
            moved_scopes,
        }
    }

    fn restore_borrow_state(&mut self, state: &BorrowState) {
        self.set_active_borrow_counts(&state.active_mut_borrows);
        self.apply_scope_borrows(&state.scope_borrows);
        self.apply_scope_moves(&state.moved_scopes);
        Self::intersect_counts_with_spans(&mut self.active_mut_borrows, &state.active_mut_borrows);
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
        self.set_active_borrow_counts(&merged.active_mut_borrows);
        self.apply_scope_borrows(&merged.scope_borrows);
        self.apply_scope_moves(&merged.moved_scopes);
        Self::intersect_counts_with_spans(&mut self.active_mut_borrows, &merged.active_mut_borrows);
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

    fn intersect_counts_with_spans(
        spans: &mut HashMap<String, Vec<BorrowRecord>>,
        counts: &HashMap<String, usize>,
    ) {
        spans.retain(|target, records| {
            if let Some(limit) = counts.get(target) {
                records.truncate(*limit);
                !records.is_empty()
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

    fn set_active_borrow_counts(&mut self, counts: &HashMap<String, usize>) {
        self.active_mut_borrows.clear();
        for (name, count) in counts {
            let spans = vec![
                BorrowRecord {
                    borrower: name.clone(),
                    span: Span::new(0, 0),
                };
                *count
            ];
            self.active_mut_borrows.insert(name.clone(), spans);
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

    fn binding_origin(&self, name: &str) -> Option<Span> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info.origin);
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
                    let mut err = TypeError::new(
                        &module.path,
                        span,
                        format!("`{}` was moved and can no longer be used here", name),
                    );
                    if let Some(origin) = info.last_move {
                        err = err
                            .with_label(format!("`{}` was moved here", name))
                            .with_help(format!(
                                "value was moved at bytes {}..{}; borrow a reference instead of moving if reuse is needed",
                                origin.start, origin.end
                            ));
                    }
                    errors.push(err);
                }
                return;
            }
        }
    }

    fn ensure_not_borrowed_for_move(
        &self,
        module: &Module,
        span: Span,
        name: &str,
        errors: &mut Vec<TypeError>,
    ) {
        if let Some(records) = self.active_mut_borrows.get(name) {
            if let Some(first) = records.first() {
                let mut err = TypeError::new(
                    &module.path,
                    span,
                    format!("`{}` cannot be moved because it is mutably borrowed", name),
                )
                .with_label(format!(
                    "`{}` first mutably borrowed by `{}` here",
                    name, first.borrower
                ));
                let origin_text = if let Some(origin) = self.binding_origin(name) {
                    format!(
                        " (binding declared at bytes {}..{})",
                        origin.start, origin.end
                    )
                } else {
                    String::new()
                };
                err = err.with_help(format!(
                    "borrow started at bytes {}..{}{}; borrow a reference or clone before moving",
                    first.span.start, first.span.end, origin_text
                ));
                errors.push(err);
            }
        }
    }

    fn mark_moved(&mut self, name: &str, span: Span) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.moved = true;
                info.last_move = Some(span);
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
        mutable: Mutability,
        span: Span,
        errors: &mut Vec<TypeError>,
    ) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), BindingInfo::new(ty, mutable, span));
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
                .or_insert_with(|| BindingInfo::new(None, Mutability::Immutable, Span::new(0, 0)))
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

    fn lookup_with_captures(&mut self, name: &str) -> Option<&BindingInfo> {
        let len = self.scopes.len();
        for (rev_idx, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(info) = scope.get(name) {
                let scope_index = len.saturating_sub(rev_idx + 1);
                for ctx in &mut self.capture_stack {
                    if scope_index < ctx.base_depth {
                        ctx.captured.insert(name.to_string());
                    }
                }
                return Some(info);
            }
        }
        None
    }

    fn imported_item(&self, name: &str) -> Option<&ImportedItem> {
        self.imports.selected.get(name)
    }

    fn module_alias(&self, alias: &str) -> Option<&String> {
        self.imports.modules.get(alias)
    }

    fn glob_modules(&self) -> impl Iterator<Item = &String> {
        self.imports.globs.iter()
    }

    fn clear_binding_borrows(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                let targets: Vec<String> = std::mem::take(&mut info.mut_borrows);
                for target in targets {
                    self.end_mut_borrow(&target);
                }
                return;
            }
        }
    }

    fn register_binding_borrow(&mut self, name: &str, target: String, span: Span) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.mut_borrows.push(target.clone());
                self.active_mut_borrows
                    .entry(target)
                    .or_default()
                    .push(BorrowRecord {
                        borrower: name.to_string(),
                        span,
                    });
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
        if let Some(spans) = self.active_mut_borrows.get(target) {
            let mut label = format!("`{}` is already mutably borrowed", target);
            let mut help = None;
            if let Some(first) = spans.first() {
                label = format!(
                    "`{}` first mutably borrowed by `{}` here",
                    target, first.borrower
                );
                let origin_text = if let Some(origin) = self.binding_origin(target) {
                    format!(
                        " (binding declared at bytes {}..{})",
                        origin.start, origin.end
                    )
                } else {
                    String::new()
                };
                help = Some(format!(
                    "first borrower `{}` started at bytes {}..{}{}; consider cloning or reordering borrows",
                    first.borrower, first.span.start, first.span.end, origin_text
                ));
            }
            let mut err = TypeError::new(
                &module.path,
                span,
                format!("`{}` is already mutably borrowed", target),
            )
            .with_label(label);
            if let Some(help) = help {
                err = err.with_help(help);
            }
            errors.push(err);
        }
    }

    fn end_mut_borrow(&mut self, target: &str) {
        if let Some(entry) = self.active_mut_borrows.get_mut(target) {
            if entry.len() > 1 {
                entry.pop();
            } else {
                self.active_mut_borrows.remove(target);
            }
        }
    }
}

fn instantiate_function(
    sig: &FunctionSignature,
    type_args: &[TypeExpr],
    path: &Path,
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

const INFERRED_VARIANT_PARAM: &str = "<inferred>";

fn has_inferred_variant_param(ty: &TypeExpr) -> bool {
    match ty {
        TypeExpr::Named(name, args) => {
            name == INFERRED_VARIANT_PARAM || args.iter().any(has_inferred_variant_param)
        }
        TypeExpr::Slice(t) | TypeExpr::Array { ty: t, .. } => has_inferred_variant_param(t),
        TypeExpr::Tuple(items) => items.iter().any(has_inferred_variant_param),
        _ => false,
    }
}

fn infer_variant_params(
    field: &TypeExpr,
    actual: &TypeExpr,
    params: &[String],
    subst: &mut HashMap<String, TypeExpr>,
) {
    match (field, actual) {
        (TypeExpr::Named(n, args), _) if args.is_empty() && params.contains(n) => {
            let bound = subst.entry(n.clone()).or_insert_with(|| actual.clone());
            if *bound == TypeExpr::named(INFERRED_VARIANT_PARAM) {
                *bound = actual.clone();
            }
        }
        (TypeExpr::Tuple(fs), TypeExpr::Tuple(ts))
        | (TypeExpr::Named(_, fs), TypeExpr::Named(_, ts)) => {
            for (f, t) in fs.iter().zip(ts) {
                infer_variant_params(f, t, params, subst);
            }
        }
        (TypeExpr::Slice(f), TypeExpr::Slice(t))
        | (TypeExpr::Array { ty: f, .. }, TypeExpr::Array { ty: t, .. })
        | (TypeExpr::Reference { ty: f, .. }, TypeExpr::Reference { ty: t, .. })
        | (TypeExpr::Pointer { ty: f, .. }, TypeExpr::Pointer { ty: t, .. }) => {
            infer_variant_params(f, t, params, subst)
        }
        _ => {}
    }
}

// A variant can leave a type argument unspecified, e.g. Err(e) has no Ok payload.
// A branch contributing a concrete instantiation determines that argument.
fn merge_variant_types(a: &TypeExpr, b: &TypeExpr) -> Option<TypeExpr> {
    if a == b || b == &TypeExpr::named(INFERRED_VARIANT_PARAM) {
        return Some(a.clone());
    }
    if a == &TypeExpr::named(INFERRED_VARIANT_PARAM) {
        return Some(b.clone());
    }
    if let (TypeExpr::Named(an, aa), TypeExpr::Named(bn, ba)) = (a, b)
        && an == bn
        && aa.len() == ba.len()
    {
        let args = aa
            .iter()
            .zip(ba)
            .map(|(l, r)| merge_variant_types(l, r))
            .collect::<Option<Vec<_>>>()?;
        return Some(TypeExpr::Named(an.clone(), args));
    }
    None
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

fn struct_pattern_field(
    registry: &TypeRegistry,
    def: &StructDef,
    args: &[TypeExpr],
    name: &str,
) -> Option<TypeExpr> {
    let ty = lookup_struct_field(registry, def, name)?;
    let params = def
        .type_params
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect();
    Some(ty.substitute(&params))
}

// These initializers can be checked without looking up the types of other constants.
fn is_literal_initializer(expr: &Expr) -> bool {
    match expr {
        Expr::Literal(_) => true,
        Expr::Unary { expr, .. } => is_literal_initializer(expr),
        Expr::Binary { left, right, .. } => {
            is_literal_initializer(left) && is_literal_initializer(right)
        }
        Expr::EnumLiteral { values, .. }
        | Expr::Tuple(values, _)
        | Expr::ArrayLiteral(values, _) => values.iter().all(is_literal_initializer),
        Expr::Call { callee, args, .. } if matches!(callee.as_ref(), Expr::Identifier(_)) => {
            args.iter().all(is_literal_initializer)
        }
        _ => false,
    }
}

fn is_integer_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Unary {
            op: UnaryOp::Neg,
            expr,
            ..
        } => matches!(expr.as_ref(), Expr::Literal(Literal::Int(..))),
        _ => matches!(expr, Expr::Literal(Literal::Int(..))),
    }
}

fn literal_type(lit: &Literal, expected: Option<&TypeExpr>, pointer_bits: u32) -> TypeExpr {
    match lit {
        Literal::Int(_, _) => {
            if let Some(exp) = expected {
                if numeric_kind(exp, pointer_bits).is_some() {
                    return exp.clone();
                }
            }
            int_type()
        }
        Literal::Float(_, _) => {
            if let Some(exp) = expected {
                if let Some(NumericKind::Float(_, _)) = numeric_kind(exp, pointer_bits) {
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

fn make_task_type(inner: TypeExpr) -> TypeExpr {
    TypeExpr::Named("Task".into(), vec![inner])
}

fn is_string_type(ty: &TypeExpr) -> bool {
    matches!(ty, TypeExpr::Named(name, args) if name == "string" && args.is_empty())
}

fn instantiate_variant_fields(
    info: &EnumVariantInfo,
    scrutinee: Option<&TypeExpr>,
    registry: &TypeRegistry,
) -> Vec<TypeAnnotation> {
    let Some(TypeExpr::Named(enum_name, args)) = scrutinee else {
        return info.def.fields.clone();
    };
    if enum_name != &info.enum_name {
        return info.def.fields.clone();
    }
    let Some(enum_info) = registry.enums.get(enum_name) else {
        return info.def.fields.clone();
    };
    variant_fields(&enum_info.def, args, &info.def)
}

fn variant_fields(def: &EnumDef, args: &[TypeExpr], variant: &EnumVariant) -> Vec<TypeAnnotation> {
    let mut fields = variant.fields.clone();
    if def.type_params.len() != args.len() || args.is_empty() {
        return fields;
    }
    let map: HashMap<String, TypeExpr> = def
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
        Expr::MacroCall { span, .. } => *span,
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
        Expr::Async { span, .. } => *span,
        Expr::Await { span, .. } => *span,
        Expr::Spawn { span, .. } => *span,
        Expr::Closure { span, .. } => *span,
    }
}

fn stmt_span(statement: &Statement) -> Span {
    match statement {
        Statement::Let(stmt) => stmt.span,
        Statement::Assign(AssignStmt { target, .. }) => expr_span(target),
        Statement::Expr(expr) => expr_span(&expr.expr),
        Statement::MacroSemi(expr) => expr.span,
        Statement::Return(_) => Span::new(0, 0),
        Statement::While(while_stmt) => while_stmt.body.span,
        Statement::Loop(stmt) => stmt.span,
        Statement::For(stmt) => stmt.span,
        Statement::Defer(_) => Span::new(0, 0),
        Statement::Break | Statement::Continue => Span::new(0, 0),
        Statement::Comment { span, .. } => *span,
        Statement::Block(block) => block.span,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumericKind {
    Signed(&'static str, u32),
    Unsigned(&'static str, u32),
    Float(&'static str, u32),
}

fn signed_kind_for_bits(bits: u32, pointer_bits: u32) -> NumericKind {
    let name = match bits {
        8 => "int8",
        16 => "int16",
        32 => "int32",
        64 => "int64",
        b if b == pointer_bits => "isize",
        _ => "int64",
    };
    NumericKind::Signed(name, bits)
}

fn unsigned_kind_for_bits(bits: u32, pointer_bits: u32) -> NumericKind {
    let name = match bits {
        8 => "uint8",
        16 => "uint16",
        32 => "uint32",
        64 => "uint64",
        b if b == pointer_bits => "usize",
        _ => "uint64",
    };
    NumericKind::Unsigned(name, bits)
}

fn numeric_kind_from_name(name: &str, pointer_bits: u32) -> Option<NumericKind> {
    if let Some(int) = IntegerType::from_name(name, pointer_bits) {
        return Some(if int.signed {
            NumericKind::Signed(int.name, int.bits)
        } else {
            NumericKind::Unsigned(int.name, int.bits)
        });
    }
    match name {
        "float32" => Some(NumericKind::Float("float32", 32)),
        "float64" => Some(NumericKind::Float("float64", 64)),
        _ => None,
    }
}

fn numeric_kind(ty: &TypeExpr, pointer_bits: u32) -> Option<NumericKind> {
    match ty {
        TypeExpr::Named(name, _) => numeric_kind_from_name(name, pointer_bits),
        _ => None,
    }
}

fn numeric_type_from_kind(kind: NumericKind) -> TypeExpr {
    match kind {
        NumericKind::Signed(name, _)
        | NumericKind::Unsigned(name, _)
        | NumericKind::Float(name, _) => TypeExpr::Named(name.into(), Vec::new()),
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
        | Expr::Closure { .. }
        | Expr::Index { .. }
        | Expr::Range(_)
        | Expr::MacroCall { .. }
        | Expr::Async { .. }
        | Expr::Await { .. } => true,
        Expr::Spawn { .. } => true,
        Expr::EnumLiteral { .. } => true,
    }
}

fn strip_refs_and_pointers(ty: &TypeExpr) -> &TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
            strip_refs_and_pointers(ty)
        }
        _ => ty,
    }
}

fn strip_references(ty: &TypeExpr) -> &TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } => strip_references(ty),
        _ => ty,
    }
}

fn named_type_name(ty: &TypeExpr) -> Option<&str> {
    match ty {
        TypeExpr::Named(name, _) => Some(name),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => named_type_name(ty),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::check_program;
    use crate::language::{
        ast::{Expr, Program},
        macro_expander::{self, ExpandedProgram, ExpansionTraces},
        parser::{parse_expression_snippet, parse_module},
    };
    use std::path::PathBuf;

    fn typecheck_source(source: &str) -> Result<(), Vec<super::TypeError>> {
        typecheck_source_with_options(source, &super::TypecheckOptions::default())
    }

    fn typecheck_source_with_options(
        source: &str,
        options: &super::TypecheckOptions,
    ) -> Result<(), Vec<super::TypeError>> {
        let module =
            parse_module("tests::module", PathBuf::from("test.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        let expanded = ExpandedProgram {
            program,
            traces: ExpansionTraces::default(),
            item_origins: std::collections::HashMap::new(),
        };
        super::check_program_with_options(&expanded, options)
    }

    #[allow(dead_code)]
    fn typecheck_modules(mods: Vec<(&str, &str)>) -> Result<(), Vec<super::TypeError>> {
        let modules = mods
            .into_iter()
            .map(|(name, src)| {
                parse_module(name, PathBuf::from(format!("{}.prime", name)), src).expect("parse")
            })
            .collect();
        let program = Program { modules };
        let expanded = ExpandedProgram {
            program,
            traces: ExpansionTraces::default(),
            item_origins: std::collections::HashMap::new(),
        };
        check_program(&expanded)
    }

    #[test]
    fn task_boundaries_accept_owned_recursion_and_local_borrows() {
        typecheck_source(
            r#"
module demo;
enum Chain { End, Next(Box[Chain]) }
struct Packet { chain: Chain; token: CancelToken; }
fn inspect(p: Packet, unused: &int32) -> Task[bool] {
  async { let local = 7; let borrow = &local; let n = *borrow; is_cancelled(p.token) }
}
fn main() {}
"#,
        )
        .unwrap();
    }

    #[test]
    fn type_errors_include_macro_trace() {
        let module = parse_module(
            "tests::macro_trace",
            PathBuf::from("test.prime"),
            r#"
module tests::macro_trace;

macro make_value() -> int32 {
  "oops"
}

fn use_it() {
  let int32 value = ~make_value();
}
"#,
        )
        .expect("parse macro test");
        let program = Program {
            modules: vec![module],
        };
        let expanded = macro_expander::expand_program(&program).expect("expand macros");
        let Err(errors) = check_program(&expanded) else {
            panic!("expected a type error for macro output");
        };
        let help = errors[0].help.clone().unwrap_or_default();
        assert!(
            help.contains("make_value"),
            "missing macro name in trace help: {help}"
        );
        assert!(
            help.contains("expansion"),
            "missing expansion wording in trace help: {help}"
        );
    }

    #[test]
    fn input_requires_type_argument_and_returns_result() {
        let ok = r#"
module tests::input;

enum Result[T, E] {
  Ok(T),
  Err(E),
}

fn main() {
  let result = in[int32]("Age: ");
  match result {
    Result::Ok(val) => out(val),
    Result::Err(msg) => out(msg),
  }
}
"#;
        if let Err(errs) = typecheck_source(ok) {
            panic!("expected in[int32] to typecheck, errors: {:?}", errs);
        }

        let missing_type_arg = r#"
module tests::input_missing;

enum Result[T, E] {
  Ok(T),
  Err(E),
}

fn main() {
  let bad = in("oops");
  out(bad);
}
"#;
        assert!(
            typecheck_source(missing_type_arg).is_err(),
            "expected type error when `in` is missing a type argument"
        );
    }

    #[test]
    fn input_expression_parses_with_type_arg() {
        let expr = parse_expression_snippet(PathBuf::from("test.prime"), "in[int32](\"Age: \")")
            .expect("parse in call");
        if let Expr::Call { .. } = expr {
        } else {
            panic!("expected call expression for in[int32], got {:?}", expr);
        }
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
    fn module_imports_prelude_implicitly() {
        let core_types = r#"
library core::types;

export prelude { Option, Result };

pub enum Option[T] { Some(T), None }
pub enum Result[T, E] { Ok(T), Err(E) }
"#;
        let consumer = r#"
module app::main;
import core::types;

fn demo(flag: bool) -> Result[int32, string] {
  if flag {
    Result::Ok(1)
  } else {
    Result::Err("nope")
  }
}
"#;
        let modules = vec![
            parse_module("core::types", PathBuf::from("types.prime"), core_types).unwrap(),
            parse_module("app::main", PathBuf::from("main.prime"), consumer).unwrap(),
        ];
        let program = Program { modules };
        let expanded = ExpandedProgram {
            program,
            traces: ExpansionTraces::default(),
            item_origins: std::collections::HashMap::new(),
        };
        if let Err(errs) = check_program(&expanded) {
            panic!("prelude import should typecheck, got {:?}", errs);
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

    #[test]
    fn checked_matches_distinguish_expansions_at_the_same_source_span() {
        let source = r#"
module test;
enum A[T] { Item(T), Empty }
enum B[T] { Empty, Item(T) }
macro inspect(value: tokens) -> int32 { match value { Item(_) if true => 1, _ => 0 } }
fn a(x: A[string]) -> int32 { ~inspect(x) }
fn b(x: B[bool]) -> int32 { ~inspect(x) }
"#;
        let path = PathBuf::from("test.prime");
        let module = parse_module("test", path.clone(), source).unwrap();
        let expanded = macro_expander::expand_program(&Program {
            modules: vec![module],
        })
        .unwrap();
        let checked = super::check_program_typed(&expanded, &Default::default()).unwrap();
        let matches = &checked.facts().matches;
        assert_eq!(matches.len(), 2);
        let mut spans = Vec::new();
        for (owner, tag, field) in [("A", 0, "string"), ("B", 1, "bool")] {
            let m = matches
                .values()
                .find(|m| m.scrutinee.ty.canonical_name() == format!("{owner}[{field}]"))
                .unwrap();
            assert_eq!(m.path, path);
            spans.push(m.span);
            let first = &m.arms[0];
            let variant = first.variant.as_ref().unwrap();
            assert_eq!(variant.enum_name, owner);
            assert_eq!(variant.name, "Item");
            assert_eq!(variant.tag, tag);
            assert_eq!(variant.fields[0].ty.canonical_name(), field);
            let guard = first.guard.as_ref().unwrap();
            assert_eq!(&source[guard.span.start..guard.span.end], "true");
            assert_eq!(guard.ty, super::bool_type());
            assert_eq!(first.value.as_ref().unwrap().ty.canonical_name(), "int32");
            assert!(m.arms[1].guard.is_none());
        }
        assert_eq!(spans[0], spans[1]);
    }

    #[test]
    fn rejects_a_non_boolean_guard_at_its_use() {
        let source = "module test; enum E { N(int32) } fn f(e: E) -> int32 { match e { N(n) if n => 1, _ => 0 } }";
        let errors = typecheck_source(source).unwrap_err();
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert_eq!(errors[0].message, "expected `bool`, found `int32`");
        assert_eq!(errors[0].span.start, source.find("if n").unwrap() + 3);
    }

    #[test]
    fn enum_match_requires_unguarded_coverage() {
        let source = "enum Shade { Red, Blue, Green } fn value(s: Shade) -> int32 {
            match s { Shade::Red => 1, _ if false => 2 }
        }";
        let errors = typecheck_source(source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.ends_with("missing variants: Blue, Green")),
            "{errors:?}"
        );
        assert!(typecheck_source(&source.replace("_ if false", "remaining")).is_ok());
    }

    #[test]
    fn enum_match_checks_payload_values_and_owner() {
        let source = "enum Packet { Data(int32), Empty }
            fn value(p: Packet) -> int32 {
                match p { Packet::Data(1) => 1, Packet::Empty => 0 }
            }";
        let errors = typecheck_source(source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.ends_with("missing variants: Data")),
            "{errors:?}"
        );
        assert!(typecheck_source(&source.replace("Data(1)", "Data(value)")).is_ok());

        let source = "enum A { Item } enum B { Item }
            fn value(a: A) -> int32 { match a { B::Item => 0 } }";
        let errors = typecheck_source(source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("does not match value of type `A`")),
            "{errors:?}"
        );
        assert!(
            errors
                .iter()
                .any(|e| e.message.ends_with("missing variants: Item")),
            "{errors:?}"
        );
        assert!(typecheck_source(&source.replace("B::Item", "Item")).is_ok());
        assert!(typecheck_source(&source.replace("B::Item => 0", "B::Item => 0, _ => 1")).is_err());
    }

    #[test]
    fn enum_match_combines_payload_patterns_without_losing_correlations() {
        let source = "enum Option[T] { Some(T), None } enum Pair { Bits(bool, Option[bool]) }
            fn value(p: Pair) -> int32 {
                match p {
                    Bits(true, _) => 1,
                    Bits(false, None) => 2,
                    Bits(false, Some(true)) => 3,
                    Bits(false, Some(false)) => 4,
                }
            }";
        assert!(typecheck_source(source).is_ok());
        for partial in [
            source.replace("Bits(false, Some(false))", "Bits(true, Some(false))"),
            source.replace(
                "Bits(false, Some(false))",
                "Bits(false, Some(false)) if false",
            ),
        ] {
            let errors = typecheck_source(&partial).unwrap_err();
            assert!(
                errors
                    .iter()
                    .any(|e| e.message.ends_with("missing variants: Bits")),
                "{errors:?}"
            );
        }
    }

    #[test]
    fn enum_match_accepts_struct_tuple_and_slice_partitions() {
        let source = "struct Flags { ready: bool; } enum Payload { Value((Flags, []int32)) }
            fn value(p: Payload) -> int32 {
                match p {
                    Value((Flags { ready: true }, _)) => 1,
                    Value((Flags { ready: false }, [])) => 2,
                    Value((Flags { ready: false }, [_, ..tail])) => 3,
                }
            }";
        let result = typecheck_source(source);
        assert!(result.is_ok(), "{result:?}");
        let repeated = source.replace("ready: true", "ready: true, ready: true");
        assert!(typecheck_source(&repeated).is_ok());
        assert!(
            typecheck_source(&repeated.replace("true, ready: true", "true, ready: false")).is_err()
        );
        let errors = typecheck_source(&source.replace("[_, ..tail]", "[1, ..tail]")).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.ends_with("missing variants: Value")),
            "{errors:?}"
        );
    }

    #[test]
    fn enum_match_resolves_variant_owners_in_generic_struct_fields() {
        let source = "enum A { Item } enum B { Item }
            struct Holder[T] { tag: T; } enum Outer { Wrap(Holder[A]) }
            fn value(p: Outer) -> int32 {
                match p { Wrap(Holder { tag: A::Item }) => 1 }
            }";
        let result = typecheck_source(source);
        assert!(result.is_ok(), "{result:?}");
        let errors = typecheck_source(&source.replace("tag: A::Item", "tag: B::Item")).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("does not match value of type `A`")),
            "{errors:?}"
        );
    }

    #[test]
    fn integer_literal_enum_payloads() {
        let source = "enum V { Byte(uint8), Signed(int8), Flag(bool) }
            fn main() {
                let value = V::Byte(255);
                let V low = Signed(-128);
                let V high = Signed(127);
                let V bit = Flag(true);
                match value { Byte(255) => out(true), _ => out(false) }
            }";
        let result = typecheck_source(source);
        assert!(result.is_ok(), "{result:?}");
        for (from, to, message) in [
            (
                "V::Byte(255)",
                "V::Byte(256)",
                "integer literal `256` is out of range for `uint8`",
            ),
            (
                "Signed(-128)",
                "Signed(-129)",
                "integer literal `-129` is out of range for `int8`",
            ),
            (
                "Signed(127)",
                "Signed(128)",
                "integer literal `128` is out of range for `int8`",
            ),
            (
                "V::Byte(255)",
                "V::Byte(-1)",
                "unary `-` requires a signed or floating-point type",
            ),
            ("Flag(true)", "Flag(1)", "expected `bool`, found `int32`"),
            (
                "V::Byte(255)",
                "V::Byte(true)",
                "expected `uint8`, found `bool`",
            ),
            (
                "Byte(255) =>",
                "Byte(256) =>",
                "integer literal `256` is out of range for `uint8`",
            ),
        ] {
            let errors = typecheck_source(&source.replace(from, to)).expect_err(to);
            assert!(
                errors.iter().any(|e| e.message == message),
                "{to}: {errors:?}"
            );
        }
    }

    #[test]
    fn integer_literal_context_and_explicit_conversions() {
        let source = "enum Wrap[T] { Item(T) }
            enum ByteValue { Byte(uint8) }
            const PACKET = ByteValue::Byte(255);
            const COMPUTED = ByteValue::Byte(254 + 1);
            const COUNT = 1;
            const NEXT = COUNT + 1;
            fn number(flag: bool) -> int8 { if flag { -128 } else { 127 } }
            fn take(x: uint8) {}
            fn main() {
                let value = -2147483648;
                let uint64 wide = 18446744073709551615;
                let int64 minimum = -9223372036854775808;
                let Wrap[uint8] wrapped = Wrap::Item(255);
                let Wrap[int8] unqualified = Item(-128);
                let from_bound_owner = wrapped::Item(254);
                let uint8 addition = 250 + 5;
                let uint8 converted = cast[uint8](256);
                let uint64 large_cast = cast[uint64](18446744073709551615);
                let int8 negative_cast = cast[int8](-2147483649);
                let float64 floating = 2147483648;
                take(255);
            }";
        let result = typecheck_source(source);
        assert!(result.is_ok(), "{result:?}");
        for (from, to, dest) in [
            ("{ -128 }", "{ -129 }", "int8"),
            ("{ 127 }", "{ 128 }", "int8"),
            ("= -2147483648;", "= -2147483649;", "int32"),
            ("= -2147483648;", "= 2147483648;", "int32"),
            (
                "wide = 18446744073709551615",
                "wide = 18446744073709551616",
                "uint64",
            ),
            (
                "minimum = -9223372036854775808",
                "minimum = -9223372036854775809",
                "int64",
            ),
            ("Wrap::Item(255)", "Wrap::Item(256)", "uint8"),
            ("wrapped::Item(254)", "wrapped::Item(256)", "uint8"),
            ("Item(-128)", "Item(-129)", "int8"),
            ("ByteValue::Byte(255)", "ByteValue::Byte(256)", "uint8"),
            ("254 + 1", "256 + 1", "uint8"),
            ("COUNT = 1", "COUNT = 2147483648", "int32"),
            ("take(255)", "take(256)", "uint8"),
        ] {
            let errors = typecheck_source(&source.replace(from, to)).expect_err(to);
            assert!(
                errors
                    .iter()
                    .any(|e| e.message.ends_with(&format!("out of range for `{dest}`"))),
                "{to}: {errors:?}"
            );
        }
        assert!(
            typecheck_source(&source.replace("cast[uint8](256)", "cast[uint8](true)")).is_err()
        );
    }

    #[test]
    fn integer_literal_macro_error_points_at_the_call() {
        let source = "macro bad_byte() -> uint8 { 256 } fn main() { let uint8 b = ~bad_byte(); }";
        let module = parse_module("literal_macro", PathBuf::from("macro.prime"), source).unwrap();
        let expanded = macro_expander::expand_program(&Program {
            modules: vec![module],
        })
        .unwrap();
        let errors = check_program(&expanded).unwrap_err();
        let error = errors
            .iter()
            .find(|e| e.message.contains("out of range for `uint8`"))
            .unwrap();
        assert_eq!(&source[error.span.start..error.span.end], "~bad_byte()");
        assert!(
            error
                .help
                .as_deref()
                .unwrap_or_default()
                .contains("bad_byte")
        );
    }

    #[test]
    fn integer_literal_pointer_width_uses_the_build_target() {
        use crate::target::{BuildTarget, ESP32C3_TRIPLE};

        for (target, bits) in [
            (BuildTarget::Host, usize::BITS),
            (BuildTarget::Triple(ESP32C3_TRIPLE.into()), 32),
        ] {
            let options = super::TypecheckOptions { target };
            for (ty, min, max) in [
                ("usize", 0_i128, (1_i128 << bits) - 1),
                ("isize", -(1_i128 << (bits - 1)), (1_i128 << (bits - 1)) - 1),
            ] {
                for (value, fits) in [(min - 1, false), (min, true), (max, true), (max + 1, false)]
                {
                    let source = format!("fn value() -> {ty} {{ {value} }}");
                    let result = typecheck_source_with_options(&source, &options);
                    assert_eq!(
                        result.is_ok(),
                        fits,
                        "{bits}-bit target, {source}: {result:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn enum_match_does_not_assume_integer_payloads_are_narrowed() {
        let arms = (0..=255)
            .map(|v| format!("Byte({v}) => 0"))
            .collect::<Vec<_>>()
            .join(", ");
        let source =
            format!("enum V {{ Byte(uint8) }} fn value(v: V) -> int32 {{ match v {{ {arms} }} }}");
        let errors = typecheck_source(&source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.ends_with("missing variants: Byte")),
            "{errors:?}"
        );
        assert!(typecheck_source(&source.replace("Byte(255) => 0", "Byte(_) => 0")).is_ok());
    }

    #[test]
    fn closure_type_annotation_allows_assignment() {
        let src = r#"
module tests::closures;

fn main() {
  let add = |x: int32| x + 1;
  let y = add(2);
}
"#;
        assert!(typecheck_source(src).is_ok());
    }

    #[test]
    fn closure_capture_moves_binding() {
        let src = r#"
        fn main() {
            let x: int32 = 5;
            let f = |y: int32| x + y;
            let _ = f(1);
            let _ = x;
        }
        "#;
        assert!(typecheck_source(src).is_err());
    }
}
