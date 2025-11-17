use crate::language::{
    ast::*,
    types::{Mutability, TypeAnnotation, TypeExpr},
};
use crate::project::Package;
use crate::runtime::{
    environment::Environment,
    error::{RuntimeError, RuntimeResult},
    value::{
        BoxValue, EnumValue, MapValue, RangeValue, ReferenceValue, SliceValue, StructInstance,
        Value,
    },
};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

pub struct Interpreter {
    package: Package,
    env: Environment,
    structs: HashMap<String, StructEntry>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    functions: HashMap<FunctionKey, FunctionInfo>,
    interfaces: HashMap<String, InterfaceEntry>,
    impls: HashSet<ImplKey>,
    consts: Vec<(String, ConstDef)>,
    deprecated_warnings: HashSet<String>,
    module_stack: Vec<String>,
}

#[derive(Clone)]
struct FunctionInfo {
    module: String,
    def: FunctionDef,
    receiver: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionKey {
    name: String,
    receiver: Option<String>,
    type_args: Option<Vec<String>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ImplKey {
    interface: String,
    type_args: Vec<String>,
    target: String,
}

#[derive(Clone)]
struct StructEntry {
    module: String,
    def: StructDef,
}

#[derive(Clone)]
struct InterfaceEntry {
    module: String,
    def: InterfaceDef,
}

#[derive(Clone)]
struct EnumVariantInfo {
    enum_name: String,
    fields: Vec<TypeAnnotation>,
    module: String,
    visibility: Visibility,
}

enum FlowSignal {
    Break,
    Continue,
    Return(Vec<Value>),
}

enum BlockEval {
    Value(Value),
    Flow(FlowSignal),
}

impl Interpreter {
    pub fn new(package: Package) -> Self {
        Self {
            package,
            env: Environment::new(),
            structs: HashMap::new(),
            enum_variants: HashMap::new(),
            functions: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashSet::new(),
            consts: Vec::new(),
            deprecated_warnings: HashSet::new(),
            module_stack: Vec::new(),
        }
    }

    pub fn run(&mut self) -> RuntimeResult<()> {
        self.bootstrap()?;
        let _ = self.call_function("main", None, &[], Vec::new())?;
        Ok(())
    }

    fn bootstrap(&mut self) -> RuntimeResult<()> {
        for module in self.package.program.modules.clone() {
            for item in &module.items {
                match item {
                    Item::Struct(def) => {
                        if self.structs.contains_key(&def.name) {
                            return Err(RuntimeError::Panic {
                                message: format!("Duplicate struct `{}`", def.name),
                            });
                        }
                        self.structs.insert(
                            def.name.clone(),
                            StructEntry {
                                module: module.name.clone(),
                                def: def.clone(),
                            },
                        );
                    }
                    Item::Enum(def) => {
                        if self
                            .enum_variants
                            .values()
                            .any(|info| info.enum_name == def.name)
                        {
                            return Err(RuntimeError::Panic {
                                message: format!("Duplicate enum `{}`", def.name),
                            });
                        }
                        for variant in &def.variants {
                            self.enum_variants.insert(
                                variant.name.clone(),
                                EnumVariantInfo {
                                    enum_name: def.name.clone(),
                                    fields: variant.fields.clone(),
                                    module: module.name.clone(),
                                    visibility: def.visibility,
                                },
                            );
                        }
                    }
                    Item::Interface(def) => {
                        self.register_interface(&module.name, def.clone())?;
                    }
                    Item::Impl(block) => {
                        self.register_impl(&module.name, block.clone())?;
                    }
                    Item::Function(def) => {
                        self.register_function(&module.name, def.clone())?;
                    }
                    Item::Const(const_def) => {
                        self.consts.push((module.name.clone(), const_def.clone()));
                    }
                }
            }
        }

        for (module_name, const_def) in self.consts.clone() {
            self.module_stack.push(module_name.clone());
            let value = {
                let result = self.eval_expression(&const_def.value);
                self.module_stack.pop();
                result?
            };
            self.env
                .declare(&const_def.name, value, false)
                .map_err(|err| err)?;
        }
        Ok(())
    }

    fn register_function(&mut self, module: &str, def: FunctionDef) -> RuntimeResult<()> {
        let receiver = receiver_type_name(&def, &self.structs);
        let base_key = FunctionKey {
            name: def.name.clone(),
            receiver: receiver.clone(),
            type_args: None,
        };
        if self.functions.contains_key(&base_key) {
            return Err(RuntimeError::Panic {
                message: format!(
                    "Duplicate function `{}` for receiver `{:?}`",
                    def.name, receiver
                ),
            });
        }
        let info = FunctionInfo {
            module: module.to_string(),
            receiver: receiver.clone(),
            def: def.clone(),
        };
        self.functions.insert(base_key, info);

        let qualified = format!("{}::{}", module, def.name);
        let qualified_key = FunctionKey {
            name: qualified,
            receiver: receiver_type_name(&def, &self.structs),
            type_args: None,
        };
        self.functions.insert(
            qualified_key,
            FunctionInfo {
                module: module.to_string(),
                receiver,
                def,
            },
        );
        Ok(())
    }

    fn register_interface(&mut self, module: &str, def: InterfaceDef) -> RuntimeResult<()> {
        if self.interfaces.contains_key(&def.name) {
            return Err(RuntimeError::Panic {
                message: format!("Duplicate interface `{}`", def.name),
            });
        }
        self.interfaces.insert(
            def.name.clone(),
            InterfaceEntry {
                module: module.to_string(),
                def,
            },
        );
        Ok(())
    }

    fn register_impl(&mut self, module: &str, block: ImplBlock) -> RuntimeResult<()> {
        if !self.interfaces.contains_key(&block.interface) {
            return Err(RuntimeError::Panic {
                message: format!("Unknown interface `{}`", block.interface),
            });
        }
        if !self.structs.contains_key(&block.target) {
            return Err(RuntimeError::Panic {
                message: format!("Unknown target type `{}`", block.target),
            });
        }
        let iface_entry = self.interfaces.get(&block.interface).cloned().unwrap();
        let iface = iface_entry.def;
        if iface.type_params.len() != block.type_args.len() {
            return Err(RuntimeError::Panic {
                message: format!(
                    "`{}` expects {} type arguments, got {}",
                    block.interface,
                    iface.type_params.len(),
                    block.type_args.len()
                ),
            });
        }
        let type_arg_names: Vec<String> = block
            .type_args
            .iter()
            .map(|ty| ty.canonical_name())
            .collect();
        let key = ImplKey {
            interface: block.interface.clone(),
            type_args: type_arg_names.clone(),
            target: block.target.clone(),
        };
        if self.impls.contains(&key) {
            return Err(RuntimeError::Panic {
                message: format!(
                    "`{}` already implemented for `{}`",
                    block.interface, block.target
                ),
            });
        }
        let mut provided: HashSet<String> = HashSet::new();
        for method in &block.methods {
            let mut method_def = method.clone();
            substitute_self_in_function(&mut method_def, &block.target);
            provided.insert(method_def.name.clone());
            if let Some(first) = method_def.params.first() {
                if let Some(name) = type_name_from_annotation(&first.ty) {
                    if name != block.target {
                        return Err(RuntimeError::Panic {
                            message: format!(
                                "First parameter of `{}` must be `{}`",
                                method_def.name, block.target
                            ),
                        });
                    }
                }
            }
            self.register_function(module, method_def)?;
        }
        for sig in iface.methods {
            if !provided.contains(&sig.name) {
                return Err(RuntimeError::Panic {
                    message: format!(
                        "`{}` missing method `{}` required by interface `{}`",
                        block.target, sig.name, block.interface
                    ),
                });
            }
        }
        self.impls.insert(key);
        Ok(())
    }

    fn call_function(
        &mut self,
        name: &str,
        receiver: Option<String>,
        type_args: &[TypeExpr],
        args: Vec<Value>,
    ) -> RuntimeResult<Vec<Value>> {
        if name == "out" {
            if !type_args.is_empty() {
                return Err(RuntimeError::Unsupported {
                    message: "`out` does not accept type arguments".into(),
                });
            }
            return self.call_out(args);
        }
        if !type_args.is_empty() && self.is_builtin_name(name) {
            return Err(RuntimeError::Unsupported {
                message: format!("`{}` does not accept type arguments", name),
            });
        }
        if let Some(result) = self.call_builtin(name, args.clone()) {
            return result;
        }
        let info = self.resolve_function(name, receiver, type_args, &args)?;
        if info.def.params.len() != args.len() {
            return Err(RuntimeError::ArityMismatch {
                name: name.to_string(),
                expected: info.def.params.len(),
                received: args.len(),
            });
        }

        self.ensure_item_visible(
            &info.module,
            info.def.visibility,
            &info.def.name,
            "function",
        )?;
        self.ensure_interface_arguments(&info.def.params, &args)?;
        self.module_stack.push(info.module.clone());
        let result = (|| {
            self.env.push_scope();
            for (param, value) in info.def.params.iter().zip(args.into_iter()) {
                self.env
                    .declare(&param.name, value, param.mutability == Mutability::Mutable)?;
            }

            let result = match &info.def.body {
                FunctionBody::Block(block) => self.eval_block(block)?,
                FunctionBody::Expr(expr) => BlockEval::Value(self.eval_expression(&expr.node)?),
            };

            let values = match result {
                BlockEval::Value(value) => {
                    if info.def.returns.len() <= 1 {
                        if info.def.returns.is_empty() {
                            Vec::new()
                        } else {
                            vec![value]
                        }
                    } else if let Value::Tuple(values) = value {
                        values
                    } else {
                        vec![value]
                    }
                }
                BlockEval::Flow(FlowSignal::Return(values)) => values,
                BlockEval::Flow(FlowSignal::Break) => {
                    return Err(RuntimeError::Panic {
                        message: "break outside loop".into(),
                    });
                }
                BlockEval::Flow(FlowSignal::Continue) => {
                    return Err(RuntimeError::Panic {
                        message: "continue outside loop".into(),
                    });
                }
            };

            self.execute_deferred()?;
            self.env.pop_scope();
            Ok(values)
        })();
        self.module_stack.pop();
        result
    }

    fn resolve_function(
        &mut self,
        name: &str,
        receiver: Option<String>,
        type_args: &[TypeExpr],
        args: &[Value],
    ) -> RuntimeResult<FunctionInfo> {
        let type_arg_names = if type_args.is_empty() {
            None
        } else {
            Some(type_args.iter().map(|ty| ty.canonical_name()).collect())
        };
        let resolved_receiver = receiver.clone().or_else(|| self.receiver_from_args(args));
        if let Some(info) = self
            .functions
            .get(&FunctionKey {
                name: name.to_string(),
                receiver: resolved_receiver.clone(),
                type_args: type_arg_names.clone(),
            })
            .cloned()
        {
            return Ok(info);
        }
        if let Some(info) = self
            .functions
            .get(&FunctionKey {
                name: name.to_string(),
                receiver: None,
                type_args: type_arg_names.clone(),
            })
            .cloned()
        {
            return Ok(info);
        }

        let base = self
            .functions
            .get(&FunctionKey {
                name: name.to_string(),
                receiver: resolved_receiver.clone(),
                type_args: None,
            })
            .cloned()
            .or_else(|| {
                self.functions
                    .get(&FunctionKey {
                        name: name.to_string(),
                        receiver: None,
                        type_args: None,
                    })
                    .cloned()
            })
            .ok_or_else(|| RuntimeError::UnknownSymbol {
                name: name.to_string(),
            })?;

        if base.def.type_params.is_empty() {
            if type_arg_names.is_some() {
                return Err(RuntimeError::Unsupported {
                    message: format!("`{}` is not generic", name),
                });
            }
            return Ok(base);
        }

        if type_args.is_empty() {
            return Err(RuntimeError::Unsupported {
                message: format!("`{}` requires type arguments", name),
            });
        }
        if type_args.len() != base.def.type_params.len() {
            return Err(RuntimeError::Unsupported {
                message: format!(
                    "`{}` expects {} type arguments, got {}",
                    name,
                    base.def.type_params.len(),
                    type_args.len()
                ),
            });
        }

        let instantiated = self.instantiate_function(&base, type_args)?;
        let key = FunctionKey {
            name: name.to_string(),
            receiver: resolved_receiver,
            type_args: type_arg_names,
        };
        self.functions.insert(key.clone(), instantiated.clone());
        Ok(instantiated)
    }

    fn instantiate_function(
        &self,
        info: &FunctionInfo,
        type_args: &[TypeExpr],
    ) -> RuntimeResult<FunctionInfo> {
        if type_args.len() != info.def.type_params.len() {
            return Err(RuntimeError::Unsupported {
                message: format!(
                    "`{}` expects {} type arguments, got {}",
                    info.def.name,
                    info.def.type_params.len(),
                    type_args.len()
                ),
            });
        }
        let mut map = HashMap::new();
        for (param, ty) in info.def.type_params.iter().zip(type_args.iter()) {
            map.insert(param.clone(), ty.clone());
        }
        let mut new_def = info.def.clone();
        new_def.type_params.clear();
        for param in &mut new_def.params {
            param.ty = param.ty.substitute(&map);
        }
        for ret in &mut new_def.returns {
            *ret = ret.substitute(&map);
        }
        Ok(FunctionInfo {
            module: info.module.clone(),
            receiver: info.receiver.clone(),
            def: new_def,
        })
    }

    fn ensure_interface_arguments(
        &self,
        params: &[FunctionParam],
        args: &[Value],
    ) -> RuntimeResult<()> {
        for (param, value) in params.iter().zip(args.iter()) {
            if let Some((interface, type_args)) = self.interface_name_from_type(&param.ty.ty) {
                self.ensure_interface_compat(&interface, &type_args, value)?;
            }
        }
        Ok(())
    }

    fn interface_name_from_type(&self, ty: &TypeExpr) -> Option<(String, Vec<TypeExpr>)> {
        match ty {
            TypeExpr::Named(name, args) => self
                .interfaces
                .contains_key(name)
                .then(|| (name.clone(), args.clone())),
            TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
                self.interface_name_from_type(ty)
            }
            _ => None,
        }
    }

    fn ensure_interface_compat(
        &self,
        interface: &str,
        type_args: &[TypeExpr],
        value: &Value,
    ) -> RuntimeResult<()> {
        let entry = self
            .interfaces
            .get(interface)
            .ok_or_else(|| RuntimeError::UnknownSymbol {
                name: interface.to_string(),
            })?;
        self.ensure_item_visible(&entry.module, entry.def.visibility, interface, "interface")?;
        let struct_name =
            self.value_struct_name(value)
                .ok_or_else(|| RuntimeError::TypeMismatch {
                    message: format!(
                        "Interface `{}` expects struct implementing it, found incompatible value",
                        interface
                    ),
                })?;
        let key = ImplKey {
            interface: interface.to_string(),
            type_args: type_args.iter().map(|ty| ty.canonical_name()).collect(),
            target: struct_name.clone(),
        };
        if self.impls.contains(&key) {
            Ok(())
        } else {
            Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`{}` does not implement interface `{}` with these type arguments",
                    struct_name, interface
                ),
            })
        }
    }

    fn current_module(&self) -> Option<&str> {
        self.module_stack.last().map(|s| s.as_str())
    }

    fn can_access(&self, owner: &str, visibility: Visibility) -> bool {
        matches!(visibility, Visibility::Public)
            || self
                .current_module()
                .map_or(true, |current| current == owner)
    }

    fn ensure_item_visible(
        &self,
        owner: &str,
        visibility: Visibility,
        name: &str,
        kind: &str,
    ) -> RuntimeResult<()> {
        if self.can_access(owner, visibility) {
            Ok(())
        } else {
            Err(RuntimeError::Panic {
                message: format!("{kind} `{}` is private to module `{}`", name, owner),
            })
        }
    }

    fn receiver_from_args(&self, args: &[Value]) -> Option<String> {
        args.first().and_then(|value| self.value_struct_name(value))
    }

    fn call_out(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        if args.len() != 1 {
            return Err(RuntimeError::ArityMismatch {
                name: "out".into(),
                expected: 1,
                received: args.len(),
            });
        }
        println!("{}", args[0]);
        Ok(Vec::new())
    }

    fn warn_deprecated(&mut self, name: &str) {
        if self.deprecated_warnings.insert(name.to_string()) {
            eprintln!(
                "warning: `{}` is deprecated; prefer slice/map literals or direct methods",
                name
            );
        }
    }

    fn call_builtin(&mut self, name: &str, args: Vec<Value>) -> Option<RuntimeResult<Vec<Value>>> {
        let result = match name {
            "box_new" => self.builtin_box_new(args),
            "box_get" => self.builtin_box_get(args),
            "box_set" => self.builtin_box_set(args),
            "box_take" => self.builtin_box_take(args),
            "slice_new" => self.builtin_slice_new(args),
            "slice_push" => self.builtin_slice_push(args),
            "slice_len" => self.builtin_slice_len(args),
            "slice_get" => self.builtin_slice_get(args),
            "map_new" => self.builtin_map_new(args),
            "map_insert" => self.builtin_map_insert(args),
            "map_get" => self.builtin_map_get(args),
            _ => return None,
        };
        Some(result)
    }

    fn is_builtin_name(&self, name: &str) -> bool {
        matches!(
            name,
            "box_new"
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

    fn expect_arity(&self, name: &str, args: &[Value], expected: usize) -> RuntimeResult<()> {
        if args.len() != expected {
            Err(RuntimeError::ArityMismatch {
                name: name.into(),
                expected,
                received: args.len(),
            })
        } else {
            Ok(())
        }
    }

    fn builtin_box_new(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("box_new", &args, 1)?;
        let value = args.remove(0);
        Ok(vec![Value::Boxed(BoxValue::new(value))])
    }

    fn expect_box(&self, name: &str, value: Value) -> RuntimeResult<BoxValue> {
        match value {
            Value::Boxed(b) => Ok(b),
            Value::Reference(reference) => {
                let cloned = reference.cell.borrow().clone();
                self.expect_box(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects Box value"),
            }),
        }
    }

    fn builtin_box_get(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("box_get", &args, 1)?;
        let boxed = self.expect_box("box_get", args.remove(0))?;
        Ok(vec![boxed.cell.borrow().clone()])
    }

    fn builtin_box_set(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("box_set", &args, 2)?;
        let boxed = self.expect_box("box_set", args.remove(0))?;
        let value = args.remove(0);
        boxed.replace(value);
        Ok(Vec::new())
    }

    fn builtin_box_take(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("box_take", &args, 1)?;
        let boxed = self.expect_box("box_take", args.remove(0))?;
        Ok(vec![boxed.take()])
    }

    fn builtin_slice_new(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("slice_new");
        self.expect_arity("slice_new", &args, 0)?;
        Ok(vec![Value::Slice(SliceValue::new())])
    }

    fn expect_slice(&self, name: &str, value: Value) -> RuntimeResult<SliceValue> {
        match value {
            Value::Slice(slice) => Ok(slice),
            Value::Reference(reference) => {
                let cloned = reference.cell.borrow().clone();
                self.expect_slice(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects slice value"),
            }),
        }
    }

    fn builtin_slice_push(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("slice_push");
        if args.len() != 2 {
            return Err(RuntimeError::ArityMismatch {
                name: "slice_push".into(),
                expected: 2,
                received: args.len(),
            });
        }
        let slice = self.expect_slice("slice_push", args.remove(0))?;
        let value = args.remove(0);
        slice.push(value);
        Ok(Vec::new())
    }

    fn builtin_slice_len(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("slice_len");
        self.expect_arity("slice_len", &args, 1)?;
        let slice = self.expect_slice("slice_len", args.remove(0))?;
        let len = slice.len() as i128;
        Ok(vec![Value::Int(len)])
    }

    fn builtin_slice_get(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("slice_get");
        self.expect_arity("slice_get", &args, 2)?;
        let slice = self.expect_slice("slice_get", args.remove(0))?;
        let index = match args.remove(0) {
            Value::Int(i) => i,
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "slice_get expects integer index".into(),
                });
            }
        };
        if index < 0 {
            let none = self.instantiate_enum("Option", "None", Vec::new())?;
            return Ok(vec![none]);
        }
        let value = slice.get(index as usize);
        if value.is_none() {
            let none = self.instantiate_enum("Option", "None", Vec::new())?;
            return Ok(vec![none]);
        }
        let value = value.unwrap();
        let some = self.instantiate_enum("Option", "Some", vec![value])?;
        Ok(vec![some])
    }

    fn builtin_map_new(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("map_new");
        self.expect_arity("map_new", &args, 0)?;
        Ok(vec![Value::Map(MapValue::new())])
    }

    fn expect_map(&self, name: &str, value: Value) -> RuntimeResult<MapValue> {
        match value {
            Value::Map(map) => Ok(map),
            Value::Reference(reference) => {
                let cloned = reference.cell.borrow().clone();
                self.expect_map(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects map value"),
            }),
        }
    }

    fn expect_string(&self, name: &str, value: Value) -> RuntimeResult<String> {
        if let Value::String(s) = value {
            Ok(s)
        } else {
            Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects string key"),
            })
        }
    }

    fn builtin_map_insert(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("map_insert");
        if args.len() != 3 {
            return Err(RuntimeError::ArityMismatch {
                name: "map_insert".into(),
                expected: 3,
                received: args.len(),
            });
        }
        let map = self.expect_map("map_insert", args.remove(0))?;
        let key = self.expect_string("map_insert", args.remove(0))?;
        let value = args.remove(0);
        map.insert(key, value);
        Ok(Vec::new())
    }

    fn builtin_map_get(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.warn_deprecated("map_get");
        self.expect_arity("map_get", &args, 2)?;
        let map = self.expect_map("map_get", args.remove(0))?;
        let key = self.expect_string("map_get", args.remove(0))?;
        if let Some(value) = map.get(&key) {
            let some = self.instantiate_enum("Option", "Some", vec![value.clone()])?;
            Ok(vec![some])
        } else {
            let none = self.instantiate_enum("Option", "None", Vec::new())?;
            Ok(vec![none])
        }
    }

    fn eval_statement(&mut self, statement: &Statement) -> RuntimeResult<Option<FlowSignal>> {
        match statement {
            Statement::Let(stmt) => {
                let value = match &stmt.value {
                    Some(expr) => self.eval_expression(expr)?,
                    None => Value::Unit,
                };
                self.env
                    .declare(&stmt.name, value, stmt.mutability == Mutability::Mutable)?;
                Ok(None)
            }
            Statement::Assign(stmt) => {
                match &stmt.target {
                    Expr::Identifier(ident) => {
                        let value = self.eval_expression(&stmt.value)?;
                        self.env.assign(&ident.name, value)?;
                    }
                    Expr::Deref { expr, .. } => {
                        let target = self.eval_expression(expr)?;
                        match target {
                            Value::Reference(reference) => {
                                if !reference.mutable {
                                    return Err(RuntimeError::Panic {
                                        message: "Cannot assign through immutable reference".into(),
                                    });
                                }
                                let value = self.eval_expression(&stmt.value)?;
                                *reference.cell.borrow_mut() = value;
                            }
                            _ => {
                                return Err(RuntimeError::TypeMismatch {
                                    message: "Cannot assign through non-reference value".into(),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(RuntimeError::Unsupported {
                            message: "Only identifier or dereference assignments are supported"
                                .into(),
                        });
                    }
                }
                Ok(None)
            }
            Statement::Expr(stmt) => {
                self.eval_expression(&stmt.expr)?;
                Ok(None)
            }
            Statement::Return(stmt) => {
                let mut values = Vec::new();
                for expr in &stmt.values {
                    values.push(self.eval_expression(expr)?);
                }
                Ok(Some(FlowSignal::Return(values)))
            }
            Statement::While(stmt) => {
                loop {
                    let condition = self.eval_expression(&stmt.condition)?;
                    if !condition.as_bool() {
                        break;
                    }
                    let result = self.eval_block(&stmt.body)?;
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                    }
                }
                Ok(None)
            }
            Statement::ForRange(stmt) => {
                let range = self.eval_range_expr(&stmt.range)?;
                let end = if range.inclusive {
                    range.end + 1
                } else {
                    range.end
                };
                for i in range.start..end {
                    self.env.push_scope();
                    self.env.declare(&stmt.binding, Value::Int(i), false)?;
                    let result = self.eval_block(&stmt.body)?;
                    self.execute_deferred()?;
                    self.env.pop_scope();
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                    }
                }
                Ok(None)
            }
            Statement::Break => Ok(Some(FlowSignal::Break)),
            Statement::Continue => Ok(Some(FlowSignal::Continue)),
            Statement::Defer(stmt) => {
                self.env.defer(stmt.expr.clone());
                Ok(None)
            }
            Statement::Block(block) => match self.eval_block(block)? {
                BlockEval::Value(_) => Ok(None),
                BlockEval::Flow(flow) => Ok(Some(flow)),
            },
        }
    }

    fn eval_block(&mut self, block: &Block) -> RuntimeResult<BlockEval> {
        self.env.push_scope();
        for statement in &block.statements {
            if let Some(flow) = self.eval_statement(statement)? {
                self.execute_deferred()?;
                self.env.pop_scope();
                return Ok(BlockEval::Flow(flow));
            }
        }
        let value = if let Some(tail) = &block.tail {
            self.eval_expression(tail)?
        } else {
            Value::Unit
        };
        self.execute_deferred()?;
        self.env.pop_scope();
        Ok(BlockEval::Value(value))
    }

    fn execute_deferred(&mut self) -> RuntimeResult<()> {
        let mut deferred = self.env.drain_deferred();
        while let Some(expr) = deferred.pop() {
            self.eval_expression(&expr)?;
        }
        Ok(())
    }

    fn eval_arguments(&mut self, args: &[Expr]) -> RuntimeResult<Vec<Value>> {
        args.iter().map(|expr| self.eval_expression(expr)).collect()
    }

    fn value_struct_name(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Reference(reference) => self.value_struct_name(&reference.cell.borrow()),
            Value::Boxed(inner) => self.value_struct_name(&inner.cell.borrow()),
            _ => None,
        }
    }

    fn eval_expression(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        match expr {
            Expr::Identifier(ident) => {
                let value =
                    self.env
                        .get(&ident.name)
                        .ok_or_else(|| RuntimeError::UnknownSymbol {
                            name: ident.name.clone(),
                        })?;
                if matches!(value, Value::Moved) {
                    return Err(RuntimeError::MovedValue {
                        name: ident.name.clone(),
                    });
                }
                Ok(value)
            }
            Expr::Literal(lit) => Ok(match lit {
                Literal::Int(value, _) => Value::Int(*value),
                Literal::Float(value, _) => Value::Float(*value),
                Literal::Bool(value, _) => Value::Bool(*value),
                Literal::String(value, _) => Value::String(value.clone()),
                Literal::Rune(value, _) => Value::Int(*value as i128),
            }),
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = self.eval_expression(left)?;
                let rhs = self.eval_expression(right)?;
                self.eval_binary(*op, lhs, rhs)
            }
            Expr::Unary { op, expr, .. } => {
                let value = self.eval_expression(expr)?;
                match op {
                    UnaryOp::Neg => match value {
                        Value::Int(v) => Ok(Value::Int(-v)),
                        Value::Float(v) => Ok(Value::Float(-v)),
                        _ => Err(RuntimeError::TypeMismatch {
                            message: "Unary - expects numeric value".into(),
                        }),
                    },
                    UnaryOp::Not => Ok(Value::Bool(!value.as_bool())),
                }
            }
            Expr::Call {
                callee,
                type_args,
                args,
                ..
            } => match callee.as_ref() {
                Expr::Identifier(ident) => {
                    let arg_values = self.eval_arguments(args)?;
                    if let Some(variant) = self.enum_variants.get(&ident.name) {
                        let enum_name = variant.enum_name.clone();
                        let variant_name = ident.name.clone();
                        return self.instantiate_enum(&enum_name, &variant_name, arg_values);
                    }
                    let results = self.call_function(&ident.name, None, type_args, arg_values)?;
                    Ok(match results.len() {
                        0 => Value::Unit,
                        1 => results.into_iter().next().unwrap(),
                        _ => Value::Tuple(results),
                    })
                }
                Expr::FieldAccess { base, field, .. } => {
                    if let Expr::Identifier(module_ident) = base.as_ref() {
                        let qualified = format!("{}::{}", module_ident.name, field);
                        if self.functions.contains_key(&FunctionKey {
                            name: qualified.clone(),
                            receiver: None,
                            type_args: None,
                        }) {
                            let arg_values = self.eval_arguments(args)?;
                            let results =
                                self.call_function(&qualified, None, type_args, arg_values)?;
                            return Ok(match results.len() {
                                0 => Value::Unit,
                                1 => results.into_iter().next().unwrap(),
                                _ => Value::Tuple(results),
                            });
                        }
                    }
                    let receiver = self.eval_expression(base)?;
                    let receiver_type = self.value_struct_name(&receiver);
                    let mut method_args = Vec::with_capacity(args.len() + 1);
                    method_args.push(receiver);
                    for expr in args {
                        method_args.push(self.eval_expression(expr)?);
                    }
                    let results =
                        self.call_function(field, receiver_type, type_args, method_args)?;
                    Ok(match results.len() {
                        0 => Value::Unit,
                        1 => results.into_iter().next().unwrap(),
                        _ => Value::Tuple(results),
                    })
                }
                _ => Err(RuntimeError::Unsupported {
                    message: "Unsupported call target".into(),
                }),
            },
            Expr::FieldAccess { base, field, .. } => {
                let value = self.eval_expression(base)?;
                match value {
                    Value::Struct(instance) => {
                        instance
                            .get_field(field)
                            .ok_or_else(|| RuntimeError::UnknownSymbol {
                                name: field.clone(),
                            })
                    }
                    _ => Err(RuntimeError::TypeMismatch {
                        message: "Field access requires struct value".into(),
                    }),
                }
            }
            Expr::StructLiteral { name, fields, .. } => self.instantiate_struct(name, fields),
            Expr::Match(expr) => self.eval_match(expr),
            Expr::Block(block) => match self.eval_block(block)? {
                BlockEval::Value(value) => Ok(value),
                BlockEval::Flow(flow) => Err(RuntimeError::Panic {
                    message: format!(
                        "Control flow {} not allowed in expression",
                        flow_name(&flow)
                    ),
                }),
            },
            Expr::If(if_expr) => {
                let condition = self.eval_expression(&if_expr.condition)?;
                if condition.as_bool() {
                    match self.eval_block(&if_expr.then_branch)? {
                        BlockEval::Value(value) => Ok(value),
                        BlockEval::Flow(flow) => Err(RuntimeError::Panic {
                            message: format!(
                                "Control flow {} not allowed in expression",
                                flow_name(&flow)
                            ),
                        }),
                    }
                } else if let Some(else_block) = &if_expr.else_branch {
                    match self.eval_block(else_block)? {
                        BlockEval::Value(value) => Ok(value),
                        BlockEval::Flow(flow) => Err(RuntimeError::Panic {
                            message: format!(
                                "Control flow {} not allowed in expression",
                                flow_name(&flow)
                            ),
                        }),
                    }
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::Tuple(values, _) => {
                let evaluated = values
                    .iter()
                    .map(|expr| self.eval_expression(expr))
                    .collect::<RuntimeResult<Vec<_>>>()?;
                Ok(Value::Tuple(evaluated))
            }
            Expr::Range(range) => {
                let range_value = self.eval_range_expr(range)?;
                Ok(Value::Range(range_value))
            }
            Expr::Reference { mutable, expr, .. } => self.build_reference(expr, *mutable),
            Expr::Deref { expr, .. } => {
                let value = self.eval_expression(expr)?;
                match value {
                    Value::Reference(reference) => Ok(reference.cell.borrow().clone()),
                    _ => Err(RuntimeError::TypeMismatch {
                        message: "Cannot dereference non-reference value".into(),
                    }),
                }
            }
            Expr::ArrayLiteral(values, _) => self.eval_array_literal(values),
            Expr::Move { expr, .. } => self.eval_move_expression(expr),
        }
    }

    fn build_reference(&mut self, expr: &Expr, mutable: bool) -> RuntimeResult<Value> {
        match expr {
            Expr::Identifier(ident) => {
                let (cell, binding_mut) =
                    self.env
                        .get_cell(&ident.name)
                        .ok_or_else(|| RuntimeError::UnknownSymbol {
                            name: ident.name.clone(),
                        })?;
                if mutable && !binding_mut {
                    return Err(RuntimeError::ImmutableBinding {
                        name: ident.name.clone(),
                    });
                }
                if matches!(*cell.borrow(), Value::Moved) {
                    return Err(RuntimeError::MovedValue {
                        name: ident.name.clone(),
                    });
                }
                Ok(Value::Reference(ReferenceValue {
                    cell,
                    mutable,
                    origin: Some(ident.name.clone()),
                }))
            }
            _ => {
                let value = self.eval_expression(expr)?;
                Ok(Value::Reference(ReferenceValue {
                    cell: Rc::new(RefCell::new(value)),
                    mutable,
                    origin: None,
                }))
            }
        }
    }

    fn eval_array_literal(&mut self, values: &[Expr]) -> RuntimeResult<Value> {
        let mut items = Vec::with_capacity(values.len());
        for expr in values {
            items.push(self.eval_expression(expr)?);
        }
        Ok(Value::Slice(SliceValue::from_vec(items)))
    }

    fn eval_move_expression(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        match expr {
            Expr::Identifier(ident) => {
                let (cell, _) =
                    self.env
                        .get_cell(&ident.name)
                        .ok_or_else(|| RuntimeError::UnknownSymbol {
                            name: ident.name.clone(),
                        })?;
                if self.env.is_mut_borrowed(&ident.name) {
                    return Err(RuntimeError::Panic {
                        message: format!("Cannot move `{}` while it is borrowed", ident.name),
                    });
                }
                let mut slot = cell.borrow_mut();
                if matches!(*slot, Value::Moved) {
                    return Err(RuntimeError::MovedValue {
                        name: ident.name.clone(),
                    });
                }
                if !Self::is_heap_value(&slot) {
                    return Err(RuntimeError::Panic {
                        message: format!(
                            "`{}` cannot be moved; only boxes, slices, and maps support move semantics",
                            ident.name
                        ),
                    });
                }
                let moved = std::mem::replace(&mut *slot, Value::Moved);
                self.env.register_move(&ident.name);
                Ok(moved)
            }
            _ => Err(RuntimeError::Unsupported {
                message: "move expressions require identifiers".into(),
            }),
        }
    }

    fn is_heap_value(value: &Value) -> bool {
        matches!(value, Value::Boxed(_) | Value::Slice(_) | Value::Map(_))
    }

    fn eval_match(&mut self, expr: &MatchExpr) -> RuntimeResult<Value> {
        let target = self.eval_expression(&expr.expr)?;
        for arm in &expr.arms {
            self.env.push_scope();
            if self.match_pattern(&arm.pattern, &target)? {
                let value = self.eval_expression(&arm.value)?;
                self.execute_deferred()?;
                self.env.pop_scope();
                return Ok(value);
            }
            self.env.pop_scope();
        }
        Err(RuntimeError::MatchError {
            message: "No match arm matched value".into(),
        })
    }

    fn match_pattern(&mut self, pattern: &Pattern, value: &Value) -> RuntimeResult<bool> {
        match pattern {
            Pattern::Wildcard => Ok(true),
            Pattern::Identifier(name, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                self.env.declare(name, concrete, false)?;
                Ok(true)
            }
            Pattern::Literal(lit) => {
                let lit_value = match lit {
                    Literal::Int(v, _) => Value::Int(*v),
                    Literal::Float(v, _) => Value::Float(*v),
                    Literal::Bool(v, _) => Value::Bool(*v),
                    Literal::String(v, _) => Value::String(v.clone()),
                    Literal::Rune(v, _) => Value::Int(*v as i128),
                };
                let target = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                Ok(match (target, lit_value) {
                    (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
                    (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
                    (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
                    (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
                    _ => false,
                })
            }
            Pattern::EnumVariant {
                enum_name,
                variant,
                bindings,
                ..
            } => {
                let target = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Enum(enum_value) = target {
                    if &enum_value.variant == variant
                        && enum_name
                            .as_ref()
                            .map(|name| &enum_value.enum_name == name)
                            .unwrap_or(true)
                    {
                        for (binding, val) in bindings.iter().zip(enum_value.values.iter()) {
                            if !self.match_pattern(binding, val)? {
                                return Ok(false);
                            }
                        }
                        return Ok(true);
                    }
                }
                Ok(false)
            }
        }
    }

    fn instantiate_struct(
        &mut self,
        name: &str,
        fields: &StructLiteralKind,
    ) -> RuntimeResult<Value> {
        let def = self
            .structs
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::UnknownSymbol {
                name: name.to_string(),
            })?;
        self.ensure_item_visible(&def.module, def.def.visibility, name, "struct")?;
        let def = def.def;
        let mut field_map = BTreeMap::new();
        let mut embedded = Vec::new();

        match fields {
            StructLiteralKind::Named(named_fields) => {
                for field in named_fields {
                    let value = self.eval_expression(&field.value)?;
                    field_map.insert(field.name.clone(), value);
                }
            }
            StructLiteralKind::Positional(values) => {
                if values.len() != def.fields.len() {
                    return Err(RuntimeError::TypeMismatch {
                        message: format!(
                            "Struct `{}` expects {} fields but got {}",
                            name,
                            def.fields.len(),
                            values.len()
                        ),
                    });
                }
                for (field_def, expr) in def.fields.iter().zip(values.iter()) {
                    let is_embedded = field_def.embedded;
                    let field_name = field_def.name.clone();
                    let value = self.eval_expression(expr)?;
                    if is_embedded {
                        embedded.push(value);
                    } else if let Some(field_name) = field_name {
                        field_map.insert(field_name, value);
                    }
                }
            }
        }

        Ok(Value::Struct(StructInstance {
            name: name.to_string(),
            fields: field_map,
            embedded,
        }))
    }

    fn instantiate_enum(
        &mut self,
        enum_name: &str,
        variant: &str,
        values: Vec<Value>,
    ) -> RuntimeResult<Value> {
        let info = self
            .enum_variants
            .get(variant)
            .ok_or_else(|| RuntimeError::UnknownSymbol {
                name: variant.to_string(),
            })?;
        self.ensure_item_visible(&info.module, info.visibility, enum_name, "enum")?;
        if info.fields.len() != values.len() {
            return Err(RuntimeError::ArityMismatch {
                name: variant.to_string(),
                expected: info.fields.len(),
                received: values.len(),
            });
        }
        Ok(Value::Enum(EnumValue {
            enum_name: enum_name.to_string(),
            variant: variant.to_string(),
            values,
        }))
    }

    fn eval_binary(&self, op: BinaryOp, left: Value, right: Value) -> RuntimeResult<Value> {
        use BinaryOp::*;
        match op {
            Add | Sub | Mul | Div | Rem => self.eval_numeric(op, left, right),
            And => Ok(Value::Bool(left.as_bool() && right.as_bool())),
            Or => Ok(Value::Bool(left.as_bool() || right.as_bool())),
            BitAnd | BitOr | BitXor => self.eval_bitwise(op, left, right),
            Eq => Ok(Value::Bool(self.values_equal(&left, &right)?)),
            NotEq => Ok(Value::Bool(!self.values_equal(&left, &right)?)),
            Lt => self.eval_compare(left, right, |a, b| a < b),
            LtEq => self.eval_compare(left, right, |a, b| a <= b),
            Gt => self.eval_compare(left, right, |a, b| a > b),
            GtEq => self.eval_compare(left, right, |a, b| a >= b),
        }
    }

    fn eval_numeric(&self, op: BinaryOp, left: Value, right: Value) -> RuntimeResult<Value> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => match op {
                BinaryOp::Add => Ok(Value::Int(a + b)),
                BinaryOp::Sub => Ok(Value::Int(a - b)),
                BinaryOp::Mul => Ok(Value::Int(a * b)),
                BinaryOp::Div => Ok(Value::Int(a / b)),
                BinaryOp::Rem => Ok(Value::Int(a % b)),
                _ => unreachable!(),
            },
            (Value::Float(a), Value::Float(b)) => match op {
                BinaryOp::Add => Ok(Value::Float(a + b)),
                BinaryOp::Sub => Ok(Value::Float(a - b)),
                BinaryOp::Mul => Ok(Value::Float(a * b)),
                BinaryOp::Div => Ok(Value::Float(a / b)),
                BinaryOp::Rem => Ok(Value::Float(a % b)),
                _ => unreachable!(),
            },
            (Value::Int(a), Value::Float(b)) => {
                self.eval_numeric(op, Value::Float(a as f64), Value::Float(b))
            }
            (Value::Float(a), Value::Int(b)) => {
                self.eval_numeric(op, Value::Float(a), Value::Float(b as f64))
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: "Numeric operation expects numbers".into(),
            }),
        }
    }

    fn eval_bitwise(&self, op: BinaryOp, left: Value, right: Value) -> RuntimeResult<Value> {
        let (lhs, rhs) = match (left, right) {
            (Value::Int(a), Value::Int(b)) => (a, b),
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "Bitwise operations require integers".into(),
                });
            }
        };
        let result = match op {
            BinaryOp::BitAnd => lhs & rhs,
            BinaryOp::BitOr => lhs | rhs,
            BinaryOp::BitXor => lhs ^ rhs,
            _ => unreachable!(),
        };
        Ok(Value::Int(result))
    }

    fn eval_compare<F>(&self, left: Value, right: Value, cmp: F) -> RuntimeResult<Value>
    where
        F: Fn(f64, f64) -> bool,
    {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(cmp(a as f64, b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(cmp(a, b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(cmp(a as f64, b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(cmp(a, b as f64))),
            _ => Err(RuntimeError::TypeMismatch {
                message: "Comparison expects numeric values".into(),
            }),
        }
    }

    fn values_equal(&self, left: &Value, right: &Value) -> RuntimeResult<bool> {
        let left_val = match left {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other.clone(),
        };
        let right_val = match right {
            Value::Reference(reference) => reference.cell.borrow().clone(),
            other => other.clone(),
        };
        match (left_val, right_val) {
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::Int(a), Value::Int(b)) => Ok(a == b),
            (Value::Float(a), Value::Float(b)) => Ok(a == b),
            (Value::String(a), Value::String(b)) => Ok(a == b),
            _ => Err(RuntimeError::TypeMismatch {
                message: "Equality comparison expects similar types".into(),
            }),
        }
    }

    fn eval_range_expr(&mut self, range: &RangeExpr) -> RuntimeResult<RangeValue> {
        let start = self.eval_expression(&range.start)?;
        let end = self.eval_expression(&range.end)?;
        let start_int = match start {
            Value::Int(v) => v,
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "Range start must be integer".into(),
                });
            }
        };
        let end_int = match end {
            Value::Int(v) => v,
            _ => {
                return Err(RuntimeError::TypeMismatch {
                    message: "Range end must be integer".into(),
                });
            }
        };
        Ok(RangeValue {
            start: start_int,
            end: end_int,
            inclusive: range.inclusive,
        })
    }
}

fn flow_name(flow: &FlowSignal) -> &'static str {
    match flow {
        FlowSignal::Break => "break",
        FlowSignal::Continue => "continue",
        FlowSignal::Return(_) => "return",
    }
}
fn receiver_type_name(def: &FunctionDef, structs: &HashMap<String, StructEntry>) -> Option<String> {
    def.params
        .first()
        .and_then(|param| type_name_from_annotation(&param.ty))
        .filter(|name| structs.contains_key(name))
}

fn type_name_from_annotation(annotation: &TypeAnnotation) -> Option<String> {
    type_name_from_type_expr(&annotation.ty)
}

fn type_name_from_type_expr(expr: &TypeExpr) -> Option<String> {
    match expr {
        TypeExpr::Named(name, _) => Some(name.clone()),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => {
            type_name_from_type_expr(ty)
        }
        _ => None,
    }
}

fn substitute_self_in_function(def: &mut FunctionDef, target: &str) {
    let concrete = TypeExpr::Named(target.to_string(), Vec::new());
    for param in &mut def.params {
        param.ty = param.ty.replace_self(&concrete);
    }
    for ret in &mut def.returns {
        *ret = ret.replace_self(&concrete);
    }
}
