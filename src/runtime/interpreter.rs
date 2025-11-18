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
    Propagate(Value),
}

enum BlockEval {
    Value(Value),
    Flow(FlowSignal),
}

enum EvalOutcome<T> {
    Value(T),
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
                match result? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => {
                        return Err(RuntimeError::Panic {
                            message: format!(
                                "Control flow {} not allowed in const initializer",
                                flow_name(&flow)
                            ),
                        });
                    }
                }
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
                FunctionBody::Expr(expr) => match self.eval_expression(&expr.node)? {
                    EvalOutcome::Value(value) => BlockEval::Value(value),
                    EvalOutcome::Flow(flow) => BlockEval::Flow(flow),
                },
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
                BlockEval::Flow(FlowSignal::Propagate(value)) => vec![value],
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

            if let Some(flow) = self.execute_deferred()? {
                self.env.pop_scope();
                return match flow {
                    FlowSignal::Propagate(value) => Ok(vec![value]),
                    other => Err(RuntimeError::Panic {
                        message: format!(
                            "Control flow {} not allowed when leaving function",
                            flow_name(&other)
                        ),
                    }),
                };
            }
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
            "len" => self.builtin_len(args),
            "get" => self.builtin_get(args),
            "push" => self.builtin_push(args),
            "insert" => self.builtin_insert(args),
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
                | "len"
                | "get"
                | "push"
                | "insert"
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

    fn expect_int_value(&self, name: &str, value: Value) -> RuntimeResult<i128> {
        match value {
            Value::Int(i) => Ok(i),
            Value::Reference(reference) => {
                let cloned = reference.cell.borrow().clone();
                self.expect_int_value(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects integer value"),
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

    fn builtin_len(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("len", &args, 1)?;
        let value = args.remove(0);
        match value {
            Value::Slice(slice) => Ok(vec![Value::Int(slice.len() as i128)]),
            Value::Map(map) => Ok(vec![Value::Int(map.len() as i128)]),
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.builtin_len(vec![inner])
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!("`len` not supported for {}", self.describe_value(&other)),
            }),
        }
    }

    fn builtin_get(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("get", &args, 2)?;
        let receiver = args.remove(0);
        match receiver {
            Value::Slice(slice) => {
                let index = self.expect_int_value("get", args.remove(0))?;
                if index < 0 {
                    let none = self.instantiate_enum("Option", "None", Vec::new())?;
                    return Ok(vec![none]);
                }
                if let Some(value) = slice.get(index as usize) {
                    let some = self.instantiate_enum("Option", "Some", vec![value])?;
                    Ok(vec![some])
                } else {
                    let none = self.instantiate_enum("Option", "None", Vec::new())?;
                    Ok(vec![none])
                }
            }
            Value::Map(map) => {
                let key = self.expect_string("get", args.remove(0))?;
                if let Some(value) = map.get(&key) {
                    let some = self.instantiate_enum("Option", "Some", vec![value])?;
                    Ok(vec![some])
                } else {
                    let none = self.instantiate_enum("Option", "None", Vec::new())?;
                    Ok(vec![none])
                }
            }
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                let mut new_args = Vec::with_capacity(args.len() + 1);
                new_args.push(inner);
                new_args.extend(args);
                self.builtin_get(new_args)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!("`get` not supported for {}", self.describe_value(&other)),
            }),
        }
    }

    fn builtin_push(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("push", &args, 2)?;
        let slice = self.expect_slice("push", args.remove(0))?;
        let value = args.remove(0);
        slice.push(value);
        Ok(Vec::new())
    }

    fn builtin_insert(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("insert", &args, 3)?;
        let map = self.expect_map("insert", args.remove(0))?;
        let key = self.expect_string("insert", args.remove(0))?;
        let value = args.remove(0);
        map.insert(key, value);
        Ok(Vec::new())
    }

    fn eval_statement(&mut self, statement: &Statement) -> RuntimeResult<Option<FlowSignal>> {
        match statement {
            Statement::Let(stmt) => {
                match &stmt.pattern {
                    Pattern::Identifier(name, _) => {
                        let value = match &stmt.value {
                            Some(expr) => match self.eval_expression(expr)? {
                                EvalOutcome::Value(value) => value,
                                EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                            },
                            None => Value::Unit,
                        };
                        self.env
                            .declare(name, value, stmt.mutability == Mutability::Mutable)?;
                    }
                    pattern => {
                        let expr = stmt.value.as_ref().ok_or_else(|| RuntimeError::Panic {
                            message: "Destructuring bindings require an initializer".into(),
                        })?;
                        let value = match self.eval_expression(expr)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        let allow_mut = stmt.mutability == Mutability::Mutable;
                        if !self.match_pattern(pattern, &value, allow_mut)? {
                            return Err(RuntimeError::MatchError {
                                message: "Pattern did not match value".into(),
                            });
                        }
                    }
                }
                Ok(None)
            }
            Statement::Assign(stmt) => {
                match &stmt.target {
                    Expr::Identifier(ident) => {
                        let value = match self.eval_expression(&stmt.value)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        self.env.assign(&ident.name, value)?;
                    }
                    Expr::Deref { expr, .. } => {
                        let target = match self.eval_expression(expr)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        match target {
                            Value::Reference(reference) => {
                                if !reference.mutable {
                                    return Err(RuntimeError::Panic {
                                        message: "Cannot assign through immutable reference".into(),
                                    });
                                }
                                let value = match self.eval_expression(&stmt.value)? {
                                    EvalOutcome::Value(value) => value,
                                    EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                                };
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
            Statement::Expr(stmt) => match self.eval_expression(&stmt.expr)? {
                EvalOutcome::Value(_) => Ok(None),
                EvalOutcome::Flow(flow) => Ok(Some(flow)),
            },
            Statement::Return(stmt) => {
                let mut values = Vec::new();
                for expr in &stmt.values {
                    match self.eval_expression(expr)? {
                        EvalOutcome::Value(value) => values.push(value),
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    }
                }
                Ok(Some(FlowSignal::Return(values)))
            }
            Statement::While(stmt) => match &stmt.condition {
                WhileCondition::Expr(condition_expr) => {
                    loop {
                        let condition = match self.eval_expression(condition_expr)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        if !condition.as_bool() {
                            break;
                        }
                        let result = self.eval_block(&stmt.body)?;
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => continue,
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
                        }
                    }
                    Ok(None)
                }
                WhileCondition::Let { pattern, value } => {
                    loop {
                        let candidate = match self.eval_expression(value)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        self.env.push_scope();
                        let matched = self.match_pattern(pattern, &candidate, false)?;
                        if !matched {
                            self.env.pop_scope();
                            break;
                        }
                        let result = self.eval_block(&stmt.body)?;
                        if let Some(flow) = self.execute_deferred()? {
                            self.env.pop_scope();
                            return Ok(Some(flow));
                        }
                        self.env.pop_scope();
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => continue,
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
                        }
                    }
                    Ok(None)
                }
            },
            Statement::For(stmt) => match &stmt.target {
                ForTarget::Range(range_expr) => {
                    let range = match self.eval_range_expr(range_expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let end = if range.inclusive {
                        range.end + 1
                    } else {
                        range.end
                    };
                    for i in range.start..end {
                        self.env.push_scope();
                        self.env.declare(&stmt.binding, Value::Int(i), false)?;
                        let result = self.eval_block(&stmt.body)?;
                        if let Some(flow) = self.execute_deferred()? {
                            self.env.pop_scope();
                            return Ok(Some(flow));
                        }
                        self.env.pop_scope();
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => continue,
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
                        }
                    }
                    Ok(None)
                }
                ForTarget::Collection(expr) => {
                    let iterable = match self.eval_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                    };
                    let elements = self.collect_iterable_values(iterable)?;
                    for element in elements {
                        self.env.push_scope();
                        self.env.declare(&stmt.binding, element, false)?;
                        let result = self.eval_block(&stmt.body)?;
                        if let Some(flow) = self.execute_deferred()? {
                            self.env.pop_scope();
                            return Ok(Some(flow));
                        }
                        self.env.pop_scope();
                        match result {
                            BlockEval::Value(_) => {}
                            BlockEval::Flow(FlowSignal::Continue) => continue,
                            BlockEval::Flow(FlowSignal::Break) => break,
                            BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                            BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => {
                                return Ok(Some(flow));
                            }
                        }
                    }
                    Ok(None)
                }
            },
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
                if let Some(defer_flow) = self.execute_deferred()? {
                    self.env.pop_scope();
                    return Ok(BlockEval::Flow(defer_flow));
                }
                self.env.pop_scope();
                return Ok(BlockEval::Flow(flow));
            }
        }
        let outcome = if let Some(tail) = &block.tail {
            match self.eval_expression(tail)? {
                EvalOutcome::Value(value) => BlockEval::Value(value),
                EvalOutcome::Flow(flow) => BlockEval::Flow(flow),
            }
        } else {
            BlockEval::Value(Value::Unit)
        };
        if let Some(flow) = self.execute_deferred()? {
            self.env.pop_scope();
            return Ok(BlockEval::Flow(flow));
        }
        self.env.pop_scope();
        Ok(outcome)
    }

    fn execute_deferred(&mut self) -> RuntimeResult<Option<FlowSignal>> {
        let mut deferred = self.env.drain_deferred();
        let mut pending_flow: Option<FlowSignal> = None;
        while let Some(expr) = deferred.pop() {
            match self.eval_expression(&expr)? {
                EvalOutcome::Value(_) => {}
                EvalOutcome::Flow(FlowSignal::Propagate(value)) => {
                    if pending_flow.is_none() {
                        pending_flow = Some(FlowSignal::Propagate(value));
                    }
                }
                EvalOutcome::Flow(flow) => {
                    return Err(RuntimeError::Panic {
                        message: format!(
                            "Control flow {} not allowed in deferred expression",
                            flow_name(&flow)
                        ),
                    });
                }
            }
        }
        Ok(pending_flow)
    }

    fn eval_arguments(&mut self, args: &[Expr]) -> RuntimeResult<EvalOutcome<Vec<Value>>> {
        let mut values = Vec::with_capacity(args.len());
        for expr in args {
            match self.eval_expression(expr)? {
                EvalOutcome::Value(value) => values.push(value),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        Ok(EvalOutcome::Value(values))
    }

    fn value_struct_name(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Reference(reference) => self.value_struct_name(&reference.cell.borrow()),
            Value::Boxed(inner) => self.value_struct_name(&inner.cell.borrow()),
            _ => None,
        }
    }

    fn eval_expression(&mut self, expr: &Expr) -> RuntimeResult<EvalOutcome<Value>> {
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
                Ok(EvalOutcome::Value(value))
            }
            Expr::Literal(lit) => Ok(EvalOutcome::Value(match lit {
                Literal::Int(value, _) => Value::Int(*value),
                Literal::Float(value, _) => Value::Float(*value),
                Literal::Bool(value, _) => Value::Bool(*value),
                Literal::String(value, _) => Value::String(value.clone()),
                Literal::Rune(value, _) => Value::Int(*value as i128),
            })),
            Expr::Try { block, .. } => match self.eval_block(block)? {
                BlockEval::Value(value) => {
                    let wrapped = self.instantiate_enum("Result", "Ok", vec![value])?;
                    Ok(EvalOutcome::Value(wrapped))
                }
                BlockEval::Flow(FlowSignal::Propagate(value)) => Ok(EvalOutcome::Value(value)),
                BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::TryPropagate { expr, .. } => match self.eval_expression(expr)? {
                EvalOutcome::Value(value) => self.eval_try_operator(value),
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::Binary {
                op, left, right, ..
            } => {
                let lhs = match self.eval_expression(left)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let rhs = match self.eval_expression(right)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                self.eval_binary(*op, lhs, rhs).map(EvalOutcome::Value)
            }
            Expr::Unary { op, expr, .. } => {
                let value = match self.eval_expression(expr)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let evaluated = match op {
                    UnaryOp::Neg => match value {
                        Value::Int(v) => Value::Int(-v),
                        Value::Float(v) => Value::Float(-v),
                        _ => {
                            return Err(RuntimeError::TypeMismatch {
                                message: "Unary - expects numeric value".into(),
                            });
                        }
                    },
                    UnaryOp::Not => Value::Bool(!value.as_bool()),
                };
                Ok(EvalOutcome::Value(evaluated))
            }
            Expr::Call {
                callee,
                type_args,
                args,
                ..
            } => match callee.as_ref() {
                Expr::Identifier(ident) => {
                    let arg_values = match self.eval_arguments(args)? {
                        EvalOutcome::Value(values) => values,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    if let Some(variant) = self.enum_variants.get(&ident.name) {
                        let enum_name = variant.enum_name.clone();
                        let variant_name = ident.name.clone();
                        let value = self.instantiate_enum(&enum_name, &variant_name, arg_values)?;
                        return Ok(EvalOutcome::Value(value));
                    }
                    let results = self.call_function(&ident.name, None, type_args, arg_values)?;
                    let value = match results.len() {
                        0 => Value::Unit,
                        1 => results.into_iter().next().unwrap(),
                        _ => Value::Tuple(results),
                    };
                    Ok(EvalOutcome::Value(value))
                }
                Expr::FieldAccess { base, field, .. } => {
                    if let Expr::Identifier(module_ident) = base.as_ref() {
                        let qualified = format!("{}::{}", module_ident.name, field);
                        if self.functions.contains_key(&FunctionKey {
                            name: qualified.clone(),
                            receiver: None,
                            type_args: None,
                        }) {
                            let arg_values = match self.eval_arguments(args)? {
                                EvalOutcome::Value(values) => values,
                                EvalOutcome::Flow(flow) => {
                                    return Ok(EvalOutcome::Flow(flow));
                                }
                            };
                            let results =
                                self.call_function(&qualified, None, type_args, arg_values)?;
                            let value = match results.len() {
                                0 => Value::Unit,
                                1 => results.into_iter().next().unwrap(),
                                _ => Value::Tuple(results),
                            };
                            return Ok(EvalOutcome::Value(value));
                        }
                    }
                    let receiver = match self.eval_expression(base)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    let receiver_type = self.value_struct_name(&receiver);
                    let mut method_args = match self.eval_arguments(args)? {
                        EvalOutcome::Value(values) => values,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    method_args.insert(0, receiver);
                    let results =
                        self.call_function(field, receiver_type, type_args, method_args)?;
                    let value = match results.len() {
                        0 => Value::Unit,
                        1 => results.into_iter().next().unwrap(),
                        _ => Value::Tuple(results),
                    };
                    Ok(EvalOutcome::Value(value))
                }
                _ => Err(RuntimeError::Unsupported {
                    message: "Unsupported call target".into(),
                }),
            },
            Expr::FieldAccess { base, field, .. } => {
                let value = match self.eval_expression(base)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                match value {
                    Value::Struct(instance) => {
                        let field_value = instance.get_field(field).ok_or_else(|| {
                            RuntimeError::UnknownSymbol {
                                name: field.clone(),
                            }
                        })?;
                        Ok(EvalOutcome::Value(field_value))
                    }
                    _ => Err(RuntimeError::TypeMismatch {
                        message: "Field access requires struct value".into(),
                    }),
                }
            }
            Expr::StructLiteral { name, fields, .. } => self.instantiate_struct(name, fields),
            Expr::MapLiteral { entries, .. } => self.eval_map_literal(entries),
            Expr::Match(expr) => self.eval_match(expr),
            Expr::Block(block) => match self.eval_block(block)? {
                BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::If(if_expr) => self.eval_if_expression(if_expr),
            Expr::Tuple(values, _) => {
                let mut evaluated = Vec::with_capacity(values.len());
                for expr in values {
                    match self.eval_expression(expr)? {
                        EvalOutcome::Value(value) => evaluated.push(value),
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                Ok(EvalOutcome::Value(Value::Tuple(evaluated)))
            }
            Expr::Range(range) => match self.eval_range_expr(range)? {
                EvalOutcome::Value(range_value) => {
                    Ok(EvalOutcome::Value(Value::Range(range_value)))
                }
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::Reference { mutable, expr, .. } => self.build_reference(expr, *mutable),
            Expr::Deref { expr, .. } => match self.eval_expression(expr)? {
                EvalOutcome::Value(Value::Reference(reference)) => {
                    Ok(EvalOutcome::Value(reference.cell.borrow().clone()))
                }
                EvalOutcome::Value(_) => Err(RuntimeError::TypeMismatch {
                    message: "Cannot dereference non-reference value".into(),
                }),
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::ArrayLiteral(values, _) => self.eval_array_literal(values),
            Expr::Move { expr, .. } => self.eval_move_expression(expr).map(EvalOutcome::Value),
        }
    }

    fn build_reference(&mut self, expr: &Expr, mutable: bool) -> RuntimeResult<EvalOutcome<Value>> {
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
                Ok(EvalOutcome::Value(Value::Reference(ReferenceValue {
                    cell,
                    mutable,
                    origin: Some(ident.name.clone()),
                })))
            }
            _ => match self.eval_expression(expr)? {
                EvalOutcome::Value(value) => {
                    Ok(EvalOutcome::Value(Value::Reference(ReferenceValue {
                        cell: Rc::new(RefCell::new(value)),
                        mutable,
                        origin: None,
                    })))
                }
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
        }
    }

    fn eval_array_literal(&mut self, values: &[Expr]) -> RuntimeResult<EvalOutcome<Value>> {
        let mut items = Vec::with_capacity(values.len());
        for expr in values {
            match self.eval_expression(expr)? {
                EvalOutcome::Value(value) => items.push(value),
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            }
        }
        Ok(EvalOutcome::Value(Value::Slice(SliceValue::from_vec(
            items,
        ))))
    }

    fn eval_map_literal(
        &mut self,
        entries: &[MapLiteralEntry],
    ) -> RuntimeResult<EvalOutcome<Value>> {
        let mut pairs = Vec::with_capacity(entries.len());
        for entry in entries {
            let key_value = match self.eval_expression(&entry.key)? {
                EvalOutcome::Value(value) => value,
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            let key = match key_value {
                Value::String(text) => text,
                Value::Reference(reference) => {
                    if let Value::String(text) = reference.cell.borrow().clone() {
                        text
                    } else {
                        return Err(RuntimeError::TypeMismatch {
                            message: "Map literal keys must be strings".into(),
                        });
                    }
                }
                _ => {
                    return Err(RuntimeError::TypeMismatch {
                        message: "Map literal keys must be strings".into(),
                    });
                }
            };
            let value = match self.eval_expression(&entry.value)? {
                EvalOutcome::Value(value) => value,
                EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
            };
            pairs.push((key, value));
        }
        Ok(EvalOutcome::Value(Value::Map(MapValue::from_entries(
            pairs,
        ))))
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

    fn eval_match(&mut self, expr: &MatchExpr) -> RuntimeResult<EvalOutcome<Value>> {
        let target = match self.eval_expression(&expr.expr)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        for arm in &expr.arms {
            self.env.push_scope();
            if self.match_pattern(&arm.pattern, &target, false)? {
                if let Some(guard_expr) = &arm.guard {
                    let guard_value = match self.eval_expression(guard_expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => {
                            self.env.pop_scope();
                            return Ok(EvalOutcome::Flow(flow));
                        }
                    };
                    let guard_passed = guard_value.as_bool();
                    if let Some(flow) = self.execute_deferred()? {
                        self.env.pop_scope();
                        return Ok(EvalOutcome::Flow(flow));
                    }
                    if !guard_passed {
                        self.env.pop_scope();
                        continue;
                    }
                }
                let outcome = self.eval_expression(&arm.value)?;
                if let Some(flow) = self.execute_deferred()? {
                    self.env.pop_scope();
                    return Ok(EvalOutcome::Flow(flow));
                }
                self.env.pop_scope();
                return Ok(outcome);
            }
            self.env.pop_scope();
        }
        Err(RuntimeError::MatchError {
            message: "No match arm matched value".into(),
        })
    }

    fn match_pattern(
        &mut self,
        pattern: &Pattern,
        value: &Value,
        mutable_bindings: bool,
    ) -> RuntimeResult<bool> {
        match pattern {
            Pattern::Wildcard => Ok(true),
            Pattern::Identifier(name, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                self.env.declare(name, concrete, mutable_bindings)?;
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
                            if !self.match_pattern(binding, val, mutable_bindings)? {
                                return Ok(false);
                            }
                        }
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Pattern::Tuple(patterns, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Tuple(values) = concrete {
                    if patterns.len() != values.len() {
                        return Ok(false);
                    }
                    for (pat, val) in patterns.iter().zip(values.iter()) {
                        if !self.match_pattern(pat, val, mutable_bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Map(entries, _) => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Map(map) = concrete {
                    for entry in entries {
                        let Some(val) = map.get(&entry.key) else {
                            return Ok(false);
                        };
                        if !self.match_pattern(&entry.pattern, &val, mutable_bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Struct {
                struct_name,
                fields,
                has_spread: _,
                ..
            } => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                if let Value::Struct(instance) = concrete {
                    if let Some(expected) = struct_name {
                        if &instance.name != expected {
                            return Ok(false);
                        }
                    }
                    for field in fields {
                        let Some(field_value) = instance.get_field(&field.name) else {
                            return Ok(false);
                        };
                        if !self.match_pattern(&field.pattern, &field_value, mutable_bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Slice {
                prefix,
                rest,
                suffix,
                ..
            } => {
                let concrete = match value {
                    Value::Reference(reference) => reference.cell.borrow().clone(),
                    other => other.clone(),
                };
                let elements = match concrete {
                    Value::Slice(slice) => slice.items.borrow().clone(),
                    _ => return Ok(false),
                };
                if rest.is_none() && elements.len() != prefix.len() + suffix.len() {
                    return Ok(false);
                }
                if prefix.len() + suffix.len() > elements.len() {
                    return Ok(false);
                }
                for (pat, val) in prefix.iter().zip(elements.iter()) {
                    if !self.match_pattern(pat, val, mutable_bindings)? {
                        return Ok(false);
                    }
                }
                for (pat, val) in suffix.iter().rev().zip(elements.iter().rev()) {
                    if !self.match_pattern(pat, val, mutable_bindings)? {
                        return Ok(false);
                    }
                }
                if let Some(rest_pattern) = rest {
                    let start = prefix.len();
                    let end = elements.len() - suffix.len();
                    let slice = SliceValue::from_vec(elements[start..end].to_vec());
                    if !self.match_pattern(rest_pattern, &Value::Slice(slice), mutable_bindings)? {
                        return Ok(false);
                    }
                } else if elements.len() != prefix.len() + suffix.len() {
                    return Ok(false);
                }
                Ok(true)
            }
        }
    }

    fn instantiate_struct(
        &mut self,
        name: &str,
        fields: &StructLiteralKind,
    ) -> RuntimeResult<EvalOutcome<Value>> {
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
                    let value = match self.eval_expression(&field.value)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
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
                    let value = match self.eval_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    };
                    if is_embedded {
                        embedded.push(value);
                    } else if let Some(field_name) = field_name {
                        field_map.insert(field_name, value);
                    }
                }
            }
        }

        Ok(EvalOutcome::Value(Value::Struct(StructInstance {
            name: name.to_string(),
            fields: field_map,
            embedded,
        })))
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

    fn eval_range_expr(&mut self, range: &RangeExpr) -> RuntimeResult<EvalOutcome<RangeValue>> {
        let start = match self.eval_expression(&range.start)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
        let end = match self.eval_expression(&range.end)? {
            EvalOutcome::Value(value) => value,
            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
        };
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
        Ok(EvalOutcome::Value(RangeValue {
            start: start_int,
            end: end_int,
            inclusive: range.inclusive,
        }))
    }

    fn eval_try_operator(&self, value: Value) -> RuntimeResult<EvalOutcome<Value>> {
        match value {
            Value::Enum(enum_value) => {
                if enum_value.enum_name != "Result" {
                    return Err(RuntimeError::TypeMismatch {
                        message: "? operator expects Result value".into(),
                    });
                }
                match enum_value.variant.as_str() {
                    "Ok" => {
                        if enum_value.values.is_empty() {
                            Ok(EvalOutcome::Value(Value::Unit))
                        } else if enum_value.values.len() == 1 {
                            Ok(EvalOutcome::Value(
                                enum_value.values.into_iter().next().unwrap(),
                            ))
                        } else {
                            Ok(EvalOutcome::Value(Value::Tuple(enum_value.values)))
                        }
                    }
                    "Err" => Ok(EvalOutcome::Flow(FlowSignal::Propagate(Value::Enum(
                        enum_value,
                    )))),
                    _ => Err(RuntimeError::TypeMismatch {
                        message: "? operator expects Result value".into(),
                    }),
                }
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: "? operator expects Result value".into(),
            }),
        }
    }

    fn eval_if_expression(&mut self, if_expr: &IfExpr) -> RuntimeResult<EvalOutcome<Value>> {
        match &if_expr.condition {
            IfCondition::Expr(condition) => {
                let value = match self.eval_expression(condition)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                if value.as_bool() {
                    match self.eval_block(&if_expr.then_branch)? {
                        BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                        BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                    }
                } else if let Some(else_branch) = &if_expr.else_branch {
                    match else_branch {
                        ElseBranch::Block(block) => match self.eval_block(block)? {
                            BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                            BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                        },
                        ElseBranch::ElseIf(nested) => self.eval_if_expression(nested),
                    }
                } else {
                    Ok(EvalOutcome::Value(Value::Unit))
                }
            }
            IfCondition::Let { pattern, value, .. } => {
                let scrutinee = match self.eval_expression(value)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                self.env.push_scope();
                let matches = self.match_pattern(pattern, &scrutinee, false)?;
                if matches {
                    match self.eval_block(&if_expr.then_branch)? {
                        BlockEval::Value(value) => {
                            self.env.pop_scope();
                            Ok(EvalOutcome::Value(value))
                        }
                        BlockEval::Flow(flow) => {
                            self.env.pop_scope();
                            Ok(EvalOutcome::Flow(flow))
                        }
                    }
                } else {
                    self.env.pop_scope();
                    if let Some(else_branch) = &if_expr.else_branch {
                        match else_branch {
                            ElseBranch::Block(block) => match self.eval_block(block)? {
                                BlockEval::Value(value) => Ok(EvalOutcome::Value(value)),
                                BlockEval::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
                            },
                            ElseBranch::ElseIf(nested) => self.eval_if_expression(nested),
                        }
                    } else {
                        Ok(EvalOutcome::Value(Value::Unit))
                    }
                }
            }
        }
    }

    fn collect_iterable_values(&self, value: Value) -> RuntimeResult<Vec<Value>> {
        match value {
            Value::Slice(slice) => {
                let mut items = Vec::new();
                for idx in 0..slice.len() {
                    if let Some(item) = slice.get(idx) {
                        items.push(item);
                    }
                }
                Ok(items)
            }
            Value::Map(map) => {
                let mut items = Vec::new();
                for (key, value) in map.entries.borrow().iter() {
                    items.push(Value::Tuple(vec![
                        Value::String(key.clone()),
                        value.clone(),
                    ]));
                }
                Ok(items)
            }
            Value::Reference(reference) => {
                let inner = reference.cell.borrow().clone();
                self.collect_iterable_values(inner)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`for ... in` only supports slices or maps; found {}",
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn describe_value(&self, value: &Value) -> &'static str {
        match value {
            Value::Slice(_) => "slice",
            Value::Map(_) => "map",
            Value::Boxed(_) => "box",
            Value::Reference(_) => "reference",
            Value::Struct(_) => "struct",
            Value::Enum(_) => "enum",
            Value::Tuple(_) => "tuple",
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Range(_) => "range",
            Value::Unit => "unit",
            Value::Moved => "moved value",
        }
    }
}

fn flow_name(flow: &FlowSignal) -> &'static str {
    match flow {
        FlowSignal::Break => "break",
        FlowSignal::Continue => "continue",
        FlowSignal::Return(_) => "return",
        FlowSignal::Propagate(_) => "error propagation",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::{ast::Program, parser::parse_module};
    use crate::project::Package;
    use std::path::PathBuf;

fn interpreter_from_source(source: &str) -> Interpreter {
        let module =
            parse_module("tests::runtime", PathBuf::from("test.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        Interpreter::new(Package {
            program,
            modules: Vec::new(),
        })
    }

    fn call_unit(interpreter: &mut Interpreter, name: &str) {
        interpreter
            .call_function(name, None, &[], Vec::new())
            .expect(name);
    }

    #[test]
fn interpreter_releases_borrows_after_control_flow() {
        let source = r#"
module tests::runtime;

fn release_after_if() {
  let mut int32 value = 0;
  if true {
    let &mut int32 alias = &mut value;
    *alias = 1;
  } else {
    let &mut int32 alias = &mut value;
    *alias = 2;
  }
  let &mut int32 again = &mut value;
  *again = 3;
}

fn release_after_match() {
  let mut int32 value = 0;
  match true {
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

fn release_after_for_range() {
  let mut int32 value = 0;
  for count in 0..1 {
    let &mut int32 alias = &mut value;
    *alias = count;
  }
  let &mut int32 after = &mut value;
  *after = 7;
}

fn release_after_for_collection() {
  let []int32 entries = [1, 2];
  let mut int32 value = 0;
  for entry in entries {
    let &mut int32 alias = &mut value;
    *alias = entry;
  }
  let &mut int32 after = &mut value;
  *after = 8;
}

fn release_in_nested_block() {
  let mut int32 value = 0;
  {
    let &mut int32 alias = &mut value;
    *alias = 9;
  }
  let &mut int32 after = &mut value;
  *after = 10;
}

fn release_after_early_return(flag: bool) -> int32 {
  let mut int32 value = 0;
  if flag {
    let &mut int32 alias = &mut value;
    *alias = 11;
    return value;
  }
  let &mut int32 final_ref = &mut value;
  *final_ref = 12;
  value
}
fn release_after_nested_match() {
  let mut int32 value = 0;
  match true {
    true => {
      match false {
        true => {
          let &mut int32 alias = &mut value;
          *alias = 13;
        },
        false => {
          let &mut int32 alias = &mut value;
          *alias = 14;
        },
      }
    },
    false => {},
  }
  let &mut int32 after = &mut value;
  *after = 15;
}

fn release_after_defer() {
  let mut int32 value = 0;
  {
    defer {
      let &mut int32 alias = &mut value;
      *alias = 16;
    };
  }
  let &mut int32 after = &mut value;
  *after = 17;
}

"#;
        let mut interpreter = interpreter_from_source(source);
        interpreter.bootstrap().expect("bootstrap");
        for func in [
            "release_after_if",
            "release_after_match",
            "release_after_while",
            "release_after_while_let",
            "release_after_for_range",
            "release_after_for_collection",
            "release_in_nested_block",
            "release_after_nested_match",
            "release_after_defer",
        ] {
            call_unit(&mut interpreter, func);
        }
        for flag in [true, false] {
            let result = interpreter
                .call_function(
                    "release_after_early_return",
                    None,
                    &[],
                    vec![Value::Bool(flag)],
                )
                .expect("early return result");
            match result.as_slice() {
                [Value::Int(val)] => {
                    if flag {
                        assert_eq!(*val, 11);
                    } else {
                        assert_eq!(*val, 12);
                    }
                }
                other => panic!("unexpected early return result: {:?}", other),
            }
        }
    }

    #[test]
    fn interpreter_reports_live_alias() {
        let source = r#"
module tests::runtime;

fn alias_survives_scope() {
  let mut int32 value = 0;
  let &mut int32 alias = &mut value;
  let &mut int32 second = &mut value;
  *second = 1;
}
"#;
        let mut interpreter = interpreter_from_source(source);
        interpreter.bootstrap().expect("bootstrap");
        let err = interpreter
            .call_function("alias_survives_scope", None, &[], Vec::new())
            .expect_err("expected borrow violation");
        match err {
            RuntimeError::Panic { message } => {
                assert!(
                    message.contains("already mutably borrowed"),
                    "unexpected panic message: {message}"
                );
            }
            other => panic!("unexpected error: {:?}", other),
        }
    }

    #[test]
    fn interpreter_mutable_destructuring_returns_values() {
        let source = r#"
module tests::runtime;

struct Telemetry {
  hp: int32;
  mp: int32;
  notes: []string;
}

fn tuple_mut() -> int32 {
  let mut (left, right) = (10, 5);
  left = left + right;
  left
}

fn map_mut() -> int32 {
  let mut #{ "hp": hp_score, "mp": mp_score } = #{
    "hp": 80,
    "mp": 40,
  };
  hp_score = hp_score + mp_score;
  hp_score
}

fn struct_mut() -> int32 {
  let mut Telemetry{ hp, mp, .. } = Telemetry{
    hp: 70,
    mp: 35,
    notes: ["alpha"],
  };
  hp = hp + mp;
  hp
}

fn slice_mut() -> string {
  let mut [first, ..rest] = ["steady", "ready"];
  first = "launch";
  first
}
"#;
        let mut interpreter = interpreter_from_source(source);
        interpreter.bootstrap().expect("bootstrap");

        let tuple = interpreter
            .call_function("tuple_mut", None, &[], Vec::new())
            .expect("tuple result");
        match tuple.as_slice() {
            [Value::Int(value)] => assert_eq!(*value, 15),
            other => panic!("unexpected tuple result: {:?}", other),
        }

        let map = interpreter
            .call_function("map_mut", None, &[], Vec::new())
            .expect("map result");
        match map.as_slice() {
            [Value::Int(total)] => assert_eq!(*total, 120),
            other => panic!("unexpected map result: {:?}", other),
        }

        let structure = interpreter
            .call_function("struct_mut", None, &[], Vec::new())
            .expect("struct result");
        match structure.as_slice() {
            [Value::Int(total)] => assert_eq!(*total, 105),
            other => panic!("unexpected struct result: {:?}", other),
        }

        let slice = interpreter
            .call_function("slice_mut", None, &[], Vec::new())
            .expect("slice result");
        match slice.as_slice() {
            [Value::String(text)] => assert_eq!(text, "launch"),
            other => panic!("unexpected slice result: {:?}", other),
        }
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
