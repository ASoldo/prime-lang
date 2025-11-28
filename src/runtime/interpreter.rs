use crate::language::{
    ast::*,
    types::{Mutability, TypeAnnotation, TypeExpr},
};
use crate::project::Package;
use crate::runtime::{
    environment::{CleanupAction, DropRecord, Environment},
    error::{RuntimeError, RuntimeResult},
    value::{
        BoxValue, CapturedValue, ChannelReceiver, ChannelSender, ClosureValue, EnumValue,
        FormatRuntimeSegment, FormatTemplateValue, IteratorValue, JoinHandleValue, MapValue,
        PointerValue, RangeValue, ReferenceValue, SliceValue, StructInstance, Value,
    },
    platform::platform,
};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::sync::mpsc;
use std::sync::{Arc, Mutex, OnceLock};
use std::thread;

pub struct Interpreter {
    package: Package,
    env: Environment,
    structs: HashMap<String, StructEntry>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    functions: HashMap<FunctionKey, FunctionInfo>,
    interfaces: HashMap<String, InterfaceEntry>,
    impls: HashSet<ImplKey>,
    consts: Vec<(String, ConstDef)>,
    drop_impls: HashMap<String, FunctionKey>,
    suppress_drop_schedule: bool,
    deprecated_warnings: HashSet<String>,
    module_stack: Vec<String>,
    bootstrapped: bool,
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

static TEST_INPUTS: OnceLock<Mutex<VecDeque<String>>> = OnceLock::new();

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
            drop_impls: HashMap::new(),
            suppress_drop_schedule: false,
            deprecated_warnings: HashSet::new(),
            module_stack: Vec::new(),
            bootstrapped: false,
        }
    }

    /// Seed the shared test input queue from environment variables.
    pub fn seed_test_inputs_from_env() {
        let debug = env::var("PRIME_TEST_INPUTS_DEBUG").is_ok();
        let mut guard = TEST_INPUTS
            .get_or_init(|| Mutex::new(VecDeque::new()))
            .lock()
            .unwrap();
        guard.clear();
        Self::populate_test_inputs(&mut guard, debug, true);
    }

    pub fn run(&mut self) -> RuntimeResult<()> {
        self.bootstrap()?;
        let _ = self.call_function("main", None, &[], Vec::new())?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn run_function(&mut self, name: &str) -> RuntimeResult<Vec<Value>> {
        self.bootstrap()?;
        self.call_function(name, None, &[], Vec::new())
    }

    pub fn run_function_with_args(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> RuntimeResult<Vec<Value>> {
        self.bootstrap()?;
        self.call_function(name, None, &[], args)
    }

    pub fn bootstrap(&mut self) -> RuntimeResult<()> {
        if self.bootstrapped {
            return Ok(());
        }
        // First collect types and interfaces so impl blocks do not depend on module order.
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
                    Item::Macro(_)
                    | Item::MacroInvocation(_)
                    | Item::Impl(_)
                    | Item::Function(_)
                    | Item::Const(_) => {}
                }
            }
        }

        // Now that interfaces/types exist, register impls, functions, and consts.
        for module in self.package.program.modules.clone() {
            for item in &module.items {
                match item {
                    Item::Impl(block) => {
                        self.register_impl(&module.name, block.clone())?;
                    }
                    Item::Function(def) => {
                        self.register_function(&module.name, def.clone())?;
                    }
                    Item::Const(const_def) => {
                        self.consts.push((module.name.clone(), const_def.clone()));
                    }
                    Item::Struct(_)
                    | Item::Enum(_)
                    | Item::Interface(_)
                    | Item::Macro(_)
                    | Item::MacroInvocation(_) => {}
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
        self.bootstrapped = true;
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
                message: if let Some(existing) = self.functions.get(&base_key) {
                    format!(
                        "Duplicate function `{}` for receiver `{:?}` (existing in module `{}`, new in module `{}`)",
                        def.name, receiver, existing.module, module
                    )
                } else {
                    format!(
                        "Duplicate function `{}` for receiver `{:?}`",
                        def.name, receiver
                    )
                },
            });
        }
        if def.name == "drop" {
            if let Some(ref recv) = receiver {
                if let Some(existing) = self.drop_impls.insert(recv.clone(), base_key.clone()) {
                    return Err(RuntimeError::Panic {
                        message: format!(
                            "`{}` already has a drop implementation (previously registered {:?})",
                            recv, existing
                        ),
                    });
                }
            }
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
        let is_drop_like = block.inherent || block.interface == "Drop";
        let target_is_struct = self.structs.contains_key(&block.target);
        let target_is_enum = self
            .enum_variants
            .values()
            .any(|info| info.enum_name == block.target);
        if !target_is_struct && !(is_drop_like && target_is_enum) {
            return Err(RuntimeError::Panic {
                message: format!("Unknown target type `{}`", block.target),
            });
        }
        if is_drop_like {
            for method in &block.methods {
                let mut method_def = method.clone();
                substitute_self_in_function(&mut method_def, &block.target);
                self.register_function(module, method_def)?;
            }
            return Ok(());
        }
        if !self.interfaces.contains_key(&block.interface) {
            return Err(RuntimeError::Panic {
                message: format!("Unknown interface `{}`", block.interface),
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
                if let Some(name) = first.ty.as_ref().and_then(type_name_from_annotation) {
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
        if name == "in" {
            if type_args.len() != 1 {
                return Err(RuntimeError::Unsupported {
                    message: "`in` expects exactly one type argument, e.g. in[int32](\"prompt\")"
                        .into(),
                });
            }
            return self.call_in(args, &type_args[0]);
        }
        let allows_type_args = name == "channel";
        if !allows_type_args && !type_args.is_empty() && self.is_builtin_name(name) {
            return Err(RuntimeError::Unsupported {
                message: format!("`{}` does not accept type arguments", name),
            });
        }
        if let Some(result) = self.call_builtin(name, args.clone(), type_args) {
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
                self.declare_with_drop(
                    &param.name,
                    value,
                    param.mutability == Mutability::Mutable,
                )?;
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

            if let Some(flow) = self.execute_cleanups()? {
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

    fn call_closure_value(
        &mut self,
        closure: &ClosureValue,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        if closure.params.len() != args.len() {
            return Err(RuntimeError::ArityMismatch {
                name: "closure".into(),
                expected: closure.params.len(),
                received: args.len(),
            });
        }
        self.env.push_scope();
        for captured in &closure.captures {
            self.declare_with_drop(&captured.name, captured.value.clone(), captured.mutable)?;
        }
        for (param, value) in closure.params.iter().zip(args.into_iter()) {
            self.declare_with_drop(
                &param.name,
                value,
                param.mutability == Mutability::Mutable,
            )?;
        }
        let result = match &closure.body {
            ClosureBody::Block(block) => self.eval_block(block)?,
            ClosureBody::Expr(expr) => match self.eval_expression(expr.node.as_ref())? {
                EvalOutcome::Value(value) => BlockEval::Value(value),
                EvalOutcome::Flow(flow) => BlockEval::Flow(flow),
            },
        };
        let mut value = match result {
            BlockEval::Value(value) => value,
            BlockEval::Flow(FlowSignal::Return(mut values)) => {
                if values.len() == 1 {
                    values.remove(0)
                } else {
                    Value::Tuple(values)
                }
            }
            BlockEval::Flow(FlowSignal::Propagate(value)) => value,
            BlockEval::Flow(FlowSignal::Break) => {
                return Err(RuntimeError::Panic {
                    message: "break outside closure".into(),
                });
            }
            BlockEval::Flow(FlowSignal::Continue) => {
                return Err(RuntimeError::Panic {
                    message: "continue outside closure".into(),
                });
            }
        };
        if let Some(flow) = self.execute_cleanups()? {
            self.env.pop_scope();
            value = match flow {
                FlowSignal::Propagate(value) => value,
                other => {
                    return Err(RuntimeError::Panic {
                        message: format!(
                            "Control flow {} not allowed when leaving closure",
                            flow_name(&other)
                        ),
                    });
                }
            };
        }
        self.env.pop_scope();
        Ok(value)
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
            if let Some(ty) = param.ty.as_mut() {
                *ty = ty.substitute(&map);
            }
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
            if let Some(ty) = param.ty.as_ref() {
                if let Some((interface, type_args)) = self.interface_name_from_type(&ty.ty) {
                    self.ensure_interface_compat(&interface, &type_args, value)?;
                }
            }
        }
        Ok(())
    }

    fn clone_for_spawn(&self) -> Interpreter {
        Interpreter {
            package: self.package.clone(),
            env: self.env.clone(),
            structs: self.structs.clone(),
            enum_variants: self.enum_variants.clone(),
            functions: self.functions.clone(),
            interfaces: self.interfaces.clone(),
            impls: self.impls.clone(),
            consts: self.consts.clone(),
            drop_impls: self.drop_impls.clone(),
            suppress_drop_schedule: false,
            deprecated_warnings: HashSet::new(),
            module_stack: Vec::new(),
            bootstrapped: true,
        }
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
        if args.is_empty() {
            return Err(RuntimeError::ArityMismatch {
                name: "out".into(),
                expected: 1,
                received: 0,
            });
        }
        let mut iter = args.into_iter();
        let first = iter.next().unwrap();
        match first {
            Value::FormatTemplate(template) => {
                let provided: Vec<Value> = iter.collect();
                if template.implicit_placeholders != provided.len() {
                    return Err(RuntimeError::ArityMismatch {
                        name: "out".into(),
                        expected: template.implicit_placeholders,
                        received: provided.len(),
                    });
                }
                self.print_format_template(template, provided)?;
            }
            other => {
                if iter.next().is_some() {
                    return Err(RuntimeError::Panic {
                        message: "`out` with multiple arguments requires a format string literal"
                            .into(),
                    });
                }
                println!("{}", other);
            }
        }
        Ok(Vec::new())
    }

    fn call_in(&mut self, args: Vec<Value>, ty: &TypeExpr) -> RuntimeResult<Vec<Value>> {
        if args.is_empty() {
            return Err(RuntimeError::ArityMismatch {
                name: "in".into(),
                expected: 1,
                received: 0,
            });
        }
        let mut iter = args.into_iter();
        let prompt_value = iter.next().unwrap();
        let prompt = match prompt_value {
            Value::FormatTemplate(template) => {
                let provided: Vec<Value> = iter.collect();
                if template.implicit_placeholders != provided.len() {
                    return Err(RuntimeError::ArityMismatch {
                        name: "in".into(),
                        expected: template.implicit_placeholders,
                        received: provided.len(),
                    });
                }
                self.render_format_template(template, provided)?
            }
            other => {
                if iter.next().is_some() {
                    return Err(RuntimeError::Panic {
                        message: "`in` with multiple arguments requires a format string literal"
                            .into(),
                    });
                }
                self.format_value(&other)
            }
        };

        let raw = if let Some(test_input) = Self::next_test_input() {
            if env::var("PRIME_TEST_INPUTS_DEBUG").is_ok() {
                eprintln!("[prime-test-input] {}", test_input);
            }
            test_input
        } else {
            use std::io::{self, Write};
            print!("{prompt}");
            io::stdout().flush().ok();

            let mut buffer = String::new();
            io::stdin()
                .read_line(&mut buffer)
                .map_err(|err| RuntimeError::Panic {
                    message: format!("failed to read input: {err}"),
                })?;
            buffer.trim_end_matches(&['\n', '\r'][..]).to_string()
        };
        let parsed = match self.parse_input_value(&raw, ty) {
            Ok(value) => self.instantiate_enum("Result", "Ok", vec![value]),
            Err(msg) => {
                if env::var("PRIME_TEST_INPUTS_DEBUG").is_ok() {
                    eprintln!("[prime-test-input:parse-err] {}", msg);
                }
                self.instantiate_enum("Result", "Err", vec![Value::String(msg)])
            }
        }?;
        Ok(vec![parsed])
    }

    fn parse_input_value(&self, raw: &str, ty: &TypeExpr) -> Result<Value, String> {
        match ty {
            TypeExpr::Named(name, _) if name == "string" => Ok(Value::String(raw.to_string())),
            TypeExpr::Named(name, _) if name == "bool" => {
                match raw.trim().to_ascii_lowercase().as_str() {
                    "true" => Ok(Value::Bool(true)),
                    "false" => Ok(Value::Bool(false)),
                    _ => Err("expected `true` or `false`".into()),
                }
            }
            TypeExpr::Named(name, _) if name.eq_ignore_ascii_case("rune") => {
                let mut chars = raw.chars().filter(|c| *c != '\r' && *c != '\n');
                if let Some(ch) = chars.next() {
                    if chars.next().is_none() {
                        return Ok(Value::Int(ch as i128));
                    }
                }
                Err("expected single rune".into())
            }
            TypeExpr::Named(name, _) if name.starts_with("int") || name == "isize" => {
                let parsed = raw
                    .trim()
                    .parse::<i128>()
                    .map_err(|_| "invalid integer input")?;
                let (min, max) = match name.as_str() {
                    "int8" => (i8::MIN as i128, i8::MAX as i128),
                    "int16" => (i16::MIN as i128, i16::MAX as i128),
                    "int32" => (i32::MIN as i128, i32::MAX as i128),
                    "int64" => (i64::MIN as i128, i64::MAX as i128),
                    "isize" => (isize::MIN as i128, isize::MAX as i128),
                    _ => (i128::MIN, i128::MAX),
                };
                if parsed < min || parsed > max {
                    return Err(format!("integer out of range for {}", name));
                }
                Ok(Value::Int(parsed))
            }
            TypeExpr::Named(name, _) if name.starts_with("uint") || name == "usize" => {
                let parsed = raw
                    .trim()
                    .parse::<u128>()
                    .map_err(|_| "invalid integer input")?;
                let max = match name.as_str() {
                    "uint8" => u8::MAX as u128,
                    "uint16" => u16::MAX as u128,
                    "uint32" => u32::MAX as u128,
                    "uint64" => u64::MAX as u128,
                    "usize" => usize::MAX as u128,
                    _ => u128::MAX,
                };
                if parsed > max {
                    return Err(format!("integer out of range for {}", name));
                }
                Ok(Value::Int(parsed as i128))
            }
            TypeExpr::Named(name, _) if name == "float32" || name == "float64" => {
                let parsed = raw
                    .trim()
                    .parse::<f64>()
                    .map_err(|_| "invalid float input")?;
                Ok(Value::Float(parsed))
            }
            _ => {
                if env::var("PRIME_TEST_INPUTS_DEBUG").is_ok() {
                    eprintln!("[prime-test-input:unsupported] {}", ty.canonical_name());
                }
                Err(format!("unsupported input type {}", ty.canonical_name()))
            }
        }
    }

    fn warn_deprecated(&mut self, name: &str) {
        if self.deprecated_warnings.insert(name.to_string()) {
            eprintln!(
                "warning: `{}` is deprecated; prefer slice/map literals or direct methods",
                name
            );
        }
    }

    fn call_builtin(
        &mut self,
        name: &str,
        args: Vec<Value>,
        type_args: &[TypeExpr],
    ) -> Option<RuntimeResult<Vec<Value>>> {
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
            "map_keys" => self.builtin_map_keys(args),
            "map_values" => self.builtin_map_values(args),
            "len" => self.builtin_len(args),
            "get" => self.builtin_get(args),
            "push" => self.builtin_push(args),
            "insert" => self.builtin_insert(args),
            "iter" => self.builtin_iter(args),
            "next" => self.builtin_iter_next(args),
            "assert" => self.builtin_assert(args),
            "expect" => self.builtin_expect(args),
            "str_len" => self.builtin_str_len(args),
            "str_contains" => self.builtin_str_contains(args),
            "str_trim" => self.builtin_str_trim(args),
            "str_split" => self.builtin_str_split(args),
            "min" => self.builtin_min(args),
            "max" => self.builtin_max(args),
            "abs" => self.builtin_abs(args),
            "channel" => self.builtin_channel(args),
            "send" => self.builtin_send(args),
            "recv" => self.builtin_recv(args),
            "recv_timeout" => self.builtin_recv_timeout(args, type_args),
            "close" => self.builtin_close(args),
            "join" => self.builtin_join(args),
            "sleep" => self.builtin_sleep(args),
            "sleep_ms" => self.builtin_sleep_ms(args),
            "now_ms" => self.builtin_now_ms(args),
            "fs_exists" => self.builtin_fs_exists(args),
            "fs_read" => self.builtin_fs_read(args),
            "fs_write" => self.builtin_fs_write(args),
            "ptr" => self.builtin_ptr(args, false),
            "ptr_mut" => self.builtin_ptr(args, true),
            "cast" => self.builtin_cast(args, type_args),
            "assert_eq" => self.builtin_assert_eq(args),
            "panic" => self.builtin_panic(args),
            _ => return None,
        };
        Some(result)
    }

    fn call_builtin_method(
        &mut self,
        name: &str,
        mut args: Vec<Value>,
    ) -> Option<RuntimeResult<Vec<Value>>> {
        let receiver = args.remove(0);
        match (receiver, name) {
            (Value::String(s), "str_len") => Some(self.builtin_str_len(vec![Value::String(s)])),
            (Value::String(s), "str_contains") => {
                args.insert(0, Value::String(s));
                Some(self.builtin_str_contains(args))
            }
            (Value::String(s), "str_trim") => Some(self.builtin_str_trim(vec![Value::String(s)])),
            (Value::String(s), "str_split") => {
                args.insert(0, Value::String(s));
                Some(self.builtin_str_split(args))
            }
            (Value::FormatTemplate(template), "str_len") => {
                Some(self.builtin_str_len(vec![Value::FormatTemplate(template)]))
            }
            (Value::FormatTemplate(template), "str_contains") => {
                args.insert(0, Value::FormatTemplate(template));
                Some(self.builtin_str_contains(args))
            }
            (Value::FormatTemplate(template), "str_trim") => {
                Some(self.builtin_str_trim(vec![Value::FormatTemplate(template)]))
            }
            (Value::FormatTemplate(template), "str_split") => {
                args.insert(0, Value::FormatTemplate(template));
                Some(self.builtin_str_split(args))
            }
            (Value::Int(v), "abs") => Some(self.builtin_abs(vec![Value::Int(v)])),
            (Value::Int(v), "min") => {
                args.insert(0, Value::Int(v));
                Some(self.builtin_min(args))
            }
            (Value::Int(v), "max") => {
                args.insert(0, Value::Int(v));
                Some(self.builtin_max(args))
            }
            (Value::Float(f), "abs") => Some(self.builtin_abs(vec![Value::Float(f)])),
            (Value::Float(f), "min") => {
                args.insert(0, Value::Float(f));
                Some(self.builtin_min(args))
            }
            (Value::Float(f), "max") => {
                args.insert(0, Value::Float(f));
                Some(self.builtin_max(args))
            }
            (Value::Slice(slice), "iter") => {
                Some(self.builtin_iter(vec![Value::Slice(slice)]))
            }
            (Value::Map(map), "iter") => Some(self.builtin_iter(vec![Value::Map(map)])),
            (Value::Map(map), "map_keys") => Some(self.builtin_map_keys(vec![Value::Map(map)])),
            (Value::Map(map), "map_values") => Some(self.builtin_map_values(vec![Value::Map(map)])),
            (Value::JoinHandle(handle), "join") => {
                args.insert(0, Value::JoinHandle(handle));
                Some(self.builtin_join(args))
            }
            (Value::Iterator(iter), "next") => Some(self.builtin_iter_next(vec![Value::Iterator(iter)])),
            (Value::Pointer(pointer), _) => {
                let cloned = pointer.cell.lock().unwrap().clone();
                let mut all_args = vec![cloned];
                all_args.extend(args);
                self.call_builtin_method(name, all_args)
            }
            (Value::Reference(reference), _) => {
                let cloned = reference.cell.lock().unwrap().clone();
                let mut all_args = vec![cloned];
                all_args.extend(args);
                self.call_builtin_method(name, all_args)
            }
            _ => None,
        }
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
                | "map_keys"
                | "map_values"
                | "len"
                | "get"
                | "push"
                | "insert"
                | "iter"
                | "next"
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
                | "recv_timeout"
                | "close"
                | "join"
                | "sleep"
                | "sleep_ms"
                | "now_ms"
                | "fs_exists"
                | "fs_read"
                | "fs_write"
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
                let cloned = reference.cell.lock().unwrap().clone();
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
        Ok(vec![boxed.cell.lock().unwrap().clone()])
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
                let cloned = reference.cell.lock().unwrap().clone();
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
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_map(name, cloned)
            }
            Value::Pointer(pointer) => {
                let cloned = pointer.cell.lock().unwrap().clone();
                self.expect_map(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects map value"),
            }),
        }
    }

    fn expect_iterator(&self, name: &str, value: Value) -> RuntimeResult<IteratorValue> {
        match value {
            Value::Iterator(iter) => Ok(iter),
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_iterator(name, cloned)
            }
            Value::Pointer(pointer) => {
                let cloned = pointer.cell.lock().unwrap().clone();
                self.expect_iterator(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects iterator"),
            }),
        }
    }

    fn iter_items_from_value(&self, value: Value) -> RuntimeResult<Vec<Value>> {
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
                for (key, value) in map.entries.lock().unwrap().iter() {
                    items.push(Value::Tuple(vec![Value::String(key.clone()), value.clone()]));
                }
                Ok(items)
            }
            Value::Iterator(iter) => {
                let start = *iter.index.lock().unwrap();
                let guard = iter.items.lock().unwrap();
                Ok(guard.iter().skip(start).cloned().collect())
            }
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.iter_items_from_value(inner)
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone();
                self.iter_items_from_value(inner)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!("iter not supported for {}", self.describe_value(&other)),
            }),
        }
    }

    fn expect_int_value(&self, name: &str, value: Value) -> RuntimeResult<i128> {
        match value {
            Value::Int(i) => Ok(i),
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_int_value(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects integer value"),
            }),
        }
    }

    fn numeric_kind_from_type<'a>(&self, ty: &'a TypeExpr) -> Option<&'a str> {
        match ty {
            TypeExpr::Named(name, _) => match name.as_str() {
                "int8" | "int16" | "int32" | "int64" | "isize" => Some(name),
                "uint8" | "uint16" | "uint32" | "uint64" | "usize" => Some(name),
                "float32" | "float64" => Some(name),
                _ => None,
            },
            _ => None,
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

    fn expect_string_or_format(&self, name: &str, value: Value) -> RuntimeResult<String> {
        match value {
            Value::String(s) => Ok(s),
            Value::FormatTemplate(template) => {
                let mut buf = String::new();
                for segment in template.segments {
                    match segment {
                        FormatRuntimeSegment::Literal(lit) => buf.push_str(&lit),
                        _ => {}
                    }
                }
                Ok(buf)
            }
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_string_or_format(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects string"),
            }),
        }
    }

    fn expect_bool(&self, name: &str, value: Value) -> RuntimeResult<bool> {
        match value {
            Value::Bool(flag) => Ok(flag),
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_bool(name, cloned)
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: format!("{name} expects bool"),
            }),
        }
    }

    fn eval_index_value(&mut self, base: Value, index: Value) -> RuntimeResult<Value> {
        match base {
            Value::Slice(slice) => {
                let idx = self.expect_int_value("index", index)?;
                if idx < 0 {
                    return self.instantiate_enum("Option", "None", Vec::new());
                }
                match slice.get(idx as usize) {
                    Some(value) => self.instantiate_enum("Option", "Some", vec![value]),
                    None => self.instantiate_enum("Option", "None", Vec::new()),
                }
            }
            Value::Map(map) => {
                let key = self.expect_string("index", index)?;
                match map.get(&key) {
                    Some(value) => self.instantiate_enum("Option", "Some", vec![value]),
                    None => self.instantiate_enum("Option", "None", Vec::new()),
                }
            }
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.eval_index_value(cloned, index)
            }
            Value::Pointer(pointer) => {
                let cloned = pointer.cell.lock().unwrap().clone();
                self.eval_index_value(cloned, index)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!("`{}` cannot be indexed", self.describe_value(&other)),
            }),
        }
    }

    fn assign_index_value(&mut self, base: Value, index: Value, value: Value) -> RuntimeResult<()> {
        match base {
            Value::Slice(slice) => {
                let idx = self.expect_int_value("index", index)?;
                if idx < 0 {
                    return Err(RuntimeError::Panic {
                        message: "slice index cannot be negative".into(),
                    });
                }
                if slice.set(idx as usize, value) {
                    Ok(())
                } else {
                    Err(RuntimeError::Panic {
                        message: format!("slice index {} out of bounds", idx),
                    })
                }
            }
            Value::Map(map) => {
                let key = self.expect_string("index", index)?;
                map.insert(key, value);
                Ok(())
            }
            Value::Reference(reference) => {
                if !reference.mutable {
                    return Err(RuntimeError::Panic {
                        message: "Cannot assign through immutable reference".into(),
                    });
                }
                let inner = reference.cell.lock().unwrap().clone();
                self.assign_index_value(inner, index, value)
            }
            Value::Pointer(pointer) => {
                if !pointer.mutable {
                    return Err(RuntimeError::Panic {
                        message: "Cannot assign through immutable reference".into(),
                    });
                }
                let inner = pointer.cell.lock().unwrap().clone();
                self.assign_index_value(inner, index, value)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!("`{}` cannot be indexed", self.describe_value(&other)),
            }),
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

    fn builtin_map_keys(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("map_keys", &args, 1)?;
        let map = self.expect_map("map_keys", args.remove(0))?;
        let mut keys = Vec::new();
        for key in map.entries.lock().unwrap().keys() {
            keys.push(Value::String(key.clone()));
        }
        Ok(vec![Value::Slice(SliceValue::from_vec(keys))])
    }

    fn builtin_map_values(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("map_values", &args, 1)?;
        let map = self.expect_map("map_values", args.remove(0))?;
        let mut values = Vec::new();
        for value in map.entries.lock().unwrap().values() {
            values.push(value.clone());
        }
        Ok(vec![Value::Slice(SliceValue::from_vec(values))])
    }

    fn builtin_iter(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("iter", &args, 1)?;
        let target = args.remove(0);
        let items = self.iter_items_from_value(target)?;
        Ok(vec![Value::Iterator(IteratorValue::from_items(items))])
    }

    fn builtin_iter_next(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("next", &args, 1)?;
        let iter = self.expect_iterator("next", args.remove(0))?;
        match iter.next() {
            Some(value) => {
                let some = self.instantiate_enum("Option", "Some", vec![value])?;
                Ok(vec![some])
            }
            None => {
                let none = self.instantiate_enum("Option", "None", Vec::new())?;
                Ok(vec![none])
            }
        }
    }

    fn builtin_len(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("len", &args, 1)?;
        let value = args.remove(0);
        match value {
            Value::Slice(slice) => Ok(vec![Value::Int(slice.len() as i128)]),
            Value::Map(map) => Ok(vec![Value::Int(map.len() as i128)]),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.builtin_len(vec![inner])
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!("`len` not supported for {}", self.describe_value(&other)),
            }),
        }
    }

    fn builtin_assert(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("assert", &args, 1)?;
        let cond = self.expect_bool("assert", args.remove(0))?;
        if !cond {
            return Err(RuntimeError::Panic {
                message: "assertion failed".into(),
            });
        }
        Ok(Vec::new())
    }

    fn builtin_assert_eq(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("assert_eq", &args, 2)?;
        let left = args.remove(0);
        let right = args.remove(0);
        let equal = self.values_equal(&left, &right)?;
        if !equal {
            return Err(RuntimeError::Panic {
                message: format!(
                    "assertion failed: left `{}` != right `{}`",
                    self.describe_value(&left),
                    self.describe_value(&right)
                ),
            });
        }
        Ok(Vec::new())
    }

    fn builtin_panic(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("panic", &args, 1)?;
        let message = self.expect_string_or_format("panic", args.remove(0))?;
        Err(RuntimeError::Panic { message })
    }

    fn builtin_expect(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("expect", &args, 2)?;
        let cond = self.expect_bool("expect", args.remove(0))?;
        let msg = self.expect_string("expect", args.remove(0))?;
        if !cond {
            return Err(RuntimeError::Panic { message: msg });
        }
        Ok(Vec::new())
    }

    fn builtin_str_len(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("str_len", &args, 1)?;
        let s = self.expect_string_or_format("str_len", args.remove(0))?;
        Ok(vec![Value::Int(s.len() as i128)])
    }

    fn builtin_str_contains(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("str_contains", &args, 2)?;
        let haystack = self.expect_string_or_format("str_contains", args.remove(0))?;
        let needle = self.expect_string_or_format("str_contains", args.remove(0))?;
        Ok(vec![Value::Bool(haystack.contains(&needle))])
    }

    fn builtin_str_trim(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("str_trim", &args, 1)?;
        let s = self.expect_string_or_format("str_trim", args.remove(0))?;
        Ok(vec![Value::String(s.trim().to_string())])
    }

    fn builtin_str_split(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("str_split", &args, 2)?;
        let input = self.expect_string_or_format("str_split", args.remove(0))?;
        let delim = self.expect_string_or_format("str_split", args.remove(0))?;
        let mut items = Vec::new();
        for part in input.split(&delim) {
            items.push(Value::String(part.to_string()));
        }
        Ok(vec![Value::Slice(SliceValue::from_vec(items))])
    }

    fn builtin_min(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("min", &args, 2)?;
        match (args.remove(0), args.remove(0)) {
            (Value::Int(a), Value::Int(b)) => Ok(vec![Value::Int(a.min(b))]),
            (Value::Float(a), Value::Float(b)) => Ok(vec![Value::Float(a.min(b))]),
            _ => Err(RuntimeError::TypeMismatch {
                message: "`min` expects numbers of the same type".into(),
            }),
        }
    }

    fn builtin_max(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("max", &args, 2)?;
        match (args.remove(0), args.remove(0)) {
            (Value::Int(a), Value::Int(b)) => Ok(vec![Value::Int(a.max(b))]),
            (Value::Float(a), Value::Float(b)) => Ok(vec![Value::Float(a.max(b))]),
            _ => Err(RuntimeError::TypeMismatch {
                message: "`max` expects numbers of the same type".into(),
            }),
        }
    }

    fn builtin_abs(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("abs", &args, 1)?;
        match args.remove(0) {
            Value::Int(v) => Ok(vec![Value::Int(v.abs())]),
            Value::Float(v) => Ok(vec![Value::Float(v.abs())]),
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.builtin_abs(vec![inner])
            }
            _ => Err(RuntimeError::TypeMismatch {
                message: "`abs` expects int or float".into(),
            }),
        }
    }

    fn builtin_channel(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("channel", &args, 0)?;
        let (tx, rx) = mpsc::channel();
        let shared_tx = Arc::new(Mutex::new(Some(tx)));
        let shared_rx = Arc::new(Mutex::new(rx));
        let sender = Value::Sender(ChannelSender::new(shared_tx.clone()));
        let receiver = Value::Receiver(ChannelReceiver::new(shared_tx, shared_rx));
        Ok(vec![sender, receiver])
    }

    fn expect_sender(&self, name: &str, value: Value) -> RuntimeResult<ChannelSender> {
        match value {
            Value::Sender(tx) => Ok(tx),
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_sender(name, cloned)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`{name}` expects Sender, found {}",
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn expect_receiver(&self, name: &str, value: Value) -> RuntimeResult<ChannelReceiver> {
        match value {
            Value::Receiver(rx) => Ok(rx),
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.expect_receiver(name, cloned)
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`{name}` expects Receiver, found {}",
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn builtin_send(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("send", &args, 2)?;
        let value = args.remove(1);
        let sender = self.expect_sender("send", args.remove(0))?;
        match sender.send(value) {
            Ok(()) => {
                let ok = self.instantiate_enum("Result", "Ok", vec![Value::Unit])?;
                Ok(vec![ok])
            }
            Err(msg) => {
                let err = self.instantiate_enum("Result", "Err", vec![Value::String(msg)])?;
                Ok(vec![err])
            }
        }
    }

    fn builtin_recv(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("recv", &args, 1)?;
        let receiver = self.expect_receiver("recv", args.remove(0))?;
        match receiver.recv() {
            Some(value) => {
                let some = self.instantiate_enum("Option", "Some", vec![value])?;
                Ok(vec![some])
            }
            None => {
                let none = self.instantiate_enum("Option", "None", Vec::new())?;
                Ok(vec![none])
            }
        }
    }

    fn builtin_recv_timeout(
        &mut self,
        mut args: Vec<Value>,
        type_args: &[TypeExpr],
    ) -> RuntimeResult<Vec<Value>> {
        if !type_args.is_empty() {
            return Err(RuntimeError::Unsupported {
                message: "`recv_timeout` does not accept type arguments".into(),
            });
        }
        self.expect_arity("recv_timeout", &args, 2)?;
        let millis = self.expect_int_value("recv_timeout", args.pop().unwrap())?;
        let receiver = self.expect_receiver("recv_timeout", args.remove(0))?;
        match receiver.recv_timeout(millis as i64) {
            Some(value) => {
                let some = self.instantiate_enum("Option", "Some", vec![value])?;
                Ok(vec![some])
            }
            None => {
                let none = self.instantiate_enum("Option", "None", Vec::new())?;
                Ok(vec![none])
            }
        }
    }

    fn builtin_close(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("close", &args, 1)?;
        match args.remove(0) {
            Value::Sender(tx) => {
                tx.close();
                Ok(vec![Value::Unit])
            }
            Value::Receiver(rx) => {
                rx.close();
                Ok(vec![Value::Unit])
            }
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.builtin_close(vec![cloned])
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`close` expects channel endpoint, found {}",
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn builtin_now_ms(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("now_ms", &args, 0)?;
        let millis = platform().now_ms();
        Ok(vec![Value::Int(millis)])
    }

    fn builtin_sleep_ms(&mut self, args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("sleep_ms", &args, 1)?;
        self.builtin_sleep(args)
    }

    fn builtin_fs_exists(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("fs_exists", &args, 1)?;
        let path = self.expect_string_or_format("fs_exists", args.remove(0))?;
        Ok(vec![Value::Bool(platform().fs_exists(&path))])
    }

    fn builtin_fs_read(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("fs_read", &args, 1)?;
        let path = self.expect_string_or_format("fs_read", args.remove(0))?;
        match platform().fs_read(&path) {
            Ok(contents) => {
                let ok = self.instantiate_enum("Result", "Ok", vec![Value::String(contents)])?;
                Ok(vec![ok])
            }
            Err(err) => {
                let err_val = self.instantiate_enum(
                    "Result",
                    "Err",
                    vec![Value::String(err.to_string())],
                )?;
                Ok(vec![err_val])
            }
        }
    }

    fn builtin_fs_write(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("fs_write", &args, 2)?;
        let path = self.expect_string_or_format("fs_write", args.remove(0))?;
        let contents = self.expect_string_or_format("fs_write", args.remove(0))?;
        match platform().fs_write(&path, &contents) {
            Ok(()) => {
                let ok = self.instantiate_enum("Result", "Ok", vec![Value::Unit])?;
                Ok(vec![ok])
            }
            Err(err) => {
                let err_val = self.instantiate_enum(
                    "Result",
                    "Err",
                    vec![Value::String(err.to_string())],
                )?;
                Ok(vec![err_val])
            }
        }
    }

    fn builtin_sleep(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("sleep", &args, 1)?;
        let millis = self.expect_int_value("sleep", args.remove(0))?;
        platform().sleep_ms(millis);
        Ok(vec![Value::Unit])
    }

    fn builtin_join(&mut self, mut args: Vec<Value>) -> RuntimeResult<Vec<Value>> {
        self.expect_arity("join", &args, 1)?;
        match args.remove(0) {
            Value::JoinHandle(handle) => {
                let value = handle
                    .join()
                    .map_err(|msg| RuntimeError::Panic { message: msg })?;
                Ok(vec![value])
            }
            Value::Reference(reference) => {
                let cloned = reference.cell.lock().unwrap().clone();
                self.builtin_join(vec![cloned])
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`join` expects JoinHandle, found {}",
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn builtin_ptr(&mut self, mut args: Vec<Value>, mutable: bool) -> RuntimeResult<Vec<Value>> {
        self.expect_arity(if mutable { "ptr_mut" } else { "ptr" }, &args, 1)?;
        let value = args.remove(0);
        match value {
            Value::Reference(reference) => Ok(vec![Value::Pointer(PointerValue {
                cell: reference.cell,
                mutable,
            })]),
            Value::Pointer(ptr) => Ok(vec![Value::Pointer(PointerValue {
                cell: ptr.cell,
                mutable: ptr.mutable || mutable,
            })]),
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`{}` expects a reference or pointer, found {}",
                    if mutable { "ptr_mut" } else { "ptr" },
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn builtin_cast(
        &mut self,
        mut args: Vec<Value>,
        type_args: &[TypeExpr],
    ) -> RuntimeResult<Vec<Value>> {
        if type_args.len() != 1 {
            return Err(RuntimeError::Unsupported {
                message: "`cast` expects one type argument".into(),
            });
        }
        self.expect_arity("cast", &args, 1)?;
        let target = &type_args[0];
        let Some(target_name) = self.numeric_kind_from_type(target) else {
            return Err(RuntimeError::Unsupported {
                message: "`cast` only supports numeric target types".into(),
            });
        };
        let value = args.remove(0);
        let casted = match (target_name, value) {
            (_, Value::Reference(reference)) => {
                let cloned = reference.cell.lock().unwrap().clone();
                return self.builtin_cast(vec![cloned], type_args);
            }
            ("float32" | "float64", Value::Int(i)) => Value::Float(i as f64),
            ("float32" | "float64", Value::Float(f)) => Value::Float(f),
            (target, Value::Float(f))
                if target.starts_with("int") || target.starts_with("uint") =>
            {
                Value::Int(f as i128)
            }
            (target, Value::Int(i)) if target.starts_with("int") || target.starts_with("uint") => {
                Value::Int(i)
            }
            (_, other) => {
                return Err(RuntimeError::TypeMismatch {
                    message: format!(
                        "`cast` only supports numeric values (got {})",
                        self.describe_value(&other)
                    ),
                });
            }
        };
        Ok(vec![casted])
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
                let inner = reference.cell.lock().unwrap().clone();
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
                        self.declare_with_drop(
                            name,
                            value,
                            stmt.mutability == Mutability::Mutable,
                        )?;
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
                                *reference.cell.lock().unwrap() = value;
                            }
                            Value::Pointer(pointer) => {
                                if !pointer.mutable {
                                    return Err(RuntimeError::Panic {
                                        message: "Cannot assign through immutable reference".into(),
                                    });
                                }
                                let value = match self.eval_expression(&stmt.value)? {
                                    EvalOutcome::Value(value) => value,
                                    EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                                };
                                *pointer.cell.lock().unwrap() = value;
                            }
                            _ => {
                                return Err(RuntimeError::TypeMismatch {
                                    message: "Cannot assign through non-reference value".into(),
                                });
                            }
                        }
                    }
                    Expr::Index { base, index, .. } => {
                        let target = match self.eval_expression(base)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        let index_value = match self.eval_expression(index)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        let value = match self.eval_expression(&stmt.value)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(Some(flow)),
                        };
                        self.assign_index_value(target, index_value, value)?;
                    }
                    _ => {
                        return Err(RuntimeError::Unsupported {
                            message:
                                "Only identifier, dereference, or index assignments are supported"
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
            Statement::MacroSemi(stmt) => match self.eval_expression(&stmt.node)? {
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
                        if let Some(flow) = self.execute_cleanups()? {
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
            Statement::Loop(loop_stmt) => {
                loop {
                    let result = self.eval_block(&loop_stmt.body)?;
                    match result {
                        BlockEval::Value(_) => {}
                        BlockEval::Flow(FlowSignal::Continue) => continue,
                        BlockEval::Flow(FlowSignal::Break) => break,
                        BlockEval::Flow(flow @ FlowSignal::Return(_)) => return Ok(Some(flow)),
                        BlockEval::Flow(flow @ FlowSignal::Propagate(_)) => return Ok(Some(flow)),
                    }
                }
                Ok(None)
            }
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
                        self.declare_with_drop(&stmt.binding, Value::Int(i), false)?;
                        let result = self.eval_block(&stmt.body)?;
                        if let Some(flow) = self.execute_cleanups()? {
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
                        self.declare_with_drop(&stmt.binding, element, false)?;
                        let result = self.eval_block(&stmt.body)?;
                        if let Some(flow) = self.execute_cleanups()? {
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
                if let Some(defer_flow) = self.execute_cleanups()? {
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
        if let Some(flow) = self.execute_cleanups()? {
            self.env.pop_scope();
            return Ok(BlockEval::Flow(flow));
        }
        self.env.pop_scope();
        Ok(outcome)
    }

    fn execute_cleanups(&mut self) -> RuntimeResult<Option<FlowSignal>> {
        let mut cleanups = self.env.drain_cleanups();
        let mut pending_flow: Option<FlowSignal> = None;
        while let Some(action) = cleanups.pop() {
            match action {
                CleanupAction::Defer(expr) => match self.eval_expression(&expr)? {
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
                },
                CleanupAction::Drop(record) => {
                    self.run_drop(record)?;
                }
            }
        }
        Ok(pending_flow)
    }

    fn run_drop(&mut self, record: DropRecord) -> RuntimeResult<()> {
        let Some(key) = self.drop_impls.get(&record.type_name).cloned() else {
            return Ok(());
        };
        let Some((cell, _)) = self.env.get_cell(&record.binding) else {
            return Ok(());
        };
        {
            let guard = cell.lock().unwrap();
            if matches!(*guard, Value::Moved) {
                return Ok(());
            }
            if let Value::Struct(_instance) = &*guard {}
        }
        let reference = Value::Reference(ReferenceValue {
            cell: cell.clone(),
            mutable: true,
            origin: Some(record.binding.clone()),
        });
        let prev = self.suppress_drop_schedule;
        self.suppress_drop_schedule = true;
        let _ = self.call_function(&key.name, key.receiver.clone(), &[], vec![reference])?;
        self.suppress_drop_schedule = prev;
        Ok(())
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

    fn declare_with_drop(
        &mut self,
        name: &str,
        value: Value,
        mutable: bool,
    ) -> RuntimeResult<()> {
        self.env.declare(name, value.clone(), mutable)?;
        if !self.suppress_drop_schedule {
            self.schedule_drop_for_value(name, &value);
        }
        Ok(())
    }

    fn schedule_drop_for_value(&mut self, name: &str, value: &Value) {
        if let Some(type_name) = self.drop_type_for_value(value) {
            if self.drop_impls.contains_key(&type_name) {
                self.env.schedule_drop(DropRecord {
                    binding: name.to_string(),
                    type_name,
                });
            }
        }
    }

    fn value_struct_name(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Reference(reference) => self.value_struct_name(&reference.cell.lock().unwrap()),
            Value::Pointer(pointer) => self.value_struct_name(&pointer.cell.lock().unwrap()),
            Value::Boxed(inner) => self.value_struct_name(&inner.cell.lock().unwrap()),
            _ => None,
        }
    }

    fn drop_type_for_value(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(instance) => Some(instance.name.clone()),
            Value::Enum(enum_value) => Some(enum_value.enum_name.clone()),
            _ => None,
        }
    }

    fn eval_expression(&mut self, expr: &Expr) -> RuntimeResult<EvalOutcome<Value>> {
        match expr {
            Expr::Identifier(ident) => {
                let value = self
                    .env
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
            Expr::FormatString(literal) => {
                let value = self.evaluate_format_string(literal)?;
                Ok(EvalOutcome::Value(value))
            }
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
            } => {
                let arg_values = match self.eval_arguments(args)? {
                    EvalOutcome::Value(values) => values,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                match callee.as_ref() {
                    Expr::Identifier(ident) => {
                        if let Some(variant) = self.enum_variants.get(&ident.name) {
                            let enum_name = variant.enum_name.clone();
                            let variant_name = ident.name.clone();
                            let value =
                                self.instantiate_enum(&enum_name, &variant_name, arg_values)?;
                            return Ok(EvalOutcome::Value(value));
                        }
                        if let Some(value) = self.env.get(&ident.name) {
                            if let Value::Closure(closure) = value {
                                let value = self.call_closure_value(&closure, arg_values)?;
                                return Ok(EvalOutcome::Value(value));
                            }
                        }
                        let results =
                            self.call_function(&ident.name, None, type_args, arg_values)?;
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
                        let mut method_args = arg_values.clone();
                        method_args.insert(0, receiver);
                        if let Some(result) = self.call_builtin_method(field, method_args.clone()) {
                            let values = result?;
                            let value = match values.len() {
                                0 => Value::Unit,
                                1 => values.into_iter().next().unwrap(),
                                _ => Value::Tuple(values),
                            };
                            return Ok(EvalOutcome::Value(value));
                        }
                        let results =
                            self.call_function(field, receiver_type, type_args, method_args)?;
                        let value = match results.len() {
                            0 => Value::Unit,
                            1 => results.into_iter().next().unwrap(),
                            _ => Value::Tuple(results),
                        };
                        Ok(EvalOutcome::Value(value))
                    }
                    _ => {
                        let callee_value = match self.eval_expression(callee)? {
                            EvalOutcome::Value(value) => value,
                            EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                        };
                        if let Value::Closure(closure) = callee_value {
                            let value = self.call_closure_value(&closure, arg_values)?;
                            Ok(EvalOutcome::Value(value))
                        } else {
                            Err(RuntimeError::Unsupported {
                                message: "Unsupported call target".into(),
                            })
                        }
                    }
                }
            }
            Expr::FieldAccess { base, field, .. } => {
                let value = match self.eval_expression(base)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let struct_value = match value {
                    Value::Struct(instance) => Ok(instance),
                    Value::Reference(reference) => {
                        let inner = reference.cell.lock().unwrap().clone();
                        match inner {
                            Value::Struct(instance) => Ok(instance),
                            other => Err(RuntimeError::TypeMismatch {
                                message: format!(
                                    "Field access requires struct value (found {})",
                                    other
                                ),
                            }),
                        }
                    }
                    Value::Pointer(pointer) => {
                        let inner = pointer.cell.lock().unwrap().clone();
                        match inner {
                            Value::Struct(instance) => Ok(instance),
                            other => Err(RuntimeError::TypeMismatch {
                                message: format!(
                                    "Field access requires struct value (found {})",
                                    other
                                ),
                            }),
                        }
                    }
                    Value::Boxed(inner) => {
                        let inner = inner.cell.lock().unwrap().clone();
                        match inner {
                            Value::Struct(instance) => Ok(instance),
                            other => Err(RuntimeError::TypeMismatch {
                                message: format!(
                                    "Field access requires struct value (found {})",
                                    other
                                ),
                            }),
                        }
                    }
                    other => Err(RuntimeError::TypeMismatch {
                        message: format!(
                            "Field access requires struct value (found {})",
                            self.describe_value(&other)
                        ),
                    }),
                }?;
                let struct_name = struct_value.name.clone();
                let field_value = struct_value.get_field(field).ok_or_else(|| {
                    RuntimeError::UnknownSymbol {
                        name: format!("{}::{}", struct_name, field),
                    }
                })?;
                Ok(EvalOutcome::Value(field_value))
            }
            Expr::StructLiteral { name, fields, .. } => self.instantiate_struct(name, fields),
            Expr::EnumLiteral {
                enum_name,
                variant,
                values,
                ..
            } => {
                let mut evaluated = Vec::new();
                for expr in values {
                    match self.eval_expression(expr)? {
                        EvalOutcome::Value(value) => evaluated.push(value),
                        EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                    }
                }
                let enum_name = enum_name.clone().unwrap_or_else(|| variant.clone());
                self.instantiate_enum(&enum_name, variant, evaluated)
                    .map(EvalOutcome::Value)
            }
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
            Expr::Index { base, index, .. } => {
                let base_value = match self.eval_expression(base)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let index_value = match self.eval_expression(index)? {
                    EvalOutcome::Value(value) => value,
                    EvalOutcome::Flow(flow) => return Ok(EvalOutcome::Flow(flow)),
                };
                let value = self.eval_index_value(base_value, index_value)?;
                Ok(EvalOutcome::Value(value))
            }
            Expr::Reference { mutable, expr, .. } => self.build_reference(expr, *mutable),
            Expr::Deref { expr, .. } => match self.eval_expression(expr)? {
                EvalOutcome::Value(Value::Reference(reference)) => {
                    Ok(EvalOutcome::Value(reference.cell.lock().unwrap().clone()))
                }
                EvalOutcome::Value(Value::Pointer(pointer)) => {
                    Ok(EvalOutcome::Value(pointer.cell.lock().unwrap().clone()))
                }
                EvalOutcome::Value(_) => Err(RuntimeError::TypeMismatch {
                    message: "Cannot dereference non-reference value".into(),
                }),
                EvalOutcome::Flow(flow) => Ok(EvalOutcome::Flow(flow)),
            },
            Expr::ArrayLiteral(values, _) => self.eval_array_literal(values),
            Expr::Move { expr, .. } => self.eval_move_expression(expr).map(EvalOutcome::Value),
            Expr::Spawn { expr, .. } => {
                self.bootstrap()?;
                let expr_clone = expr.clone();
                let child = self.clone_for_spawn();
                let handle = thread::spawn(move || {
                    let mut runner = child;
                    match runner.eval_expression(&expr_clone) {
                        Ok(EvalOutcome::Value(value)) => Ok(value),
                        Ok(EvalOutcome::Flow(FlowSignal::Return(values))) => {
                            if values.len() == 1 {
                                Ok(values.into_iter().next().unwrap())
                            } else {
                                Ok(Value::Tuple(values))
                            }
                        }
                        Ok(EvalOutcome::Flow(flow @ FlowSignal::Propagate(_))) => {
                            Err(RuntimeError::Panic {
                                message: format!("spawned task encountered {}", flow_name(&flow)),
                            })
                        }
                        Ok(EvalOutcome::Flow(flow)) => Err(RuntimeError::Panic {
                            message: format!("spawned task exited with {}", flow_name(&flow)),
                        }),
                        Err(err) => Err(err),
                    }
                });
                Ok(EvalOutcome::Value(Value::JoinHandle(Box::new(
                    JoinHandleValue::new(handle),
                ))))
            }
            Expr::Closure {
                params,
                body,
                ret,
                captures,
                ..
            } => {
                let mut captured_values = Vec::new();
                for captured in captures.read().unwrap().iter() {
                    let value = self.env.get(&captured.name).ok_or_else(|| {
                        RuntimeError::UnknownSymbol {
                            name: captured.name.clone(),
                        }
                    })?;
                    captured_values.push(CapturedValue {
                        name: captured.name.clone(),
                        value,
                        mutable: captured.mutable,
                    });
                }
                Ok(EvalOutcome::Value(Value::Closure(ClosureValue {
                    params: params.clone(),
                    body: body.clone(),
                    ret: ret.clone(),
                    captures: captured_values,
                })))
            }
            Expr::MacroCall { name, .. } => Err(RuntimeError::Unsupported {
                message: format!(
                    "macro `{}` must be expanded before interpretation",
                    name.name
                ),
            }),
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
                if matches!(*cell.lock().unwrap(), Value::Moved) {
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
                EvalOutcome::Value(value) => match value {
                    Value::Sender(_) | Value::Receiver(_) | Value::JoinHandle(_) => {
                        Err(RuntimeError::TypeMismatch {
                            message: "Cannot take reference to channel or join handle".into(),
                        })
                    }
                    other => Ok(EvalOutcome::Value(Value::Reference(ReferenceValue {
                        cell: Arc::new(Mutex::new(other)),
                        mutable,
                        origin: None,
                    }))),
                },
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
                    if let Value::String(text) = reference.cell.lock().unwrap().clone() {
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
                let mut slot = cell.lock().unwrap();
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
                    if !guard_passed {
                        if let Some(flow) = self.execute_cleanups()? {
                            self.env.pop_scope();
                            return Ok(EvalOutcome::Flow(flow));
                        }
                        self.env.pop_scope();
                        continue;
                    }
                }
                let outcome = self.eval_expression(&arm.value)?;
                if let Some(flow) = self.execute_cleanups()? {
                    self.env.pop_scope();
                    return Ok(EvalOutcome::Flow(flow));
                }
                self.env.pop_scope();
                return Ok(outcome);
            }
            if let Some(flow) = self.execute_cleanups()? {
                self.env.pop_scope();
                return Ok(EvalOutcome::Flow(flow));
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
                    other => other.clone(),
                };
                self.declare_with_drop(name, concrete, mutable_bindings)?;
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
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
                    Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
                    other => other.clone(),
                };
                let elements = match concrete {
                    Value::Slice(slice) => slice.items.lock().unwrap().clone(),
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
            Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
            other => other.clone(),
        };
        let right_val = match right {
            Value::Reference(reference) => reference.cell.lock().unwrap().clone(),
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
                    let outcome = match self.eval_block(&if_expr.then_branch)? {
                        BlockEval::Value(value) => EvalOutcome::Value(value),
                        BlockEval::Flow(flow) => EvalOutcome::Flow(flow),
                    };
                    if let Some(flow) = self.execute_cleanups()? {
                        self.env.pop_scope();
                        return Ok(EvalOutcome::Flow(flow));
                    }
                    self.env.pop_scope();
                    Ok(outcome)
                } else {
                    if let Some(flow) = self.execute_cleanups()? {
                        self.env.pop_scope();
                        return Ok(EvalOutcome::Flow(flow));
                    }
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

    fn collect_iterable_values(&mut self, value: Value) -> RuntimeResult<Vec<Value>> {
        match value {
            Value::Range(range) => {
                let end = if range.inclusive {
                    range.end + 1
                } else {
                    range.end
                };
                Ok((range.start..end).map(Value::Int).collect())
            }
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
                for (key, value) in map.entries.lock().unwrap().iter() {
                    items.push(Value::Tuple(vec![
                        Value::String(key.clone()),
                        value.clone(),
                    ]));
                }
                Ok(items)
            }
            Value::Reference(reference) => {
                let inner = reference.cell.lock().unwrap().clone();
                self.collect_iterable_values(inner)
            }
            Value::Iterator(iter) => {
                let mut items = Vec::new();
                while let Some(value) = iter.next() {
                    items.push(value);
                }
                Ok(items)
            }
            Value::Pointer(pointer) => {
                let inner = pointer.cell.lock().unwrap().clone();
                self.collect_iterable_values(inner)
            }
            Value::Struct(_) => {
                if let Some(struct_name) = self.value_struct_name(&value) {
                    let iter_outcome =
                        self.call_function("iter", Some(struct_name), &[], vec![value])?;
                    if iter_outcome.len() != 1 {
                        return Err(RuntimeError::Panic {
                            message: "`iter` must return a single iterable value".into(),
                        });
                    }
                    self.collect_iterable_values(iter_outcome.into_iter().next().unwrap())
                } else {
                    Err(RuntimeError::TypeMismatch {
                        message: "`iter` requires a struct receiver".into(),
                    })
                }
            }
            other => Err(RuntimeError::TypeMismatch {
                message: format!(
                    "`for ... in` only supports ranges, slices, maps, or values with iter(); found {}",
                    self.describe_value(&other)
                ),
            }),
        }
    }

    fn evaluate_format_string(&mut self, literal: &FormatStringLiteral) -> RuntimeResult<Value> {
        let mut segments = Vec::new();
        let mut implicit = 0usize;
        let mut rendered = String::new();
        let mut has_placeholders = false;
        for segment in &literal.segments {
            match segment {
                FormatSegment::Literal(text) => {
                    rendered.push_str(text);
                    segments.push(FormatRuntimeSegment::Literal(text.clone()));
                }
                FormatSegment::Expr { expr, .. } => {
                    has_placeholders = true;
                    let value = match self.eval_expression(expr)? {
                        EvalOutcome::Value(value) => value,
                        EvalOutcome::Flow(_) => {
                            return Err(RuntimeError::Panic {
                                message: "control flow not allowed inside format string".into(),
                            });
                        }
                    };
                    segments.push(FormatRuntimeSegment::Named(value));
                }
                FormatSegment::Implicit(_) => {
                    has_placeholders = true;
                    implicit += 1;
                    segments.push(FormatRuntimeSegment::Implicit);
                }
            }
        }
        if !has_placeholders {
            Ok(Value::String(rendered))
        } else {
            Ok(Value::FormatTemplate(FormatTemplateValue {
                segments,
                implicit_placeholders: implicit,
            }))
        }
    }

    fn format_value(&self, value: &Value) -> String {
        format!("{value}")
    }

    fn print_format_template(
        &mut self,
        template: FormatTemplateValue,
        mut values: Vec<Value>,
    ) -> RuntimeResult<()> {
        let mut value_iter = values.drain(..);
        let mut output = String::new();
        for segment in template.segments {
            match segment {
                FormatRuntimeSegment::Literal(text) => output.push_str(&text),
                FormatRuntimeSegment::Named(value) => output.push_str(&self.format_value(&value)),
                FormatRuntimeSegment::Implicit => {
                    if let Some(value) = value_iter.next() {
                        output.push_str(&self.format_value(&value));
                    }
                }
            }
        }
        println!("{output}");
        Ok(())
    }

    fn render_format_template(
        &mut self,
        template: FormatTemplateValue,
        mut values: Vec<Value>,
    ) -> RuntimeResult<String> {
        let mut value_iter = values.drain(..);
        let mut output = String::new();
        for segment in template.segments {
            match segment {
                FormatRuntimeSegment::Literal(text) => output.push_str(&text),
                FormatRuntimeSegment::Named(value) => output.push_str(&self.format_value(&value)),
                FormatRuntimeSegment::Implicit => {
                    if let Some(value) = value_iter.next() {
                        output.push_str(&self.format_value(&value));
                    }
                }
            }
        }
        Ok(output)
    }

    fn populate_test_inputs(queue: &mut VecDeque<String>, debug: bool, allow_default: bool) {
        if let Ok(raw) = env::var("PRIME_TEST_INPUTS") {
            if !raw.is_empty() {
                for chunk in raw.split(|c| c == '|' || c == ',' || c == ';') {
                    let trimmed = chunk.trim().to_string();
                    if debug {
                        eprintln!("[prime-test-input:init] push '{trimmed}'");
                    }
                    queue.push_back(trimmed);
                }
            } else {
                if debug {
                    eprintln!("[prime-test-input:init] push empty");
                }
                queue.push_back(String::new());
            }
        } else if let Ok(path) = env::var("PRIME_TEST_INPUTS_FILE") {
            if let Ok(contents) = fs::read_to_string(path) {
                for line in contents.lines() {
                    let trimmed = line.trim_end_matches(&['\r', '\n'][..]).to_string();
                    if debug {
                        eprintln!("[prime-test-input:init] push '{trimmed}'");
                    }
                    queue.push_back(trimmed);
                }
            }
        } else if debug {
            eprintln!("[prime-test-input:init] no PRIME_TEST_INPUTS/FILE set");
        }

        if allow_default && queue.is_empty() {
            let defaults = [
                "21",      // int32 ok
                "abc",     // int32 err
                "true",    // bool ok
                "maybe",   // bool err
                "98.6",    // float64 ok
                "nope",    // float64 err
                "Prime",   // string ok
                "Y",       // rune ok
                "200",     // int8 err
                "42",      // uint8 ok
                "-1",      // uint8 err
                "500",     // uint16 ok
                "70000",   // uint32 ok
                "1000000", // uint64 ok
                "128",     // usize ok
                "3.14",    // float32 ok
                "badf",    // float32 err
            ];
            for value in defaults {
                if debug {
                    eprintln!("[prime-test-input:init] default '{value}'");
                }
                queue.push_back(value.to_string());
            }
        }
    }

    fn next_test_input() -> Option<String> {
        let queue = TEST_INPUTS.get_or_init(|| Mutex::new(VecDeque::new()));
        let mut guard = queue.lock().unwrap();
        if guard.is_empty() {
            let debug = env::var("PRIME_TEST_INPUTS_DEBUG").is_ok();
            Self::populate_test_inputs(&mut guard, debug, false);
        }
        guard.pop_front()
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
            Value::FormatTemplate(_) => "format string",
            Value::Sender(_) => "channel sender",
            Value::Receiver(_) => "channel receiver",
            Value::Iterator(_) => "iterator",
            Value::Pointer(_) => "pointer",
            Value::JoinHandle(_) => "join handle",
            Value::Closure(_) => "closure",
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
    use crate::project::{Package, load_package};
    use std::path::{Path, PathBuf};

    fn interpreter_from_source(source: &str) -> Interpreter {
        let module =
            parse_module("tests::runtime", PathBuf::from("test.prime"), source).expect("parse");
        let program = Program {
            modules: vec![module],
        };
        let mut interpreter = Interpreter::new(Package {
            program,
            modules: Vec::new(),
        });
        interpreter.bootstrap().expect("bootstrap");
        interpreter
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

    #[test]
    fn interpreter_builds_format_template_values() {
        use crate::language::span::Span;

        let mut interpreter = interpreter_from_source(
            r#"
module tests::runtime;

fn stub() {}
"#,
        );
        interpreter.bootstrap().expect("bootstrap");
        interpreter
            .env
            .declare("hp", Value::Int(12), false)
            .expect("declare hp");

        let literal = FormatStringLiteral {
            segments: vec![
                FormatSegment::Literal("HP ".into()),
                FormatSegment::Expr {
                    expr: Expr::Identifier(Identifier {
                        name: "hp".into(),
                        span: Span::new(0, 0),
                    }),
                    span: Span::new(0, 0),
                },
                FormatSegment::Literal(" delta ".into()),
                FormatSegment::Implicit(Span::new(0, 0)),
            ],
            span: Span::new(0, 0),
        };

        let value = interpreter
            .evaluate_format_string(&literal)
            .expect("format literal");
        match value {
            Value::FormatTemplate(template) => {
                assert_eq!(template.implicit_placeholders, 1);
                assert!(matches!(
                    template.segments.first(),
                    Some(FormatRuntimeSegment::Literal(text)) if text == "HP "
                ));
                assert!(matches!(
                    template.segments.get(1),
                    Some(FormatRuntimeSegment::Named(Value::Int(value))) if *value == 12
                ));
            }
            other => panic!("expected format template value, got {:?}", other),
        }
    }

    #[test]
    fn interpreter_out_requires_all_arguments() {
        let source = r#"
module tests::runtime;

fn broken() {
  out(`numbers {} {} {}`, 1, 2);
}
"#;
        let mut interpreter = interpreter_from_source(source);
        interpreter.bootstrap().expect("bootstrap");
        let err = interpreter
            .call_function("broken", None, &[], Vec::new())
            .expect_err("expected runtime error");
        match err {
            RuntimeError::ArityMismatch {
                name,
                expected,
                received,
            } => {
                assert_eq!(name, "out");
                assert_eq!(expected, 3);
                assert_eq!(received, 2);
            }
            other => panic!("unexpected error: {:?}", other),
        }
    }

    fn interpreter_from_entry(entry: &str) -> Interpreter {
        let package = load_package(Path::new(entry)).expect(&format!("load package for {}", entry));
        Interpreter::new(package)
    }

    #[test]
    fn borrow_demo_executes_successfully() {
        let mut interpreter = interpreter_from_entry("workspace/demos/borrow/borrow_demo.prime");
        interpreter.bootstrap().expect("bootstrap");
        interpreter
            .call_function("main", None, &[], Vec::new())
            .expect("run borrow demo");
    }

    #[test]
    fn pattern_demo_executes_successfully() {
        let mut interpreter = interpreter_from_entry("workspace/demos/patterns/pattern_demo.prime");
        interpreter.bootstrap().expect("bootstrap");
        interpreter
            .call_function("main", None, &[], Vec::new())
            .expect("run pattern demo");
    }

    #[test]
    fn closures_capture_and_call() {
        let source = r#"
module tests::runtime;

fn make_adder() -> int32 {
  let int32 base = 2;
  let add = |y: int32| base + y;
  add(3)
}
"#;
        let mut interpreter = interpreter_from_source(source);
        let values = interpreter
            .call_function("make_adder", None, &[], Vec::new())
            .expect("run closure");
        assert_eq!(values.len(), 1);
        assert!(matches!(values[0], Value::Int(5)));
    }
}
fn receiver_type_name(def: &FunctionDef, structs: &HashMap<String, StructEntry>) -> Option<String> {
    def.params
        .first()
        .and_then(|param| param.ty.as_ref())
        .and_then(type_name_from_annotation)
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
        if let Some(ty) = param.ty.as_mut() {
            *ty = ty.replace_self(&concrete);
        }
    }
    for ret in &mut def.returns {
        *ret = ret.replace_self(&concrete);
    }
}
