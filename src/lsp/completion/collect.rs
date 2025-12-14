use super::*;

#[derive(Default, Clone)]
struct RawStructInfo {
    fields: Vec<StructFieldInfo>,
    embedded: Vec<String>,
    methods: Vec<MethodInfo>,
}

#[derive(Clone)]
struct RawStructEntry {
    module_name: String,
    info: RawStructInfo,
}

pub fn select_struct_info<'a>(
    structs: &'a StructInfoMap,
    name: &str,
    module_name: Option<&str>,
) -> Option<&'a StructInfo> {
    let list = structs.get(name)?;
    if let Some(module_name) = module_name {
        if let Some(info) = list.iter().find(|info| info.module_name == module_name) {
            return Some(info);
        }
    }
    list.first()
}

pub fn collect_struct_info(modules: &[Module]) -> StructInfoMap {
    let mut raw: HashMap<String, Vec<RawStructEntry>> = HashMap::new();
    for module in modules {
        for item in &module.items {
            if let Item::Struct(def) = item {
                let mut struct_fields = Vec::new();
                let mut embedded = Vec::new();
                for field in &def.fields {
                    if let Some(name) = &field.name {
                        struct_fields.push(StructFieldInfo {
                            name: name.clone(),
                            ty: field.ty.ty.clone(),
                            declared_in: def.name.clone(),
                        });
                    } else if field.embedded {
                        if let Some(target) = struct_name_from_type(&field.ty.ty) {
                            embedded.push(target.to_string());
                        }
                    }
                }
                raw.entry(def.name.clone())
                    .or_default()
                    .push(RawStructEntry {
                        module_name: module.name.clone(),
                        info: RawStructInfo {
                            fields: struct_fields,
                            embedded,
                            methods: Vec::new(),
                        },
                    });
            }
        }
    }
    for module in modules {
        for item in &module.items {
            if let Item::Function(func) = item {
                if let Some(first_param) = func.params.first() {
                    if let Some(receiver) = first_param
                        .ty
                        .as_ref()
                        .and_then(|ty| receiver_type_name(&ty.ty))
                    {
                        if let Some(entry) =
                            select_raw_struct_entry_mut(&mut raw, &receiver, &module.name)
                        {
                            entry.info.methods.push(MethodInfo {
                                name: func.name.clone(),
                                signature: format_function_signature(func),
                                declared_in: receiver,
                            });
                        }
                    }
                }
            } else if let Item::Impl(block) = item {
                let target = block.target.clone();
                for method in &block.methods {
                    if let Some(first_param) = method.params.first() {
                        if let Some(receiver) = first_param
                            .ty
                            .as_ref()
                            .and_then(|ty| receiver_type_name(&ty.ty))
                        {
                            if receiver == target {
                                if let Some(entry) =
                                    select_raw_struct_entry_mut(&mut raw, &receiver, &module.name)
                                {
                                    entry.info.methods.push(MethodInfo {
                                        name: method.name.clone(),
                                        signature: format_function_signature(method),
                                        declared_in: receiver.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    let mut info: StructInfoMap = HashMap::new();
    let mut fields_cache: HashMap<(String, String), Vec<StructFieldInfo>> = HashMap::new();
    let mut methods_cache: HashMap<(String, String), Vec<MethodInfo>> = HashMap::new();
    let struct_names: Vec<String> = raw.keys().cloned().collect();
    for name in struct_names {
        if let Some(entries) = raw.get(&name) {
            for entry in entries {
                let mut field_stack = HashSet::new();
                let fields = flatten_struct_fields(
                    &name,
                    &entry.module_name,
                    &raw,
                    &mut fields_cache,
                    &mut field_stack,
                );
                let mut method_stack = HashSet::new();
                let methods = flatten_struct_methods(
                    &name,
                    &entry.module_name,
                    &raw,
                    &mut methods_cache,
                    &mut method_stack,
                );
                info.entry(name.clone()).or_default().push(StructInfo {
                    module_name: entry.module_name.clone(),
                    fields,
                    methods,
                });
            }
        }
    }
    info
}

pub fn collect_interface_info(modules: &[Module]) -> HashMap<String, Vec<InterfaceInfo>> {
    let mut map: HashMap<String, Vec<InterfaceInfo>> = HashMap::new();
    for module in modules {
        for item in &module.items {
            if let Item::Interface(def) = item {
                map.entry(def.name.clone())
                    .or_default()
                    .push(InterfaceInfo {
                        module_name: module.name.clone(),
                        type_params: def.type_params.clone(),
                        methods: def.methods.clone(),
                    });
            }
        }
    }
    map
}

pub fn select_interface_info<'a>(
    interfaces: &'a HashMap<String, Vec<InterfaceInfo>>,
    name: &str,
    module_name: &str,
) -> Option<&'a InterfaceInfo> {
    let list = interfaces.get(name)?;
    list.iter()
        .find(|info| info.module_name == module_name)
        .or_else(|| list.first())
}

fn select_raw_struct_entry<'a>(
    raw: &'a HashMap<String, Vec<RawStructEntry>>,
    name: &str,
    module_name: &str,
) -> Option<&'a RawStructEntry> {
    raw.get(name).and_then(|entries| {
        entries
            .iter()
            .find(|entry| entry.module_name == module_name)
            .or_else(|| entries.first())
    })
}

fn select_raw_struct_entry_mut<'a>(
    raw: &'a mut HashMap<String, Vec<RawStructEntry>>,
    name: &str,
    module_name: &str,
) -> Option<&'a mut RawStructEntry> {
    let entries = raw.get_mut(name)?;
    if entries.len() == 1 {
        return entries.first_mut();
    }
    let idx = entries
        .iter()
        .position(|entry| entry.module_name == module_name)
        .unwrap_or(0);
    entries.get_mut(idx)
}

fn flatten_struct_fields(
    name: &str,
    module_name: &str,
    raw: &HashMap<String, Vec<RawStructEntry>>,
    cache: &mut HashMap<(String, String), Vec<StructFieldInfo>>,
    stack: &mut HashSet<(String, String)>,
) -> Vec<StructFieldInfo> {
    let key = (module_name.to_string(), name.to_string());
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if !stack.insert(key.clone()) {
        return Vec::new();
    }
    let mut fields = Vec::new();
    if let Some(entry) = select_raw_struct_entry(raw, name, module_name) {
        fields.extend(entry.info.fields.clone());
        for embedded in &entry.info.embedded {
            if let Some(next_entry) = select_raw_struct_entry(raw, embedded, module_name) {
                fields.extend(flatten_struct_fields(
                    embedded,
                    &next_entry.module_name,
                    raw,
                    cache,
                    stack,
                ));
            }
        }
    }
    stack.remove(&key);
    cache.insert(key, fields.clone());
    fields
}

fn flatten_struct_methods(
    name: &str,
    module_name: &str,
    raw: &HashMap<String, Vec<RawStructEntry>>,
    cache: &mut HashMap<(String, String), Vec<MethodInfo>>,
    stack: &mut HashSet<(String, String)>,
) -> Vec<MethodInfo> {
    let key = (module_name.to_string(), name.to_string());
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if !stack.insert(key.clone()) {
        return Vec::new();
    }
    let mut methods = Vec::new();
    if let Some(entry) = select_raw_struct_entry(raw, name, module_name) {
        methods.extend(entry.info.methods.clone());
        for embedded in &entry.info.embedded {
            if let Some(next_entry) = select_raw_struct_entry(raw, embedded, module_name) {
                methods.extend(flatten_struct_methods(
                    embedded,
                    &next_entry.module_name,
                    raw,
                    cache,
                    stack,
                ));
            }
        }
    }
    stack.remove(&key);
    cache.insert(key, methods.clone());
    methods
}
