use super::*;

#[derive(Default)]
pub(super) struct Documents {
    inner: RwLock<HashMap<Uri, String>>,
}

impl Documents {
    pub(super) async fn insert(&self, uri: Uri, text: String) {
        self.inner.write().await.insert(uri, text);
    }

    pub(super) async fn remove(&self, uri: &Uri) {
        self.inner.write().await.remove(uri);
    }

    pub(super) async fn get(&self, uri: &Uri) -> Option<String> {
        self.inner.read().await.get(uri).cloned()
    }
}

#[derive(Default)]
pub(super) struct ModuleCache {
    inner: RwLock<HashMap<Uri, Module>>,
}

impl ModuleCache {
    pub(super) async fn insert(&self, uri: Uri, module: Module) {
        self.inner.write().await.insert(uri, module);
    }

    pub(super) async fn get(&self, uri: &Uri) -> Option<Module> {
        self.inner.read().await.get(uri).cloned()
    }

    pub(super) async fn snapshot(&self) -> Vec<(Uri, Module)> {
        self.inner
            .read()
            .await
            .iter()
            .map(|(uri, module)| (uri.clone(), module.clone()))
            .collect()
    }
}

#[derive(Clone)]
pub(super) struct SymbolLocation {
    pub(super) uri: Uri,
    pub(super) span: Span,
    pub(super) kind: SymbolKind,
    pub(super) module_name: String,
    pub(super) module_kind: ModuleKind,
    pub(super) visibility: Visibility,
}

#[derive(Default)]
pub(super) struct SymbolIndex {
    inner: RwLock<HashMap<String, Vec<SymbolLocation>>>,
}

impl SymbolIndex {
    pub(super) async fn update_module(&self, uri: &Uri, module: &Module) {
        let mut map = self.inner.write().await;
        for locations in map.values_mut() {
            locations.retain(|loc| &loc.uri != uri);
        }
        map.retain(|_, locations| !locations.is_empty());
        for (name, span, kind, visibility, module_kind) in module_symbol_definitions(module) {
            map.entry(name).or_default().push(SymbolLocation {
                uri: uri.clone(),
                span,
                kind,
                module_name: module.name.clone(),
                module_kind,
                visibility,
            });
        }
    }

    pub(super) async fn lookup(&self, name: &str) -> Vec<SymbolLocation> {
        self.inner
            .read()
            .await
            .get(name)
            .cloned()
            .unwrap_or_default()
    }

    pub(super) async fn search(&self, query: &str) -> Vec<(String, SymbolLocation)> {
        let needle = query.to_lowercase();
        self.inner
            .read()
            .await
            .iter()
            .filter(|(name, _)| name.to_lowercase().contains(&needle))
            .flat_map(|(name, locs)| locs.iter().cloned().map(move |loc| (name.clone(), loc)))
            .collect()
    }

    pub(super) async fn duplicates_for_uri(&self, uri: &Uri) -> Vec<(String, Span, Vec<Uri>)> {
        let map = self.inner.read().await;
        let mut results = Vec::new();
        for (name, locs) in map.iter() {
            if locs.len() < 2 {
                continue;
            }
            let others: Vec<Uri> = locs
                .iter()
                .filter(|loc| loc.module_kind == ModuleKind::Library && &loc.uri != uri)
                .map(|loc| loc.uri.clone())
                .collect();
            if others.is_empty() {
                continue;
            }
            for loc in locs
                .iter()
                .filter(|loc| loc.module_kind == ModuleKind::Library && &loc.uri == uri)
            {
                results.push((name.clone(), loc.span, others.clone()));
            }
        }
        results
    }
}

fn module_symbol_definitions(
    module: &Module,
) -> Vec<(String, Span, SymbolKind, Visibility, ModuleKind)> {
    let mut defs = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => defs.push((
                func.name.clone(),
                func.name_span,
                SymbolKind::FUNCTION,
                func.visibility,
                module.kind,
            )),
            Item::Struct(def) => defs.push((
                def.name.clone(),
                def.span,
                SymbolKind::STRUCT,
                def.visibility,
                module.kind,
            )),
            Item::Enum(def) => {
                defs.push((
                    def.name.clone(),
                    def.span,
                    SymbolKind::ENUM,
                    def.visibility,
                    module.kind,
                ));
                for variant in &def.variants {
                    defs.push((
                        variant.name.clone(),
                        variant.span,
                        SymbolKind::ENUM_MEMBER,
                        def.visibility,
                        module.kind,
                    ));
                }
            }
            Item::Interface(def) => {
                defs.push((
                    def.name.clone(),
                    def.span, // keep full span for interface keyword+name
                    SymbolKind::INTERFACE,
                    def.visibility,
                    module.kind,
                ));
            }
            Item::Const(def) => defs.push((
                def.name.clone(),
                def.span,
                SymbolKind::CONSTANT,
                def.visibility,
                module.kind,
            )),
            Item::Macro(def) => defs.push((
                def.name.clone(),
                def.name_span,
                SymbolKind::FUNCTION,
                def.visibility,
                module.kind,
            )),
            Item::Impl(_) | Item::MacroInvocation(_) | Item::Comment { .. } => {}
        }
    }
    defs
}

pub(super) fn select_symbol_location<'a>(
    current_uri: &Uri,
    module: Option<&Module>,
    candidates: &'a [SymbolLocation],
) -> Option<&'a SymbolLocation> {
    if candidates.is_empty() {
        return None;
    }
    if let Some(loc) = candidates.iter().find(|loc| &loc.uri == current_uri) {
        return Some(loc);
    }
    if let Some(module) = module {
        if let Some(loc) = candidates.iter().find(|loc| loc.module_name == module.name) {
            return Some(loc);
        }
    }
    if let Some(loc) = candidates.iter().find(|loc| {
        matches!(loc.module_kind, ModuleKind::Module) && loc.visibility == Visibility::Public
    }) {
        return Some(loc);
    }
    if let Some(loc) = candidates
        .iter()
        .find(|loc| matches!(loc.visibility, Visibility::Public))
    {
        return Some(loc);
    }
    candidates.first()
}
