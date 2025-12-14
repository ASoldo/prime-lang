use super::*;

pub(super) fn collect_symbols(uri: &Uri, text: &str, module: &Module) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => symbols.push(make_symbol(
                uri,
                text,
                &func.name,
                SymbolKind::FUNCTION,
                func.name_span,
            )),
            Item::Struct(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::STRUCT,
                def.span,
            )),
            Item::Enum(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::ENUM,
                def.span,
            )),
            Item::Interface(def) => {
                symbols.push(make_symbol(
                    uri,
                    text,
                    &def.name,
                    SymbolKind::INTERFACE,
                    def.span,
                ));
                for method in &def.methods {
                    symbols.push(make_symbol(
                        uri,
                        text,
                        &format!("{}::{}", def.name, method.name),
                        SymbolKind::METHOD,
                        method.span,
                    ));
                }
            }
            Item::Const(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::CONSTANT,
                def.span,
            )),
            Item::Macro(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::FUNCTION,
                def.name_span,
            )),
            Item::Impl(block) => {
                if let Some(first) = block.methods.first() {
                    symbols.push(make_symbol(
                        uri,
                        text,
                        &format!("impl {} for {}", block.interface, block.target),
                        SymbolKind::INTERFACE,
                        first.span,
                    ));
                }
                for method in &block.methods {
                    symbols.push(make_symbol(
                        uri,
                        text,
                        &format!("{}::{}", block.target, method.name),
                        SymbolKind::METHOD,
                        method.span,
                    ));
                }
            }
            Item::MacroInvocation(_) | Item::Comment { .. } => {}
        }
    }
    symbols
}

fn make_symbol(
    uri: &Uri,
    text: &str,
    name: &str,
    kind: SymbolKind,
    span: Span,
) -> SymbolInformation {
    #[allow(deprecated)]
    SymbolInformation {
        name: name.to_string(),
        kind,
        location: Location::new(uri.clone(), span_to_range(text, span)),
        container_name: None,
        deprecated: None,
        tags: None,
    }
}
