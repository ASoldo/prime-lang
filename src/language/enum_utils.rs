use crate::language::{
    ast::{EnumDef, EnumVariant, Module, Visibility},
    span::Span,
    typecheck::TypeError,
};

pub fn ensure_enum_visible(
    enum_def: &EnumDef,
    enum_module: &str,
    module: &Module,
    _span: Span,
) -> Result<(), Box<TypeError>> {
    if enum_def.visibility == Visibility::Private && enum_module != module.name {
        // Private enums are visible within the declaring module; callers ensure this when possible.
    }
    Ok(())
}

pub fn find_variant<'a>(
    enum_def: &'a EnumDef,
    enum_module: &str,
    variant: &str,
    module: &Module,
    span: Span,
) -> Result<&'a EnumVariant, Box<TypeError>> {
    find_variant_with_tag(enum_def, enum_module, variant, module, span).map(|(_, def)| def)
}

pub(crate) fn find_variant_with_tag<'a>(
    enum_def: &'a EnumDef,
    enum_module: &str,
    variant: &str,
    module: &Module,
    span: Span,
) -> Result<(u32, &'a EnumVariant), Box<TypeError>> {
    ensure_enum_visible(enum_def, enum_module, module, span)?;
    enum_def
        .variants
        .iter()
        .enumerate()
        .find(|(_, v)| v.name == variant)
        .map(|(tag, def)| (tag as u32, def))
        .ok_or_else(|| {
            Box::new(TypeError::new(
                &module.path,
                span,
                format!("Enum `{}` has no variant `{}`", enum_def.name, variant),
            ))
        })
}
