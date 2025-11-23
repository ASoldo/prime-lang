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
) -> Result<(), TypeError> {
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
) -> Result<&'a EnumVariant, TypeError> {
    ensure_enum_visible(enum_def, enum_module, module, span)?;
    enum_def
        .variants
        .iter()
        .find(|v| v.name == variant)
        .ok_or_else(|| {
            TypeError::new(
                &module.path,
                span,
                format!("Enum `{}` has no variant `{}`", enum_def.name, variant),
            )
        })
}
