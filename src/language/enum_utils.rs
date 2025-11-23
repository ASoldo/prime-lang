use crate::language::{
    ast::{EnumDef, EnumVariant, Module, Visibility},
    span::Span,
    typecheck::TypeError,
};

pub fn ensure_enum_visible(
    enum_def: &EnumDef,
    module: &Module,
    span: Span,
) -> Result<(), TypeError> {
    if enum_def.visibility == Visibility::Private && enum_def.name != module.name {
        return Err(TypeError::new(
            &module.path,
            span,
            format!("Enum `{}` is not visible here", enum_def.name),
        ));
    }
    Ok(())
}

pub fn find_variant<'a>(
    enum_def: &'a EnumDef,
    variant: &str,
    module: &Module,
    span: Span,
) -> Result<&'a EnumVariant, TypeError> {
    ensure_enum_visible(enum_def, module, span)?;
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
