use super::*;

pub fn named_type_with_args(ty: &TypeExpr) -> Option<(String, Vec<TypeExpr>)> {
    match ty {
        TypeExpr::Named(name, args) => Some((name.clone(), args.clone())),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => named_type_with_args(ty),
        _ => None,
    }
}

pub(super) fn strip_type_refs(ty: &TypeExpr) -> &TypeExpr {
    match ty {
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => strip_type_refs(ty),
        _ => ty,
    }
}

pub(super) fn build_type_subst(
    params: &[String],
    args: &[TypeExpr],
) -> Option<HashMap<String, TypeExpr>> {
    if params.is_empty() || params.len() != args.len() {
        return None;
    }
    let mut map = HashMap::new();
    for (param, arg) in params.iter().zip(args) {
        map.insert(param.clone(), arg.clone());
    }
    Some(map)
}

pub(super) fn struct_name_from_type(ty: &TypeExpr) -> Option<&str> {
    match ty {
        TypeExpr::Named(name, _) => Some(name),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => struct_name_from_type(ty),
        _ => None,
    }
}
