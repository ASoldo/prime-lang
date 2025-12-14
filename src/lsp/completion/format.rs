use super::*;

pub(super) fn format_macro_detail(def: &MacroDef) -> String {
    let params = def
        .params
        .iter()
        .map(|p| match &p.ty {
            Some(ty) => format!("{}: {}", p.name, format_type_expr(&ty.ty)),
            None => p.name.clone(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    let ret = def
        .return_ty
        .as_ref()
        .map(|ty| format!(" -> {}", format_type_expr(&ty.ty)))
        .unwrap_or_default();
    let vis = match def.visibility {
        Visibility::Public => "pub",
        Visibility::Package => "pub(package)",
        Visibility::Private => "private",
    };
    format!("{} macro ~{}({}){}", vis, def.name, params, ret)
}

pub fn format_function_signature(func: &FunctionDef) -> String {
    let mut signature = String::from("fn ");
    signature.push_str(&func.name);
    signature.push_str(&format_type_params(&func.type_params));
    signature.push('(');
    let params = func
        .params
        .iter()
        .map(format_function_param)
        .collect::<Vec<_>>()
        .join(", ");
    signature.push_str(&params);
    signature.push(')');
    if !func.returns.is_empty() {
        signature.push_str(" -> ");
        if func.returns.len() == 1 {
            signature.push_str(&format_type_expr(&func.returns[0].ty));
        } else {
            let returns = func
                .returns
                .iter()
                .map(|ret| format_type_expr(&ret.ty))
                .collect::<Vec<_>>()
                .join(", ");
            signature.push('(');
            signature.push_str(&returns);
            signature.push(')');
        }
    }
    signature
}

pub fn format_function_param(param: &FunctionParam) -> String {
    if let Some(ty) = &param.ty {
        if param.name == "self" {
            if let Some(shorthand) = format_self_param(&ty.ty) {
                let mut text = String::new();
                if param.mutability.is_mutable() {
                    text.push_str("mut ");
                }
                text.push_str(&shorthand);
                return text;
            }
        }
        let mut text = String::new();
        if param.mutability.is_mutable() {
            text.push_str("mut ");
        }
        text.push_str(&param.name);
        text.push_str(": ");
        text.push_str(&format_type_expr(&ty.ty));
        text
    } else {
        let mut text = String::new();
        if param.mutability.is_mutable() {
            text.push_str("mut ");
        }
        text.push_str(&param.name);
        text
    }
}

pub fn format_interface_method_signature(
    method: &InterfaceMethod,
    subst: Option<&HashMap<String, TypeExpr>>,
) -> String {
    let mut signature = String::from("fn ");
    signature.push_str(&method.name);
    signature.push('(');
    let params = method
        .params
        .iter()
        .map(|param| {
            let mut cloned = param.clone();
            if let Some(map) = subst {
                if let Some(ty) = cloned.ty.as_mut() {
                    *ty = ty.substitute(map);
                }
            }
            format_function_param(&cloned)
        })
        .collect::<Vec<_>>()
        .join(", ");
    signature.push_str(&params);
    signature.push(')');
    if !method.returns.is_empty() {
        signature.push_str(" -> ");
        if method.returns.len() == 1 {
            if let Some(map) = subst {
                let mut ret = method.returns[0].clone();
                ret.ty = ret.ty.substitute(map);
                signature.push_str(&format_type_expr(&ret.ty));
            } else {
                signature.push_str(&format_type_expr(&method.returns[0].ty));
            }
        } else {
            let returns = method
                .returns
                .iter()
                .map(|ret| {
                    if let Some(map) = subst {
                        let mut cloned = ret.clone();
                        cloned.ty = cloned.ty.substitute(map);
                        format_type_expr(&cloned.ty)
                    } else {
                        format_type_expr(&ret.ty)
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            signature.push('(');
            signature.push_str(&returns);
            signature.push(')');
        }
    }
    signature
}

fn format_self_param(ty: &TypeExpr) -> Option<String> {
    match ty {
        TypeExpr::Reference { mutable, ty } => {
            if matches!(**ty, TypeExpr::SelfType) {
                if *mutable {
                    Some("&mut self".into())
                } else {
                    Some("&self".into())
                }
            } else {
                None
            }
        }
        TypeExpr::SelfType => Some("self".into()),
        _ => None,
    }
}

pub fn format_type_expr(expr: &TypeExpr) -> String {
    match expr {
        TypeExpr::Named(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let params = args
                    .iter()
                    .map(format_type_expr)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}[{params}]")
            }
        }
        TypeExpr::Slice(inner) => format!("[]{}", format_type_expr(inner)),
        TypeExpr::Array { size, ty } => format!("[{}]{}", size, format_type_expr(ty)),
        TypeExpr::Reference { mutable, ty } => {
            if *mutable {
                format!("&mut {}", format_type_expr(ty))
            } else {
                format!("&{}", format_type_expr(ty))
            }
        }
        TypeExpr::Pointer { mutable, ty } => {
            if *mutable {
                format!("*mut {}", format_type_expr(ty))
            } else {
                format!("*{}", format_type_expr(ty))
            }
        }
        TypeExpr::Tuple(types) => {
            let inner = types
                .iter()
                .map(format_type_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        TypeExpr::Function { params, returns } => {
            let params_str = params
                .iter()
                .map(format_type_expr)
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = if returns.is_empty() {
                "()".into()
            } else if returns.len() == 1 {
                format_type_expr(&returns[0])
            } else {
                format!(
                    "({})",
                    returns
                        .iter()
                        .map(format_type_expr)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            format!("fn({}) -> {}", params_str, ret_str)
        }
        TypeExpr::Unit => "()".into(),
        TypeExpr::SelfType => "Self".into(),
    }
}

pub fn format_type_params(params: &[String]) -> String {
    if params.is_empty() {
        String::new()
    } else {
        format!("[{}]", params.join(", "))
    }
}

pub fn format_type_arguments(args: &[TypeExpr]) -> String {
    if args.is_empty() {
        String::new()
    } else {
        let rendered = args
            .iter()
            .map(format_type_expr)
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{}]", rendered)
    }
}
