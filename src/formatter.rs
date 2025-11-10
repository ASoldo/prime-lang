use crate::language::{ast::*, types::TypeExpr};

pub fn format_module(module: &Module) -> String {
    let mut out = String::new();
    for import in &module.imports {
        out.push_str(&format!("import \"{}\"", import.path));
        if let Some(alias) = &import.alias {
            out.push_str(&format!(" as {}", alias));
        }
        out.push_str(";\n");
    }
    if !module.imports.is_empty() {
        out.push('\n');
    }

    for (idx, item) in module.items.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        match item {
            Item::Struct(def) => format_struct(&mut out, def),
            Item::Enum(def) => format_enum(&mut out, def),
            Item::Function(def) => format_function(&mut out, def),
            Item::Const(def) => {
                out.push_str(&format!("const {} = /* ... */;\n", def.name));
            }
        }
    }

    out
}

fn format_struct(out: &mut String, def: &StructDef) {
    out.push_str(&format!("struct {} {{\n", def.name));
    for field in &def.fields {
        if field.embedded {
            if let Some(name) = &field.name {
                out.push_str(&format!("  {};\n", name));
            } else {
                out.push_str("  // embedded field\n");
            }
        } else if let Some(name) = &field.name {
            out.push_str(&format!("  {}: {};\n", name, format_type(&field.ty.ty)));
        }
    }
    out.push_str("}\n");
}

fn format_enum(out: &mut String, def: &EnumDef) {
    out.push_str(&format!("enum {} {{\n", def.name));
    for variant in &def.variants {
        if variant.fields.is_empty() {
            out.push_str(&format!("  {},\n", variant.name));
        } else {
            let fields: Vec<_> = variant
                .fields
                .iter()
                .map(|ty| format_type(&ty.ty))
                .collect();
            out.push_str(&format!("  {}({}),\n", variant.name, fields.join(", ")));
        }
    }
    out.push_str("}\n");
}

fn format_function(out: &mut String, def: &FunctionDef) {
    out.push_str(&format!("fn {}(", def.name));
    for (idx, param) in def.params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        if param.mutability.is_mutable() {
            out.push_str("mut ");
        }
        out.push_str(&format!("{}: {}", param.name, format_type(&param.ty.ty)));
    }
    out.push(')');
    if !def.returns.is_empty() {
        let ret = def
            .returns
            .iter()
            .map(|ty| format_type(&ty.ty))
            .collect::<Vec<_>>()
            .join(", ");
        if def.returns.len() == 1 {
            out.push_str(&format!(" -> {}", ret));
        } else {
            out.push_str(&format!(" -> ({})", ret));
        }
    }
    out.push_str(" {\n");
    if let FunctionBody::Block(block) = &def.body {
        format_block(out, block, 2);
    } else if let FunctionBody::Expr(expr) = &def.body {
        out.push_str(&format!("  {}\n", format_expr(&expr.node)));
    }
    out.push_str("}\n");
}

fn format_block(out: &mut String, block: &Block, indent: usize) {
    let prefix = " ".repeat(indent);
    for statement in &block.statements {
        let formatted = match statement {
            Statement::Let(stmt) => {
                let value = stmt
                    .value
                    .as_ref()
                    .map(|expr| format_expr(expr))
                    .unwrap_or_else(|| "0".into());
                let mutability = if stmt.mutability.is_mutable() {
                    "mut "
                } else {
                    ""
                };
                format!("let {}{} = {};", mutability, stmt.name, value)
            }
            Statement::Expr(expr) => format!("{};", format_expr(&expr.expr)),
            Statement::Return(ret) => {
                if ret.values.is_empty() {
                    "return;".into()
                } else {
                    let values = ret
                        .values
                        .iter()
                        .map(|expr| format_expr(expr))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("return {};", values)
                }
            }
            other => format!("// unsupported statement: {:?}", other),
        };
        out.push_str(&prefix);
        out.push_str(&formatted);
        out.push('\n');
    }
    if let Some(tail) = &block.tail {
        out.push_str(&prefix);
        out.push_str(&format!("{};\n", format_expr(tail)));
    }
}

fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Literal(Literal::Int(value, _)) => value.to_string(),
        Expr::Literal(Literal::Float(value, _)) => value.to_string(),
        Expr::Literal(Literal::Bool(value, _)) => value.to_string(),
        Expr::Literal(Literal::String(value, _)) => format!("\"{}\"", value),
        Expr::Identifier(ident) => ident.name.clone(),
        Expr::Binary {
            op, left, right, ..
        } => {
            format!(
                "({} {} {})",
                format_expr(left),
                format_op(*op),
                format_expr(right)
            )
        }
        Expr::Call { callee, args, .. } => {
            let args = args
                .iter()
                .map(|expr| format_expr(expr))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", format_expr(callee), args)
        }
        Expr::StructLiteral { name, fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                let inner = named
                    .iter()
                    .map(|field| format!("{}: {}", field.name, format_expr(&field.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}{{ {} }}", name, inner)
            }
            StructLiteralKind::Positional(values) => {
                let inner = values
                    .iter()
                    .map(|expr| format_expr(expr))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}{{ {} }}", name, inner)
            }
        },
        _ => "/* unsupported expr */".into(),
    }
}

fn format_type(ty: &TypeExpr) -> String {
    match ty {
        TypeExpr::Named(name, _) => name.clone(),
        TypeExpr::Slice(inner) => format!("[]{}", format_type(inner)),
        TypeExpr::Array { size, ty } => format!("[{}]{}", size, format_type(ty)),
        TypeExpr::Reference { mutable, ty } => {
            if *mutable {
                format!("&mut {}", format_type(ty))
            } else {
                format!("&{}", format_type(ty))
            }
        }
        TypeExpr::Pointer { mutable, ty } => {
            if *mutable {
                format!("*mut {}", format_type(ty))
            } else {
                format!("*{}", format_type(ty))
            }
        }
        TypeExpr::Tuple(types) => {
            let inner = types.iter().map(format_type).collect::<Vec<_>>().join(", ");
            format!("({})", inner)
        }
        TypeExpr::Unit => "()".into(),
    }
}

fn format_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Eq => "==",
        BinaryOp::NotEq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::LtEq => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::GtEq => ">=",
    }
}
