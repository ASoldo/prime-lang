use crate::language::{
    ast::*,
    token::{Token, TokenKind},
    types::TypeExpr,
};
pub fn format_module(module: &Module) -> String {
    let mut out = String::new();
    if let Some(doc) = &module.doc {
        write_doc_comment(&mut out, doc, true, 0);
    }
    if let Some(name) = module.declared_name.as_ref() {
        let header = match module.kind {
            ModuleKind::Module => "module",
            ModuleKind::Library => "library",
            ModuleKind::Test => "test",
        };
        out.push_str(&format!("{header} {};\n\n", name));
    }
    for import in &module.imports {
        write_visibility(&mut out, import.visibility);
        out.push_str("import ");
        out.push_str(&format_import_path(&import.path));
        if let Some(alias) = &import.alias {
            out.push_str(&format!(" as {}", alias));
        }
        if let Some(selectors) = &import.selectors {
            out.push_str("::{");
            write_inline_selectors(&mut out, selectors);
            out.push('}');
        }
        out.push_str(";\n");
    }
    if !module.imports.is_empty() {
        out.push('\n');
    }

    if !module.prelude.is_empty() {
        out.push_str("export prelude {\n");
        write_multiline_selectors(&mut out, &module.prelude);
        out.push_str("};\n\n");
    }

    for (idx, item) in module.items.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        match item {
            Item::Struct(def) => format_struct(&mut out, def),
            Item::Enum(def) => format_enum(&mut out, def),
            Item::Interface(def) => format_interface(&mut out, def),
            Item::Impl(block) => format_impl(&mut out, block),
            Item::Function(def) => format_function(&mut out, def),
            Item::Macro(def) => format_macro(&mut out, def),
            Item::Const(def) => format_const(&mut out, def),
            Item::MacroInvocation(inv) => format_macro_invocation(&mut out, inv),
            Item::Comment { text, .. } => {
                write_comment_lines(&mut out, 0, text);
            }
        }
    }

    out
}

fn format_import_path(path: &ImportPath) -> String {
    path.segments.join("::")
}

fn write_inline_selectors(out: &mut String, selectors: &[ImportSelector]) {
    for (idx, sel) in selectors.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        match sel {
            ImportSelector::Name { name, alias, .. } => {
                out.push_str(name);
                if let Some(alias) = alias {
                    out.push_str(&format!(" as {}", alias));
                }
            }
            ImportSelector::Glob(_) => out.push('*'),
        }
    }
}

fn write_multiline_selectors(out: &mut String, selectors: &[ImportSelector]) {
    for sel in selectors {
        match sel {
            ImportSelector::Name { name, alias, .. } => {
                if let Some(alias) = alias {
                    out.push_str(&format!("  {} as {},\n", name, alias));
                } else {
                    out.push_str(&format!("  {},\n", name));
                }
            }
            ImportSelector::Glob(_) => out.push_str("  *,\n"),
        }
    }
}

fn write_visibility(out: &mut String, visibility: Visibility) {
    match visibility {
        Visibility::Public => out.push_str("pub "),
        Visibility::Package => out.push_str("pub(package) "),
        Visibility::Private => {}
    }
}

fn format_struct(out: &mut String, def: &StructDef) {
    let params = format_type_params(&def.type_params);
    write_visibility(out, def.visibility);
    if let Some(doc) = &def.doc {
        write_doc_comment(out, doc, false, 0);
    }
    out.push_str(&format!("struct {}{} {{\n", def.name, params));
    for field in &def.fields {
        if field.embedded {
            if let Some(name) = &field.name {
                out.push_str(&format!("  {};\n", name));
            } else {
                out.push_str(&format!("  {};\n", format_type(&field.ty.ty)));
            }
        } else if let Some(name) = &field.name {
            out.push_str(&format!("  {}: {};\n", name, format_type(&field.ty.ty)));
        }
    }
    out.push_str("}\n");
}

fn format_enum(out: &mut String, def: &EnumDef) {
    let params = format_type_params(&def.type_params);
    write_visibility(out, def.visibility);
    if let Some(doc) = &def.doc {
        write_doc_comment(out, doc, false, 0);
    }
    out.push_str(&format!("enum {}{} {{\n", def.name, params));
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

fn format_interface(out: &mut String, def: &InterfaceDef) {
    let params = format_type_params(&def.type_params);
    write_visibility(out, def.visibility);
    if let Some(doc) = &def.doc {
        write_doc_comment(out, doc, false, 0);
    }
    out.push_str(&format!("interface {}{} {{\n", def.name, params));
    for method in &def.methods {
        out.push_str("  fn ");
        out.push_str(&method.name);
        out.push('(');
        let params = method
            .params
            .iter()
            .map(format_param_signature)
            .collect::<Vec<_>>()
            .join(", ");
        out.push_str(&params);
        out.push(')');
        if !method.returns.is_empty() {
            if method.returns.len() == 1 {
                out.push_str(&format!(" -> {}", format_type(&method.returns[0].ty)));
            } else {
                let returns = method
                    .returns
                    .iter()
                    .map(|ty| format_type(&ty.ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!(" -> ({})", returns));
            }
        }
        out.push_str(";\n");
    }
    out.push_str("}\n");
}

fn format_impl(out: &mut String, block: &ImplBlock) {
    if let Some(doc) = &block.doc {
        write_doc_comment(out, doc, false, 0);
    }
    if block.inherent {
        out.push_str(&format!("impl {} {{\n", block.target));
    } else {
        let args = format_type_arguments(&block.type_args);
        out.push_str(&format!(
            "impl {}{} for {} {{\n",
            block.interface, args, block.target
        ));
    }
    for (idx, method) in block.methods.iter().enumerate() {
        if let Some(doc) = &method.doc {
            write_doc_comment(out, doc, false, 2);
        }
        format_function_with_indent(out, method, 2);
        if idx + 1 < block.methods.len() {
            out.push('\n');
        }
    }
    out.push_str("}\n");
}

fn format_function(out: &mut String, def: &FunctionDef) {
    let params = format_type_params(&def.type_params);
    write_visibility(out, def.visibility);
    if let Some(doc) = &def.doc {
        write_doc_comment(out, doc, false, 0);
    }
    out.push_str(&format!("fn {}{}(", def.name, params));
    for (idx, param) in def.params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_param_signature(param));
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
        if needs_leading_blank_line(block) {
            out.push('\n');
        }
        format_block(out, block, 2);
    } else if let FunctionBody::Expr(expr) = &def.body {
        write_indent(out, 2);
        out.push_str(&format!("{}\n", format_expr(&expr.node)));
    }
    out.push_str("}\n");
}

fn format_macro(out: &mut String, def: &MacroDef) {
    write_visibility(out, def.visibility);
    if let Some(doc) = &def.doc {
        write_doc_comment(out, doc, false, 0);
    }
    out.push_str(&format!("macro {}(", def.name));
    for (idx, param) in def.params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_macro_param(param));
    }
    out.push(')');
    if let Some(ret) = &def.return_ty {
        out.push_str(&format!(" -> {}", format_type(&ret.ty)));
    }
    match &def.body {
        MacroBody::Block(block) => {
            out.push_str(" {\n");
            if needs_leading_blank_line(block) {
                out.push('\n');
            }
            format_block(out, block, 2);
            out.push_str("}\n");
        }
        MacroBody::Expr(expr) => {
            out.push_str(" => ");
            out.push_str(&format_expr(&expr.node));
            out.push_str(";\n");
        }
        MacroBody::Items(items, _) => {
            out.push_str(" {\n");
            format_macro_items(out, items);
            out.push_str("}\n");
        }
    }
}

fn format_macro_param(param: &MacroParam) -> String {
    if let Some(ty) = &param.ty {
        return format!("{}: {}", param.name, format_type(&ty.ty));
    }
    let kind = match param.kind {
        MacroParamKind::Block => Some("block"),
        MacroParamKind::Pattern => Some("pattern"),
        MacroParamKind::Tokens => Some("tokens"),
        MacroParamKind::Repeat => Some(
            match param
                .repeat_quantifier
                .unwrap_or(MacroRepeatQuantifier::OneOrMore)
            {
                MacroRepeatQuantifier::OneOrMore => "repeat+",
                MacroRepeatQuantifier::ZeroOrMore => "repeat*",
            },
        ),
        MacroParamKind::Expr => None,
    };
    if let Some(kind) = kind {
        format!("{}: {}", param.name, kind)
    } else {
        param.name.clone()
    }
}

fn format_macro_items(out: &mut String, items: &[Item]) {
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        let mut buf = String::new();
        match item {
            Item::Struct(def) => format_struct(&mut buf, def),
            Item::Enum(def) => format_enum(&mut buf, def),
            Item::Interface(def) => format_interface(&mut buf, def),
            Item::Impl(def) => format_impl(&mut buf, def),
            Item::Function(def) => format_function(&mut buf, def),
            Item::Macro(def) => format_macro(&mut buf, def),
            Item::Const(def) => format_const(&mut buf, def),
            Item::MacroInvocation(inv) => format_macro_invocation(&mut buf, inv),
            Item::Comment { text, .. } => {
                write_comment_lines(&mut buf, 0, text);
            }
        }
        push_indented(out, &buf, 2);
    }
}

fn format_macro_invocation(out: &mut String, inv: &MacroInvocation) {
    out.push_str(&format!("~{}(", inv.name.name));
    for (idx, arg) in inv.args.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_expr(&arg.expr));
    }
    out.push_str(");\n");
}

fn push_indented(out: &mut String, text: &str, indent: usize) {
    for line in text.lines() {
        for _ in 0..indent {
            out.push(' ');
        }
        out.push_str(line);
        out.push('\n');
    }
}

fn format_function_with_indent(out: &mut String, def: &FunctionDef, indent: usize) {
    write_indent(out, indent);
    let params = format_type_params(&def.type_params);
    write_visibility(out, def.visibility);
    out.push_str(&format!("fn {}{}(", def.name, params));
    for (idx, param) in def.params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_param_signature(param));
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
        if needs_leading_blank_line(block) {
            out.push('\n');
        }
        format_block(out, block, indent + 2);
    } else if let FunctionBody::Expr(expr) = &def.body {
        write_indent(out, indent + 2);
        out.push_str(&format!("{}\n", format_expr(&expr.node)));
    }
    write_indent(out, indent);
    out.push_str("}\n");
}

fn format_param_signature(param: &FunctionParam) -> String {
    if let Some(ty) = &param.ty {
        if param.name == "self" {
            if let Some(shorthand) = format_self_shorthand(&ty.ty) {
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
        text.push_str(&format_type(&ty.ty));
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

fn format_self_shorthand(ty: &TypeExpr) -> Option<String> {
    match ty {
        TypeExpr::SelfType => Some("self".into()),
        TypeExpr::Reference { mutable, ty } => {
            if matches!(ty.as_ref(), TypeExpr::SelfType) {
                if *mutable {
                    Some("&mut self".into())
                } else {
                    Some("&self".into())
                }
            } else {
                None
            }
        }
        TypeExpr::Pointer { mutable, ty } => {
            if matches!(ty.as_ref(), TypeExpr::SelfType) {
                if *mutable {
                    Some("*mut self".into())
                } else {
                    Some("*self".into())
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn format_call_type_args(args: &[TypeExpr]) -> String {
    args.iter()
        .map(|ty| format_type(ty))
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_const(out: &mut String, def: &ConstDef) {
    write_visibility(out, def.visibility);
    if let Some(doc) = &def.doc {
        write_doc_comment(out, doc, false, 0);
    }
    out.push_str("const ");
    out.push_str(&def.name);
    if let Some(ty) = &def.ty {
        out.push_str(": ");
        out.push_str(&format_type(&ty.ty));
    }
    out.push_str(" = ");
    if let Expr::StructLiteral { name, fields, .. } = &def.value {
        format_struct_literal_inline(out, 0, name, fields);
        out.push_str(";\n");
    } else {
        out.push_str(&format!("{};\n", format_expr(&def.value)));
    }
}

fn format_block(out: &mut String, block: &Block, indent: usize) {
    let mut prev_kind: Option<&Statement> = None;
    for statement in &block.statements {
        if matches!(statement, Statement::Let(_))
            && !matches!(
                prev_kind,
                None | Some(Statement::Let(_) | Statement::Assign(_))
            )
        {
            out.push('\n');
        }
        let extra_blank = format_statement(out, statement, indent);
        if extra_blank {
            out.push('\n');
        }
        prev_kind = Some(statement);
    }
    if let Some(tail) = &block.tail {
        format_tail_expression(out, tail, indent);
    }
}

fn write_comment_lines(out: &mut String, indent: usize, text: &str) {
    for line in text.lines() {
        write_indent(out, indent);
        out.push_str("//");
        if line.is_empty() {
            out.push('\n');
        } else if line.starts_with(' ') {
            out.push_str(line);
            out.push('\n');
        } else {
            out.push(' ');
            out.push_str(line.trim_end());
            out.push('\n');
        }
    }
    if text.is_empty() {
        write_indent(out, indent);
        out.push_str("//\n");
    }
}

fn format_statement(out: &mut String, statement: &Statement, indent: usize) -> bool {
    match statement {
        Statement::Let(stmt) => format_let_statement(out, stmt, indent),
        Statement::Assign(stmt) => format_assign_statement(out, stmt, indent),
        Statement::MacroSemi(expr) => {
            write_indent(out, indent);
            out.push_str(&format!("{};\n", format_expr(&expr.node)));
            false
        }
        Statement::Comment { text, .. } => {
            write_comment_lines(out, indent, text);
            false
        }
        Statement::Expr(expr) => {
            match &expr.expr {
                Expr::Match(match_expr) => {
                    write_match_expression(out, match_expr, indent, true);
                    out.push('\n');
                }
                Expr::If(if_expr) => {
                    emit_if_expression(out, indent, if_expr, true);
                    out.push('\n');
                }
                Expr::MapLiteral { entries, .. } => {
                    emit_map_literal(out, indent, entries);
                    out.push('\n');
                }
                Expr::Try { block, .. } => {
                    emit_try_expression(out, indent, block);
                    out.push_str(";\n");
                }
                Expr::StructLiteral { name, fields, .. } => {
                    emit_struct_literal(out, indent, name, fields);
                    out.push_str(";\n");
                }
                Expr::ArrayLiteral(values, _) => {
                    emit_array_literal(out, indent, values);
                    out.push_str(";\n");
                }
                _ => {
                    write_indent(out, indent);
                    let formatted = format_expr(&expr.expr);
                    indent_multiline_into(out, indent, formatted);
                    out.push_str(";\n");
                }
            }
            false
        }
        Statement::Return(ret) => {
            write_indent(out, indent);
            if ret.values.is_empty() {
                out.push_str("return;\n");
            } else if ret.values.len() == 1 {
                if let Expr::StructLiteral { name, fields, .. } = &ret.values[0] {
                    out.push_str("return ");
                    emit_struct_literal(out, indent + 2, name, fields);
                    out.push_str(";\n");
                } else {
                    out.push_str(&format!("return {};\n", format_expr(&ret.values[0])));
                }
            } else {
                let values = ret
                    .values
                    .iter()
                    .map(|expr| format_expr(expr))
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!("return {};\n", values));
            }
            false
        }
        Statement::While(while_stmt) => {
            format_while_statement(out, while_stmt, indent);
            false
        }
        Statement::Loop(loop_stmt) => {
            write_indent(out, indent);
            out.push_str("loop {\n");
            format_block(out, &loop_stmt.body, indent + 2);
            write_indent(out, indent);
            out.push_str("}\n");
            false
        }
        Statement::For(for_stmt) => {
            format_for_statement(out, for_stmt, indent);
            false
        }
        Statement::Defer(stmt) => {
            write_indent(out, indent);
            out.push_str(&format!("defer {};\n", format_expr(&stmt.expr)));
            false
        }
        Statement::Break => {
            write_indent(out, indent);
            out.push_str("break;\n");
            false
        }
        Statement::Continue => {
            write_indent(out, indent);
            out.push_str("continue;\n");
            false
        }
        Statement::Block(block) => {
            write_indent(out, indent);
            out.push_str("{\n");
            format_block(out, block, indent + 2);
            write_indent(out, indent);
            out.push_str("}\n");
            false
        }
    }
}

fn format_tail_expression(out: &mut String, expr: &Expr, indent: usize) {
    match expr {
        Expr::Match(match_expr) => {
            write_match_expression(out, match_expr, indent, true);
            out.push('\n');
        }
        Expr::MapLiteral { entries, .. } => {
            emit_map_literal(out, indent, entries);
            out.push('\n');
        }
        Expr::If(if_expr) => {
            emit_if_expression(out, indent, if_expr, true);
            out.push('\n');
        }
        Expr::Try { block, .. } => {
            emit_try_expression(out, indent, block);
            out.push('\n');
        }
        Expr::StructLiteral { name, fields, .. } => {
            emit_struct_literal(out, indent, name, fields);
            out.push('\n');
        }
        Expr::ArrayLiteral(values, _) => {
            emit_array_literal(out, indent, values);
            out.push('\n');
        }
        _ => {
            write_indent(out, indent);
            out.push_str(&format!("{}\n", format_expr(expr)));
        }
    }
}

fn write_indent(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push(' ');
    }
}

fn format_let_statement(out: &mut String, stmt: &LetStmt, indent: usize) -> bool {
    let mutability = if stmt.mutability.is_mutable() {
        "mut "
    } else {
        ""
    };
    let pattern_text = format_pattern(&stmt.pattern);
    let binding = if let Some(ty) = &stmt.ty {
        format!("{} {}", format_type(&ty.ty), pattern_text)
    } else {
        pattern_text
    };
    if let Some(expr) = &stmt.value {
        write_indent(out, indent);
        out.push_str(&format!("let {}{} = ", mutability, binding));
        if emit_initializer_expression(out, indent, expr) {
            out.push_str(";\n");
        } else {
            out.push_str(&format!("{};\n", format_expr(expr)));
        }
    } else {
        write_indent(out, indent);
        out.push_str(&format!("let {}{};\n", mutability, binding));
    }
    false
}

fn format_assign_statement(out: &mut String, stmt: &AssignStmt, indent: usize) -> bool {
    write_indent(out, indent);
    out.push_str(&format!("{} = ", format_expr(&stmt.target)));
    if emit_initializer_expression(out, indent, &stmt.value) {
        out.push_str(";\n");
    } else {
        out.push_str(&format!("{};\n", format_expr(&stmt.value)));
    }
    false
}

fn emit_initializer_expression(out: &mut String, base_indent: usize, expr: &Expr) -> bool {
    match expr {
        Expr::StructLiteral { name, fields, .. } => {
            format_struct_literal_inline(out, base_indent, name, fields);
            true
        }
        Expr::MapLiteral { entries, .. } => {
            format_map_literal_inline(out, base_indent, entries);
            true
        }
        Expr::If(if_expr) => {
            emit_if_expression(out, base_indent, if_expr, false);
            true
        }
        Expr::Block(block) => {
            out.push_str("{\n");
            format_block(out, block, base_indent + 2);
            write_indent(out, base_indent);
            out.push('}');
            true
        }
        Expr::Async { block, .. } => {
            out.push_str("async {\n");
            format_block(out, block, base_indent + 2);
            write_indent(out, base_indent);
            out.push('}');
            true
        }
        Expr::Spawn { expr, .. } => {
            if let Expr::Async { block, .. } = expr.as_ref() {
                out.push_str("spawn async {\n");
                format_block(out, block, base_indent + 2);
                write_indent(out, base_indent);
                out.push('}');
                true
            } else {
                false
            }
        }
        Expr::Match(match_expr) => {
            write_match_expression(out, match_expr, base_indent, false);
            true
        }
        Expr::Try { block, .. } => {
            emit_try_expression(out, base_indent, block);
            true
        }
        Expr::ArrayLiteral(values, _) => {
            emit_array_literal_inline_noindent(out, base_indent, values);
            true
        }
        Expr::Closure {
            params,
            body: ClosureBody::Block(block),
            ret,
            ..
        } => {
            let params_str = params
                .iter()
                .map(format_param_signature)
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = ret
                .as_ref()
                .map(|ty| format!(" -> {}", format_type(&ty.ty)))
                .unwrap_or_default();
            out.push_str(&format!("|{params_str}|{ret_str} "));
            out.push_str("{\n");
            format_block(out, block, base_indent + 2);
            write_indent(out, base_indent);
            out.push('}');
            true
        }
        _ => false,
    }
}

fn format_while_statement(out: &mut String, stmt: &WhileStmt, indent: usize) {
    write_indent(out, indent);
    match &stmt.condition {
        WhileCondition::Expr(expr) => {
            out.push_str(&format!("while {} {{\n", format_expr(expr)));
        }
        WhileCondition::Let { pattern, value } => {
            out.push_str(&format!(
                "while let {} = {} {{\n",
                format_pattern(pattern),
                format_expr(value)
            ));
        }
    }
    format_block(out, &stmt.body, indent + 2);
    write_indent(out, indent);
    out.push_str("}\n");
}

fn write_doc_comment(out: &mut String, doc: &str, _module: bool, indent: usize) {
    let prefix = " ".repeat(indent);
    for line in doc.lines() {
        out.push_str(&prefix);
        out.push_str("///");
        if !line.is_empty() {
            out.push(' ');
            out.push_str(line);
        }
        out.push('\n');
    }
}

fn format_for_statement(out: &mut String, stmt: &ForStmt, indent: usize) {
    write_indent(out, indent);
    let iterable = match &stmt.target {
        ForTarget::Range(range) => format_range(range),
        ForTarget::Collection(expr) => format_expr(expr),
    };
    out.push_str(&format!("for {} in {} {{\n", stmt.binding, iterable));
    format_block(out, &stmt.body, indent + 2);
    write_indent(out, indent);
    out.push_str("}\n");
}

fn write_match_expression(
    out: &mut String,
    expr: &MatchExpr,
    indent: usize,
    indent_first_line: bool,
) {
    if indent_first_line {
        write_indent(out, indent);
    }
    out.push_str(&format!("match {} {{\n", format_expr(&expr.expr)));
    for arm in &expr.arms {
        write_indent(out, indent + 2);
        out.push_str(&format!("{}", format_pattern(&arm.pattern)));
        if let Some(guard) = &arm.guard {
            out.push_str(&format!(" if {}", format_expr(guard)));
        }
        out.push_str(" => ");
        if let Expr::Block(block) = &arm.value {
            out.push_str("{\n");
            format_block(out, block, indent + 4);
            write_indent(out, indent + 2);
            out.push_str("},\n");
        } else if let Expr::StructLiteral { name, fields, .. } = &arm.value {
            emit_struct_literal(out, indent + 2, name, fields);
            out.push_str(",\n");
        } else {
            out.push_str(&format!("{},\n", format_expr(&arm.value)));
        }
    }
    write_indent(out, indent);
    out.push('}');
}

fn format_expr(expr: &Expr) -> String {
    format_expr_prec(expr, 0)
}

fn format_expr_prec(expr: &Expr, parent_prec: u8) -> String {
    match expr {
        Expr::Literal(lit) => format_literal(lit),
        Expr::Identifier(ident) => ident.name.clone(),
        Expr::Try { block, .. } => {
            let mut buf = String::new();
            emit_try_expression(&mut buf, 0, block);
            buf
        }
        Expr::TryPropagate { expr, .. } => {
            let inner = format_expr_prec(expr, 120);
            format!("{inner}?")
        }
        Expr::Binary {
            op, left, right, ..
        } => {
            let prec = precedence(*op);
            let left_str = format_expr_prec(left, prec);
            let right_str = format_expr_prec(right, prec + 1);
            let inner = format!("{left_str} {} {right_str}", format_op(*op));
            if prec < parent_prec {
                format!("({inner})")
            } else {
                inner
            }
        }
        Expr::Unary { op, expr, .. } => {
            let inner = format_expr_prec(expr, 100);
            match op {
                UnaryOp::Neg => format!("-{inner}"),
                UnaryOp::Not => format!("!{inner}"),
            }
        }
        Expr::Call {
            callee,
            type_args,
            args,
            ..
        } => {
            let args = args
                .iter()
                .map(|expr| format_expr(expr))
                .collect::<Vec<_>>()
                .join(", ");
            let type_args_str = if type_args.is_empty() {
                String::new()
            } else {
                format!("[{}]", format_call_type_args(type_args))
            };
            format!(
                "{}{}({})",
                format_expr_prec(callee, 100),
                type_args_str,
                args
            )
        }
        Expr::MacroCall { name, args, .. } => {
            let args = args
                .iter()
                .map(|arg| format_macro_arg(arg))
                .map(indent_multiline_arg)
                .collect::<Vec<_>>()
                .join(", ");
            format!("~{}({})", name.name, args)
        }
        Expr::StructLiteral { name, fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                let inner = named
                    .iter()
                    .map(|field| format!("{}: {}", field.name, format_expr(&field.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}{{ {inner} }}")
            }
            StructLiteralKind::Positional(values) => {
                let inner = values
                    .iter()
                    .map(|expr| format_expr(expr))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}{{ {inner} }}")
            }
        },
        Expr::EnumLiteral {
            enum_name,
            variant,
            values,
            ..
        } => {
            let prefix = enum_name.clone().unwrap_or_default();
            let values_str = values
                .iter()
                .map(format_expr)
                .collect::<Vec<_>>()
                .join(", ");
            if prefix.is_empty() {
                format!("{variant}({values_str})")
            } else {
                format!("{prefix}::{variant}({values_str})")
            }
        }
        Expr::MapLiteral { entries, .. } => format_map_literal(entries),
        Expr::FieldAccess { base, field, .. } => {
            let base_str = format_expr_prec(base, 100);
            format!("{base_str}.{field}")
        }
        Expr::Index { base, index, .. } => {
            let base_str = format_expr_prec(base, 100);
            format!("{base_str}[{}]", format_expr(index))
        }
        Expr::Tuple(values, _) => {
            let inner = values
                .iter()
                .map(|expr| format_expr(expr))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({inner})")
        }
        Expr::ArrayLiteral(values, _) => {
            let inner = values
                .iter()
                .map(|expr| format_expr(expr))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{inner}]")
        }
        Expr::FormatString(literal) => format_format_string(literal),
        Expr::Move { expr, .. } => format!("move {}", format_expr_prec(expr, 100)),
        Expr::Spawn { expr, .. } => format!("spawn {}", format_expr_prec(expr, 100)),
        Expr::Closure {
            params, body, ret, ..
        } => {
            let params_str = params
                .iter()
                .map(format_param_signature)
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = ret
                .as_ref()
                .map(|ty| format!(" -> {}", format_type(&ty.ty)))
                .unwrap_or_default();
            let mut text = format!("|{params_str}|{ret_str} ");
            match body {
                ClosureBody::Block(block) => {
                    let mut buf = String::new();
                    buf.push_str("{\n");
                    format_block(&mut buf, block, 2);
                    buf.push('}');
                    text.push_str(&buf);
                }
                ClosureBody::Expr(expr) => {
                    text.push_str(&format_expr_prec(expr.node.as_ref(), 1));
                }
            }
            text
        }
        Expr::Range(range) => format_range(range),
        Expr::Async { block, .. } => {
            let mut buf = String::new();
            buf.push_str("async {\n");
            format_block(&mut buf, block, 2);
            buf.push('}');
            buf
        }
        Expr::Await { expr, .. } => {
            let inner = format_expr_prec(expr, 100);
            format!("await {inner}")
        }
        Expr::Reference { mutable, expr, .. } => {
            if *mutable {
                format!("&mut {}", format_expr_prec(expr, 100))
            } else {
                format!("&{}", format_expr_prec(expr, 100))
            }
        }
        Expr::Deref { expr, .. } => format!("*{}", format_expr_prec(expr, 100)),
        Expr::Block(block) => {
            let mut buf = String::new();
            buf.push_str("{\n");
            format_block(&mut buf, block, 2);
            buf.push('}');
            buf
        }
        Expr::If(if_expr) => format_if_expression(if_expr),
        Expr::Match(match_expr) => {
            let mut buf = String::new();
            write_match_expression(&mut buf, match_expr, 0, true);
            buf
        }
    }
}

fn precedence(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => 40,
        BinaryOp::Add | BinaryOp::Sub => 30,
        BinaryOp::BitAnd => 28,
        BinaryOp::BitOr | BinaryOp::BitXor => 26,
        BinaryOp::Eq
        | BinaryOp::NotEq
        | BinaryOp::Lt
        | BinaryOp::LtEq
        | BinaryOp::Gt
        | BinaryOp::GtEq => 24,
        BinaryOp::And => 20,
        BinaryOp::Or => 18,
    }
}

fn format_macro_arg(arg: &MacroArg) -> String {
    let inner = format_expr(&arg.expr);
    if let Some(tokens) = &arg.tokens {
        if let Some(sep) = macro_sep_prefix(tokens) {
            return format!("@sep = {} {}", sep, inner);
        }
    }
    inner
}

fn macro_sep_prefix(tokens: &[Token]) -> Option<String> {
    for window in tokens.windows(4) {
        if let [first, second, third, fourth] = window {
            if first.kind == TokenKind::At {
                if let TokenKind::Identifier(name) = &second.kind {
                    if name == "sep" && matches!(third.kind, TokenKind::Eq) {
                        return Some(token_kind_display(&fourth.kind));
                    }
                }
            }
        }
    }
    None
}

fn token_kind_display(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Identifier(name) => name.clone(),
        TokenKind::Comma => ",".into(),
        TokenKind::Semi => ";".into(),
        TokenKind::Pipe => "|".into(),
        TokenKind::Slash => "/".into(),
        TokenKind::Plus => "+".into(),
        TokenKind::Minus => "-".into(),
        TokenKind::Star => "*".into(),
        TokenKind::Percent => "%".into(),
        TokenKind::Ampersand => "&".into(),
        TokenKind::PipePipe => "||".into(),
        TokenKind::AmpersandAmpersand => "&&".into(),
        TokenKind::Caret => "^".into(),
        TokenKind::Dot => ".".into(),
        TokenKind::Colon => ":".into(),
        TokenKind::ColonColon => "::".into(),
        TokenKind::Arrow => "->".into(),
        TokenKind::FatArrow => "=>".into(),
        TokenKind::Question => "?".into(),
        TokenKind::Bang => "!".into(),
        TokenKind::Eq => "=".into(),
        TokenKind::Lt => "<".into(),
        TokenKind::Gt => ">".into(),
        TokenKind::LtEq => "<=".into(),
        TokenKind::GtEq => ">=".into(),
        TokenKind::BangEq => "!=".into(),
        TokenKind::DotDot => "..".into(),
        TokenKind::DotDotEq => "..=".into(),
        TokenKind::Hash => "#".into(),
        TokenKind::LParen => "(".into(),
        TokenKind::RParen => ")".into(),
        TokenKind::LBrace => "{".into(),
        TokenKind::RBrace => "}".into(),
        TokenKind::LBracket => "[".into(),
        TokenKind::RBracket => "]".into(),
        TokenKind::Tilde => "~".into(),
        TokenKind::At => "@".into(),
        TokenKind::String(s) => s.clone(),
        TokenKind::Rune(ch) => ch.to_string(),
        _ => format!("{:?}", kind),
    }
}

fn indent_multiline_arg(arg: String) -> String {
    if !arg.contains('\n') {
        return arg;
    }
    let mut parts = arg.split('\n').peekable();
    let mut out = String::new();
    if let Some(first) = parts.next() {
        out.push_str(first);
    }
    while let Some(line) = parts.next() {
        out.push('\n');
        out.push_str("  ");
        out.push_str(line);
    }
    out
}

fn indent_multiline_into(out: &mut String, indent: usize, text: String) {
    if !text.contains('\n') {
        out.push_str(&text);
        return;
    }
    let mut lines = text.split('\n');
    if let Some(first) = lines.next() {
        out.push_str(first);
    }
    for line in lines {
        out.push('\n');
        if !line.is_empty() {
            write_indent(out, indent);
        }
        out.push_str(line);
    }
}

fn format_struct_literal_inline(
    out: &mut String,
    indent: usize,
    name: &str,
    fields: &StructLiteralKind,
) {
    out.push_str(&format!("{name}{{\n"));
    let field_indent = " ".repeat(indent + 2);
    match fields {
        StructLiteralKind::Named(named) => {
            for field in named {
                out.push_str(&field_indent);
                out.push_str(&format!("{}: {},\n", field.name, format_expr(&field.value)));
            }
        }
        StructLiteralKind::Positional(values) => {
            for value in values {
                out.push_str(&field_indent);
                out.push_str(&format!("{},\n", format_expr(value)));
            }
        }
    }
    out.push_str(&" ".repeat(indent));
    out.push('}');
}

fn emit_struct_literal(out: &mut String, indent: usize, name: &str, fields: &StructLiteralKind) {
    write_indent(out, indent);
    out.push_str(&format!("{name}{{\n"));
    match fields {
        StructLiteralKind::Named(named) => {
            for field in named {
                write_indent(out, indent + 2);
                out.push_str(&format!("{}: ", field.name));
                emit_composite_value(out, indent + 2, &field.value);
                out.push_str(",\n");
            }
        }
        StructLiteralKind::Positional(values) => {
            for value in values {
                match value {
                    Expr::StructLiteral { name, fields, .. } => {
                        emit_struct_literal(out, indent + 2, name, fields);
                        out.push_str(",\n");
                    }
                    Expr::MapLiteral { entries, .. } => {
                        emit_map_literal(out, indent + 2, entries);
                        out.push_str(",\n");
                    }
                    _ => {
                        write_indent(out, indent + 2);
                        out.push_str(&format!("{},\n", format_expr(value)));
                    }
                }
            }
        }
    }
    write_indent(out, indent);
    out.push('}');
}

fn emit_map_literal(out: &mut String, indent: usize, entries: &[MapLiteralEntry]) {
    write_indent(out, indent);
    emit_map_literal_inline(out, indent, entries);
}

fn emit_map_literal_inline(out: &mut String, indent: usize, entries: &[MapLiteralEntry]) {
    // start on the current line to keep `field: #{` compact
    out.push_str("#{");
    if entries.is_empty() {
        out.push('}');
        return;
    }
    out.push('\n');
    for entry in entries {
        write_indent(out, indent + 2);
        out.push_str(&format!("{}: ", format_expr(&entry.key)));
        emit_composite_value(out, indent + 2, &entry.value);
        out.push_str(",\n");
    }
    write_indent(out, indent);
    out.push('}');
}

fn emit_array_literal(out: &mut String, indent: usize, values: &[Expr]) {
    write_indent(out, indent);
    emit_array_literal_inline(out, indent, values);
}

fn emit_array_literal_inline_noindent(out: &mut String, indent: usize, values: &[Expr]) {
    // ensure only one space precedes the literal when used after `= `
    while out.ends_with(' ') {
        out.pop();
    }
    out.push(' ');
    out.push('[');
    if values.is_empty() {
        out.push(']');
        return;
    }
    out.push('\n');
    for value in values {
        emit_array_value(out, indent + 2, value);
        out.push_str(",\n");
    }
    write_indent(out, indent);
    out.push(']');
}

fn emit_array_literal_inline(out: &mut String, indent: usize, values: &[Expr]) {
    // start on the current line to keep `field: [` compact
    out.push('[');
    if values.is_empty() {
        out.push(']');
        return;
    }
    out.push('\n');
    for value in values {
        emit_array_value(out, indent + 2, value);
        out.push_str(",\n");
    }
    write_indent(out, indent);
    out.push(']');
}

fn emit_array_value(out: &mut String, indent: usize, expr: &Expr) {
    match expr {
        Expr::StructLiteral { name, fields, .. } => {
            emit_struct_literal(out, indent, name, fields);
        }
        Expr::MapLiteral { entries, .. } => emit_map_literal(out, indent, entries),
        Expr::ArrayLiteral(values, _) => emit_array_literal(out, indent, values),
        Expr::If(if_expr) => {
            emit_if_expression(out, indent, if_expr, true);
        }
        Expr::Match(match_expr) => {
            write_match_expression(out, match_expr, indent, true);
        }
        Expr::Block(block) => {
            write_indent(out, indent);
            out.push_str("{\n");
            format_block(out, block, indent + 2);
            write_indent(out, indent);
            out.push('}');
        }
        _ => {
            write_indent(out, indent);
            out.push_str(&format_expr(expr));
        }
    }
}

fn emit_composite_value(out: &mut String, indent: usize, expr: &Expr) {
    match expr {
        Expr::StructLiteral { name, fields, .. } => {
            out.push('\n');
            emit_struct_literal(out, indent + 2, name, fields);
        }
        Expr::MapLiteral { entries, .. } => {
            emit_map_literal_inline(out, indent, entries);
        }
        Expr::ArrayLiteral(values, _) => {
            emit_array_literal_inline(out, indent, values);
        }
        _ => out.push_str(&format_expr(expr)),
    }
}

fn format_range(range: &RangeExpr) -> String {
    let op = if range.inclusive { "..=" } else { ".." };
    format!(
        "{}{}{}",
        format_expr(&range.start),
        op,
        format_expr(&range.end)
    )
}

fn format_map_literal(entries: &[MapLiteralEntry]) -> String {
    let mut buf = String::new();
    format_map_literal_inline(&mut buf, 0, entries);
    buf
}

fn format_map_literal_inline(out: &mut String, indent: usize, entries: &[MapLiteralEntry]) {
    out.push_str("#{");
    if entries.is_empty() {
        out.push('}');
        return;
    }
    out.push('\n');
    for entry in entries {
        write_indent(out, indent + 2);
        out.push_str(&format!(
            "{}: {}",
            format_expr(&entry.key),
            format_expr(&entry.value)
        ));
        out.push_str(",\n");
    }
    write_indent(out, indent);
    out.push('}');
}

fn format_if_expression(if_expr: &IfExpr) -> String {
    let mut buf = String::new();
    emit_if_expression(&mut buf, 0, if_expr, true);
    buf
}

fn emit_if_expression(out: &mut String, indent: usize, if_expr: &IfExpr, include_indent: bool) {
    if include_indent {
        write_indent(out, indent);
    }
    out.push_str(&format!(
        "if {} {{\n",
        format_if_condition(&if_expr.condition)
    ));
    format_block(out, &if_expr.then_branch, indent + 2);
    write_indent(out, indent);
    out.push('}');
    if let Some(else_branch) = &if_expr.else_branch {
        match else_branch {
            ElseBranch::Block(block) => {
                out.push_str(" else {\n");
                format_block(out, block, indent + 2);
                write_indent(out, indent);
                out.push('}');
            }
            ElseBranch::ElseIf(nested) => {
                out.push_str(" else ");
                emit_if_expression(out, indent, nested, false);
            }
        }
    }
}

fn emit_try_expression(out: &mut String, indent: usize, block: &Block) {
    write_indent(out, indent);
    out.push_str("try {\n");
    format_block(out, block, indent + 2);
    write_indent(out, indent);
    out.push('}');
}

fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard => "_".into(),
        Pattern::Identifier(name, _) => name.clone(),
        Pattern::Literal(lit) => format_literal(lit),
        Pattern::EnumVariant {
            enum_name,
            variant,
            bindings,
            ..
        } => {
            let prefix = enum_name
                .as_ref()
                .map(|name| format!("{name}::{variant}"))
                .unwrap_or_else(|| variant.clone());
            if bindings.is_empty() {
                prefix
            } else {
                let args = bindings
                    .iter()
                    .map(format_pattern)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{prefix}({args})")
            }
        }
        Pattern::Tuple(elements, _) => {
            let inner = elements
                .iter()
                .map(format_pattern)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({inner})")
        }
        Pattern::Map(entries, _) => {
            if entries.is_empty() {
                return "#{{}}".into();
            }
            let inner = entries
                .iter()
                .map(|entry| format!("\"{}\": {}", entry.key, format_pattern(&entry.pattern)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("#{{ {inner} }}")
        }
        Pattern::Struct {
            struct_name,
            fields,
            has_spread,
            ..
        } => {
            let mut parts = Vec::new();
            for field in fields {
                parts.push(format!(
                    "{}: {}",
                    field.name,
                    format_pattern(&field.pattern)
                ));
            }
            if *has_spread {
                parts.push("..".into());
            }
            let inner = parts.join(", ");
            if let Some(name) = struct_name {
                format!("{name}{{ {inner} }}")
            } else {
                format!("{{ {inner} }}")
            }
        }
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            ..
        } => {
            let mut parts: Vec<String> = prefix.iter().map(format_pattern).collect();
            if let Some(rest_pattern) = rest {
                if matches!(**rest_pattern, Pattern::Wildcard) {
                    parts.push("..".into());
                } else {
                    parts.push(format!("..{}", format_pattern(rest_pattern)));
                }
            }
            parts.extend(suffix.iter().map(format_pattern));
            format!("[{}]", parts.join(", "))
        }
    }
}

fn format_if_condition(condition: &IfCondition) -> String {
    match condition {
        IfCondition::Expr(expr) => format_expr(expr),
        IfCondition::Let { pattern, value, .. } => {
            format!("let {} = {}", format_pattern(pattern), format_expr(value))
        }
    }
}

fn format_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(value, _) => value.to_string(),
        Literal::Float(value, _) => {
            if value.fract() == 0.0 {
                format!("{:.1}", value)
            } else {
                value.to_string()
            }
        }
        Literal::Bool(value, _) => value.to_string(),
        Literal::String(value, _) => format!("\"{}\"", escape_string(value)),
        Literal::Rune(value, _) => format!("'{}'", escape_rune(*value)),
    }
}

fn format_format_string(literal: &FormatStringLiteral) -> String {
    let mut buf = String::new();
    buf.push('`');
    for segment in &literal.segments {
        match segment {
            FormatSegment::Literal(text) => buf.push_str(&escape_format_literal(text)),
            FormatSegment::Implicit(_) => buf.push_str("{}"),
            FormatSegment::Expr { expr, .. } => {
                buf.push('{');
                buf.push_str(&format_expr(expr));
                buf.push('}');
            }
        }
    }
    buf.push('`');
    buf
}

fn needs_leading_blank_line(_block: &Block) -> bool {
    false
}

fn escape_string(value: &str) -> String {
    value
        .chars()
        .map(|ch| match ch {
            '\\' => "\\\\".into(),
            '"' => "\\\"".into(),
            '\n' => "\\n".into(),
            '\r' => "\\r".into(),
            '\t' => "\\t".into(),
            other => other.to_string(),
        })
        .collect::<String>()
}

fn escape_format_literal(value: &str) -> String {
    value
        .chars()
        .map(|ch| match ch {
            '\\' => "\\\\".into(),
            '`' => "\\`".into(),
            '\n' => "\\n".into(),
            '\r' => "\\r".into(),
            '\t' => "\\t".into(),
            '{' => "{{".into(),
            '}' => "}}".into(),
            other => other.to_string(),
        })
        .collect()
}

fn escape_rune(value: char) -> String {
    match value {
        '\\' => "\\\\".into(),
        '\'' => "\\'".into(),
        '\n' => "\\n".into(),
        '\r' => "\\r".into(),
        '\t' => "\\t".into(),
        other => other.to_string(),
    }
}

fn format_type(ty: &TypeExpr) -> String {
    match ty {
        TypeExpr::Named(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let params = args.iter().map(format_type).collect::<Vec<_>>().join(", ");
                format!("{name}[{params}]")
            }
        }
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
        TypeExpr::Function { params, returns } => {
            let params_str = params
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = if returns.is_empty() {
                "()".into()
            } else if returns.len() == 1 {
                format_type(&returns[0])
            } else {
                format!(
                    "({})",
                    returns
                        .iter()
                        .map(format_type)
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

fn format_type_params(params: &[String]) -> String {
    if params.is_empty() {
        String::new()
    } else {
        format!("[{}]", params.join(", "))
    }
}

fn format_type_arguments(args: &[TypeExpr]) -> String {
    if args.is_empty() {
        String::new()
    } else {
        format!(
            "[{}]",
            args.iter().map(format_type).collect::<Vec<_>>().join(", ")
        )
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn format_fixture(input: &str) -> String {
        let module = crate::language::parser::parse_module(
            "tests::fixture",
            std::path::PathBuf::from("fixture.prime"),
            input,
        )
        .expect("parse");
        format_module(&module)
    }

    #[test]
    fn formatter_literals_and_maps() {
        let input = fs::read_to_string("workspace/tests/golden/formatter_literals_in.prime")
            .expect("fixture input");
        let output = fs::read_to_string("workspace/tests/golden/formatter_literals_out.prime")
            .expect("fixture out");
        let formatted = format_fixture(&input);
        assert_eq!(formatted, output);
    }

    #[test]
    fn formatter_interface_self() {
        let input = fs::read_to_string("workspace/tests/golden/formatter_interface_self_in.prime")
            .expect("fixture input");
        let output =
            fs::read_to_string("workspace/tests/golden/formatter_interface_self_out.prime")
                .expect("fixture out");
        let formatted = format_fixture(&input);
        assert_eq!(formatted, output);
    }

    #[test]
    fn formatter_async_initializer() {
        let input = fs::read_to_string("workspace/tests/golden/formatter_async_init_in.prime")
            .expect("fixture input");
        let output = fs::read_to_string("workspace/tests/golden/formatter_async_init_out.prime")
            .expect("fixture out");
        let formatted = format_fixture(&input);
        assert_eq!(formatted, output);
    }

    #[test]
    fn formatter_spawn_async_initializer() {
        let input = fs::read_to_string("workspace/tests/golden/formatter_spawn_async_in.prime")
            .expect("fixture input");
        let output = fs::read_to_string("workspace/tests/golden/formatter_spawn_async_out.prime")
            .expect("fixture out");
        let formatted = format_fixture(&input);
        assert_eq!(formatted, output);
    }

    #[test]
    fn formatter_multiline_let_bindings() {
        let input = fs::read_to_string("workspace/tests/golden/formatter_multiline_let_in.prime")
            .expect("fixture input");
        let output = fs::read_to_string("workspace/tests/golden/formatter_multiline_let_out.prime")
            .expect("fixture out");
        let formatted = format_fixture(&input);
        assert_eq!(formatted, output);
    }

    #[test]
    fn formatter_preserves_comments() {
        let input = fs::read_to_string("workspace/tests/golden/formatter_comments_in.prime")
            .expect("fixture input");
        let output = fs::read_to_string("workspace/tests/golden/formatter_comments_out.prime")
            .expect("fixture out");
        let formatted = format_fixture(&input);
        assert_eq!(formatted, output);
    }

    #[test]
    fn formatter_macro_call_blocks_indent() {
        let input = r#"test tests::formatter_macro_call;

macro tally(values: repeat+) -> int32 {
  values
}

fn repeat_param_supports_semicolon_separator() -> bool {
  let mut int32 seen = 0;
  let int32 result = ~tally({ seen = seen + 1; }, { seen = seen + 2; }, seen);
  seen == 3 && result == 3
}
"#;
        let expected = r#"test tests::formatter_macro_call;

macro tally(values: repeat+) -> int32 {
  values
}

fn repeat_param_supports_semicolon_separator() -> bool {
  let mut int32 seen = 0;
  let int32 result = ~tally({
    seen = seen + 1;
  }, {
    seen = seen + 2;
  }, seen);
  seen == 3 && result == 3
}
"#;

        let formatted = format_fixture(input);
        assert_eq!(formatted, expected);
    }
}
