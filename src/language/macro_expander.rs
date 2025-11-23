use crate::language::{
    ast::*,
    errors::SyntaxError,
    span::{Span, Spanned},
    types::Mutability,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MacroExpansionError {
    pub path: std::path::PathBuf,
    pub errors: Vec<SyntaxError>,
}

pub fn expand_program(program: &Program) -> Result<Program, Vec<MacroExpansionError>> {
    let registry = MacroRegistry::build(program);
    if !registry.errors.is_empty() {
        return Err(registry.errors);
    }
    let mut expander = Expander::new(registry);
    expander.expand_program(program)
}

struct MacroRegistry<'a> {
    macros: HashMap<String, (&'a MacroDef, &'a Module)>,
    errors: Vec<MacroExpansionError>,
}

impl<'a> MacroRegistry<'a> {
    fn build(program: &'a Program) -> Self {
        let mut macros: HashMap<String, (&MacroDef, &Module)> = HashMap::new();
        let mut error_map: HashMap<std::path::PathBuf, Vec<SyntaxError>> = HashMap::new();
        for module in &program.modules {
            for item in &module.items {
                if let Item::Macro(def) = item {
                    if let Some((_, existing_module)) = macros.get(&def.name) {
                        error_map
                            .entry(existing_module.path.clone())
                            .or_default()
                            .push(SyntaxError::new(
                                format!("macro `{}` already defined", def.name),
                                def.span,
                            ));
                        error_map
                            .entry(module.path.clone())
                            .or_default()
                            .push(SyntaxError::new(
                                format!("duplicate macro `{}`", def.name),
                                def.span,
                            ));
                    } else {
                        macros.insert(def.name.clone(), (def, module));
                    }
                }
            }
        }
        let errors = error_map
            .into_iter()
            .map(|(path, errs)| MacroExpansionError { path, errors: errs })
            .collect();
        Self { macros, errors }
    }

    fn get(&self, name: &str) -> Option<&(&'a MacroDef, &'a Module)> {
        self.macros.get(name)
    }
}


struct Expander<'a> {
    registry: MacroRegistry<'a>,
    gensym: usize,
}

impl<'a> Expander<'a> {
    fn new(registry: MacroRegistry<'a>) -> Self {
        Self { registry, gensym: 0 }
    }

    fn expand_program(&mut self, program: &Program) -> Result<Program, Vec<MacroExpansionError>> {
        let mut expanded_modules = Vec::new();
        let mut errors = Vec::new();
        for module in &program.modules {
            let mut module_errors = Vec::new();
            let expanded_items = module
                .items
                .iter()
                .map(|item| self.expand_item(item, &mut module_errors))
                .collect();
            if !module_errors.is_empty() {
                errors.push(MacroExpansionError {
                    path: module.path.clone(),
                    errors: module_errors,
                });
            }
            expanded_modules.push(Module {
                name: module.name.clone(),
                kind: module.kind,
                path: module.path.clone(),
                declared_name: module.declared_name.clone(),
                declared_span: module.declared_span,
                redundant_module_spans: module.redundant_module_spans.clone(),
                imports: module.imports.clone(),
                items: expanded_items,
            });
        }
        if errors.is_empty() {
            Ok(Program {
                modules: expanded_modules,
            })
        } else {
            Err(errors)
        }
    }

    fn expand_item(&mut self, item: &Item, errors: &mut Vec<SyntaxError>) -> Item {
        match item {
            Item::Function(def) => Item::Function(self.expand_function(def, errors)),
            Item::Const(def) => Item::Const(self.expand_const(def, errors)),
            Item::Impl(block) => Item::Impl(self.expand_impl(block, errors)),
            Item::Struct(_) | Item::Enum(_) | Item::Interface(_) | Item::Macro(_) => item.clone(),
        }
    }

    fn expand_function(&mut self, def: &FunctionDef, errors: &mut Vec<SyntaxError>) -> FunctionDef {
        let body = match &def.body {
            FunctionBody::Expr(expr) => FunctionBody::Expr(Spanned::new(
                self.expand_expr(&expr.node, 0, def.span, errors),
                expr.span,
            )),
            FunctionBody::Block(block) => {
                FunctionBody::Block(Box::new(self.expand_block(block, def.span, errors)))
            }
        };
        FunctionDef { body, ..def.clone() }
    }

    fn expand_const(&mut self, def: &ConstDef, errors: &mut Vec<SyntaxError>) -> ConstDef {
        let value = self.expand_expr(&def.value, 0, def.span, errors);
        ConstDef { value, ..def.clone() }
    }

    fn expand_impl(&mut self, block: &ImplBlock, errors: &mut Vec<SyntaxError>) -> ImplBlock {
        let methods = block
            .methods
            .iter()
            .map(|method| self.expand_function(method, errors))
            .collect();
        ImplBlock {
            methods,
            ..block.clone()
        }
    }

    fn expand_block(&mut self, block: &Block, _scope_span: Span, errors: &mut Vec<SyntaxError>) -> Block {
        let mut statements = Vec::new();
        for stmt in &block.statements {
            statements.push(self.expand_statement(stmt, _scope_span, errors));
        }
        let tail = block
            .tail
            .as_ref()
            .map(|expr| Box::new(self.expand_expr(expr, 0, _scope_span, errors)));
        Block {
            statements,
            tail,
            span: block.span,
        }
    }

    fn expand_statement(
        &mut self,
        stmt: &Statement,
        _scope_span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> Statement {
        match stmt {
            Statement::Let(let_stmt) => {
                let value = let_stmt
                    .value
                    .as_ref()
                    .map(|expr| self.expand_expr(expr, 0, _scope_span, errors));
                Statement::Let(LetStmt {
                    value,
                    ..let_stmt.clone()
                })
            }
            Statement::Assign(assign) => Statement::Assign(AssignStmt {
                target: self.expand_expr(&assign.target, 0, _scope_span, errors),
                value: self.expand_expr(&assign.value, 0, _scope_span, errors),
            }),
            Statement::Expr(expr) => Statement::Expr(ExprStmt {
                expr: self.expand_expr(&expr.expr, 0, _scope_span, errors),
            }),
            Statement::Return(ret) => Statement::Return(ReturnStmt {
                values: ret
                    .values
                    .iter()
                    .map(|expr| self.expand_expr(expr, 0, _scope_span, errors))
                    .collect(),
            }),
            Statement::While(while_stmt) => Statement::While(WhileStmt {
                condition: match &while_stmt.condition {
                    WhileCondition::Expr(expr) => {
                        WhileCondition::Expr(self.expand_expr(expr, 0, _scope_span, errors))
                    }
                    WhileCondition::Let { pattern, value } => WhileCondition::Let {
                        pattern: pattern.clone(),
                        value: self.expand_expr(value, 0, _scope_span, errors),
                    },
                },
                body: self.expand_block(&while_stmt.body, _scope_span, errors),
            }),
            Statement::Loop(loop_stmt) => Statement::Loop(LoopStmt {
                body: self.expand_block(&loop_stmt.body, _scope_span, errors),
                ..loop_stmt.clone()
            }),
            Statement::For(for_stmt) => Statement::For(ForStmt {
                target: match &for_stmt.target {
                    ForTarget::Range(range) => {
                        ForTarget::Range(self.expand_range(range, _scope_span, errors))
                    }
                    ForTarget::Collection(expr) => {
                        ForTarget::Collection(self.expand_expr(expr, 0, _scope_span, errors))
                    }
                },
                body: self.expand_block(&for_stmt.body, _scope_span, errors),
                ..for_stmt.clone()
            }),
            Statement::Defer(defer_stmt) => Statement::Defer(DeferStmt {
                expr: self.expand_expr(&defer_stmt.expr, 0, _scope_span, errors),
            }),
            Statement::Block(block) => Statement::Block(Box::new(self.expand_block(
                block, _scope_span, errors,
            ))),
            Statement::Break | Statement::Continue => stmt.clone(),
        }
    }

    fn expand_range(
        &mut self,
        range: &RangeExpr,
        _scope_span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> RangeExpr {
        RangeExpr {
            start: Box::new(self.expand_expr(&range.start, 0, _scope_span, errors)),
            end: Box::new(self.expand_expr(&range.end, 0, _scope_span, errors)),
            ..range.clone()
        }
    }

    fn expand_expr(
        &mut self,
        expr: &Expr,
        depth: usize,
        scope_span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> Expr {
        if depth > 32 {
            if let Expr::MacroCall { span, .. } = expr {
                errors.push(SyntaxError::new(
                    "macro expansion exceeded recursion limit",
                    *span,
                ));
            }
            return expr.clone();
        }
        match expr {
            Expr::MacroCall { name, args, span } => {
                let expanded_args: Vec<Expr> = args
                    .iter()
                    .map(|arg| self.expand_expr(arg, depth + 1, scope_span, errors))
                    .collect();
                let Some((def, _)) = self.registry.get(&name.name) else {
                    errors.push(SyntaxError::new(
                        format!("unknown macro `{}`", name.name),
                        *span,
                    ));
                    return Expr::MacroCall {
                        name: name.clone(),
                        args: expanded_args,
                        span: *span,
                    };
                };
                if def.params.len() != expanded_args.len() {
                    errors.push(SyntaxError::new(
                        format!(
                            "macro `{}` expects {} argument(s), found {}",
                            def.name,
                            def.params.len(),
                            expanded_args.len()
                        ),
                        *span,
                    ));
                    return Expr::MacroCall {
                        name: name.clone(),
                        args: expanded_args,
                        span: *span,
                    };
                }

                // Evaluate arguments once into fresh bindings to avoid double evaluation and
                // reduce capture.
                self.gensym += 1;
                let call_id = self.gensym;
                let mut param_bindings = Vec::new();
                for (idx, (param, arg)) in def.params.iter().zip(expanded_args.iter()).enumerate()
                {
                    let binding_name = format!("__macro_arg_{call_id}_{idx}");
                    param_bindings.push((
                        param,
                        arg.clone(),
                        binding_name.clone(),
                        expr_span_local(arg),
                    ));
                }

                let mut substitution_map = HashMap::new();
                for (param, _, binding_name, arg_span) in &param_bindings {
                    substitution_map.insert(
                        param.name.clone(),
                        Expr::Identifier(Identifier {
                            name: binding_name.clone(),
                            span: *arg_span,
                        }),
                    );
                }

                let inlined_body = match &def.body {
                    MacroBody::Expr(expr) => substitute_expr(expr.node.clone(), &substitution_map),
                    MacroBody::Block(block) => substitute_expr(Expr::Block(block.clone()), &substitution_map),
                };
                let expanded_body = self.expand_expr(&inlined_body, depth + 1, scope_span, errors);

                let mut statements = Vec::new();
                for (param, arg_expr, binding_name, arg_span) in param_bindings {
                    let let_span = Span::new(span.start, arg_span.end.max(span.end));
                    statements.push(Statement::Let(LetStmt {
                        pattern: Pattern::Identifier(binding_name, arg_span),
                        ty: param.ty.clone(),
                        value: Some(arg_expr),
                        mutability: Mutability::Immutable,
                        span: let_span,
                    }));
                }

                let mut block_span_end = span.end;
                if let Expr::Block(body_block) = &expanded_body {
                    block_span_end = body_block.span.end.max(block_span_end);
                    let mut merged_stmts = statements;
                    merged_stmts.extend(body_block.statements.clone());
                    let tail = body_block.tail.clone();
                    return Expr::Block(Box::new(Block {
                        statements: merged_stmts,
                        tail,
                        span: Span::new(span.start, block_span_end),
                    }));
                }

                let tail_span = expr_span_local(&expanded_body);
                block_span_end = block_span_end.max(tail_span.end);
                let block = Block {
                    statements,
                    tail: Some(Box::new(expanded_body)),
                    span: Span::new(span.start, block_span_end),
                };
                Expr::Block(Box::new(block))
            }
        Expr::Call {
            callee,
            type_args,
            args,
            span,
        } => Expr::Call {
            callee: Box::new(self.expand_expr(callee, depth + 1, scope_span, errors)),
            type_args: type_args.clone(),
            args: args
                .iter()
                .map(|arg| self.expand_expr(arg, depth + 1, scope_span, errors))
                .collect(),
            span: *span,
        },
        Expr::Binary { op, left, right, span } => Expr::Binary {
            op: *op,
            left: Box::new(self.expand_expr(left, depth + 1, scope_span, errors)),
            right: Box::new(self.expand_expr(right, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op: *op,
            expr: Box::new(self.expand_expr(expr, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::FieldAccess { base, field, span } => Expr::FieldAccess {
            base: Box::new(self.expand_expr(base, depth + 1, scope_span, errors)),
            field: field.clone(),
            span: *span,
        },
        Expr::StructLiteral { name, fields, span } => Expr::StructLiteral {
            name: name.clone(),
            fields: match fields {
                StructLiteralKind::Named(entries) => StructLiteralKind::Named(
                    entries
                        .iter()
                        .map(|entry| StructLiteralField {
                            name: entry.name.clone(),
                            value: self.expand_expr(&entry.value, depth + 1, scope_span, errors),
                        })
                        .collect(),
                ),
                StructLiteralKind::Positional(values) => StructLiteralKind::Positional(
                    values
                        .iter()
                        .map(|value| self.expand_expr(value, depth + 1, scope_span, errors))
                        .collect(),
                ),
            },
            span: *span,
        },
        Expr::EnumLiteral {
            enum_name,
            variant,
            values,
            span,
        } => Expr::EnumLiteral {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            values: values
                .iter()
                .map(|v| self.expand_expr(v, depth + 1, scope_span, errors))
                .collect(),
            span: *span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .iter()
                .map(|entry| MapLiteralEntry {
                    key: self.expand_expr(&entry.key, depth + 1, scope_span, errors),
                    value: self.expand_expr(&entry.value, depth + 1, scope_span, errors),
                })
                .collect(),
            span: *span,
        },
        Expr::Tuple(values, span) => Expr::Tuple(
            values
                .iter()
                .map(|v| self.expand_expr(v, depth + 1, scope_span, errors))
                .collect(),
            *span,
        ),
        Expr::ArrayLiteral(values, span) => Expr::ArrayLiteral(
            values
                .iter()
                .map(|v| self.expand_expr(v, depth + 1, scope_span, errors))
                .collect(),
            *span,
        ),
        Expr::Range(range) => Expr::Range(self.expand_range(range, scope_span, errors)),
        Expr::Index { base, index, span } => Expr::Index {
            base: Box::new(self.expand_expr(base, depth + 1, scope_span, errors)),
            index: Box::new(self.expand_expr(index, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::Block(block) => Expr::Block(Box::new(self.expand_block(block, scope_span, errors))),
        Expr::If(if_expr) => Expr::If(Box::new(self.expand_if(if_expr, scope_span, errors))),
        Expr::Match(match_expr) => Expr::Match(self.expand_match(match_expr, scope_span, errors)),
        Expr::Reference { mutable, expr, span } => Expr::Reference {
            mutable: *mutable,
            expr: Box::new(self.expand_expr(expr, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::Deref { expr, span } => Expr::Deref {
            expr: Box::new(self.expand_expr(expr, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::Move { expr, span } => Expr::Move {
            expr: Box::new(self.expand_expr(expr, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::Spawn { expr, span } => Expr::Spawn {
            expr: Box::new(self.expand_expr(expr, depth + 1, scope_span, errors)),
            span: *span,
        },
        Expr::FormatString(literal) => Expr::FormatString(FormatStringLiteral {
            segments: literal
                .segments
                .iter()
                .map(|seg| match seg {
                    FormatSegment::Literal(text) => FormatSegment::Literal(text.clone()),
                    FormatSegment::Implicit(span) => FormatSegment::Implicit(*span),
                    FormatSegment::Expr { expr, span } => FormatSegment::Expr {
                        expr: self.expand_expr(expr, depth + 1, scope_span, errors),
                        span: *span,
                    },
                })
                .collect(),
            span: literal.span,
        }),
        Expr::Identifier(_)
        | Expr::Literal(_)
        | Expr::Try { .. }
        | Expr::TryPropagate { .. } => expr.clone(),
    }
}

    fn expand_if(
        &mut self,
        expr: &IfExpr,
        scope_span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> IfExpr {
        IfExpr {
            condition: match &expr.condition {
                IfCondition::Expr(e) => {
                    IfCondition::Expr(self.expand_expr(e, 0, scope_span, errors))
                }
                IfCondition::Let { pattern, value } => IfCondition::Let {
                    pattern: pattern.clone(),
                    value: self.expand_expr(value, 0, scope_span, errors),
                },
            },
            then_branch: self.expand_block(&expr.then_branch, scope_span, errors),
            else_branch: expr
                .else_branch
                .as_ref()
                .map(|branch| match branch {
                    ElseBranch::Block(block) => {
                        ElseBranch::Block(self.expand_block(block, scope_span, errors))
                    }
                    ElseBranch::ElseIf(inner) => {
                        ElseBranch::ElseIf(Box::new(self.expand_if(inner, scope_span, errors)))
                    }
                }),
            span: expr.span,
        }
    }

    fn expand_match(
        &mut self,
        expr: &MatchExpr,
        scope_span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> MatchExpr {
        MatchExpr {
            expr: Box::new(self.expand_expr(&expr.expr, 0, scope_span, errors)),
            arms: expr
                .arms
                .iter()
                .map(|arm| MatchArmExpr {
                    pattern: arm.pattern.clone(),
                    guard: arm
                        .guard
                        .as_ref()
                        .map(|g| self.expand_expr(g, 0, scope_span, errors)),
                    value: self.expand_expr(&arm.value, 0, scope_span, errors),
                })
                .collect(),
            span: expr.span,
        }
    }

}

fn substitute_expr(expr: Expr, map: &HashMap<String, Expr>) -> Expr {
    match expr {
        Expr::Identifier(ident) => map.get(&ident.name).cloned().unwrap_or(Expr::Identifier(ident)),
        Expr::Binary { op, left, right, span } => Expr::Binary {
            op,
            left: Box::new(substitute_expr(*left, map)),
            right: Box::new(substitute_expr(*right, map)),
            span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op,
            expr: Box::new(substitute_expr(*expr, map)),
            span,
        },
        Expr::Call { callee, type_args, args, span } => Expr::Call {
            callee: Box::new(substitute_expr(*callee, map)),
            type_args,
            args: args.into_iter().map(|arg| substitute_expr(arg, map)).collect(),
            span,
        },
        Expr::FieldAccess { base, field, span } => Expr::FieldAccess {
            base: Box::new(substitute_expr(*base, map)),
            field,
            span,
        },
        Expr::StructLiteral { name, fields, span } => Expr::StructLiteral {
            name,
            fields: match fields {
                StructLiteralKind::Named(entries) => StructLiteralKind::Named(
                    entries
                        .into_iter()
                        .map(|entry| StructLiteralField {
                            name: entry.name,
                            value: substitute_expr(entry.value, map),
                        })
                        .collect(),
                ),
                StructLiteralKind::Positional(values) => StructLiteralKind::Positional(
                    values.into_iter().map(|v| substitute_expr(v, map)).collect(),
                ),
            },
            span,
        },
        Expr::EnumLiteral { enum_name, variant, values, span } => Expr::EnumLiteral {
            enum_name,
            variant,
            values: values.into_iter().map(|v| substitute_expr(v, map)).collect(),
            span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .into_iter()
                .map(|entry| MapLiteralEntry {
                    key: substitute_expr(entry.key, map),
                    value: substitute_expr(entry.value, map),
                })
                .collect(),
            span,
        },
        Expr::Block(block) => Expr::Block(Box::new(substitute_block(*block, map))),
        Expr::If(if_expr) => Expr::If(Box::new(substitute_if(*if_expr, map))),
        Expr::Match(match_expr) => Expr::Match(MatchExpr {
            expr: Box::new(substitute_expr(*match_expr.expr, map)),
            arms: match_expr
                .arms
                .into_iter()
                .map(|arm| MatchArmExpr {
                    pattern: arm.pattern,
                    guard: arm.guard.map(|g| substitute_expr(g, map)),
                    value: substitute_expr(arm.value, map),
                })
                .collect(),
            span: match_expr.span,
        }),
        Expr::Tuple(values, span) => Expr::Tuple(
            values.into_iter().map(|v| substitute_expr(v, map)).collect(),
            span,
        ),
        Expr::ArrayLiteral(values, span) => Expr::ArrayLiteral(
            values.into_iter().map(|v| substitute_expr(v, map)).collect(),
            span,
        ),
        Expr::Range(range) => Expr::Range(RangeExpr {
            start: Box::new(substitute_expr(*range.start, map)),
            end: Box::new(substitute_expr(*range.end, map)),
            inclusive: range.inclusive,
            span: range.span,
        }),
        Expr::Index { base, index, span } => Expr::Index {
            base: Box::new(substitute_expr(*base, map)),
            index: Box::new(substitute_expr(*index, map)),
            span,
        },
        Expr::Reference { mutable, expr, span } => Expr::Reference {
            mutable,
            expr: Box::new(substitute_expr(*expr, map)),
            span,
        },
        Expr::Deref { expr, span } => Expr::Deref {
            expr: Box::new(substitute_expr(*expr, map)),
            span,
        },
        Expr::Move { expr, span } => Expr::Move {
            expr: Box::new(substitute_expr(*expr, map)),
            span,
        },
        Expr::Spawn { expr, span } => Expr::Spawn {
            expr: Box::new(substitute_expr(*expr, map)),
            span,
        },
        Expr::FormatString(literal) => Expr::FormatString(FormatStringLiteral {
            segments: literal
                .segments
                .into_iter()
                .map(|seg| match seg {
                    FormatSegment::Literal(text) => FormatSegment::Literal(text),
                    FormatSegment::Implicit(span) => FormatSegment::Implicit(span),
                    FormatSegment::Expr { expr, span } => FormatSegment::Expr {
                        expr: substitute_expr(expr, map),
                        span,
                    },
                })
                .collect(),
            span: literal.span,
        }),
        Expr::MacroCall { name, args, span } => Expr::MacroCall {
            name,
            args: args.into_iter().map(|a| substitute_expr(a, map)).collect(),
            span,
        },
        Expr::Literal(_) | Expr::Try { .. } | Expr::TryPropagate { .. } => expr,
    }
}

fn substitute_block(block: Block, map: &HashMap<String, Expr>) -> Block {
    Block {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| substitute_statement(stmt, map))
            .collect(),
        tail: block.tail.map(|expr| Box::new(substitute_expr(*expr, map))),
        span: block.span,
    }
}

fn substitute_statement(stmt: Statement, map: &HashMap<String, Expr>) -> Statement {
    match stmt {
        Statement::Let(let_stmt) => Statement::Let(LetStmt {
            value: let_stmt.value.map(|expr| substitute_expr(expr, map)),
            ..let_stmt
        }),
        Statement::Assign(assign) => Statement::Assign(AssignStmt {
            target: substitute_expr(assign.target, map),
            value: substitute_expr(assign.value, map),
        }),
        Statement::Expr(expr) => Statement::Expr(ExprStmt {
            expr: substitute_expr(expr.expr, map),
        }),
        Statement::Return(ret) => Statement::Return(ReturnStmt {
            values: ret
                .values
                .into_iter()
                .map(|expr| substitute_expr(expr, map))
                .collect(),
        }),
        Statement::While(while_stmt) => Statement::While(WhileStmt {
            condition: match while_stmt.condition {
                WhileCondition::Expr(expr) => {
                    WhileCondition::Expr(substitute_expr(expr, map))
                }
                WhileCondition::Let { pattern, value } => WhileCondition::Let {
                    pattern,
                    value: substitute_expr(value, map),
                },
            },
            body: substitute_block(while_stmt.body, map),
        }),
        Statement::Loop(loop_stmt) => Statement::Loop(LoopStmt {
            body: substitute_block(loop_stmt.body, map),
            span: loop_stmt.span,
        }),
        Statement::For(for_stmt) => Statement::For(ForStmt {
            binding: for_stmt.binding,
            target: match for_stmt.target {
                ForTarget::Range(range) => ForTarget::Range(RangeExpr {
                    start: Box::new(substitute_expr(*range.start, map)),
                    end: Box::new(substitute_expr(*range.end, map)),
                    inclusive: range.inclusive,
                    span: range.span,
                }),
                ForTarget::Collection(expr) => {
                    ForTarget::Collection(substitute_expr(expr, map))
                }
            },
            body: substitute_block(for_stmt.body, map),
            span: for_stmt.span,
        }),
        Statement::Defer(defer_stmt) => Statement::Defer(DeferStmt {
            expr: substitute_expr(defer_stmt.expr, map),
        }),
        Statement::Block(block) => Statement::Block(Box::new(substitute_block(*block, map))),
        Statement::Break | Statement::Continue => stmt,
    }
}

fn substitute_if(if_expr: IfExpr, map: &HashMap<String, Expr>) -> IfExpr {
    IfExpr {
        condition: match if_expr.condition {
            IfCondition::Expr(expr) => IfCondition::Expr(substitute_expr(expr, map)),
            IfCondition::Let { pattern, value } => IfCondition::Let {
                pattern,
                value: substitute_expr(value, map),
            },
        },
        then_branch: substitute_block(if_expr.then_branch, map),
        else_branch: if_expr
            .else_branch
            .map(|branch| match branch {
                ElseBranch::Block(block) => ElseBranch::Block(substitute_block(block, map)),
                ElseBranch::ElseIf(inner) => {
                    ElseBranch::ElseIf(Box::new(substitute_if(*inner, map)))
                }
            }),
        span: if_expr.span,
    }
}

fn expr_span_local(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(Literal::Int(_, span))
        | Expr::Literal(Literal::Float(_, span))
        | Expr::Literal(Literal::Bool(_, span))
        | Expr::Literal(Literal::String(_, span))
        | Expr::Literal(Literal::Rune(_, span)) => *span,
        Expr::FormatString(lit) => lit.span,
        Expr::Binary { span, .. }
        | Expr::Unary { span, .. }
        | Expr::Call { span, .. }
        | Expr::FieldAccess { span, .. }
        | Expr::StructLiteral { span, .. }
        | Expr::EnumLiteral { span, .. }
        | Expr::MapLiteral { span, .. }
        | Expr::Tuple(_, span)
        | Expr::ArrayLiteral(_, span)
        | Expr::Range(RangeExpr { span, .. })
        | Expr::Index { span, .. }
        | Expr::Reference { span, .. }
        | Expr::Deref { span, .. }
        | Expr::Move { span, .. }
        | Expr::Spawn { span, .. }
        | Expr::Try { span, .. }
        | Expr::TryPropagate { span, .. }
        | Expr::MacroCall { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(expr) => expr.span,
        Expr::Match(expr) => expr.span,
    }
}
