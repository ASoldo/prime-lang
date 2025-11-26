use crate::language::{
    ast::*,
    errors::SyntaxError,
    span::{Span, Spanned},
    token::{Token, TokenKind},
    types::Mutability,
};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

#[derive(Debug, Clone)]
pub struct ExpandedProgram {
    pub program: Program,
    pub traces: ExpansionTraces,
    pub item_origins: HashMap<PathBuf, Vec<Option<String>>>,
}

#[derive(Debug, Clone)]
pub struct MacroExpansionError {
    pub path: std::path::PathBuf,
    pub errors: Vec<SyntaxError>,
}

#[derive(Debug, Clone, Default)]
pub struct ExpansionTraces {
    entries: HashMap<PathBuf, Vec<TraceEntry>>,
}

#[derive(Debug, Clone)]
struct TraceEntry {
    span: Span,
    frames: Vec<MacroFrame>,
}

pub fn expand_program(program: &Program) -> Result<ExpandedProgram, Vec<MacroExpansionError>> {
    let registry = MacroRegistry::build(program);
    if !registry.errors.is_empty() {
        return Err(registry.errors);
    }
    let mut expander = Expander::new(registry);
    let program = expander.expand_program(program)?;
    Ok(ExpandedProgram {
        program,
        traces: expander.traces,
        item_origins: expander.item_origins,
    })
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

    fn get(&self, name: &str, requester: Option<&Path>) -> Option<&(&'a MacroDef, &'a Module)> {
        let entry = self.macros.get(name)?;
        let (_def, module) = entry;
        match entry.0.visibility {
            Visibility::Public => Some(entry),
            Visibility::Package => {
                if let (Some(req), Some(pkg)) = (requester, module.path.parent()) {
                    if req.starts_with(pkg) {
                        return Some(entry);
                    }
                }
                None
            }
            Visibility::Private => {
                if let Some(req) = requester {
                    if req == module.path {
                        return Some(entry);
                    }
                }
                None
            }
        }
    }
}

struct Expander<'a> {
    registry: MacroRegistry<'a>,
    gensym: usize,
    stack: Vec<MacroFrame>,
    traces: ExpansionTraces,
    current_path: Option<PathBuf>,
    item_origins: HashMap<PathBuf, Vec<Option<String>>>,
}

#[derive(Debug, Clone)]
struct MacroFrame {
    name: String,
    call_span: Span,
}

struct Substitution {
    exprs: HashMap<String, Expr>,
    patterns: HashMap<String, Pattern>,
    pattern_params: HashSet<String>,
}

#[derive(Clone)]
struct RepeatFragments {
    parts: Vec<Expr>,
    separator: Option<crate::language::token::TokenKind>,
    span: Span,
}

fn validate_no_duplicate_items(items: &[(Item, Option<String>)]) -> Vec<SyntaxError> {
    let mut seen_funcs: HashMap<String, (Span, Option<String>)> = HashMap::new();
    let mut seen_structs: HashMap<String, (Span, Option<String>)> = HashMap::new();
    let mut seen_enums: HashMap<String, (Span, Option<String>)> = HashMap::new();
    let mut seen_interfaces: HashMap<String, (Span, Option<String>)> = HashMap::new();
    let mut seen_macros: HashMap<String, (Span, Option<String>)> = HashMap::new();
    let mut seen_consts: HashMap<String, (Span, Option<String>)> = HashMap::new();
    let mut errors = Vec::new();
    for (item, origin) in items {
        match item {
            Item::Function(def) => push_dup(
                &mut seen_funcs,
                &mut errors,
                &def.name,
                def.name_span,
                "function",
                origin.clone(),
            ),
            Item::Struct(def) => push_dup(
                &mut seen_structs,
                &mut errors,
                &def.name,
                def.span,
                "struct",
                origin.clone(),
            ),
            Item::Enum(def) => push_dup(
                &mut seen_enums,
                &mut errors,
                &def.name,
                def.span,
                "enum",
                origin.clone(),
            ),
            Item::Interface(def) => push_dup(
                &mut seen_interfaces,
                &mut errors,
                &def.name,
                def.span,
                "interface",
                origin.clone(),
            ),
            Item::Macro(def) => push_dup(
                &mut seen_macros,
                &mut errors,
                &def.name,
                def.name_span,
                "macro",
                origin.clone(),
            ),
            Item::Const(def) => push_dup(
                &mut seen_consts,
                &mut errors,
                &def.name,
                def.span,
                "const",
                origin.clone(),
            ),
            Item::Impl(_) | Item::MacroInvocation(_) => {}
        }
    }
    errors
}

fn push_dup(
    map: &mut HashMap<String, (Span, Option<String>)>,
    errors: &mut Vec<SyntaxError>,
    name: &str,
    span: Span,
    kind: &str,
    origin: Option<String>,
) {
    if let Some((_prev, prev_origin)) = map.get(name) {
        let origin_desc = match prev_origin {
            Some(src) => format!("first from macro `{}`", src),
            None => "first defined earlier in module".to_string(),
        };
        errors.push(SyntaxError::new(
            format!("duplicate {} `{}` ({})", kind, name, origin_desc),
            span,
        ));
    } else {
        map.insert(name.to_string(), (span, origin));
    }
}

struct RepeatSpec {
    separator: Option<TokenKind>,
    skip: usize,
}

fn repeat_quantifier(param: &MacroParam) -> MacroRepeatQuantifier {
    param
        .repeat_quantifier
        .unwrap_or(MacroRepeatQuantifier::OneOrMore)
}

fn min_args(def: &MacroDef) -> usize {
    if let Some(last) = def.params.last() {
        if matches!(last.kind, MacroParamKind::Repeat) {
            let fixed = def.params.len().saturating_sub(1);
            return fixed
                + match repeat_quantifier(last) {
                    MacroRepeatQuantifier::OneOrMore => 1,
                    MacroRepeatQuantifier::ZeroOrMore => 0,
                };
        }
    }
    def.params.len()
}

fn arity_error(def: &MacroDef, args_len: usize) -> Option<String> {
    let min = min_args(def);
    if let Some(last) = def.params.last() {
        if matches!(last.kind, MacroParamKind::Repeat) {
            if args_len < min {
                return Some(format!(
                    "macro `{}` expects at least {} argument(s), found {}",
                    def.name, min, args_len
                ));
            }
            return None;
        }
    }
    if args_len != min {
        Some(format!(
            "macro `{}` expects {} argument(s), found {}",
            def.name, min, args_len
        ))
    } else {
        None
    }
}

fn coalesce_repeat_args(
    params: &[MacroParam],
    args: Vec<MacroArg>,
    call_span: Span,
) -> Vec<MacroArg> {
    if params.last().map(|p| p.kind) != Some(MacroParamKind::Repeat) {
        return args;
    }
    let fixed = params.len().saturating_sub(1);
    let mut out = Vec::new();
    out.extend_from_slice(&args[..fixed.min(args.len())]);
    let repeat_slice = if args.len() > fixed {
        &args[fixed..]
    } else {
        &[]
    };
    let exprs: Vec<Expr> = repeat_slice.iter().map(|a| a.expr.clone()).collect();
    let span = match (repeat_slice.first(), repeat_slice.last()) {
        (Some(first), Some(last)) => {
            let start = expr_span_local(&first.expr).start;
            let end = expr_span_local(&last.expr).end;
            Span::new(start, end)
        }
        _ => call_span,
    };
    let combined_expr = Expr::Tuple(exprs, span);
    let combined_tokens =
        merge_tokens_with_separator(repeat_slice, repeat_separator_hint(repeat_slice));
    out.push(MacroArg {
        expr: combined_expr,
        tokens: combined_tokens,
    });
    out
}

fn merge_tokens_with_separator(args: &[MacroArg], separator: TokenKind) -> Option<Vec<Token>> {
    let mut merged: Vec<Token> = Vec::new();
    for (idx, arg) in args.iter().enumerate() {
        let Some(tokens) = &arg.tokens else {
            return None;
        };
        if idx > 0 {
            if let (Some(prev_end), Some(next_start)) = (
                merged.last().map(|t| t.span.end),
                tokens.first().map(|t| t.span.start),
            ) {
                merged.push(Token {
                    kind: separator.clone(),
                    span: Span::new(prev_end, next_start.max(prev_end + 1)),
                });
            }
        }
        merged.extend_from_slice(tokens);
    }
    Some(merged)
}

fn repeat_separator_hint(args: &[MacroArg]) -> TokenKind {
    args.iter()
        .find_map(|arg| {
            arg.tokens
                .as_ref()
                .map(|tokens| find_repeat_spec(tokens).separator.clone())
        })
        .flatten()
        .unwrap_or(TokenKind::Comma)
}

impl ExpansionTraces {
    fn record(&mut self, path: &Path, span: Span, frames: &[MacroFrame]) {
        if frames.is_empty() {
            return;
        }
        let entry = TraceEntry {
            span,
            frames: frames.to_vec(),
        };
        self.entries
            .entry(path.to_path_buf())
            .or_default()
            .push(entry);
    }

    pub fn help_for(&self, path: &Path, span: Span) -> Option<String> {
        let entries = self.entries.get(path)?;
        let mut best: Option<&TraceEntry> = None;
        for entry in entries {
            if entry.span.start <= span.start && entry.span.end >= span.end {
                let is_narrower = best
                    .map(|best| {
                        let current_len = entry.span.len();
                        let best_len = best.span.len();
                        current_len < best_len
                            || (current_len == best_len && entry.span.end < best.span.end)
                    })
                    .unwrap_or(true);
                if is_narrower {
                    best = Some(entry);
                }
            }
        }
        let entry = best.or_else(|| entries.iter().find(|e| spans_overlap(e.span, span)))?;
        Some(format_trace(&entry.frames))
    }

    pub fn macro_names_for(&self, path: &Path, span: Span) -> Option<Vec<String>> {
        let entries = self.entries.get(path)?;
        let mut best: Option<&TraceEntry> = None;
        for entry in entries {
            if entry.span.start <= span.start && entry.span.end >= span.end {
                let is_narrower = best
                    .map(|best| {
                        let current_len = entry.span.len();
                        let best_len = best.span.len();
                        current_len < best_len
                            || (current_len == best_len && entry.span.end < best.span.end)
                    })
                    .unwrap_or(true);
                if is_narrower {
                    best = Some(entry);
                }
            }
        }
        let entry = best.or_else(|| entries.iter().find(|e| spans_overlap(e.span, span)))?;
        Some(entry.frames.iter().map(|f| f.name.clone()).collect())
    }
}

impl<'a> Expander<'a> {
    fn new(registry: MacroRegistry<'a>) -> Self {
        Self {
            registry,
            gensym: 0,
            stack: Vec::new(),
            traces: ExpansionTraces::default(),
            current_path: None,
            item_origins: HashMap::new(),
        }
    }

    fn expand_program(&mut self, program: &Program) -> Result<Program, Vec<MacroExpansionError>> {
        let mut expanded_modules = Vec::new();
        let mut errors = Vec::new();
        for module in &program.modules {
            self.current_path = Some(module.path.clone());
            let mut module_errors = Vec::new();
            let mut expanded_items: Vec<(Item, Option<String>)> = Vec::new();
            for item in &module.items {
                self.expand_item(item, &mut module_errors, &mut expanded_items);
            }
            module_errors.extend(validate_no_duplicate_items(&expanded_items));
            if !module_errors.is_empty() {
                errors.push(MacroExpansionError {
                    path: module.path.clone(),
                    errors: module_errors,
                });
            }
            let items = expanded_items
                .iter()
                .map(|(item, _)| item.clone())
                .collect::<Vec<_>>();
            let origins = expanded_items
                .iter()
                .map(|(_, origin)| origin.clone())
                .collect();
            expanded_modules.push(Module {
                name: module.name.clone(),
                kind: module.kind,
                path: module.path.clone(),
                declared_name: module.declared_name.clone(),
                declared_span: module.declared_span,
                redundant_module_spans: module.redundant_module_spans.clone(),
                imports: module.imports.clone(),
                prelude: module.prelude.clone(),
                items,
            });
            self.item_origins.insert(module.path.clone(), origins);
        }
        self.current_path = None;
        if errors.is_empty() {
            Ok(Program {
                modules: expanded_modules,
            })
        } else {
            Err(errors)
        }
    }

    fn expand_item(
        &mut self,
        item: &Item,
        errors: &mut Vec<SyntaxError>,
        out: &mut Vec<(Item, Option<String>)>,
    ) {
        let origin = self.stack.last().map(|f| f.name.clone());
        match item {
            Item::Function(def) => {
                out.push((Item::Function(self.expand_function(def, errors)), origin))
            }
            Item::Const(def) => out.push((Item::Const(self.expand_const(def, errors)), origin)),
            Item::Impl(block) => out.push((Item::Impl(self.expand_impl(block, errors)), origin)),
            Item::Struct(_) | Item::Enum(_) | Item::Interface(_) | Item::Macro(_) => {
                out.push((item.clone(), origin))
            }
            Item::MacroInvocation(invocation) => {
                let expanded = self.expand_item_macro(invocation, errors);
                out.extend(expanded);
            }
        }
    }

    fn expand_item_macro(
        &mut self,
        invocation: &MacroInvocation,
        errors: &mut Vec<SyntaxError>,
    ) -> Vec<(Item, Option<String>)> {
        self.stack.push(MacroFrame {
            name: invocation.name.name.clone(),
            call_span: invocation.span,
        });
        let expanded_args: Vec<MacroArg> = invocation
            .args
            .iter()
            .map(|arg| MacroArg {
                expr: self.expand_expr(&arg.expr, 1, invocation.span, errors),
                tokens: arg.tokens.clone(),
            })
            .collect();
        let Some((def, _module)) = self
            .registry
            .get(&invocation.name.name, self.current_path.as_deref())
        else {
            let mut err = SyntaxError::new(
                format!("unknown macro `{}`", invocation.name.name),
                invocation.span,
            );
            err.help = self.trace_help();
            errors.push(err);
            self.stack.pop();
            return vec![(
                Item::MacroInvocation(MacroInvocation {
                    name: invocation.name.clone(),
                    args: expanded_args,
                    span: invocation.span,
                }),
                None,
            )];
        };
        if let Some(msg) = arity_error(def, expanded_args.len()) {
            let mut err = SyntaxError::new(msg, invocation.span);
            err.help = self.trace_help();
            errors.push(err);
            self.stack.pop();
            return vec![(
                Item::MacroInvocation(MacroInvocation {
                    name: invocation.name.clone(),
                    args: expanded_args,
                    span: invocation.span,
                }),
                None,
            )];
        }
        let expanded_args = coalesce_repeat_args(&def.params, expanded_args, invocation.span);
        if let Some(msg) = arity_error(def, expanded_args.len()) {
            let mut err = SyntaxError::new(msg, invocation.span);
            err.help = self.trace_help();
            errors.push(err);
            self.stack.pop();
            return vec![(
                Item::MacroInvocation(MacroInvocation {
                    name: invocation.name.clone(),
                    args: expanded_args,
                    span: invocation.span,
                }),
                None,
            )];
        }

        let MacroBody::Items(items, body_span) = &def.body else {
            let mut err = SyntaxError::new(
                format!("macro `{}` cannot be used as an item macro", def.name),
                invocation.span,
            );
            err.help = self.trace_help();
            errors.push(err);
            self.stack.pop();
            return vec![(
                Item::MacroInvocation(MacroInvocation {
                    name: invocation.name.clone(),
                    args: expanded_args,
                    span: invocation.span,
                }),
                None,
            )];
        };

        let mut substitution_map = HashMap::new();
        let mut pattern_params = HashSet::new();
        let mut pattern_map = HashMap::new();
        for (param, arg) in def.params.iter().zip(expanded_args.iter()) {
            if matches!(param.kind, MacroParamKind::Pattern) {
                pattern_params.insert(param.name.clone());
                if let Some(tokens) = &arg.tokens {
                    if let Some(path) = &self.current_path {
                        match crate::language::parser::parse_pattern_from_tokens(
                            path.clone(),
                            tokens.clone(),
                        ) {
                            Ok(pat) => {
                                pattern_map.insert(param.name.clone(), pat);
                            }
                            Err(errs) => {
                                self.push_macro_errors(errors, errs.errors);
                            }
                        }
                    }
                }
            }
            substitution_map.insert(
                param.name.clone(),
                self.macro_arg_to_expr(param.kind, arg, invocation.span, errors),
            );
        }
        let subst = Substitution {
            exprs: substitution_map,
            patterns: pattern_map,
            pattern_params,
        };

        self.gensym += 1;
        let call_id = self.gensym;

        let mut substituted: Vec<Item> = items
            .iter()
            .cloned()
            .map(|item| substitute_item(item, &subst))
            .collect();

        let macro_locals = collect_bindings_from_body(&def.body);
        substituted = rename_macro_locals_items(substituted, call_id, &macro_locals);

        let mut output = Vec::new();
        for item in substituted {
            self.expand_item(&item, errors, &mut output);
        }

        self.record_macro_trace(invocation.span.union(*body_span));
        self.stack.pop();
        output
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
        FunctionDef {
            body,
            ..def.clone()
        }
    }

    fn expand_const(&mut self, def: &ConstDef, errors: &mut Vec<SyntaxError>) -> ConstDef {
        let value = self.expand_expr(&def.value, 0, def.span, errors);
        ConstDef {
            value,
            ..def.clone()
        }
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

    fn expand_block(
        &mut self,
        block: &Block,
        _scope_span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> Block {
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
            Statement::MacroSemi(expr) => Statement::MacroSemi(Spanned::new(
                self.expand_expr(&expr.node, 0, _scope_span, errors),
                expr.span,
            )),
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
            Statement::Block(block) => {
                Statement::Block(Box::new(self.expand_block(block, _scope_span, errors)))
            }
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
                let mut err = SyntaxError::new("macro expansion exceeded recursion limit", *span);
                err.help = self.trace_help();
                errors.push(err);
            }
            return expr.clone();
        }
        match expr {
            Expr::MacroCall { name, args, span } => {
                self.stack.push(MacroFrame {
                    name: name.name.clone(),
                    call_span: *span,
                });
                let expanded_args_raw: Vec<MacroArg> = args
                    .iter()
                    .map(|arg| MacroArg {
                        expr: self.expand_expr(&arg.expr, depth + 1, scope_span, errors),
                        tokens: arg.tokens.clone(),
                    })
                    .collect();
                let result = match self.registry.get(&name.name, self.current_path.as_deref()) {
                    None => {
                        let mut err =
                            SyntaxError::new(format!("unknown macro `{}`", name.name), *span);
                        err.help = self.trace_help();
                        errors.push(err);
                        Expr::MacroCall {
                            name: name.clone(),
                            args: expanded_args_raw,
                            span: *span,
                        }
                    }
                    Some((def, _)) => {
                        if let Some(msg) = arity_error(def, expanded_args_raw.len()) {
                            let mut err = SyntaxError::new(msg, *span);
                            err.help = self.trace_help();
                            errors.push(err);
                            return Expr::MacroCall {
                                name: name.clone(),
                                args: expanded_args_raw,
                                span: *span,
                            };
                        }
                        let expanded_args =
                            coalesce_repeat_args(&def.params, expanded_args_raw, *span);
                        if let Some(msg) = arity_error(def, expanded_args.len()) {
                            let mut err = SyntaxError::new(msg, *span);
                            err.help = self.trace_help();
                            errors.push(err);
                            Expr::MacroCall {
                                name: name.clone(),
                                args: expanded_args,
                                span: *span,
                            }
                        } else {
                            // Evaluate arguments once into fresh bindings to avoid double evaluation and
                            // reduce capture.
                            self.gensym += 1;
                            let call_id = self.gensym;
                            let mut param_bindings = Vec::new();
                            let mut substitution_map = HashMap::new();
                            let mut pattern_params = HashSet::new();
                            let mut pattern_map = HashMap::new();
                            for (idx, (param, arg)) in
                                def.params.iter().zip(expanded_args.iter()).enumerate()
                            {
                                match param.kind {
                                    MacroParamKind::Expr => {
                                        let binding_name = format!("__macro_arg_{call_id}_{idx}");
                                        param_bindings.push((
                                            param,
                                            arg.expr.clone(),
                                            binding_name.clone(),
                                        ));
                                        substitution_map.insert(
                                            param.name.clone(),
                                            Expr::Identifier(Identifier {
                                                name: binding_name,
                                                span: *span,
                                            }),
                                        );
                                    }
                                    MacroParamKind::Block
                                    | MacroParamKind::Tokens
                                    | MacroParamKind::Repeat => {
                                        substitution_map.insert(
                                            param.name.clone(),
                                            self.macro_arg_to_expr(param.kind, arg, *span, errors),
                                        );
                                    }
                                    MacroParamKind::Pattern => {
                                        pattern_params.insert(param.name.clone());
                                        if let Some(tokens) = &arg.tokens {
                                            if let Some(path) = &self.current_path {
                                                match crate::language::parser::parse_pattern_from_tokens(
                                                    path.clone(),
                                                    tokens.clone(),
                                                ) {
                                                    Ok(pat) => {
                                                        pattern_map.insert(param.name.clone(), pat);
                                                    }
                                                    Err(errs) => {
                                                        self.push_macro_errors(errors, errs.errors);
                                                    }
                                                }
                                            }
                                        }
                                        substitution_map.insert(
                                            param.name.clone(),
                                            self.macro_arg_to_expr(param.kind, arg, *span, errors),
                                        );
                                    }
                                }
                            }
                            let subst = Substitution {
                                exprs: substitution_map,
                                patterns: pattern_map,
                                pattern_params,
                            };

                            let inlined_body = match &def.body {
                                MacroBody::Expr(expr) => substitute_expr(expr.node.clone(), &subst),
                                MacroBody::Block(block) => {
                                    substitute_expr(Expr::Block(block.clone()), &subst)
                                }
                                MacroBody::Items(_, _) => {
                                    let mut err = SyntaxError::new(
                                        format!(
                                            "macro `{}` cannot be used as an expression macro",
                                            def.name
                                        ),
                                        *span,
                                    );
                                    err.help = self.trace_help();
                                    errors.push(err);
                                    self.stack.pop();
                                    return Expr::MacroCall {
                                        name: name.clone(),
                                        args: expanded_args,
                                        span: *span,
                                    };
                                }
                            };
                            let macro_locals = collect_bindings_from_body(&def.body);
                            let renamed_body =
                                rename_macro_locals(&inlined_body, call_id, &macro_locals);
                            let expanded_body =
                                self.expand_expr(&renamed_body, depth + 1, scope_span, errors);

                            let mut statements = Vec::new();
                            for (param, arg_expr, binding_name) in param_bindings {
                                let let_span = *span;
                                statements.push(Statement::Let(LetStmt {
                                    pattern: Pattern::Identifier(binding_name, let_span),
                                    ty: param.ty.clone(),
                                    value: Some(arg_expr),
                                    mutability: Mutability::Immutable,
                                    span: let_span,
                                }));
                            }

                            let mut block_span_end = span.end;
                            let (statements, tail) = if let Expr::Block(body_block) = &expanded_body
                            {
                                block_span_end = body_block.span.end.max(block_span_end);
                                let mut merged_stmts = statements;
                                merged_stmts.extend(body_block.statements.clone());
                                (merged_stmts, body_block.tail.clone())
                            } else {
                                let tail_span = expr_span_local(&expanded_body);
                                block_span_end = block_span_end.max(tail_span.end);
                                (statements, Some(Box::new(expanded_body)))
                            };

                            let block_span = Span::new(span.start, block_span_end);
                            let block = Block {
                                statements,
                                tail,
                                span: block_span,
                            };
                            self.record_macro_trace(block_span);
                            Expr::Block(Box::new(block))
                        }
                    }
                };
                self.stack.pop();
                result
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
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => Expr::Binary {
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
                                value: self.expand_expr(
                                    &entry.value,
                                    depth + 1,
                                    scope_span,
                                    errors,
                                ),
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
            Expr::Block(block) => {
                Expr::Block(Box::new(self.expand_block(block, scope_span, errors)))
            }
            Expr::If(if_expr) => Expr::If(Box::new(self.expand_if(if_expr, scope_span, errors))),
            Expr::Match(match_expr) => {
                Expr::Match(self.expand_match(match_expr, scope_span, errors))
            }
            Expr::Reference {
                mutable,
                expr,
                span,
            } => Expr::Reference {
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
            else_branch: expr.else_branch.as_ref().map(|branch| match branch {
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

fn substitute_expr(expr: Expr, subst: &Substitution) -> Expr {
    match expr {
        Expr::Identifier(mut ident) => {
            if let Some(raw) = ident.name.strip_prefix('@') {
                ident.name = raw.to_string();
                return Expr::Identifier(ident);
            }
            subst
                .exprs
                .get(&ident.name)
                .cloned()
                .unwrap_or(Expr::Identifier(ident))
        }
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => Expr::Binary {
            op,
            left: Box::new(substitute_expr(*left, subst)),
            right: Box::new(substitute_expr(*right, subst)),
            span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op,
            expr: Box::new(substitute_expr(*expr, subst)),
            span,
        },
        Expr::Call {
            callee,
            type_args,
            args,
            span,
        } => Expr::Call {
            callee: Box::new(substitute_expr(*callee, subst)),
            type_args,
            args: args
                .into_iter()
                .map(|arg| substitute_expr(arg, subst))
                .collect(),
            span,
        },
        Expr::FieldAccess { base, field, span } => Expr::FieldAccess {
            base: Box::new(substitute_expr(*base, subst)),
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
                            value: substitute_expr(entry.value, subst),
                        })
                        .collect(),
                ),
                StructLiteralKind::Positional(values) => StructLiteralKind::Positional(
                    values
                        .into_iter()
                        .map(|v| substitute_expr(v, subst))
                        .collect(),
                ),
            },
            span,
        },
        Expr::EnumLiteral {
            enum_name,
            variant,
            values,
            span,
        } => Expr::EnumLiteral {
            enum_name,
            variant,
            values: values
                .into_iter()
                .map(|v| substitute_expr(v, subst))
                .collect(),
            span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .into_iter()
                .map(|entry| MapLiteralEntry {
                    key: substitute_expr(entry.key, subst),
                    value: substitute_expr(entry.value, subst),
                })
                .collect(),
            span,
        },
        Expr::Block(block) => Expr::Block(Box::new(substitute_block(*block, subst))),
        Expr::If(if_expr) => Expr::If(Box::new(substitute_if(*if_expr, subst))),
        Expr::Match(match_expr) => Expr::Match(MatchExpr {
            expr: Box::new(substitute_expr(*match_expr.expr, subst)),
            arms: match_expr
                .arms
                .into_iter()
                .map(|arm| MatchArmExpr {
                    pattern: substitute_pattern(arm.pattern, subst),
                    guard: arm.guard.map(|g| substitute_expr(g, subst)),
                    value: substitute_expr(arm.value, subst),
                })
                .collect(),
            span: match_expr.span,
        }),
        Expr::Tuple(values, span) => Expr::Tuple(
            values
                .into_iter()
                .map(|v| substitute_expr(v, subst))
                .collect(),
            span,
        ),
        Expr::ArrayLiteral(values, span) => Expr::ArrayLiteral(
            values
                .into_iter()
                .map(|v| substitute_expr(v, subst))
                .collect(),
            span,
        ),
        Expr::Range(range) => Expr::Range(RangeExpr {
            start: Box::new(substitute_expr(*range.start, subst)),
            end: Box::new(substitute_expr(*range.end, subst)),
            inclusive: range.inclusive,
            span: range.span,
        }),
        Expr::Index { base, index, span } => Expr::Index {
            base: Box::new(substitute_expr(*base, subst)),
            index: Box::new(substitute_expr(*index, subst)),
            span,
        },
        Expr::Reference {
            mutable,
            expr,
            span,
        } => Expr::Reference {
            mutable,
            expr: Box::new(substitute_expr(*expr, subst)),
            span,
        },
        Expr::Deref { expr, span } => Expr::Deref {
            expr: Box::new(substitute_expr(*expr, subst)),
            span,
        },
        Expr::Move { expr, span } => Expr::Move {
            expr: Box::new(substitute_expr(*expr, subst)),
            span,
        },
        Expr::Spawn { expr, span } => Expr::Spawn {
            expr: Box::new(substitute_expr(*expr, subst)),
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
                        expr: substitute_expr(expr, subst),
                        span,
                    },
                })
                .collect(),
            span: literal.span,
        }),
        Expr::MacroCall { name, args, span } => Expr::MacroCall {
            name,
            args: args
                .into_iter()
                .map(|arg| MacroArg {
                    expr: substitute_expr(arg.expr, subst),
                    tokens: arg.tokens,
                })
                .collect(),
            span,
        },
        Expr::Literal(_) | Expr::Try { .. } | Expr::TryPropagate { .. } => expr,
    }
}

fn substitute_block(block: Block, subst: &Substitution) -> Block {
    Block {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| substitute_statement(stmt, subst))
            .collect(),
        tail: block
            .tail
            .map(|expr| Box::new(substitute_expr(*expr, subst))),
        span: block.span,
    }
}

fn substitute_statement(stmt: Statement, subst: &Substitution) -> Statement {
    match stmt {
        Statement::Let(let_stmt) => Statement::Let(LetStmt {
            value: let_stmt.value.map(|expr| substitute_expr(expr, subst)),
            pattern: substitute_pattern(let_stmt.pattern, subst),
            ..let_stmt
        }),
        Statement::MacroSemi(expr) => {
            Statement::MacroSemi(Spanned::new(substitute_expr(expr.node, subst), expr.span))
        }
        Statement::Assign(assign) => Statement::Assign(AssignStmt {
            target: substitute_expr(assign.target, subst),
            value: substitute_expr(assign.value, subst),
        }),
        Statement::Expr(expr) => Statement::Expr(ExprStmt {
            expr: substitute_expr(expr.expr, subst),
        }),
        Statement::Return(ret) => Statement::Return(ReturnStmt {
            values: ret
                .values
                .into_iter()
                .map(|expr| substitute_expr(expr, subst))
                .collect(),
        }),
        Statement::While(while_stmt) => Statement::While(WhileStmt {
            condition: match while_stmt.condition {
                WhileCondition::Expr(expr) => WhileCondition::Expr(substitute_expr(expr, subst)),
                WhileCondition::Let { pattern, value } => WhileCondition::Let {
                    pattern: substitute_pattern(pattern, subst),
                    value: substitute_expr(value, subst),
                },
            },
            body: substitute_block(while_stmt.body, subst),
        }),
        Statement::Loop(loop_stmt) => Statement::Loop(LoopStmt {
            body: substitute_block(loop_stmt.body, subst),
            span: loop_stmt.span,
        }),
        Statement::For(for_stmt) => Statement::For(ForStmt {
            binding: for_stmt.binding,
            target: match for_stmt.target {
                ForTarget::Range(range) => ForTarget::Range(RangeExpr {
                    start: Box::new(substitute_expr(*range.start, subst)),
                    end: Box::new(substitute_expr(*range.end, subst)),
                    inclusive: range.inclusive,
                    span: range.span,
                }),
                ForTarget::Collection(expr) => ForTarget::Collection(substitute_expr(expr, subst)),
            },
            body: substitute_block(for_stmt.body, subst),
            span: for_stmt.span,
        }),
        Statement::Defer(defer_stmt) => Statement::Defer(DeferStmt {
            expr: substitute_expr(defer_stmt.expr, subst),
        }),
        Statement::Block(block) => Statement::Block(Box::new(substitute_block(*block, subst))),
        Statement::Break | Statement::Continue => stmt,
    }
}

fn substitute_if(if_expr: IfExpr, subst: &Substitution) -> IfExpr {
    IfExpr {
        condition: match if_expr.condition {
            IfCondition::Expr(expr) => IfCondition::Expr(substitute_expr(expr, subst)),
            IfCondition::Let { pattern, value } => IfCondition::Let {
                pattern: substitute_pattern(pattern, subst),
                value: substitute_expr(value, subst),
            },
        },
        then_branch: substitute_block(if_expr.then_branch, subst),
        else_branch: if_expr.else_branch.map(|branch| match branch {
            ElseBranch::Block(block) => ElseBranch::Block(substitute_block(block, subst)),
            ElseBranch::ElseIf(inner) => ElseBranch::ElseIf(Box::new(substitute_if(*inner, subst))),
        }),
        span: if_expr.span,
    }
}

fn substitute_item(item: Item, subst: &Substitution) -> Item {
    match item {
        Item::Function(def) => Item::Function(substitute_function(def, subst)),
        Item::Const(def) => Item::Const(ConstDef {
            value: substitute_expr(def.value, subst),
            ..def
        }),
        Item::Impl(block) => Item::Impl(ImplBlock {
            methods: block
                .methods
                .into_iter()
                .map(|method| substitute_function(method, subst))
                .collect(),
            ..block
        }),
        Item::MacroInvocation(inv) => Item::MacroInvocation(MacroInvocation {
            args: inv
                .args
                .into_iter()
                .map(|arg| MacroArg {
                    expr: substitute_expr(arg.expr, subst),
                    tokens: arg.tokens,
                })
                .collect(),
            ..inv
        }),
        other => other,
    }
}

fn substitute_function(def: FunctionDef, subst: &Substitution) -> FunctionDef {
    let body = match def.body {
        FunctionBody::Expr(expr) => {
            FunctionBody::Expr(Spanned::new(substitute_expr(expr.node, subst), expr.span))
        }
        FunctionBody::Block(block) => {
            FunctionBody::Block(Box::new(substitute_block(*block, subst)))
        }
    };
    FunctionDef { body, ..def }
}

fn substitute_pattern(pattern: Pattern, subst: &Substitution) -> Pattern {
    match pattern {
        Pattern::Identifier(name, span) => {
            if subst.pattern_params.contains(&name) {
                if let Some(pat) = subst.patterns.get(&name) {
                    return pat.clone();
                }
                if let Some(expr) = subst.exprs.get(&name) {
                    if let Some(pat) = expr_to_pattern(expr) {
                        return pat;
                    }
                }
            }
            Pattern::Identifier(name, span)
        }
        Pattern::EnumVariant {
            enum_name,
            variant,
            bindings,
        } => Pattern::EnumVariant {
            enum_name,
            variant,
            bindings: bindings
                .into_iter()
                .map(|b| substitute_pattern(b, subst))
                .collect(),
        },
        Pattern::Tuple(bindings, span) => Pattern::Tuple(
            bindings
                .into_iter()
                .map(|b| substitute_pattern(b, subst))
                .collect(),
            span,
        ),
        Pattern::Map(entries, span) => Pattern::Map(
            entries
                .into_iter()
                .map(|entry| MapPatternEntry {
                    key: entry.key,
                    pattern: substitute_pattern(entry.pattern, subst),
                })
                .collect(),
            span,
        ),
        Pattern::Struct {
            struct_name,
            fields,
            has_spread,
            span,
        } => Pattern::Struct {
            struct_name,
            fields: fields
                .into_iter()
                .map(|field| StructPatternField {
                    name: field.name,
                    pattern: substitute_pattern(field.pattern, subst),
                })
                .collect(),
            has_spread,
            span,
        },
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            span,
        } => Pattern::Slice {
            prefix: prefix
                .into_iter()
                .map(|p| substitute_pattern(p, subst))
                .collect(),
            rest: rest.map(|p| Box::new(substitute_pattern(*p, subst))),
            suffix: suffix
                .into_iter()
                .map(|p| substitute_pattern(p, subst))
                .collect(),
            span,
        },
        Pattern::Literal(_) | Pattern::Wildcard => pattern,
    }
}

fn expr_to_pattern(expr: &Expr) -> Option<Pattern> {
    match expr {
        Expr::Identifier(ident) => Some(Pattern::Identifier(ident.name.clone(), ident.span)),
        Expr::Literal(lit) => Some(Pattern::Literal(lit.clone())),
        Expr::Tuple(values, span) => {
            let mut bindings = Vec::new();
            for v in values {
                bindings.push(expr_to_pattern(v)?);
            }
            Some(Pattern::Tuple(bindings, *span))
        }
        Expr::StructLiteral { name, fields, span } => {
            let mut pat_fields = Vec::new();
            if let StructLiteralKind::Named(entries) = fields {
                for entry in entries {
                    pat_fields.push(StructPatternField {
                        name: entry.name.clone(),
                        pattern: expr_to_pattern(&entry.value)?,
                    });
                }
                Some(Pattern::Struct {
                    struct_name: Some(name.clone()),
                    fields: pat_fields,
                    has_spread: false,
                    span: *span,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}
fn collect_bindings_from_body(body: &MacroBody) -> HashSet<String> {
    match body {
        MacroBody::Expr(expr) => collect_bindings_expr(&expr.node),
        MacroBody::Block(block) => collect_bindings_block(block),
        MacroBody::Items(items, _) => {
            let mut set = HashSet::new();
            for item in items {
                set.extend(collect_bindings_item(item));
            }
            set
        }
    }
}

fn collect_bindings_block(block: &Block) -> HashSet<String> {
    let mut set = HashSet::new();
    for stmt in &block.statements {
        set.extend(collect_bindings_statement(stmt));
    }
    if let Some(tail) = &block.tail {
        set.extend(collect_bindings_expr(tail));
    }
    set
}

fn collect_bindings_statement(stmt: &Statement) -> HashSet<String> {
    let mut set = HashSet::new();
    match stmt {
        Statement::Let(let_stmt) => set.extend(collect_bindings_pattern(&let_stmt.pattern)),
        Statement::MacroSemi(expr) => set.extend(collect_bindings_expr(&expr.node)),
        Statement::For(for_stmt) => {
            set.insert(for_stmt.binding.clone());
            match &for_stmt.target {
                ForTarget::Range(range) => {
                    set.extend(collect_bindings_expr(&range.start));
                    set.extend(collect_bindings_expr(&range.end));
                }
                ForTarget::Collection(expr) => {
                    set.extend(collect_bindings_expr(expr));
                }
            }
            set.extend(collect_bindings_block(&for_stmt.body));
        }
        Statement::While(while_stmt) => match &while_stmt.condition {
            WhileCondition::Expr(expr) => set.extend(collect_bindings_expr(expr)),
            WhileCondition::Let { pattern, value } => {
                set.extend(collect_bindings_pattern(pattern));
                set.extend(collect_bindings_expr(value));
            }
        },
        Statement::Loop(loop_stmt) => set.extend(collect_bindings_block(&loop_stmt.body)),
        Statement::Defer(defer_stmt) => set.extend(collect_bindings_expr(&defer_stmt.expr)),
        Statement::Assign(assign) => {
            set.extend(collect_bindings_expr(&assign.target));
            set.extend(collect_bindings_expr(&assign.value));
        }
        Statement::Expr(expr) => set.extend(collect_bindings_expr(&expr.expr)),
        Statement::Return(ret) => {
            for value in &ret.values {
                set.extend(collect_bindings_expr(value));
            }
        }
        Statement::Block(block) => set.extend(collect_bindings_block(block)),
        Statement::Break | Statement::Continue => {}
    }
    set
}

fn collect_bindings_expr(expr: &Expr) -> HashSet<String> {
    let mut set = HashSet::new();
    match expr {
        Expr::Identifier(_) | Expr::Literal(_) | Expr::FormatString(_) => {}
        Expr::Try { block, .. } => set.extend(collect_bindings_block(block)),
        Expr::TryPropagate { expr, .. } => set.extend(collect_bindings_expr(expr)),
        Expr::Binary { left, right, .. } => {
            set.extend(collect_bindings_expr(left));
            set.extend(collect_bindings_expr(right));
        }
        Expr::Unary { expr, .. } => set.extend(collect_bindings_expr(expr)),
        Expr::Call { callee, args, .. } => {
            set.extend(collect_bindings_expr(callee));
            for arg in args {
                set.extend(collect_bindings_expr(arg));
            }
        }
        Expr::MacroCall { args, .. } => {
            for arg in args {
                set.extend(collect_bindings_expr(&arg.expr));
            }
        }
        Expr::FieldAccess { base, .. } => set.extend(collect_bindings_expr(base)),
        Expr::Index { base, index, .. } => {
            set.extend(collect_bindings_expr(base));
            set.extend(collect_bindings_expr(index));
        }
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    set.extend(collect_bindings_expr(&field.value));
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    set.extend(collect_bindings_expr(value));
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                set.extend(collect_bindings_expr(value));
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                set.extend(collect_bindings_expr(&entry.key));
                set.extend(collect_bindings_expr(&entry.value));
            }
        }
        Expr::Block(block) => set.extend(collect_bindings_block(block)),
        Expr::If(if_expr) => {
            match &if_expr.condition {
                IfCondition::Expr(condition) => set.extend(collect_bindings_expr(condition)),
                IfCondition::Let { pattern, value } => {
                    set.extend(collect_bindings_pattern(pattern));
                    set.extend(collect_bindings_expr(value));
                }
            }
            set.extend(collect_bindings_block(&if_expr.then_branch));
            if let Some(else_branch) = &if_expr.else_branch {
                match else_branch {
                    ElseBranch::Block(block) => set.extend(collect_bindings_block(block)),
                    ElseBranch::ElseIf(inner) => {
                        let expr = Expr::If(inner.clone());
                        set.extend(collect_bindings_expr(&expr));
                    }
                }
            }
        }
        Expr::Match(match_expr) => {
            set.extend(collect_bindings_expr(&match_expr.expr));
            for arm in &match_expr.arms {
                set.extend(collect_bindings_pattern(&arm.pattern));
                if let Some(guard) = &arm.guard {
                    set.extend(collect_bindings_expr(guard));
                }
                set.extend(collect_bindings_expr(&arm.value));
            }
        }
        Expr::Tuple(values, _) => {
            for value in values {
                set.extend(collect_bindings_expr(value));
            }
        }
        Expr::ArrayLiteral(values, _) => {
            for value in values {
                set.extend(collect_bindings_expr(value));
            }
        }
        Expr::Range(range) => {
            set.extend(collect_bindings_expr(&range.start));
            set.extend(collect_bindings_expr(&range.end));
        }
        Expr::Reference { expr, .. }
        | Expr::Deref { expr, .. }
        | Expr::Move { expr, .. }
        | Expr::Spawn { expr, .. } => set.extend(collect_bindings_expr(expr)),
    }
    set
}

fn collect_bindings_pattern(pattern: &Pattern) -> HashSet<String> {
    let mut set = HashSet::new();
    match pattern {
        Pattern::Identifier(name, _) => {
            if name != "_" {
                set.insert(name.clone());
            }
        }
        Pattern::EnumVariant { bindings, .. } => {
            for b in bindings {
                set.extend(collect_bindings_pattern(b));
            }
        }
        Pattern::Tuple(bindings, _) => {
            for b in bindings {
                set.extend(collect_bindings_pattern(b));
            }
        }
        Pattern::Map(entries, _) => {
            for entry in entries {
                set.extend(collect_bindings_pattern(&entry.pattern));
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                set.extend(collect_bindings_pattern(&field.pattern));
            }
        }
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                set.extend(collect_bindings_pattern(p));
            }
            if let Some(rest) = rest {
                set.extend(collect_bindings_pattern(rest));
            }
            for p in suffix {
                set.extend(collect_bindings_pattern(p));
            }
        }
        Pattern::Literal(_) | Pattern::Wildcard => {}
    }
    set
}

fn rename_macro_locals(expr: &Expr, call_id: usize, locals: &HashSet<String>) -> Expr {
    if locals.is_empty() {
        return expr.clone();
    }
    let mut map = HashMap::new();
    for (idx, name) in locals.iter().enumerate() {
        map.insert(name.clone(), format!("__macro_local_{call_id}_{idx}"));
    }
    rename_expr(expr.clone(), &map)
}

fn rename_macro_locals_items(
    items: Vec<Item>,
    call_id: usize,
    locals: &HashSet<String>,
) -> Vec<Item> {
    if locals.is_empty() {
        return items;
    }
    let mut map = HashMap::new();
    for (idx, name) in locals.iter().enumerate() {
        map.insert(name.clone(), format!("__macro_local_{call_id}_{idx}"));
    }
    items
        .into_iter()
        .map(|item| rename_item(item, &map))
        .collect()
}

fn rename_expr(expr: Expr, map: &HashMap<String, String>) -> Expr {
    match expr {
        Expr::Identifier(mut ident) => {
            if let Some(raw) = ident.name.strip_prefix('@') {
                ident.name = raw.to_string();
                return Expr::Identifier(ident);
            }
            if let Some(new) = map.get(&ident.name) {
                ident.name = new.clone();
            }
            Expr::Identifier(ident)
        }
        Expr::Literal(_) => expr,
        Expr::FormatString(mut lit) => {
            for seg in &mut lit.segments {
                if let FormatSegment::Expr { expr, .. } = seg {
                    *expr = rename_expr(expr.clone(), map);
                }
            }
            Expr::FormatString(lit)
        }
        Expr::Try { block, span } => Expr::Try {
            block: Box::new(rename_block(*block, map)),
            span,
        },
        Expr::TryPropagate { expr, span } => Expr::TryPropagate {
            expr: Box::new(rename_expr(*expr, map)),
            span,
        },
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => Expr::Binary {
            op,
            left: Box::new(rename_expr(*left, map)),
            right: Box::new(rename_expr(*right, map)),
            span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op,
            expr: Box::new(rename_expr(*expr, map)),
            span,
        },
        Expr::Call {
            callee,
            type_args,
            args,
            span,
        } => Expr::Call {
            callee: Box::new(rename_expr(*callee, map)),
            type_args,
            args: args.into_iter().map(|arg| rename_expr(arg, map)).collect(),
            span,
        },
        Expr::MacroCall { name, args, span } => Expr::MacroCall {
            name,
            args: args
                .into_iter()
                .map(|arg| MacroArg {
                    expr: rename_expr(arg.expr, map),
                    tokens: arg.tokens,
                })
                .collect(),
            span,
        },
        Expr::FieldAccess { base, field, span } => Expr::FieldAccess {
            base: Box::new(rename_expr(*base, map)),
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
                            value: rename_expr(entry.value, map),
                        })
                        .collect(),
                ),
                StructLiteralKind::Positional(values) => StructLiteralKind::Positional(
                    values.into_iter().map(|v| rename_expr(v, map)).collect(),
                ),
            },
            span,
        },
        Expr::EnumLiteral {
            enum_name,
            variant,
            values,
            span,
        } => Expr::EnumLiteral {
            enum_name,
            variant,
            values: values.into_iter().map(|v| rename_expr(v, map)).collect(),
            span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .into_iter()
                .map(|entry| MapLiteralEntry {
                    key: rename_expr(entry.key, map),
                    value: rename_expr(entry.value, map),
                })
                .collect(),
            span,
        },
        Expr::Block(block) => Expr::Block(Box::new(rename_block(*block, map))),
        Expr::If(if_expr) => Expr::If(Box::new(rename_if(*if_expr, map))),
        Expr::Match(match_expr) => Expr::Match(rename_match(match_expr, map)),
        Expr::Tuple(values, span) => Expr::Tuple(
            values.into_iter().map(|v| rename_expr(v, map)).collect(),
            span,
        ),
        Expr::ArrayLiteral(values, span) => Expr::ArrayLiteral(
            values.into_iter().map(|v| rename_expr(v, map)).collect(),
            span,
        ),
        Expr::Range(range) => Expr::Range(RangeExpr {
            start: Box::new(rename_expr(*range.start, map)),
            end: Box::new(rename_expr(*range.end, map)),
            inclusive: range.inclusive,
            span: range.span,
        }),
        Expr::Index { base, index, span } => Expr::Index {
            base: Box::new(rename_expr(*base, map)),
            index: Box::new(rename_expr(*index, map)),
            span,
        },
        Expr::Reference {
            mutable,
            expr,
            span,
        } => Expr::Reference {
            mutable,
            expr: Box::new(rename_expr(*expr, map)),
            span,
        },
        Expr::Deref { expr, span } => Expr::Deref {
            expr: Box::new(rename_expr(*expr, map)),
            span,
        },
        Expr::Move { expr, span } => Expr::Move {
            expr: Box::new(rename_expr(*expr, map)),
            span,
        },
        Expr::Spawn { expr, span } => Expr::Spawn {
            expr: Box::new(rename_expr(*expr, map)),
            span,
        },
    }
}

fn rename_block(block: Block, map: &HashMap<String, String>) -> Block {
    Block {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| rename_statement(stmt, map))
            .collect(),
        tail: block.tail.map(|expr| Box::new(rename_expr(*expr, map))),
        span: block.span,
    }
}

fn rename_function(def: FunctionDef, map: &HashMap<String, String>) -> FunctionDef {
    let params: Vec<FunctionParam> = def
        .params
        .into_iter()
        .map(|mut param| {
            if let Some(new) = map.get(&param.name) {
                param.name = new.clone();
            }
            param
        })
        .collect();
    let body = match def.body {
        FunctionBody::Expr(expr) => {
            FunctionBody::Expr(Spanned::new(rename_expr(expr.node, map), expr.span))
        }
        FunctionBody::Block(block) => FunctionBody::Block(Box::new(rename_block(*block, map))),
    };
    FunctionDef {
        params,
        body,
        ..def
    }
}

fn rename_item(item: Item, map: &HashMap<String, String>) -> Item {
    match item {
        Item::Function(def) => Item::Function(rename_function(def, map)),
        Item::Const(def) => Item::Const(ConstDef {
            value: rename_expr(def.value, map),
            ..def
        }),
        Item::Impl(block) => Item::Impl(ImplBlock {
            methods: block
                .methods
                .into_iter()
                .map(|method| rename_function(method, map))
                .collect(),
            ..block
        }),
        Item::MacroInvocation(inv) => Item::MacroInvocation(MacroInvocation {
            args: inv
                .args
                .into_iter()
                .map(|arg| MacroArg {
                    expr: rename_expr(arg.expr, map),
                    tokens: arg.tokens,
                })
                .collect(),
            ..inv
        }),
        other => other,
    }
}

fn rename_statement(stmt: Statement, map: &HashMap<String, String>) -> Statement {
    match stmt {
        Statement::Let(let_stmt) => Statement::Let(LetStmt {
            pattern: rename_pattern(let_stmt.pattern, map),
            value: let_stmt.value.map(|expr| rename_expr(expr, map)),
            ..let_stmt
        }),
        Statement::MacroSemi(expr) => {
            Statement::MacroSemi(Spanned::new(rename_expr(expr.node, map), expr.span))
        }
        Statement::Assign(assign) => Statement::Assign(AssignStmt {
            target: rename_expr(assign.target, map),
            value: rename_expr(assign.value, map),
        }),
        Statement::Expr(expr) => Statement::Expr(ExprStmt {
            expr: rename_expr(expr.expr, map),
        }),
        Statement::Return(ret) => Statement::Return(ReturnStmt {
            values: ret
                .values
                .into_iter()
                .map(|expr| rename_expr(expr, map))
                .collect(),
        }),
        Statement::While(while_stmt) => Statement::While(WhileStmt {
            condition: match while_stmt.condition {
                WhileCondition::Expr(expr) => WhileCondition::Expr(rename_expr(expr, map)),
                WhileCondition::Let { pattern, value } => WhileCondition::Let {
                    pattern: rename_pattern(pattern, map),
                    value: rename_expr(value, map),
                },
            },
            body: rename_block(while_stmt.body, map),
        }),
        Statement::Loop(loop_stmt) => Statement::Loop(LoopStmt {
            body: rename_block(loop_stmt.body, map),
            span: loop_stmt.span,
        }),
        Statement::For(for_stmt) => {
            let new_binding = map
                .get(&for_stmt.binding)
                .cloned()
                .unwrap_or(for_stmt.binding);
            Statement::For(ForStmt {
                binding: new_binding,
                target: match for_stmt.target {
                    ForTarget::Range(range) => ForTarget::Range(RangeExpr {
                        start: Box::new(rename_expr(*range.start, map)),
                        end: Box::new(rename_expr(*range.end, map)),
                        inclusive: range.inclusive,
                        span: range.span,
                    }),
                    ForTarget::Collection(expr) => ForTarget::Collection(rename_expr(expr, map)),
                },
                body: rename_block(for_stmt.body, map),
                span: for_stmt.span,
            })
        }
        Statement::Defer(defer_stmt) => Statement::Defer(DeferStmt {
            expr: rename_expr(defer_stmt.expr, map),
        }),
        Statement::Block(block) => Statement::Block(Box::new(rename_block(*block, map))),
        Statement::Break | Statement::Continue => stmt,
    }
}

fn rename_if(if_expr: IfExpr, map: &HashMap<String, String>) -> IfExpr {
    IfExpr {
        condition: match if_expr.condition {
            IfCondition::Expr(expr) => IfCondition::Expr(rename_expr(expr, map)),
            IfCondition::Let { pattern, value } => IfCondition::Let {
                pattern: rename_pattern(pattern, map),
                value: rename_expr(value, map),
            },
        },
        then_branch: rename_block(if_expr.then_branch, map),
        else_branch: if_expr.else_branch.map(|branch| match branch {
            ElseBranch::Block(block) => ElseBranch::Block(rename_block(block, map)),
            ElseBranch::ElseIf(inner) => ElseBranch::ElseIf(Box::new(rename_if(*inner, map))),
        }),
        span: if_expr.span,
    }
}

fn rename_match(expr: MatchExpr, map: &HashMap<String, String>) -> MatchExpr {
    MatchExpr {
        expr: Box::new(rename_expr(*expr.expr, map)),
        arms: expr
            .arms
            .into_iter()
            .map(|arm| MatchArmExpr {
                pattern: rename_pattern(arm.pattern, map),
                guard: arm.guard.map(|g| rename_expr(g, map)),
                value: rename_expr(arm.value, map),
            })
            .collect(),
        span: expr.span,
    }
}

fn rename_pattern(pattern: Pattern, map: &HashMap<String, String>) -> Pattern {
    match pattern {
        Pattern::Identifier(mut name, span) => {
            if let Some(new) = map.get(&name) {
                name = new.clone();
            }
            Pattern::Identifier(name, span)
        }
        Pattern::EnumVariant {
            enum_name,
            variant,
            bindings,
        } => Pattern::EnumVariant {
            enum_name,
            variant,
            bindings: bindings
                .into_iter()
                .map(|b| rename_pattern(b, map))
                .collect(),
        },
        Pattern::Tuple(bindings, span) => Pattern::Tuple(
            bindings
                .into_iter()
                .map(|b| rename_pattern(b, map))
                .collect(),
            span,
        ),
        Pattern::Map(entries, span) => Pattern::Map(
            entries
                .into_iter()
                .map(|entry| MapPatternEntry {
                    key: entry.key,
                    pattern: rename_pattern(entry.pattern, map),
                })
                .collect(),
            span,
        ),
        Pattern::Struct {
            struct_name,
            fields,
            has_spread,
            span,
        } => Pattern::Struct {
            struct_name,
            fields: fields
                .into_iter()
                .map(|field| StructPatternField {
                    name: field.name,
                    pattern: rename_pattern(field.pattern, map),
                })
                .collect(),
            has_spread,
            span,
        },
        Pattern::Slice {
            prefix,
            rest,
            suffix,
            span,
        } => Pattern::Slice {
            prefix: prefix.into_iter().map(|p| rename_pattern(p, map)).collect(),
            rest: rest.map(|p| Box::new(rename_pattern(*p, map))),
            suffix: suffix.into_iter().map(|p| rename_pattern(p, map)).collect(),
            span,
        },
        Pattern::Literal(_) | Pattern::Wildcard => pattern,
    }
}

fn collect_bindings_item(item: &Item) -> HashSet<String> {
    let mut set = HashSet::new();
    match item {
        Item::Function(def) => {
            for param in &def.params {
                set.insert(param.name.clone());
            }
            match &def.body {
                FunctionBody::Expr(expr) => set.extend(collect_bindings_expr(&expr.node)),
                FunctionBody::Block(block) => set.extend(collect_bindings_block(block)),
            }
        }
        Item::Const(def) => set.extend(collect_bindings_expr(&def.value)),
        Item::Impl(block) => {
            for method in &block.methods {
                for param in &method.params {
                    set.insert(param.name.clone());
                }
                match &method.body {
                    FunctionBody::Expr(expr) => set.extend(collect_bindings_expr(&expr.node)),
                    FunctionBody::Block(block) => set.extend(collect_bindings_block(block)),
                }
            }
        }
        Item::MacroInvocation(inv) => {
            for arg in &inv.args {
                set.extend(collect_bindings_expr(&arg.expr));
            }
        }
        Item::Struct(_) | Item::Enum(_) | Item::Interface(_) | Item::Macro(_) => {}
    }
    set
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

impl<'a> Expander<'a> {
    fn record_macro_trace(&mut self, span: Span) {
        if self.stack.is_empty() {
            return;
        }
        if let Some(path) = &self.current_path {
            self.traces.record(path, span, &self.stack);
        }
    }

    fn macro_arg_to_expr(
        &self,
        kind: MacroParamKind,
        arg: &MacroArg,
        span: Span,
        errors: &mut Vec<SyntaxError>,
    ) -> Expr {
        match kind {
            MacroParamKind::Expr | MacroParamKind::Pattern => arg.expr.clone(),
            MacroParamKind::Block => match &arg.expr {
                Expr::Block(_) => arg.expr.clone(),
                _ => Expr::Block(Box::new(Block {
                    statements: vec![Statement::Expr(ExprStmt {
                        expr: arg.expr.clone(),
                    })],
                    tail: None,
                    span,
                })),
            },
            MacroParamKind::Tokens => {
                if let Some(tokens) = &arg.tokens {
                    if let Some(path) = &self.current_path {
                        match crate::language::parser::parse_expression_from_tokens(
                            path.clone(),
                            tokens.clone(),
                        ) {
                            Ok(expr) => return expr,
                            Err(errs) => {
                                self.push_macro_errors(errors, errs.errors);
                            }
                        }
                    }
                }
                arg.expr.clone()
            }
            MacroParamKind::Repeat => {
                if let Some(tokens) = &arg.tokens {
                    if let Some(path) = &self.current_path {
                        let spec = find_repeat_spec(tokens);
                        match parse_repeat_fragments(path, tokens, &spec) {
                            Ok(mut fragments) => {
                                if fragments.parts.is_empty() {
                                    return Expr::Tuple(Vec::new(), fragments.span);
                                }
                                if fragments.parts.len() == 1 {
                                    return fragments.parts.remove(0);
                                }
                                return match fragments.separator {
                                    Some(TokenKind::Semi) => {
                                        let mut stmts = Vec::new();
                                        for part in fragments
                                            .parts
                                            .iter()
                                            .take(fragments.parts.len().saturating_sub(1))
                                        {
                                            stmts.push(Statement::Expr(ExprStmt {
                                                expr: part.clone(),
                                            }));
                                        }
                                        let tail = fragments.parts.last().cloned().map(Box::new);
                                        Expr::Block(Box::new(Block {
                                            statements: stmts,
                                            tail,
                                            span: fragments.span,
                                        }))
                                    }
                                    _ => Expr::Tuple(fragments.parts, fragments.span),
                                };
                            }
                            Err(errs) => self.push_macro_errors(errors, errs),
                        }
                    }
                }
                arg.expr.clone()
            }
        }
    }

    fn trace_help(&self) -> Option<String> {
        if self.stack.is_empty() {
            return None;
        }
        Some(format_trace(&self.stack))
    }

    fn push_macro_errors(&self, errors: &mut Vec<SyntaxError>, mut new_errs: Vec<SyntaxError>) {
        let help = self.trace_help();
        for err in &mut new_errs {
            if err.help.is_none() {
                err.help = help.clone();
            }
        }
        errors.extend(new_errs);
    }
}

fn parse_repeat_fragments(
    path: &std::path::Path,
    tokens: &[crate::language::token::Token],
    spec: &RepeatSpec,
) -> Result<RepeatFragments, Vec<SyntaxError>> {
    let slice = &tokens[spec.skip.min(tokens.len())..];
    if slice.is_empty() {
        return Ok(RepeatFragments {
            parts: Vec::new(),
            separator: None,
            span: Span::new(0, 0),
        });
    }
    let mut depth_paren: i32 = 0;
    let mut depth_brace: i32 = 0;
    let mut depth_bracket: i32 = 0;
    let mut separator = spec.separator.clone();
    for tok in slice {
        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 {
            if matches!(tok.kind, TokenKind::Comma | TokenKind::Semi) {
                separator.get_or_insert(tok.kind.clone());
            }
        }
        match &tok.kind {
            TokenKind::LParen => depth_paren += 1,
            TokenKind::RParen => depth_paren = depth_paren.saturating_sub(1),
            TokenKind::LBrace => depth_brace += 1,
            TokenKind::RBrace => depth_brace = depth_brace.saturating_sub(1),
            TokenKind::LBracket => depth_bracket += 1,
            TokenKind::RBracket => depth_bracket = depth_bracket.saturating_sub(1),
            _ => {}
        }
    }

    let mut parts = Vec::new();
    depth_paren = 0;
    depth_brace = 0;
    depth_bracket = 0;
    let mut start = 0;
    let default_sep = TokenKind::Comma;
    let sep_ref = separator.as_ref().unwrap_or(&default_sep);
    for (idx, tok) in slice.iter().enumerate() {
        match &tok.kind {
            TokenKind::LParen => depth_paren += 1,
            TokenKind::RParen => depth_paren = depth_paren.saturating_sub(1),
            TokenKind::LBrace => depth_brace += 1,
            TokenKind::RBrace => depth_brace = depth_brace.saturating_sub(1),
            TokenKind::LBracket => depth_bracket += 1,
            TokenKind::RBracket => depth_bracket = depth_bracket.saturating_sub(1),
            kind if kind == sep_ref
                && depth_paren == 0
                && depth_brace == 0
                && depth_bracket == 0 =>
            {
                let slice_part = &slice[start..idx];
                if !slice_part.is_empty() {
                    match crate::language::parser::parse_expression_from_tokens(
                        path.to_path_buf(),
                        slice_part.to_vec(),
                    ) {
                        Ok(expr) => parts.push(expr),
                        Err(errs) => return Err(errs.errors),
                    }
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start < slice.len() {
        let slice_part = &slice[start..];
        if !slice_part.is_empty() {
            match crate::language::parser::parse_expression_from_tokens(
                path.to_path_buf(),
                slice_part.to_vec(),
            ) {
                Ok(expr) => parts.push(expr),
                Err(errs) => return Err(errs.errors),
            }
        }
    }
    let span = Span::new(
        slice.first().unwrap().span.start,
        slice.last().unwrap().span.end,
    );
    Ok(RepeatFragments {
        parts,
        separator: separator.or(Some(TokenKind::Comma)),
        span,
    })
}
fn spans_overlap(a: Span, b: Span) -> bool {
    a.start < b.end && b.start < a.end
}

fn format_trace(frames: &[MacroFrame]) -> String {
    let mut parts = Vec::new();
    for frame in frames.iter().rev() {
        parts.push(format!(
            "in expansion of `{}` at {}..{}",
            frame.name, frame.call_span.start, frame.call_span.end
        ));
    }
    parts.join(" -> ")
}

fn find_repeat_spec(tokens: &[crate::language::token::Token]) -> RepeatSpec {
    // Optional prefix: @sep = <token>
    let mut separator = None;
    let mut skip = 0;
    for (idx, window) in tokens.windows(4).enumerate() {
        if let [first, second, third, fourth] = window {
            if first.kind == TokenKind::At {
                if let TokenKind::Identifier(name) = &second.kind {
                    if name == "sep" && matches!(third.kind, TokenKind::Eq) {
                        separator = Some(fourth.kind.clone());
                        skip = idx + 4;
                    }
                }
            }
        }
        if separator.is_some() {
            break;
        }
    }
    RepeatSpec { separator, skip }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::parser::parse_module;
    use std::path::PathBuf;

    #[test]
    fn repeat_plus_requires_argument() {
        let source = r#"
module tests::repeat_plus;

macro only_positive(values: repeat) -> int32 {
  values
}

fn main() -> int32 {
  ~only_positive();
  0
}
"#;
        let module = parse_module(
            "tests::repeat_plus",
            PathBuf::from("repeat_plus.prime"),
            source,
        )
        .expect("parse module");
        let program = crate::language::ast::Program {
            modules: vec![module],
        };
        let result = expand_program(&program);
        assert!(
            result.is_err(),
            "expected arity error for repeat+ with no args"
        );
    }

    #[test]
    fn repeat_custom_separator_parses() {
        let source = r#"
module tests::repeat_custom;

macro tally(values: repeat) -> int32 { values }

fn main() -> int32 {
  let (x, y, z) = ~tally(@sep = | 5 | 6 | 7);
  x + y + z
}
"#;
        let module = parse_module(
            "tests::repeat_custom",
            PathBuf::from("repeat_custom.prime"),
            source,
        )
        .expect("parse module");
        let program = Program {
            modules: vec![module],
        };
        let result = expand_program(&program);
        assert!(result.is_ok(), "custom separator should parse/expand");
    }

    #[test]
    fn macro_visibility_private_and_package() {
        let priv_src = r#"
module pkg::a;

macro local_only() -> int32 { 1 }

fn ok() -> int32 { ~local_only() }
"#;
        let caller_src = r#"
module pkg::b;

fn fail() -> int32 { ~local_only() }
"#;
        let package_src = r#"
module pkg::c;

pub(package) macro pkg_macro() -> int32 { 2 }
fn ok() -> int32 { ~pkg_macro() }
"#;
        let outside_src = r#"
module other::d;

fn fail() -> int32 { ~pkg_macro() }
"#;
        let modules = vec![
            parse_module("pkg::a", PathBuf::from("pkg/a.prime"), priv_src).unwrap(),
            parse_module("pkg::b", PathBuf::from("pkg/b.prime"), caller_src).unwrap(),
            parse_module("pkg::c", PathBuf::from("pkg/c.prime"), package_src).unwrap(),
            parse_module("other::d", PathBuf::from("other/d.prime"), outside_src).unwrap(),
        ];
        let program = Program { modules };
        let result = expand_program(&program);
        assert!(result.is_err(), "expected visibility errors");
        let errors = result.err().unwrap();
        let messages: Vec<String> = errors
            .iter()
            .flat_map(|e| e.errors.iter().map(|err| err.message.clone()))
            .collect();
        assert!(
            messages
                .iter()
                .any(|m| m.contains("unknown macro `local_only`")),
            "missing private macro error"
        );
        assert!(
            messages
                .iter()
                .any(|m| m.contains("unknown macro `pkg_macro`")),
            "missing package macro error"
        );
    }
}
