use crate::language::{
    ast::*,
    errors::{SyntaxError, SyntaxErrors},
    lexer::lex,
    span::{Span, Spanned},
    token::{Token, TokenKind},
    types::{Mutability, TypeAnnotation, TypeExpr},
};
use std::{ops::Range, path::PathBuf};

pub fn parse_module(name: &str, path: PathBuf, source: &str) -> Result<Module, SyntaxErrors> {
    let tokens = match lex(source) {
        Ok(tokens) => tokens,
        Err(errors) => {
            let errs = errors
                .into_iter()
                .map(|err| SyntaxError::new(err.message, err.span))
                .collect();
            return Err(SyntaxErrors::new(errs));
        }
    };
    Parser::new(name, path, tokens).parse()
}

pub fn parse_expression_snippet(path: PathBuf, source: &str) -> Result<Expr, SyntaxErrors> {
    let tokens = match lex(source) {
        Ok(tokens) => tokens,
        Err(errors) => {
            let errs = errors
                .into_iter()
                .map(|err| SyntaxError::new(err.message, err.span))
                .collect();
            return Err(SyntaxErrors::new(errs));
        }
    };
    let mut parser = Parser::new("format", path, tokens);
    let expr = parser
        .parse_expression()
        .map_err(|err| SyntaxErrors::new(vec![err]))?;
    if !parser.is_eof() {
        return Err(SyntaxErrors::new(vec![SyntaxError::new(
            "Unexpected tokens after expression",
            Span::new(0, 0),
        )]));
    }
    Ok(expr)
}

pub fn parse_expression_from_tokens(
    path: PathBuf,
    tokens: Vec<Token>,
) -> Result<Expr, SyntaxErrors> {
    let mut parser = Parser::new("macro_arg", path, tokens);
    let expr = parser
        .parse_expression()
        .map_err(|err| SyntaxErrors::new(vec![err]))?;
    if !parser.is_eof() {
        return Err(SyntaxErrors::new(vec![SyntaxError::new(
            "Unexpected tokens after expression",
            Span::new(0, 0),
        )]));
    }
    Ok(expr)
}

pub fn parse_pattern_from_tokens(
    path: PathBuf,
    tokens: Vec<Token>,
) -> Result<Pattern, SyntaxErrors> {
    let mut parser = Parser::new("macro_pattern", path, tokens);
    let pat = parser
        .parse_pattern()
        .map_err(|err| SyntaxErrors::new(vec![err]))?;
    if !parser.is_eof() {
        return Err(SyntaxErrors::new(vec![SyntaxError::new(
            "Unexpected tokens after pattern",
            Span::new(0, 0),
        )]));
    }
    Ok(pat)
}

fn shift_span(span: &mut Span, delta: isize) {
    span.start = ((span.start as isize) + delta) as usize;
    span.end = ((span.end as isize) + delta) as usize;
}

fn shift_literal_span(lit: &mut Literal, delta: isize) {
    match lit {
        Literal::Int(_, span)
        | Literal::Float(_, span)
        | Literal::Bool(_, span)
        | Literal::String(_, span)
        | Literal::Rune(_, span) => shift_span(span, delta),
    }
}

fn shift_pattern_spans(pattern: &mut Pattern, delta: isize) {
    match pattern {
        Pattern::Identifier(_, span) => shift_span(span, delta),
        Pattern::Literal(lit) => shift_literal_span(lit, delta),
        Pattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                shift_pattern_spans(binding, delta);
            }
        }
        Pattern::Tuple(_, span)
        | Pattern::Map(_, span)
        | Pattern::Struct { span, .. }
        | Pattern::Slice { span, .. } => shift_span(span, delta),
        Pattern::Wildcard => {}
    }
}

fn shift_block_spans(block: &mut Block, delta: isize) {
    shift_span(&mut block.span, delta);
    for stmt in &mut block.statements {
        shift_statement_spans(stmt, delta);
    }
    if let Some(tail) = &mut block.tail {
        shift_expr_spans(tail, delta);
    }
}

fn shift_statement_spans(stmt: &mut Statement, delta: isize) {
    match stmt {
        Statement::Let(let_stmt) => {
            shift_span(&mut let_stmt.span, delta);
            if let Some(ty) = &mut let_stmt.ty {
                shift_span(&mut ty.span, delta);
            }
            shift_pattern_spans(&mut let_stmt.pattern, delta);
            if let Some(value) = &mut let_stmt.value {
                shift_expr_spans(value, delta);
            }
        }
        Statement::Assign(assign) => {
            shift_expr_spans(&mut assign.target, delta);
            shift_expr_spans(&mut assign.value, delta);
        }
        Statement::Expr(expr) => {
            shift_expr_spans(&mut expr.expr, delta);
        }
        Statement::MacroSemi(expr) => shift_expr_spans(&mut expr.node, delta),
        Statement::Return(ret) => {
            for value in &mut ret.values {
                shift_expr_spans(value, delta);
            }
        }
        Statement::While(while_stmt) => {
            match &mut while_stmt.condition {
                WhileCondition::Expr(expr) => shift_expr_spans(expr, delta),
                WhileCondition::Let { pattern, value } => {
                    shift_pattern_spans(pattern, delta);
                    shift_expr_spans(value, delta);
                }
            }
            shift_block_spans(&mut while_stmt.body, delta);
        }
        Statement::Loop(loop_stmt) => {
            shift_block_spans(&mut loop_stmt.body, delta);
        }
        Statement::For(for_stmt) => {
            match &mut for_stmt.target {
                ForTarget::Range(range) => {
                    shift_expr_spans(&mut range.start, delta);
                    shift_expr_spans(&mut range.end, delta);
                    shift_span(&mut range.span, delta);
                }
                ForTarget::Collection(expr) => shift_expr_spans(expr, delta),
            }
            shift_block_spans(&mut for_stmt.body, delta);
        }
        Statement::Defer(defer_stmt) => {
            shift_expr_spans(&mut defer_stmt.expr, delta);
        }
        Statement::Break | Statement::Continue => {}
        Statement::Block(block) => shift_block_spans(block, delta),
    }
}

fn shift_if_spans(if_expr: &mut IfExpr, delta: isize) {
    shift_span(&mut if_expr.span, delta);
    match &mut if_expr.condition {
        IfCondition::Expr(expr) => shift_expr_spans(expr, delta),
        IfCondition::Let { pattern, value } => {
            shift_pattern_spans(pattern, delta);
            shift_expr_spans(value, delta);
        }
    }
    shift_block_spans(&mut if_expr.then_branch, delta);
    if let Some(else_branch) = &mut if_expr.else_branch {
        match else_branch {
            ElseBranch::Block(block) => shift_block_spans(block, delta),
            ElseBranch::ElseIf(expr) => shift_if_spans(expr, delta),
        }
    }
}

fn shift_match_spans(match_expr: &mut MatchExpr, delta: isize) {
    shift_span(&mut match_expr.span, delta);
    shift_expr_spans(&mut match_expr.expr, delta);
    for arm in &mut match_expr.arms {
        shift_pattern_spans(&mut arm.pattern, delta);
        if let Some(guard) = &mut arm.guard {
            shift_expr_spans(guard, delta);
        }
        shift_expr_spans(&mut arm.value, delta);
    }
}

pub fn shift_expr_spans(expr: &mut Expr, delta: isize) {
    match expr {
        Expr::Identifier(ident) => shift_span(&mut ident.span, delta),
        Expr::Literal(lit) => shift_literal_span(lit, delta),
        Expr::FormatString(literal) => {
            shift_span(&mut literal.span, delta);
            for segment in &mut literal.segments {
                match segment {
                    FormatSegment::Literal(_) => {}
                    FormatSegment::Implicit(span) => shift_span(span, delta),
                    FormatSegment::Expr { expr, span } => {
                        shift_span(span, delta);
                        shift_expr_spans(expr, delta);
                    }
                }
            }
        }
        Expr::Try { block, span } => {
            shift_span(span, delta);
            shift_block_spans(block, delta);
        }
        Expr::TryPropagate { expr: inner, span } => {
            shift_span(span, delta);
            shift_expr_spans(inner, delta);
        }
        Expr::Binary { left, right, span, .. } => {
            shift_span(span, delta);
            shift_expr_spans(left, delta);
            shift_expr_spans(right, delta);
        }
        Expr::Unary { expr: inner, span, .. } => {
            shift_span(span, delta);
            shift_expr_spans(inner, delta);
        }
        Expr::Call { callee, args, span, .. } => {
            shift_span(span, delta);
            shift_expr_spans(callee, delta);
            for arg in args {
                shift_expr_spans(arg, delta);
            }
        }
        Expr::MacroCall { name, args, span } => {
            shift_span(&mut name.span, delta);
            shift_span(span, delta);
            for arg in args {
                shift_expr_spans(&mut arg.expr, delta);
                if let Some(tokens) = &mut arg.tokens {
                    for tok in tokens {
                        shift_span(&mut tok.span, delta);
                    }
                }
            }
        }
        Expr::FieldAccess { base, span, .. } => {
            shift_span(span, delta);
            shift_expr_spans(base, delta);
        }
        Expr::StructLiteral { fields, span, .. } => {
            shift_span(span, delta);
            match fields {
                StructLiteralKind::Named(entries) => {
                    for entry in entries {
                        shift_expr_spans(&mut entry.value, delta);
                    }
                }
                StructLiteralKind::Positional(values) => {
                    for value in values {
                        shift_expr_spans(value, delta);
                    }
                }
            }
        }
        Expr::EnumLiteral { values, span, .. } => {
            shift_span(span, delta);
            for value in values {
                shift_expr_spans(value, delta);
            }
        }
        Expr::MapLiteral { entries, span } => {
            shift_span(span, delta);
            for entry in entries {
                shift_expr_spans(&mut entry.key, delta);
                shift_expr_spans(&mut entry.value, delta);
            }
        }
        Expr::Block(block) => shift_block_spans(block, delta),
        Expr::If(if_expr) => shift_if_spans(if_expr, delta),
        Expr::Match(match_expr) => shift_match_spans(match_expr, delta),
        Expr::Tuple(values, span) => {
            shift_span(span, delta);
            for value in values {
                shift_expr_spans(value, delta);
            }
        }
        Expr::ArrayLiteral(values, span) => {
            shift_span(span, delta);
            for value in values {
                shift_expr_spans(value, delta);
            }
        }
        Expr::Range(range) => {
            shift_span(&mut range.span, delta);
            shift_expr_spans(&mut range.start, delta);
            shift_expr_spans(&mut range.end, delta);
        }
        Expr::Index { base, index, span } => {
            shift_span(span, delta);
            shift_expr_spans(base, delta);
            shift_expr_spans(index, delta);
        }
        Expr::Reference { expr: inner, span, .. }
        | Expr::Deref { expr: inner, span, .. }
        | Expr::Move { expr: inner, span, .. }
        | Expr::Spawn { expr: inner, span, .. } => {
            shift_span(span, delta);
            shift_expr_spans(inner, delta);
        }
    }
}

fn is_pascal_case(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

struct Parser {
    module_name: String,
    path: PathBuf,
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<SyntaxError>,
    suppress_block_literal: bool,
    block_context_stack: Vec<usize>,
    paren_depth: usize,
    last_span: Option<Range<usize>>,
    kind: ModuleKind,
}

#[derive(Debug)]
enum StatementOrTail {
    Statement(Statement),
    Tail(Expr),
}

impl Parser {
    fn new(name: &str, path: PathBuf, tokens: Vec<Token>) -> Self {
        Self {
            module_name: name.to_string(),
            path,
            tokens,
            pos: 0,
            errors: Vec::new(),
            suppress_block_literal: false,
            block_context_stack: Vec::new(),
            paren_depth: 0,
            last_span: None,
            kind: ModuleKind::Module,
        }
    }

    fn parse(mut self) -> Result<Module, SyntaxErrors> {
        let mut imports = Vec::new();
        let mut items = Vec::new();
        let mut declared_name = None;
        let mut declared_span = None;
        let mut redundant_module_spans = Vec::new();

        if matches!(
            self.peek_kind(),
            Some(TokenKind::ModuleKw | TokenKind::LibraryKw | TokenKind::TestKw)
        ) {
            match self.parse_module_declaration() {
                Ok((name, span, kind)) => {
                    declared_name = Some(name.clone());
                    declared_span = Some(span);
                    self.module_name = name;
                    self.kind = kind;
                }
                Err(err) => self.report(err),
            }
        }

        while !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }

            if matches!(
                self.peek_kind(),
                Some(TokenKind::ModuleKw | TokenKind::LibraryKw | TokenKind::TestKw)
            ) {
                match self.parse_redundant_module_decl() {
                    Ok(span) => redundant_module_spans.push(span),
                    Err(err) => self.report(err),
                }
                continue;
            }

            if matches!(
                self.peek_kind(),
                Some(TokenKind::ModuleKw | TokenKind::LibraryKw | TokenKind::TestKw)
            ) {
                let start = self.current_span_start();
                self.advance();
                while !self.check(TokenKind::Semi) && !self.is_eof() {
                    self.advance();
                }
                self.consume_optional(TokenKind::Semi);
                let end = self.last_span_end(start);
                let span = Span::new(start, end);
                self.report(SyntaxError::new(
                    "`module` declaration must appear before any other code",
                    span,
                ));
                continue;
            }

            if self.check(TokenKind::Pub) && matches!(self.peek_kind_n(1), Some(TokenKind::Import))
            {
                self.advance(); // consume pub
                match self.parse_import_with_visibility(Visibility::Public) {
                    Ok(import) => imports.push(import),
                    Err(err) => self.report(err),
                }
                continue;
            }

            if self.check(TokenKind::Import) {
                match self.parse_import_with_visibility(Visibility::Private) {
                    Ok(import) => imports.push(import),
                    Err(err) => self.report(err),
                }
                continue;
            }

            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(err) => {
                    self.report(err);
                    self.synchronize_item();
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Module {
                name: self.module_name,
                kind: self.kind,
                path: self.path,
                declared_name,
                declared_span,
                redundant_module_spans,
                imports,
                items,
            })
        } else {
            Err(SyntaxErrors::new(self.errors))
        }
    }

    fn parse_module_declaration(&mut self) -> Result<(String, Span, ModuleKind), SyntaxError> {
        let (start, kind) = match self.peek_kind() {
            Some(TokenKind::TestKw) => {
                let span = self.expect(TokenKind::TestKw)?.span.start;
                (span, ModuleKind::Test)
            }
            Some(TokenKind::ModuleKw) => {
                let span = self.expect(TokenKind::ModuleKw)?.span.start;
                (span, ModuleKind::Module)
            }
            Some(TokenKind::LibraryKw) => {
                let span = self.expect(TokenKind::LibraryKw)?.span.start;
                (span, ModuleKind::Library)
            }
            _ => return Err(self.error_here("Expected module, library, or test header")),
        };
        let path = self.parse_module_path("Expected module path after header")?;
        if path.is_empty() {
            return Err(self.error_here("Module path cannot be empty"));
        }
        self.expect(TokenKind::Semi)?;
        let end = self.last_span_end(start);
        Ok((path.to_string(), Span::new(start, end), kind))
    }

    fn parse_import_with_visibility(
        &mut self,
        visibility: Visibility,
    ) -> Result<Import, SyntaxError> {
        let start = self.expect(TokenKind::Import)?.span.start;
        let path = if matches!(self.peek_kind(), Some(TokenKind::String(_))) {
            let literal = self.expect_string_literal("Expected import path string")?;
            ImportPath {
                segments: legacy_import_segments(&literal),
            }
        } else {
            self.parse_module_path("Expected module path in import")?
        };
        if path.is_empty() {
            return Err(self.error_here("Import path cannot be empty"));
        }
        let alias = if self.matches(TokenKind::As) {
            Some(self.expect_identifier("Expected alias after 'as'")?.name)
        } else {
            None
        };
        self.consume_optional(TokenKind::Semi);
        let end = self.last_span_end(start);
        Ok(Import {
            visibility,
            path,
            alias,
            span: Span::new(start, end),
        })
    }

    fn parse_module_path(&mut self, msg: &str) -> Result<ImportPath, SyntaxError> {
        let mut segments = Vec::new();
        let first = self.expect_identifier(msg)?;
        segments.push(first.name);
        loop {
            if self.matches(TokenKind::ColonColon) || self.matches(TokenKind::Dot) {
                let ident = self.expect_identifier("Expected segment after separator")?;
                segments.push(ident.name);
            } else {
                break;
            }
        }
        Ok(ImportPath { segments })
    }

    fn parse_redundant_module_decl(&mut self) -> Result<Span, SyntaxError> {
        let start = match self.peek_kind() {
            Some(TokenKind::TestKw) => self.expect(TokenKind::TestKw)?.span.start,
            Some(TokenKind::ModuleKw) => self.expect(TokenKind::ModuleKw)?.span.start,
            Some(TokenKind::LibraryKw) => self.expect(TokenKind::LibraryKw)?.span.start,
            _ => self.current_span_start(),
        };
        let _ = self.parse_module_path("Expected module path after `module`")?;
        self.expect(TokenKind::Semi)?;
        let end = self.last_span_end(start);
        Ok(Span::new(start, end))
    }

    fn parse_item(&mut self) -> Result<Item, SyntaxError> {
        if self.matches(TokenKind::Tilde) {
            return self.parse_item_macro_invocation();
        }
        let mut visibility = Visibility::Private;
        if self.matches(TokenKind::Pub) {
            visibility = Visibility::Public;
        }
        if self.matches(TokenKind::Struct) {
            return self.parse_struct(visibility).map(Item::Struct);
        }
        if self.matches(TokenKind::Enum) {
            return self.parse_enum(visibility).map(Item::Enum);
        }
        if self.matches(TokenKind::Interface) {
            return self.parse_interface(visibility).map(Item::Interface);
        }
        if self.matches(TokenKind::Impl) {
            if visibility == Visibility::Public {
                return Err(self.error_here("`pub` is not allowed before `impl`"));
            }
            return self.parse_impl().map(Item::Impl);
        }
        if self.matches(TokenKind::Macro) {
            return self.parse_macro(visibility).map(Item::Macro);
        }
        if self.matches(TokenKind::Fn) {
            return self.parse_function(visibility).map(Item::Function);
        }
        if self.matches(TokenKind::Const) {
            return self.parse_const(visibility).map(Item::Const);
        }
        if visibility == Visibility::Public {
            return Err(self.error_here("Expected declaration after `pub`"));
        }
        Err(self.error_here("Expected declaration"))
    }

    fn parse_struct(&mut self, visibility: Visibility) -> Result<StructDef, SyntaxError> {
        let name = self.expect_identifier("Expected struct name")?;
        let type_params = self.parse_type_params()?;
        let start = name.span.start;
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            let field_start = self.current_span_start();
            let ident = self.expect_identifier("Expected field or embedded type")?;

            if self.matches(TokenKind::Colon) {
                let ty = self.parse_type_annotation()?;
                self.expect(TokenKind::Semi)?;
                fields.push(StructField {
                    name: Some(ident.name),
                    ty,
                    embedded: false,
                    span: Span::new(field_start, self.last_span_end(field_start)),
                });
            } else {
                // Treat as embedded field with implicit type name
                let ty = TypeAnnotation {
                    ty: TypeExpr::named(ident.name.clone()),
                    span: ident.span,
                };
                self.expect(TokenKind::Semi)?;
                fields.push(StructField {
                    name: None,
                    ty,
                    embedded: true,
                    span: Span::new(field_start, self.last_span_end(field_start)),
                });
            }
        }

        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(StructDef {
            name: name.name,
            type_params,
            fields,
            span: Span::new(start, end),
            visibility,
        })
    }

    fn parse_enum(&mut self, visibility: Visibility) -> Result<EnumDef, SyntaxError> {
        let name = self.expect_identifier("Expected enum name")?;
        let type_params = self.parse_type_params()?;
        let start = name.span.start;
        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            let variant_name = self.expect_identifier("Expected variant name")?;
            let mut fields = Vec::new();
            if self.matches(TokenKind::LParen) {
                if !self.check(TokenKind::RParen) {
                    loop {
                        let ty = self.parse_type_annotation()?;
                        fields.push(ty);
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                self.expect(TokenKind::RParen)?;
            }
            variants.push(EnumVariant {
                name: variant_name.name,
                fields,
                span: variant_name.span,
            });
            self.consume_optional(TokenKind::Comma);
        }

        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(EnumDef {
            name: name.name,
            type_params,
            variants,
            span: Span::new(start, end),
            visibility,
        })
    }

    fn parse_interface(&mut self, visibility: Visibility) -> Result<InterfaceDef, SyntaxError> {
        let name = self.expect_identifier("Expected interface name")?;
        let type_params = self.parse_type_params()?;
        let start = name.span.start;
        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            self.expect(TokenKind::Fn)?;
            let method_name = self.expect_identifier("Expected method name")?;
            self.expect(TokenKind::LParen)?;
            let mut params = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    params.push(self.parse_param()?);
                    if self.matches(TokenKind::Comma) {
                        continue;
                    }
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            let mut returns = Vec::new();
            if self.matches(TokenKind::Arrow) {
                if self.matches(TokenKind::LParen) {
                    if !self.check(TokenKind::RParen) {
                        loop {
                            returns.push(self.parse_type_annotation()?);
                            if self.matches(TokenKind::Comma) {
                                continue;
                            }
                            break;
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                } else {
                    returns.push(self.parse_type_annotation()?);
                }
            }
            self.expect(TokenKind::Semi)?;
            methods.push(InterfaceMethod {
                name: method_name.name,
                params,
                returns,
                span: Span::new(
                    method_name.span.start,
                    self.last_span_end(method_name.span.start),
                ),
            });
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(InterfaceDef {
            name: name.name,
            type_params,
            methods,
            span: Span::new(start, end),
            visibility,
        })
    }

    fn parse_impl(&mut self) -> Result<ImplBlock, SyntaxError> {
        let interface = self.expect_identifier("Expected interface name")?;
        let type_args = self.parse_type_args()?;
        self.expect(TokenKind::For)?;
        let target = self.expect_identifier("Expected target type")?;
        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            self.expect(TokenKind::Fn)?;
            methods.push(self.parse_function(Visibility::Private)?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(ImplBlock {
            interface: interface.name,
            type_args,
            target: target.name,
            methods,
        })
    }

    fn parse_type_params(&mut self) -> Result<Vec<String>, SyntaxError> {
        if !self.matches(TokenKind::LBracket) {
            return Ok(Vec::new());
        }
        let mut params = Vec::new();
        if !self.check(TokenKind::RBracket) {
            loop {
                let ident = self.expect_identifier("Expected type parameter name")?;
                params.push(ident.name);
                if self.matches(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(params)
    }

    fn parse_function(&mut self, visibility: Visibility) -> Result<FunctionDef, SyntaxError> {
        let name = self.expect_identifier("Expected function name")?;
        let type_params = self.parse_type_params()?;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                params.push(self.parse_param()?);
                if self.matches(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RParen)?;

        let mut returns = Vec::new();
        if self.matches(TokenKind::Arrow) {
            if self.matches(TokenKind::LParen) {
                if !self.check(TokenKind::RParen) {
                    loop {
                        returns.push(self.parse_type_annotation()?);
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                self.expect(TokenKind::RParen)?;
            } else {
                returns.push(self.parse_type_annotation()?);
            }
        }

        let body = if self.matches(TokenKind::FatArrow) {
            let expr = self.parse_expression()?;
            let span = expr_span(&expr);
            self.expect(TokenKind::Semi)?;
            FunctionBody::Expr(Spanned::new(expr, span))
        } else {
            FunctionBody::Block(Box::new(self.parse_block()?))
        };

        let span = match &body {
            FunctionBody::Expr(expr) => Span::new(name.span.start, expr.span.end),
            FunctionBody::Block(block) => Span::new(name.span.start, block.span.end),
        };

        Ok(FunctionDef {
            name: name.name,
            type_params,
            params,
            returns,
            body,
            span,
            visibility,
        })
    }

    fn parse_macro(&mut self, visibility: Visibility) -> Result<MacroDef, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let name = self.expect_identifier("Expected macro name")?;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                params.push(self.parse_macro_param()?);
                if self.matches(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RParen)?;
        let return_ty = if self.matches(TokenKind::Arrow) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        let body = if self.matches(TokenKind::FatArrow) {
            let expr = self.parse_expression()?;
            let span = expr_span(&expr);
            self.expect(TokenKind::Semi)?;
            MacroBody::Expr(Spanned::new(expr, span))
        } else if self.check(TokenKind::LBrace) && self.macro_body_looks_like_items() {
            let (items, span) = self.parse_macro_items()?;
            MacroBody::Items(items, span)
        } else {
            MacroBody::Block(Box::new(self.parse_block()?))
        };
        let end = match &body {
            MacroBody::Block(block) => block.span.end,
            MacroBody::Expr(expr) => expr.span.end,
            MacroBody::Items(_, span) => span.end,
        };
        Ok(MacroDef {
            name: name.name,
            params,
            return_ty,
            body,
            span: Span::new(start, end),
            visibility,
        })
    }

    fn parse_macro_param(&mut self) -> Result<MacroParam, SyntaxError> {
        let start = self.current_span_start();
        let name = self.expect_identifier("Expected macro parameter name")?;
        let mut kind = MacroParamKind::Expr;
        let mut ty = None;
        if self.matches(TokenKind::Colon) {
            if let Some(TokenKind::Identifier(raw_kind)) = self.peek_kind() {
                let lower = raw_kind.to_ascii_lowercase();
                if lower == "block" || lower == "pattern" || lower == "tokens" || lower == "repeat" {
                    self.advance();
                    kind = match lower.as_str() {
                        "block" => MacroParamKind::Block,
                        "pattern" => MacroParamKind::Pattern,
                        "tokens" => MacroParamKind::Tokens,
                        _ => MacroParamKind::Repeat,
                    };
                } else {
                    ty = Some(self.parse_type_annotation()?);
                }
            } else {
                ty = Some(self.parse_type_annotation()?);
            }
        }
        let end = ty.as_ref().map(|ty| ty.span.end).unwrap_or(name.span.end);
        Ok(MacroParam {
            name: name.name,
            ty,
            kind,
            span: Span::new(start, end),
        })
    }

    fn parse_macro_items(&mut self) -> Result<(Vec<Item>, Span), SyntaxError> {
        let start = self.expect(TokenKind::LBrace)?.span.start;
        let mut items = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(err) => {
                    self.report(err);
                    self.synchronize_item();
                }
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok((items, Span::new(start, end)))
    }

    fn parse_item_macro_invocation(&mut self) -> Result<Item, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let name = self.expect_identifier("Expected macro name after '~'")?;
        self.expect(TokenKind::LParen)?;
        self.enter_paren();
        let args = self.parse_macro_call_args()?;
        let end = match self.expect(TokenKind::RParen) {
            Ok(token) => token.span.end,
            Err(err) => {
                self.exit_paren();
                return Err(err);
            }
        };
        self.exit_paren();
        self.expect(TokenKind::Semi)?;
        Ok(Item::MacroInvocation(MacroInvocation {
            name,
            args,
            span: Span::new(start, end),
        }))
    }

    fn macro_body_looks_like_items(&self) -> bool {
        let mut idx = 1; // look after '{'
        while matches!(self.peek_kind_n(idx), Some(TokenKind::Semi)) {
            idx += 1;
        }
        let next = self.peek_kind_n(idx);
        match next {
            Some(TokenKind::Pub) => matches!(
                self.peek_kind_n(idx + 1),
                Some(
                    TokenKind::Struct
                        | TokenKind::Enum
                        | TokenKind::Interface
                        | TokenKind::Impl
                        | TokenKind::Macro
                        | TokenKind::Fn
                        | TokenKind::Const
                        | TokenKind::Tilde
                )
            ),
            Some(
                TokenKind::Struct
                | TokenKind::Enum
                | TokenKind::Interface
                | TokenKind::Impl
                | TokenKind::Macro
                | TokenKind::Fn
                | TokenKind::Const
                | TokenKind::ModuleKw
                | TokenKind::LibraryKw
                | TokenKind::TestKw
                | TokenKind::Tilde,
            ) => true,
            _ => false,
        }
    }

    fn parse_macro_call_args(&mut self) -> Result<Vec<MacroArg>, SyntaxError> {
        let mut args = Vec::new();
        let mut repeat_prefix = self.consume_repeat_separator_prefix();
        if !self.check(TokenKind::RParen) {
            loop {
                let mut arg = self.parse_macro_arg()?;
                if let Some(prefix_tokens) = repeat_prefix.take() {
                    if let Some(tokens) = arg.tokens.as_mut() {
                        let mut merged = prefix_tokens;
                        merged.extend(tokens.drain(..));
                        *tokens = merged;
                    } else {
                        arg.tokens = Some(prefix_tokens);
                    }
                }
                args.push(arg);
                if self.matches(TokenKind::Comma) {
                    if self.check(TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        Ok(args)
    }

    fn consume_repeat_separator_prefix(&mut self) -> Option<Vec<Token>> {
        let start_pos = self.pos;
        if self.peek_kind() != Some(TokenKind::At) {
            return None;
        }
        let Some(TokenKind::Identifier(name)) = self.peek_kind_n(1) else {
            return None;
        };
        if name != "sep" || self.peek_kind_n(2) != Some(TokenKind::Eq) {
            return None;
        }
        let Some(sep) = self.peek_kind_n(3) else {
            return None;
        };
        if !matches!(sep, TokenKind::Comma | TokenKind::Semi) {
            return None;
        }
        for _ in 0..4 {
            self.advance();
        }
        Some(self.tokens[start_pos..self.pos].to_vec())
    }

    fn parse_macro_arg(&mut self) -> Result<MacroArg, SyntaxError> {
        let start_idx = self.pos;
        let expr = self.parse_expression()?;
        let tokens = self.tokens[start_idx..self.pos].to_vec();
        Ok(MacroArg {
            expr,
            tokens: Some(tokens),
        })
    }

    fn parse_const(&mut self, visibility: Visibility) -> Result<ConstDef, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let name = self.expect_identifier("Expected const name")?;
        let ty = if self.matches(TokenKind::Colon) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expression()?;
        self.expect(TokenKind::Semi)?;
        let end = expr_span(&value).end;
        Ok(ConstDef {
            name: name.name,
            ty,
            value,
            span: Span::new(start, end),
            visibility,
        })
    }

    fn parse_param(&mut self) -> Result<FunctionParam, SyntaxError> {
        let span_start = self.current_span_start();
        let mut reference = None;
        if self.matches(TokenKind::Ampersand) {
            let ref_span_start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            let is_mut = self.matches(TokenKind::Mut);
            reference = Some((is_mut, ref_span_start));
        }
        let mutability = if self.matches(TokenKind::Mut) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let name = self.expect_identifier("Expected parameter name")?;
        if name.name == "self" && !self.check(TokenKind::Colon) {
            let mut ty = TypeExpr::SelfType;
            let ty_span_start = reference
                .as_ref()
                .map(|(_, start)| *start)
                .unwrap_or(span_start);
            if let Some((is_mut, _)) = reference {
                ty = TypeExpr::Reference {
                    mutable: is_mut,
                    ty: Box::new(ty),
                };
            }
            let span = Span::new(ty_span_start, name.span.end);
            return Ok(FunctionParam {
                name: name.name,
                ty: TypeAnnotation { ty, span },
                mutability,
                span,
            });
        }
        if reference.is_some() {
            return Err(self.error_here(
                "Reference shorthand (`&self`) is only supported with `self` parameters",
            ));
        }
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type_annotation()?;
        let span = Span::new(span_start, ty.span.end);
        Ok(FunctionParam {
            name: name.name,
            ty,
            mutability,
            span,
        })
    }

    fn parse_type_args(&mut self) -> Result<Vec<TypeExpr>, SyntaxError> {
        if !self.matches(TokenKind::LBracket) {
            return Ok(Vec::new());
        }
        let mut args = Vec::new();
        if !self.check(TokenKind::RBracket) {
            loop {
                let ty = self.parse_type_annotation()?;
                args.push(ty.ty);
                if self.matches(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Block, SyntaxError> {
        let start = self.expect(TokenKind::LBrace)?.span.start;
        let mut statements = Vec::new();
        let mut tail = None;
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            match self.parse_statement(true) {
                Ok(StatementOrTail::Statement(stmt)) => statements.push(stmt),
                Ok(StatementOrTail::Tail(expr)) => {
                    tail = Some(Box::new(expr));
                    break;
                }
                Err(err) => {
                    self.report(err);
                    self.synchronize_statement();
                }
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Block {
            statements,
            tail,
            span: Span::new(start, end),
        })
    }

    fn parse_statement(&mut self, allow_tail: bool) -> Result<StatementOrTail, SyntaxError> {
        if self.matches(TokenKind::Let) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            let stmt = self.parse_let(start)?;
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Let(stmt)));
        }
        if self.matches(TokenKind::Return) {
            let stmt = self.parse_return()?;
            return Ok(StatementOrTail::Statement(Statement::Return(stmt)));
        }
        if self.matches(TokenKind::While) {
            let stmt = self.parse_while()?;
            return Ok(StatementOrTail::Statement(Statement::While(stmt)));
        }
        if self.matches(TokenKind::Loop) {
            let stmt = self.parse_loop()?;
            return Ok(StatementOrTail::Statement(Statement::Loop(stmt)));
        }
        if self.matches(TokenKind::For) {
            let stmt = self.parse_for_statement()?;
            return Ok(StatementOrTail::Statement(Statement::For(stmt)));
        }
        if self.matches(TokenKind::Break) {
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Break));
        }
        if self.matches(TokenKind::Continue) {
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Continue));
        }
        if self.matches(TokenKind::Defer) {
            let expr = self.parse_expression()?;
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Defer(DeferStmt {
                expr,
            })));
        }
        if self.matches(TokenKind::LBrace) {
            self.rewind();
            let block = self.parse_block()?;
            return Ok(StatementOrTail::Statement(Statement::Block(Box::new(
                block,
            ))));
        }

        if self.is_assignment_start() {
            let stmt = self.parse_assignment()?;
            return Ok(StatementOrTail::Statement(Statement::Assign(stmt)));
        }

        self.parse_expression_statement(allow_tail)
    }

    fn parse_expression_statement(
        &mut self,
        allow_tail: bool,
    ) -> Result<StatementOrTail, SyntaxError> {
        let expr = self.parse_expression()?;
        if self.matches(TokenKind::Semi) {
            return Ok(StatementOrTail::Statement(match expr {
                Expr::MacroCall { span, .. } => Statement::MacroSemi(Spanned::new(expr, span)),
                _ => Statement::Expr(ExprStmt { expr }),
            }));
        }

        let next_is_rbrace = matches!(self.peek_kind(), Some(TokenKind::RBrace));

        if allow_tail && next_is_rbrace {
            return Ok(StatementOrTail::Tail(expr));
        }

        if matches!(expr, Expr::If(_) | Expr::Match(_)) {
            return Ok(StatementOrTail::Statement(Statement::Expr(ExprStmt {
                expr,
            })));
        }

        if allow_tail {
            Err(self.error_here("Expected ';' after expression"))
        } else {
            Err(self.error_here("Expected ';' after expression"))
        }
    }

    fn synchronize_statement(&mut self) {
        while !self.is_eof() {
            match self.peek_kind() {
                Some(TokenKind::Semi) => {
                    self.advance();
                    break;
                }
                Some(
                    TokenKind::RBrace
                    | TokenKind::Let
                    | TokenKind::Return
                    | TokenKind::If
                    | TokenKind::While
                    | TokenKind::Loop
                    | TokenKind::For
                    | TokenKind::Match
                    | TokenKind::Break
                    | TokenKind::Continue
                    | TokenKind::Defer,
                ) => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn is_assignment_start(&self) -> bool {
        match self.peek_kind() {
            Some(TokenKind::Identifier(_)) => self.assignment_eq_after_path(1),
            Some(TokenKind::Star) => {
                matches!(self.peek_kind_n(1), Some(TokenKind::Identifier(_)))
                    && self.assignment_eq_after_path(2)
            }
            _ => false,
        }
    }

    fn assignment_eq_after_path(&self, mut index: usize) -> bool {
        loop {
            match self.peek_kind_n(index) {
                Some(TokenKind::Dot) => {
                    index += 1;
                    if !matches!(self.peek_kind_n(index), Some(TokenKind::Identifier(_))) {
                        return false;
                    }
                    index += 1;
                }
                Some(TokenKind::LBracket) => {
                    let mut depth = 1;
                    index += 1;
                    while depth > 0 {
                        match self.peek_kind_n(index) {
                            Some(TokenKind::LBracket) => {
                                depth += 1;
                                index += 1;
                            }
                            Some(TokenKind::RBracket) => {
                                depth -= 1;
                                index += 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            Some(_) => index += 1,
                            None => return false,
                        }
                    }
                }
                _ => break,
            }
        }
        self.peek_kind_n(index) == Some(TokenKind::Eq)
    }

    fn parse_let(&mut self, start: usize) -> Result<LetStmt, SyntaxError> {
        let mutability = if self.matches(TokenKind::Mut) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };

        let mut ty = None;
        let pattern;

        if self.type_expr_without_identifier_start() {
            let annotation = self.parse_type_annotation()?;
            let ident = self.expect_identifier("Expected binding name")?;
            ty = Some(annotation);
            pattern = Pattern::Identifier(ident.name, ident.span);
        } else if self.upcoming_type_annotation_with_binding() {
            let first = self.expect_identifier("Expected type name or binding")?;
            let mut type_expr = TypeExpr::named(first.name.clone());
            let mut span_end = first.span.end;
            if self.matches(TokenKind::LBracket) {
                let (args, end) = self.parse_type_arguments()?;
                type_expr = TypeExpr::Named(first.name.clone(), args);
                span_end = end;
            }
            ty = Some(TypeAnnotation {
                ty: type_expr,
                span: Span::new(first.span.start, span_end),
            });
            let binding_ident = self.expect_identifier("Expected binding name")?;
            pattern = Pattern::Identifier(binding_ident.name, binding_ident.span);
        } else {
            pattern = self.parse_pattern()?;
        }

        if self.matches(TokenKind::Colon) {
            if let Pattern::Identifier(_, _) = pattern {
                let annotation = self.parse_type_annotation()?;
                ty = Some(annotation);
            } else {
                return Err(
                    self.error_here("type annotations are only supported for identifier bindings")
                );
            }
        }

        let value = if self.matches(TokenKind::Eq) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if value.is_none() && !matches!(pattern, Pattern::Identifier(_, _)) {
            return Err(self.error_here("destructuring bindings require an initializer expression"));
        }

        let end = value
            .as_ref()
            .map(|expr| expr_span(expr).end)
            .unwrap_or_else(|| self.last_span_end(start));

        Ok(LetStmt {
            pattern,
            ty,
            value,
            mutability,
            span: Span::new(start, end),
        })
    }

    fn parse_assignment(&mut self) -> Result<AssignStmt, SyntaxError> {
        let start = self.current_span_start();
        let mut target = if self.matches(TokenKind::Star) {
            let ident = self.expect_identifier("Expected target after '*'")?;
            let span = Span::new(start, ident.span.end);
            Expr::Deref {
                expr: Box::new(Expr::Identifier(ident)),
                span,
            }
        } else {
            Expr::Identifier(self.expect_identifier("Expected assignment target")?)
        };
        loop {
            if self.matches(TokenKind::Dot) {
                let field = self.expect_identifier("Expected field name after '.'")?;
                let span = expr_span(&target).union(field.span);
                target = Expr::FieldAccess {
                    base: Box::new(target),
                    field: field.name,
                    span,
                };
                continue;
            }
            if self.matches(TokenKind::LBracket) {
                let index = self.parse_expression()?;
                let end = self.expect(TokenKind::RBracket)?.span.end;
                let span = Span::new(expr_span(&target).start, end);
                target = Expr::Index {
                    base: Box::new(target),
                    index: Box::new(index),
                    span,
                };
                continue;
            }
            break;
        }
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expression()?;
        self.expect(TokenKind::Semi)?;
        Ok(AssignStmt { target, value })
    }

    fn parse_return(&mut self) -> Result<ReturnStmt, SyntaxError> {
        let mut values = Vec::new();
        if self.matches(TokenKind::Semi) {
            return Ok(ReturnStmt { values });
        }
        loop {
            values.push(self.parse_expression()?);
            if self.matches(TokenKind::Comma) {
                continue;
            }
            break;
        }
        self.expect(TokenKind::Semi)?;
        Ok(ReturnStmt { values })
    }

    fn parse_while(&mut self) -> Result<WhileStmt, SyntaxError> {
        self.enter_block_context();
        let condition = if self.matches(TokenKind::Let) {
            let pattern = match self.parse_pattern() {
                Ok(pattern) => pattern,
                Err(err) => {
                    self.exit_block_context();
                    return Err(err);
                }
            };
            self.expect(TokenKind::Eq)?;
            let value = match self.parse_expression() {
                Ok(expr) => expr,
                Err(err) => {
                    self.exit_block_context();
                    return Err(err);
                }
            };
            WhileCondition::Let { pattern, value }
        } else {
            match self.parse_expression() {
                Ok(expr) => WhileCondition::Expr(expr),
                Err(err) => {
                    self.exit_block_context();
                    return Err(err);
                }
            }
        };
        self.exit_block_context();
        let body = self.parse_block()?;
        Ok(WhileStmt { condition, body })
    }

    fn parse_loop(&mut self) -> Result<LoopStmt, SyntaxError> {
        let start = self.previous_span().map(|s| s.start).unwrap_or(0);
        let body = self.parse_block()?;
        let span = Span::new(start, body.span.end);
        Ok(LoopStmt { body, span })
    }

    fn type_expr_without_identifier_start(&self) -> bool {
        match self.peek_kind() {
            Some(TokenKind::LParen) => self.tuple_type_followed_by_binding(),
            Some(TokenKind::LBracket) => matches!(
                self.peek_kind_n(1),
                Some(TokenKind::RBracket) | Some(TokenKind::Integer(_))
            ),
            Some(TokenKind::Star | TokenKind::Ampersand) => true,
            _ => false,
        }
    }

    fn tuple_type_followed_by_binding(&self) -> bool {
        let mut depth = 0usize;
        let mut offset = 0usize;
        while let Some(kind) = self.peek_kind_n(offset) {
            match kind {
                TokenKind::LParen => {
                    depth += 1;
                }
                TokenKind::RParen => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        return matches!(
                            self.peek_kind_n(offset + 1),
                            Some(TokenKind::Identifier(_))
                        );
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            offset += 1;
        }
        false
    }

    fn upcoming_type_annotation_with_binding(&self) -> bool {
        match self.peek_kind() {
            Some(TokenKind::Identifier(_)) => matches!(
                self.peek_kind_n(1),
                Some(TokenKind::Identifier(_)) | Some(TokenKind::LBracket)
            ),
            _ => false,
        }
    }

    fn parse_for_statement(&mut self) -> Result<ForStmt, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let binding = self.expect_identifier("Expected loop binding")?;
        self.expect(TokenKind::In)?;
        self.enter_block_context();
        let range_expr = self.parse_expression();
        self.exit_block_context();
        let range_expr = range_expr?;
        let target = if let Expr::Range(expr) = range_expr {
            ForTarget::Range(expr)
        } else {
            ForTarget::Collection(range_expr)
        };
        let body = self.parse_block()?;
        let body_end = body.span.end;
        Ok(ForStmt {
            binding: binding.name,
            target,
            body,
            span: Span::new(start, body_end),
        })
    }

    fn parse_expression(&mut self) -> Result<Expr, SyntaxError> {
        let expr = self.parse_binary(0)?;
        if self.matches(TokenKind::DotDot) {
            let start_span = expr_span(&expr).start;
            let end_expr = self.parse_expression()?;
            let end_span = expr_span(&end_expr).end;
            let range = RangeExpr {
                start: Box::new(expr),
                end: Box::new(end_expr),
                inclusive: false,
                span: Span::new(start_span, end_span),
            };
            return Ok(Expr::Range(range));
        }
        if self.matches(TokenKind::DotDotEq) {
            let start_span = expr_span(&expr).start;
            let end_expr = self.parse_expression()?;
            let end_span = expr_span(&end_expr).end;
            let range = RangeExpr {
                start: Box::new(expr),
                end: Box::new(end_expr),
                inclusive: true,
                span: Span::new(start_span, end_span),
            };
            return Ok(Expr::Range(range));
        }
        Ok(expr)
    }

    fn parse_binary(&mut self, min_prec: u8) -> Result<Expr, SyntaxError> {
        let mut left = self.parse_unary()?;

        loop {
            let (op, prec) = match self.current_binary_op() {
                Some(info) => info,
                None => break,
            };
            if prec < min_prec {
                break;
            }
            self.advance();
            let right = self.parse_binary(prec + 1)?;
            let span = expr_span(&left).union(expr_span(&right));
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, SyntaxError> {
        if self.matches(TokenKind::Minus) {
            let expr = self.parse_unary()?;
            let span = expr_span(&expr);
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
                span,
            });
        }
        if self.matches(TokenKind::Bang) {
            let expr = self.parse_unary()?;
            let span = expr_span(&expr);
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
                span,
            });
        }
        if self.matches(TokenKind::Ampersand) {
            let mutable = self.matches(TokenKind::Mut);
            let expr = self.parse_unary()?;
            let span = expr_span(&expr);
            return Ok(Expr::Reference {
                mutable,
                expr: Box::new(expr),
                span,
            });
        }
        if self.matches(TokenKind::Star) {
            let expr = self.parse_unary()?;
            let span = expr_span(&expr);
            return Ok(Expr::Deref {
                expr: Box::new(expr),
                span,
            });
        }
        if self.matches(TokenKind::Move) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            let expr = self.parse_unary()?;
            let span = Span::new(start, expr_span(&expr).end);
            return Ok(Expr::Move {
                expr: Box::new(expr),
                span,
            });
        }
        if self.matches(TokenKind::Spawn) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            let expr = self.parse_unary()?;
            let span = Span::new(start, expr_span(&expr).end);
            return Ok(Expr::Spawn {
                expr: Box::new(expr),
                span,
            });
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.matches(TokenKind::LBracket) {
                let span_start = expr_span(&expr).start;
                let saved_pos = self.pos;
                let saved_last_span = self.last_span.clone();
                let saved_errors_len = self.errors.len();
                if self.bracket_followed_by_lparen() {
                    match self.parse_type_arguments() {
                        Ok((type_args, _)) => {
                            if self.matches(TokenKind::LParen) {
                                self.enter_paren();
                                let mut args = Vec::new();
                                if !self.check(TokenKind::RParen) {
                                    loop {
                                        args.push(self.parse_expression()?);
                                        if self.matches(TokenKind::Comma) {
                                            continue;
                                        }
                                        break;
                                    }
                                }
                                let end = match self.expect(TokenKind::RParen) {
                                    Ok(token) => token.span.end,
                                    Err(err) => {
                                        self.exit_paren();
                                        return Err(err);
                                    }
                                };
                                self.exit_paren();
                                expr = Expr::Call {
                                    callee: Box::new(expr),
                                    type_args,
                                    args,
                                    span: Span::new(span_start, end),
                                };
                                continue;
                            }
                        }
                        Err(_) => {}
                    }
                    // rewind if type argument parse failed or there was no call
                    self.pos = saved_pos;
                    self.last_span = saved_last_span;
                    self.errors.truncate(saved_errors_len);
                }
                let index_expr = self.parse_expression()?;
                let end = self.expect(TokenKind::RBracket)?.span.end;
                expr = Expr::Index {
                    base: Box::new(expr),
                    index: Box::new(index_expr),
                    span: Span::new(span_start, end),
                };
                continue;
            }
            if self.matches(TokenKind::LParen) {
                self.enter_paren();
                let span_start = expr_span(&expr).start;
                let mut args = Vec::new();
                if !self.check(TokenKind::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                let end = match self.expect(TokenKind::RParen) {
                    Ok(token) => token.span.end,
                    Err(err) => {
                        self.exit_paren();
                        return Err(err);
                    }
                };
                self.exit_paren();
                expr = Expr::Call {
                    callee: Box::new(expr),
                    type_args: Vec::new(),
                    args,
                    span: Span::new(span_start, end),
                };
                continue;
            }
            if self.matches(TokenKind::Dot) {
                let field = self.expect_identifier("Expected field name after '.'")?;
                let span = expr_span(&expr).union(field.span);
                if self.check(TokenKind::LParen) {
                    self.expect(TokenKind::LParen)?;
                    let mut values = Vec::new();
                    if !self.check(TokenKind::RParen) {
                        loop {
                            values.push(self.parse_expression()?);
                            if self.matches(TokenKind::Comma) {
                                if self.check(TokenKind::RParen) {
                                    break;
                                }
                                continue;
                            }
                            break;
                        }
                    }
                    let end = self.expect(TokenKind::RParen)?.span.end;
                    let call_span = Span::new(span.start, end);
                    if let Expr::Identifier(enum_ident) = &expr {
                        if is_pascal_case(&enum_ident.name) {
                            expr = Expr::EnumLiteral {
                                enum_name: Some(enum_ident.name.clone()),
                                variant: field.name,
                                values,
                                span: call_span,
                            };
                            continue;
                        }
                    }
                    expr = Expr::Call {
                        callee: Box::new(Expr::FieldAccess {
                            base: Box::new(expr),
                            field: field.name,
                            span,
                        }),
                        type_args: Vec::new(),
                        args: values,
                        span: call_span,
                    };
                    continue;
                } else {
                    expr = Expr::FieldAccess {
                        base: Box::new(expr),
                        field: field.name,
                        span,
                    };
                }
                continue;
            }
            if self.matches(TokenKind::Question) {
                let span = expr_span(&expr);
                let question_span = self
                    .previous_span()
                    .unwrap_or_else(|| Span::new(span.end, span.end));
                expr = Expr::TryPropagate {
                    expr: Box::new(expr),
                    span: Span::new(span.start, question_span.end),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, SyntaxError> {
        if self.matches(TokenKind::Match) {
            return self.parse_match_expression();
        }
        if self.matches(TokenKind::If) {
            return self.parse_if_expression();
        }
        if self.matches(TokenKind::Try) {
            return self.parse_try_expression();
        }
        if self.matches(TokenKind::Hash) {
            return self.parse_map_literal();
        }
        if !self.suppress_block_literal && self.matches(TokenKind::LBrace) {
            self.rewind();
            let block = self.parse_block()?;
            return Ok(Expr::Block(Box::new(block)));
        }
        if self.matches(TokenKind::At) {
            let at_start = self.previous_span().map(|s| s.start).unwrap_or_else(|| self.current_span_start());
            let ident = self.expect_identifier("Expected identifier after '@'")?;
            let span = Span::new(at_start, ident.span.end);
            return Ok(Expr::Identifier(Identifier {
                name: format!("@{}", ident.name),
                span,
            }));
        }
        if self.matches(TokenKind::Tilde) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            let name = self.expect_identifier("Expected macro name after '~'")?;
            self.expect(TokenKind::LParen)?;
            self.enter_paren();
            let args = self.parse_macro_call_args()?;
            let end = match self.expect(TokenKind::RParen) {
                Ok(token) => token.span.end,
                Err(err) => {
                    self.exit_paren();
                    return Err(err);
                }
            };
            self.exit_paren();
            return Ok(Expr::MacroCall {
                name,
                args,
                span: Span::new(start, end),
            });
        }

        match self.peek_kind() {
            Some(TokenKind::Identifier(_)) => self.parse_identifier_expression(),
            Some(TokenKind::Integer(_)) => self.parse_int_literal(),
            Some(TokenKind::Float(_)) => self.parse_float_literal(),
            Some(TokenKind::String(_)) => self.parse_string_literal(),
            Some(TokenKind::TemplateString(_)) => self.parse_template_string_literal(),
            Some(TokenKind::Rune(_)) => self.parse_rune_literal(),
            Some(TokenKind::True) => {
                let span = self.advance().span;
                Ok(Expr::Literal(Literal::Bool(true, span)))
            }
            Some(TokenKind::False) => {
                let span = self.advance().span;
                Ok(Expr::Literal(Literal::Bool(false, span)))
            }
            Some(TokenKind::LParen) => {
                let start = self.advance().span.start;
                self.enter_paren();
                let mut values = Vec::new();
                if !self.check(TokenKind::RParen) {
                    loop {
                        values.push(self.parse_expression()?);
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                let end = match self.expect(TokenKind::RParen) {
                    Ok(token) => token.span.end,
                    Err(err) => {
                        self.exit_paren();
                        return Err(err);
                    }
                };
                self.exit_paren();
                if values.len() == 1 {
                    Ok(values.into_iter().next().unwrap())
                } else {
                    Ok(Expr::Tuple(values, Span::new(start, end)))
                }
            }
            Some(TokenKind::LBracket) => {
                let start = self.advance().span.start;
                let mut values = Vec::new();
                if !self.check(TokenKind::RBracket) {
                    loop {
                        values.push(self.parse_expression()?);
                        if self.matches(TokenKind::Comma) {
                            if self.check(TokenKind::RBracket) {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                let end = self.expect(TokenKind::RBracket)?.span.end;
                Ok(Expr::ArrayLiteral(values, Span::new(start, end)))
            }
            _ => Err(self.error_here("Unexpected token in expression")),
        }
    }

    fn parse_map_literal(&mut self) -> Result<Expr, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        self.expect(TokenKind::LBrace)?;
        let mut entries = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            let key = self.parse_expression()?;
            self.expect(TokenKind::Colon)?;
            let value = self.parse_expression()?;
            entries.push(MapLiteralEntry { key, value });
            if self.matches(TokenKind::Comma) {
                if self.check(TokenKind::RBrace) {
                    break;
                }
                continue;
            }
            break;
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Expr::MapLiteral {
            entries,
            span: Span::new(start, end),
        })
    }

    fn parse_try_expression(&mut self) -> Result<Expr, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let block = self.parse_block()?;
        let span = Span::new(start, block.span.end);
        Ok(Expr::Try {
            block: Box::new(block),
            span,
        })
    }

    fn parse_identifier_expression(&mut self) -> Result<Expr, SyntaxError> {
        let ident = self.expect_identifier("Expected identifier")?;
        if self.matches(TokenKind::Colon) || self.matches(TokenKind::ColonColon) {
            let variant = self.expect_identifier("Expected enum variant name after `:`")?;
            self.expect(TokenKind::LParen)?;
            let mut values = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    values.push(self.parse_expression()?);
                    if self.matches(TokenKind::Comma) {
                        if self.check(TokenKind::RParen) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            let end = self.expect(TokenKind::RParen)?.span.end;
            let span = Span::new(ident.span.start, end);
            return Ok(Expr::EnumLiteral {
                enum_name: Some(ident.name),
                variant: variant.name,
                values,
                span,
            });
        }
        if self.struct_literals_allowed() && self.matches(TokenKind::LBrace) {
            // struct literal detection
            let mut named_fields = Vec::new();
            let mut positional = Vec::new();
            let mut is_named = None;
            if !self.check(TokenKind::RBrace) {
                loop {
                    if is_named != Some(false)
                        && self
                            .peek_kind()
                            .map(|t| matches!(t, TokenKind::Identifier(_)))
                            .unwrap_or(false)
                        && self.peek_kind_n(1) == Some(TokenKind::Colon)
                    {
                        is_named.get_or_insert(true);
                        let field = self.expect_identifier("Expected field name")?;
                        self.expect(TokenKind::Colon)?;
                        let value = self.parse_expression()?;
                        named_fields.push(StructLiteralField {
                            name: field.name,
                            value,
                        });
                    } else {
                        is_named.get_or_insert(false);
                        positional.push(self.parse_expression()?);
                    }
                    if self.matches(TokenKind::Comma) {
                        if self.check(TokenKind::RBrace) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            let end = self.expect(TokenKind::RBrace)?.span.end;
            let span = Span::new(ident.span.start, end);
            let fields = if is_named == Some(true) {
                StructLiteralKind::Named(named_fields)
            } else {
                StructLiteralKind::Positional(positional)
            };
            return Ok(Expr::StructLiteral {
                name: ident.name,
                fields,
                span,
            });
        } else if (ident.name.contains("::") || ident.name.contains('.'))
            && self.matches(TokenKind::LParen)
        {
            // Enum literal (variant call-like syntax)
            let mut values = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    values.push(self.parse_expression()?);
                    if self.matches(TokenKind::Comma) {
                        if self.check(TokenKind::RParen) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            let end = self.expect(TokenKind::RParen)?.span.end;
            let span = Span::new(ident.span.start, end);
            // Support optional Qualified::Variant syntax
            let (enum_name, variant) = if ident.name.contains("::") {
                let mut parts = ident.name.splitn(2, "::");
                let enum_name = parts.next().unwrap().to_string();
                let variant = parts.next().unwrap().to_string();
                (Some(enum_name), variant)
            } else if ident.name.contains('.') {
                let mut parts = ident.name.splitn(2, '.');
                let enum_name = parts.next().unwrap().to_string();
                let variant = parts.next().unwrap().to_string();
                (Some(enum_name), variant)
            } else {
                (None, ident.name)
            };
            return Ok(Expr::EnumLiteral {
                enum_name,
                variant,
                values,
                span,
            });
        }
        Ok(Expr::Identifier(ident))
    }

    fn enter_block_context(&mut self) {
        self.block_context_stack.push(self.paren_depth);
    }

    fn exit_block_context(&mut self) {
        self.block_context_stack.pop();
    }

    fn struct_literals_allowed(&self) -> bool {
        !self
            .block_context_stack
            .iter()
            .any(|&depth| depth == self.paren_depth)
    }

    fn enter_paren(&mut self) {
        self.paren_depth += 1;
    }

    fn exit_paren(&mut self) {
        if self.paren_depth > 0 {
            self.paren_depth -= 1;
        }
    }

    fn parse_match_expression(&mut self) -> Result<Expr, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let prev_flag = self.suppress_block_literal;
        self.suppress_block_literal = true;
        self.enter_block_context();
        let expr = match self.parse_expression() {
            Ok(expr) => expr,
            Err(err) => {
                self.exit_block_context();
                self.suppress_block_literal = prev_flag;
                return Err(err);
            }
        };
        self.exit_block_context();
        self.suppress_block_literal = prev_flag;
        self.expect(TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            let mut pattern = match self.parse_pattern() {
                Ok(pat) => pat,
                Err(_) => {
                    // Fallback: allow enum constructor syntax parsed as expression (e.g. Enum.Variant(...)) in patterns.
                    let saved_pos = self.pos;
                    let saved_last = self.last_span.clone();
                    match self.parse_expression() {
                        Ok(expr) => {
                            if let Some(pat) = expr_to_pattern(&expr) {
                                pat
                            } else {
                                self.pos = saved_pos;
                                self.last_span = saved_last;
                                return Err(self.error_here("Expected pattern"));
                            }
                        }
                        Err(_) => {
                            self.pos = saved_pos;
                            self.last_span = saved_last;
                            return Err(self.error_here("Expected pattern"));
                        }
                    }
                }
            };
            if let Pattern::Identifier(enum_name, _) = &pattern {
                if self.matches(TokenKind::Dot)
                    || self.matches(TokenKind::ColonColon)
                    || self.matches(TokenKind::Colon)
                {
                    let variant = self.expect_identifier("Expected enum variant name")?;
                    let bindings = if self.matches(TokenKind::LParen) {
                        self.parse_pattern_bindings()?
                    } else {
                        Vec::new()
                    };
                    pattern = Pattern::EnumVariant {
                        enum_name: Some(enum_name.clone()),
                        variant: variant.name,
                        bindings,
                    };
                }
            }
            let guard = if self.matches(TokenKind::If) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.expect(TokenKind::FatArrow)?;
            let value = self.parse_expression()?;
            arms.push(MatchArmExpr {
                pattern,
                guard,
                value,
            });
            if self.matches(TokenKind::Comma) {
                continue;
            }
            if self.check(TokenKind::RBrace) {
                break;
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Expr::Match(MatchExpr {
            expr: Box::new(expr),
            arms,
            span: Span::new(start, end),
        }))
    }

    fn parse_if_expression(&mut self) -> Result<Expr, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        self.enter_block_context();
        let condition = if self.matches(TokenKind::Let) {
            let pattern = match self.parse_pattern() {
                Ok(pattern) => pattern,
                Err(err) => {
                    self.exit_block_context();
                    return Err(err);
                }
            };
            self.expect(TokenKind::Eq)?;
            let value = match self.parse_expression() {
                Ok(expr) => expr,
                Err(err) => {
                    self.exit_block_context();
                    return Err(err);
                }
            };
            IfCondition::Let { pattern, value }
        } else {
            match self.parse_expression() {
                Ok(expr) => IfCondition::Expr(expr),
                Err(err) => {
                    self.exit_block_context();
                    return Err(err);
                }
            }
        };
        self.exit_block_context();
        let then_branch = self.parse_block()?;
        let mut span_end = then_branch.span.end;
        let else_branch = if self.matches(TokenKind::Else) {
            if self.matches(TokenKind::If) {
                let nested = self.parse_if_expression()?;
                if let Expr::If(if_expr) = nested {
                    span_end = if_expr.span.end;
                    Some(ElseBranch::ElseIf(if_expr))
                } else {
                    return Err(self.error_here("Expected if expression after else"));
                }
            } else {
                let block = self.parse_block()?;
                span_end = block.span.end;
                Some(ElseBranch::Block(block))
            }
        } else {
            None
        };
        let span = Span::new(start, span_end);
        Ok(Expr::If(Box::new(IfExpr {
            condition,
            then_branch,
            else_branch,
            span,
        })))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, SyntaxError> {
        // Enum variant with qualified name using '.' or '::'
        if matches!(self.peek_kind(), Some(TokenKind::Identifier(_))) {
            if let Some(sep) = self.peek_kind_n(1) {
                let is_qualified_enum = match sep {
                    TokenKind::Dot | TokenKind::ColonColon => true,
                    TokenKind::Colon => {
                        let next = self.peek_kind_n(2);
                        let after = self.peek_kind_n(3);
                        matches!(next, Some(TokenKind::Identifier(_)))
                            && matches!(
                                after,
                                Some(
                                    TokenKind::LParen
                                        | TokenKind::FatArrow
                                        | TokenKind::Comma
                                        | TokenKind::RBrace
                                        | TokenKind::RParen
                                )
                            )
                    }
                    _ => false,
                };
                if is_qualified_enum {
                    let enum_ident = self.expect_identifier("Expected enum name")?;
                    self.advance(); // consume separator
                    let variant_ident =
                        self.expect_identifier("Expected enum variant name after enum qualifier")?;
                    let bindings = if self.matches(TokenKind::LParen) {
                        self.parse_pattern_bindings()?
                    } else {
                        Vec::new()
                    };
                    return Ok(Pattern::EnumVariant {
                        enum_name: Some(enum_ident.name),
                        variant: variant_ident.name,
                        bindings,
                    });
                }
            }
        }
        if self.matches(TokenKind::Identifier("_".into())) {
            return Ok(Pattern::Wildcard);
        }
        if self.matches(TokenKind::LBracket) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            return self.parse_slice_pattern(start);
        }
        if self.matches(TokenKind::LParen) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            return self.parse_tuple_pattern(start);
        }
        if self.matches(TokenKind::Hash) {
            let start = self
                .previous_span()
                .map(|s| s.start)
                .unwrap_or_else(|| self.current_span_start());
            return self.parse_map_pattern(start);
        }
        if let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Identifier(_) => return self.parse_named_pattern(),
                TokenKind::Integer(_) => {
                    let lit = self.parse_int_literal()?;
                    if let Expr::Literal(lit) = lit {
                        return Ok(Pattern::Literal(lit));
                    }
                }
                TokenKind::Float(_) => {
                    let lit = self.parse_float_literal()?;
                    if let Expr::Literal(lit) = lit {
                        return Ok(Pattern::Literal(lit));
                    }
                }
                TokenKind::String(_) => {
                    let lit = self.parse_string_literal()?;
                    if let Expr::Literal(lit) = lit {
                        return Ok(Pattern::Literal(lit));
                    }
                }
                TokenKind::Rune(_) => {
                    let lit = self.parse_rune_literal()?;
                    if let Expr::Literal(lit) = lit {
                        return Ok(Pattern::Literal(lit));
                    }
                }
                TokenKind::True => {
                    let span = self.advance().span;
                    return Ok(Pattern::Literal(Literal::Bool(true, span)));
                }
                TokenKind::False => {
                    let span = self.advance().span;
                    return Ok(Pattern::Literal(Literal::Bool(false, span)));
                }
                _ => {}
            }
        }
        Err(self.error_here("Unsupported pattern"))
    }

    fn parse_named_pattern(&mut self) -> Result<Pattern, SyntaxError> {
        let ident = self.expect_identifier("Expected pattern identifier")?;
        if self.check(TokenKind::ColonColon) || self.check(TokenKind::Dot) || self.check(TokenKind::Colon) {
            // Avoid misinterpreting type annotations (`name: Type`) as enum qualifiers.
            if self.check(TokenKind::Colon) && matches!(self.peek_kind_n(2), Some(TokenKind::Eq)) {
                // fall through to treat as identifier pattern
            } else {
                self.advance();
                let variant = self.expect_identifier("Expected variant name after enum qualifier")?;
                let bindings = if self.matches(TokenKind::LParen) {
                    self.parse_pattern_bindings()?
                } else {
                    Vec::new()
                };
                return Ok(Pattern::EnumVariant {
                    enum_name: Some(ident.name),
                    variant: variant.name,
                    bindings,
                });
            }
        }
        if self.matches(TokenKind::LParen) {
            let bindings = self.parse_pattern_bindings()?;
            return Ok(Pattern::EnumVariant {
                enum_name: None,
                variant: ident.name,
                bindings,
            });
        }
        if self.matches(TokenKind::LBrace) {
            let start = ident.span.start;
            return self.parse_struct_pattern(Some(ident.name), start);
        }
        if is_pascal_case(&ident.name) {
            return Ok(Pattern::EnumVariant {
                enum_name: None,
                variant: ident.name,
                bindings: Vec::new(),
            });
        }
        Ok(Pattern::Identifier(ident.name, ident.span))
    }

    fn parse_pattern_bindings(&mut self) -> Result<Vec<Pattern>, SyntaxError> {
        let mut bindings = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                bindings.push(self.parse_pattern()?);
                if self.matches(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RParen)?;
        Ok(bindings)
    }

    fn parse_tuple_pattern(&mut self, start: usize) -> Result<Pattern, SyntaxError> {
        let mut elements = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                elements.push(self.parse_pattern()?);
                if self.matches(TokenKind::Comma) {
                    if self.check(TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RParen)?.span.end;
        Ok(Pattern::Tuple(elements, Span::new(start, end)))
    }

    fn parse_map_pattern(&mut self, start: usize) -> Result<Pattern, SyntaxError> {
        self.expect(TokenKind::LBrace)?;
        let mut entries = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            let key_token = self.expect_string("Expected string literal map key")?;
            self.expect(TokenKind::Colon)?;
            let pattern = self.parse_pattern()?;
            entries.push(MapPatternEntry {
                key: key_token,
                pattern,
            });
            if self.matches(TokenKind::Comma) {
                if self.check(TokenKind::RBrace) {
                    break;
                }
                continue;
            }
            break;
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Pattern::Map(entries, Span::new(start, end)))
    }

    fn parse_struct_pattern(
        &mut self,
        struct_name: Option<String>,
        start: usize,
    ) -> Result<Pattern, SyntaxError> {
        let mut fields = Vec::new();
        let mut has_spread = false;
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::DotDot) {
                if has_spread {
                    return Err(self.error_here("`..` already used in struct pattern"));
                }
                has_spread = true;
                if !self.check(TokenKind::Comma) && !self.check(TokenKind::RBrace) {
                    return Err(self.error_here("`..` cannot bind names in struct patterns yet"));
                }
            } else {
                let field_ident =
                    self.expect_identifier("Expected field name in struct pattern")?;
                let pattern = if self.matches(TokenKind::Colon) {
                    self.parse_pattern()?
                } else {
                    Pattern::Identifier(field_ident.name.clone(), field_ident.span)
                };
                fields.push(StructPatternField {
                    name: field_ident.name,
                    pattern,
                });
            }
            if self.matches(TokenKind::Comma) {
                continue;
            }
            break;
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Pattern::Struct {
            struct_name,
            fields,
            has_spread,
            span: Span::new(start, end),
        })
    }

    fn parse_slice_pattern(&mut self, start: usize) -> Result<Pattern, SyntaxError> {
        let mut prefix = Vec::new();
        let mut suffix = Vec::new();
        let mut rest: Option<Pattern> = None;
        let mut rest_seen = false;
        while !self.check(TokenKind::RBracket) && !self.is_eof() {
            if !rest_seen && self.matches(TokenKind::DotDot) {
                if rest.is_some() {
                    return Err(self.error_here("`..` already used in slice pattern"));
                }
                rest_seen = true;
                if self.check(TokenKind::Comma) || self.check(TokenKind::RBracket) {
                    rest = Some(Pattern::Wildcard);
                } else {
                    let pattern = self.parse_pattern()?;
                    rest = Some(pattern);
                }
            } else {
                let pattern = self.parse_pattern()?;
                if rest_seen {
                    suffix.push(pattern);
                } else {
                    prefix.push(pattern);
                }
            }
            if self.matches(TokenKind::Comma) {
                continue;
            }
            break;
        }
        let end = self.expect(TokenKind::RBracket)?.span.end;
        Ok(Pattern::Slice {
            prefix,
            rest: rest.map(Box::new),
            suffix,
            span: Span::new(start, end),
        })
    }

    fn expect_string(&mut self, msg: &str) -> Result<String, SyntaxError> {
        if let Some(TokenKind::String(value)) = self.peek_kind() {
            self.advance();
            Ok(value)
        } else {
            let span = self
                .tokens
                .get(self.pos)
                .map(|t| t.span)
                .unwrap_or_else(|| Span::new(0, 0));
            Err(self.error_at(span, msg))
        }
    }

    fn parse_int_literal(&mut self) -> Result<Expr, SyntaxError> {
        match self.advance().kind.clone() {
            TokenKind::Integer(value) => {
                let span = self.previous_span().unwrap();
                Ok(Expr::Literal(Literal::Int(value, span)))
            }
            _ => Err(self.error_here("Expected integer literal")),
        }
    }

    fn parse_float_literal(&mut self) -> Result<Expr, SyntaxError> {
        match self.advance().kind.clone() {
            TokenKind::Float(value) => {
                let span = self.previous_span().unwrap();
                Ok(Expr::Literal(Literal::Float(value, span)))
            }
            _ => Err(self.error_here("Expected float literal")),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expr, SyntaxError> {
        match self.advance().kind.clone() {
            TokenKind::String(value) => {
                let span = self.previous_span().unwrap();
                Ok(Expr::Literal(Literal::String(value, span)))
            }
            _ => Err(self.error_here("Expected string literal")),
        }
    }

    fn build_format_string(
        &self,
        value: String,
        span: Span,
    ) -> Result<FormatStringLiteral, SyntaxError> {
        let mut segments = Vec::new();
        let mut literal = String::new();
        let mut iter = value.char_indices().peekable();
        let content_start = span.start + 1;
        while let Some((idx, ch)) = iter.next() {
            match ch {
                '{' => {
                    if let Some((_, '{')) = iter.peek() {
                        literal.push('{');
                        iter.next();
                        continue;
                    }
                    if !literal.is_empty() {
                        segments.push(FormatSegment::Literal(std::mem::take(&mut literal)));
                    }
                    let placeholder_start = content_start + idx;
                    if let Some((close_idx, '}')) = iter.peek() {
                        let close_abs = content_start + *close_idx + 1;
                        iter.next();
                        segments.push(FormatSegment::Implicit(Span::new(
                            placeholder_start,
                            close_abs,
                        )));
                        continue;
                    }
                    let expr_start = content_start + idx + 1;
                    let mut placeholder = String::new();
                    let close_info = loop {
                        match iter.next() {
                            Some((close_idx, '}')) => break Some(close_idx),
                            Some((_, ch)) => placeholder.push(ch),
                            None => break None,
                        }
                    };
                    let Some(close_idx) = close_info else {
                        return Err(self.error_at(
                            Span::new(placeholder_start, span.end),
                            "Unterminated format placeholder",
                        ));
                    };
                    let close_abs = content_start + close_idx + 1;
                    let trimmed = placeholder.trim();
                    if trimmed.is_empty() {
                        return Err(self.error_at(
                            Span::new(placeholder_start, close_abs),
                            "Invalid format placeholder",
                        ));
                    }
                    match parse_expression_snippet(self.path.clone(), trimmed) {
                        Ok(mut expr) => {
                            shift_expr_spans(&mut expr, expr_start as isize);
                            segments.push(FormatSegment::Expr {
                                expr,
                                span: Span::new(placeholder_start, close_abs),
                            });
                        }
                        Err(_) => {
                            return Err(self.error_at(
                                Span::new(expr_start, close_abs),
                                "Expected `}` in format placeholder",
                            ));
                        }
                    }
                }
                '}' => {
                    if let Some((_, '}')) = iter.peek() {
                        literal.push('}');
                        iter.next();
                    } else {
                        let err_span = Span::new(content_start + idx, content_start + idx + 1);
                        return Err(self.error_at(err_span, "Unmatched `}` in format string"));
                    }
                }
                _ => literal.push(ch),
            }
        }
        if !literal.is_empty() {
            segments.push(FormatSegment::Literal(literal));
        }
        Ok(FormatStringLiteral { segments, span })
    }

    fn parse_template_string_literal(&mut self) -> Result<Expr, SyntaxError> {
        match self.advance().kind.clone() {
            TokenKind::TemplateString(value) => {
                let span = self.previous_span().unwrap();
                let literal = self.build_format_string(value, span)?;
                Ok(Expr::FormatString(literal))
            }
            _ => Err(self.error_here("Expected format string literal")),
        }
    }

    fn parse_rune_literal(&mut self) -> Result<Expr, SyntaxError> {
        match self.advance().kind.clone() {
            TokenKind::Rune(value) => {
                let span = self.previous_span().unwrap();
                Ok(Expr::Literal(Literal::Rune(value, span)))
            }
            _ => Err(self.error_here("Expected rune literal")),
        }
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, SyntaxError> {
        let start = self.current_span_start();
        let ty = self.parse_type_expr()?;
        let end = self.last_span_end(start);
        Ok(TypeAnnotation {
            ty,
            span: Span::new(start, end),
        })
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, SyntaxError> {
        if self.matches(TokenKind::Ampersand) {
            let mutable = self.matches(TokenKind::Mut);
            let ty = self.parse_type_expr()?;
            return Ok(TypeExpr::Reference {
                mutable,
                ty: Box::new(ty),
            });
        }
        if self.matches(TokenKind::Star) {
            let mutable = self.matches(TokenKind::Mut);
            let ty = self.parse_type_expr()?;
            return Ok(TypeExpr::Pointer {
                mutable,
                ty: Box::new(ty),
            });
        }
        if self.matches(TokenKind::LBracket) {
            if self.matches(TokenKind::RBracket) {
                let elem = self.parse_type_expr()?;
                return Ok(TypeExpr::Slice(Box::new(elem)));
            }
            let size_expr = self.expect_number_literal("Expected array length")?;
            self.expect(TokenKind::RBracket)?;
            let elem = self.parse_type_expr()?;
            if size_expr < 0 {
                return Err(self.error_here("Array length must be non-negative"));
            }
            let size = size_expr as usize;
            return Ok(TypeExpr::Array {
                size,
                ty: Box::new(elem),
            });
        }
        if self.matches(TokenKind::LParen) {
            let mut types = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    types.push(self.parse_type_expr()?);
                    if self.matches(TokenKind::Comma) {
                        continue;
                    }
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            if types.is_empty() {
                return Ok(TypeExpr::Unit);
            }
            return Ok(TypeExpr::Tuple(types));
        }
        if let Some(TokenKind::Identifier(_)) = self.peek_kind() {
            let ident = self.expect_identifier("Expected type name")?;
            let mut args = Vec::new();
            if self.matches(TokenKind::LBracket) {
                if !self.check(TokenKind::RBracket) {
                    loop {
                        args.push(self.parse_type_expr()?);
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                self.expect(TokenKind::RBracket)?;
            }
            return Ok(TypeExpr::Named(ident.name, args));
        }
        Err(self.error_here("Expected type"))
    }

    fn parse_type_arguments(&mut self) -> Result<(Vec<TypeExpr>, usize), SyntaxError> {
        let mut args = Vec::new();
        if !self.check(TokenKind::RBracket) {
            loop {
                args.push(self.parse_type_expr()?);
                if self.matches(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RBracket)?.span.end;
        Ok((args, end))
    }

    fn bracket_followed_by_lparen(&self) -> bool {
        let mut depth = 1usize;
        let mut offset = 0usize;
        while let Some(kind) = self.peek_kind_n(offset) {
            match kind {
                TokenKind::LBracket => {
                    depth += 1;
                }
                TokenKind::RBracket => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return matches!(self.peek_kind_n(offset + 1), Some(TokenKind::LParen));
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            offset += 1;
        }
        false
    }

    fn current_binary_op(&self) -> Option<(BinaryOp, u8)> {
        match self.peek_kind() {
            Some(TokenKind::Plus) => Some((BinaryOp::Add, 10)),
            Some(TokenKind::Minus) => Some((BinaryOp::Sub, 10)),
            Some(TokenKind::Star) => Some((BinaryOp::Mul, 20)),
            Some(TokenKind::Slash) => Some((BinaryOp::Div, 20)),
            Some(TokenKind::Percent) => Some((BinaryOp::Rem, 20)),
            Some(TokenKind::AmpersandAmpersand) => Some((BinaryOp::And, 4)),
            Some(TokenKind::PipePipe) => Some((BinaryOp::Or, 3)),
            Some(TokenKind::Ampersand) => Some((BinaryOp::BitAnd, 8)),
            Some(TokenKind::Pipe) => Some((BinaryOp::BitOr, 6)),
            Some(TokenKind::Caret) => Some((BinaryOp::BitXor, 7)),
            Some(TokenKind::EqEq) => Some((BinaryOp::Eq, 5)),
            Some(TokenKind::BangEq) => Some((BinaryOp::NotEq, 5)),
            Some(TokenKind::Lt) => Some((BinaryOp::Lt, 9)),
            Some(TokenKind::LtEq) => Some((BinaryOp::LtEq, 9)),
            Some(TokenKind::Gt) => Some((BinaryOp::Gt, 9)),
            Some(TokenKind::GtEq) => Some((BinaryOp::GtEq, 9)),
            _ => None,
        }
    }

    fn expect_identifier(&mut self, msg: &str) -> Result<Identifier, SyntaxError> {
        match self.peek_kind() {
            Some(TokenKind::Identifier(name)) => {
                let span = self.advance().span;
                Ok(Identifier { name, span })
            }
            Some(TokenKind::TestKw) => {
                let span = self.advance().span;
                Ok(Identifier {
                    name: "test".into(),
                    span,
                })
            }
            _ => Err(self.error_here(msg)),
        }
    }

    fn expect_string_literal(&mut self, msg: &str) -> Result<String, SyntaxError> {
        match self.peek_kind() {
            Some(TokenKind::String(value)) => {
                self.advance();
                Ok(value)
            }
            _ => Err(self.error_here(msg)),
        }
    }

    fn expect_number_literal(&mut self, msg: &str) -> Result<i128, SyntaxError> {
        match self.peek_kind() {
            Some(TokenKind::Integer(value)) => {
                self.advance();
                Ok(value)
            }
            _ => Err(self.error_here(msg)),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, SyntaxError> {
        if self.check(kind.clone()) {
            Ok(self.advance())
        } else {
            Err(self.error_here(&format!("Expected {:?}", kind)))
        }
    }

    fn consume_optional(&mut self, kind: TokenKind) -> bool {
        if self.check(kind.clone()) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn matches(&mut self, kind: TokenKind) -> bool {
        if self.check(kind.clone()) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        matches!(self.peek_kind(), Some(tk) if tk == kind)
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.pos).map(|t| t.kind.clone())
    }

    fn peek_kind_n(&self, n: usize) -> Option<TokenKind> {
        self.tokens.get(self.pos + n).map(|t| t.kind.clone())
    }

    fn advance(&mut self) -> &Token {
        let token = self
            .tokens
            .get(self.pos)
            .unwrap_or_else(|| self.tokens.last().unwrap());
        self.pos = (self.pos + 1).min(self.tokens.len());
        self.last_span = Some(token.span.start..token.span.end);
        token
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Eof) | None)
    }

    fn current_span_start(&self) -> usize {
        self.tokens
            .get(self.pos)
            .map(|t| t.span.start)
            .unwrap_or_else(|| self.tokens.last().map(|t| t.span.end).unwrap_or(0))
    }

    fn last_span_end(&self, fallback: usize) -> usize {
        self.last_span
            .as_ref()
            .map(|span| span.end)
            .or_else(|| {
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
            })
            .unwrap_or(fallback)
    }

    fn previous_span(&self) -> Option<Span> {
        if self.pos == 0 {
            None
        } else {
            Some(self.tokens[self.pos - 1].span)
        }
    }

    fn error_here(&self, message: &str) -> SyntaxError {
        let span = self
            .tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or_else(|| {
                self.tokens
                    .last()
                    .map(|t| t.span)
                    .unwrap_or_else(|| Span::new(0, 0))
            });
        SyntaxError::new(message.to_string(), span)
    }

    fn error_at(&self, span: Span, message: &str) -> SyntaxError {
        SyntaxError::new(message.to_string(), span)
    }

    fn report(&mut self, err: SyntaxError) {
        self.errors.push(err);
    }

    fn synchronize_item(&mut self) {
        while !self.is_eof() {
            match self.peek_kind() {
                Some(TokenKind::Struct | TokenKind::Enum | TokenKind::Fn | TokenKind::Const) => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    #[allow(dead_code)]
    fn is_identifier_start(ch: char) -> bool {
        ch == '_' || ch.is_ascii_alphabetic()
    }

    #[allow(dead_code)]
    fn is_identifier_part(ch: char) -> bool {
        ch == '_' || ch.is_ascii_alphanumeric()
    }

    fn rewind(&mut self) {
        self.pos = self.pos.saturating_sub(1);
    }
}

fn expr_to_pattern(expr: &Expr) -> Option<Pattern> {
    match expr {
        Expr::EnumLiteral {
            enum_name,
            variant,
            values,
            ..
        } => {
            let mut patterns = Vec::new();
            for value in values {
                if let Some(pat) = expr_to_pattern(value) {
                    patterns.push(pat);
                } else {
                    return None;
                }
            }
            Some(Pattern::EnumVariant {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                bindings: patterns,
            })
        }
        Expr::Identifier(ident) => Some(Pattern::Identifier(ident.name.clone(), ident.span)),
        Expr::Literal(lit) => Some(Pattern::Literal(lit.clone())),
        Expr::Tuple(values, span) => {
            let mut parts = Vec::new();
            for value in values {
                if let Some(pat) = expr_to_pattern(value) {
                    parts.push(pat);
                } else {
                    return None;
                }
            }
            Some(Pattern::Tuple(parts, *span))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_index_expression() {
        let expr = parse_expression_snippet(PathBuf::from("test.prime"), "values[1]").unwrap();
        if let Expr::Index { .. } = expr {
        } else {
            panic!("expected index expression, got {:?}", expr);
        }
    }

    #[test]
    fn parses_match_with_index_scrutinee() {
        let source = r#"
module test::index;

fn main() {
  let []int32 values = [1, 2];
  match values[1] {
    Some(found) => out(found),
    None => {},
  }
}
"#;
        let result = parse_module("test::index", PathBuf::from("test.prime"), source);
        assert!(
            result.is_ok(),
            "parse errors: {:?}",
            result.err().map(|e| e.errors)
        );
    }
}
fn legacy_import_segments(input: &str) -> Vec<String> {
    let without_ext = input.strip_suffix(".prime").unwrap_or(input);
    let normalized = without_ext.replace("::", "/").replace('.', "/");
    normalized
        .split('/')
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect()
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(Literal::Int(_, span))
        | Expr::Literal(Literal::Float(_, span))
        | Expr::Literal(Literal::Bool(_, span))
        | Expr::Literal(Literal::String(_, span))
        | Expr::Literal(Literal::Rune(_, span)) => *span,
        Expr::Try { span, .. } => *span,
        Expr::TryPropagate { span, .. } => *span,
        Expr::Binary { span, .. } => *span,
        Expr::Unary { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::MacroCall { span, .. } => *span,
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::MapLiteral { span, .. } => *span,
        Expr::EnumLiteral { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(expr) => expr.span,
        Expr::Match(expr) => expr.span,
        Expr::Tuple(_, span) => *span,
        Expr::ArrayLiteral(_, span) => *span,
        Expr::Range(range) => range.span,
        Expr::Index { span, .. } => *span,
        Expr::Reference { span, .. } => *span,
        Expr::Deref { span, .. } => *span,
        Expr::Move { span, .. } => *span,
        Expr::FormatString(literal) => literal.span,
        Expr::Spawn { span, .. } => *span,
    }
}
