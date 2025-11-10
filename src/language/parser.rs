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

struct Parser {
    module_name: String,
    path: PathBuf,
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<SyntaxError>,
    suppress_block_literal: bool,
    last_span: Option<Range<usize>>,
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
            last_span: None,
        }
    }

    fn parse(mut self) -> Result<Module, SyntaxErrors> {
        let mut imports = Vec::new();
        let mut items = Vec::new();

        while !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }

            if self.check(TokenKind::Import) {
                match self.parse_import() {
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
                path: self.path,
                imports,
                items,
            })
        } else {
            Err(SyntaxErrors::new(self.errors))
        }
    }

    fn parse_import(&mut self) -> Result<Import, SyntaxError> {
        let start = self.expect(TokenKind::Import)?.span.start;
        let path = self.expect_string_literal("Expected import path string")?;
        let alias = if self.matches(TokenKind::As) {
            Some(self.expect_identifier("Expected alias after 'as'")?.name)
        } else {
            None
        };
        self.consume_optional(TokenKind::Semi);
        let end = self.last_span_end(start);
        Ok(Import {
            path,
            alias,
            span: Span::new(start, end),
        })
    }

    fn parse_item(&mut self) -> Result<Item, SyntaxError> {
        if self.matches(TokenKind::Struct) {
            return self.parse_struct().map(Item::Struct);
        }
        if self.matches(TokenKind::Enum) {
            return self.parse_enum().map(Item::Enum);
        }
        if self.matches(TokenKind::Fn) {
            return self.parse_function().map(Item::Function);
        }
        if self.matches(TokenKind::Const) {
            return self.parse_const().map(Item::Const);
        }
        Err(self.error_here("Expected declaration"))
    }

    fn parse_struct(&mut self) -> Result<StructDef, SyntaxError> {
        let name = self.expect_identifier("Expected struct name")?;
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
            fields,
            span: Span::new(start, end),
        })
    }

    fn parse_enum(&mut self) -> Result<EnumDef, SyntaxError> {
        let name = self.expect_identifier("Expected enum name")?;
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
            variants,
            span: Span::new(start, end),
        })
    }

    fn parse_function(&mut self) -> Result<FunctionDef, SyntaxError> {
        let name = self.expect_identifier("Expected function name")?;
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
            params,
            returns,
            body,
            span,
        })
    }

    fn parse_const(&mut self) -> Result<ConstDef, SyntaxError> {
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
        })
    }

    fn parse_param(&mut self) -> Result<FunctionParam, SyntaxError> {
        let span_start = self.current_span_start();
        let mutability = if self.matches(TokenKind::Mut) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let name = self.expect_identifier("Expected parameter name")?;
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

    fn parse_block(&mut self) -> Result<Block, SyntaxError> {
        let start = self.expect(TokenKind::LBrace)?.span.start;
        let mut statements = Vec::new();
        let mut tail = None;
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            if self.matches(TokenKind::Semi) {
                continue;
            }
            match self.parse_statement(true)? {
                StatementOrTail::Statement(stmt) => statements.push(stmt),
                StatementOrTail::Tail(expr) => {
                    tail = Some(Box::new(expr));
                    break;
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
        if self.matches(TokenKind::For) {
            let stmt = self.parse_for_range()?;
            return Ok(StatementOrTail::Statement(Statement::ForRange(stmt)));
        }
        if self.matches(TokenKind::Break) {
            let span = self
                .previous_span()
                .unwrap_or_else(|| Span::new(self.current_span_start(), self.current_span_start()));
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Break(span)));
        }
        if self.matches(TokenKind::Continue) {
            let span = self
                .previous_span()
                .unwrap_or_else(|| Span::new(self.current_span_start(), self.current_span_start()));
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Continue(span)));
        }
        if self.matches(TokenKind::Defer) {
            let expr = self.parse_expression()?;
            let span = expr_span(&expr);
            self.expect(TokenKind::Semi)?;
            return Ok(StatementOrTail::Statement(Statement::Defer(DeferStmt {
                expr,
                span,
            })));
        }
        if self.matches(TokenKind::LBrace) {
            self.rewind();
            let block = self.parse_block()?;
            return Ok(StatementOrTail::Statement(Statement::Block(Box::new(
                block,
            ))));
        }

        self.parse_expression_statement(allow_tail)
    }

    fn parse_expression_statement(
        &mut self,
        allow_tail: bool,
    ) -> Result<StatementOrTail, SyntaxError> {
        let expr = self.parse_expression()?;
        if self.matches(TokenKind::Semi) {
            let span = expr_span(&expr);
            Ok(StatementOrTail::Statement(Statement::Expr(ExprStmt {
                expr,
                span,
            })))
        } else if allow_tail {
            Ok(StatementOrTail::Tail(expr))
        } else {
            Err(self.error_here("Expected ';' after expression"))
        }
    }

    fn parse_let(&mut self, start: usize) -> Result<LetStmt, SyntaxError> {
        let mutability = if self.matches(TokenKind::Mut) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };

        let first = self.expect_identifier("Expected binding name or type")?;
        let mut ty = None;
        let name;

        if let Some(TokenKind::Identifier(_)) = self.peek_kind() {
            let inferred_type = TypeAnnotation {
                ty: TypeExpr::named(first.name.clone()),
                span: first.span,
            };
            ty = Some(inferred_type);
            let second = self.expect_identifier("Expected binding name")?;
            name = second;
        } else if self.matches(TokenKind::Colon) {
            let annotation = self.parse_type_annotation()?;
            ty = Some(annotation);
            name = first;
        } else {
            name = first;
        }

        let value = if self.matches(TokenKind::Eq) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = value
            .as_ref()
            .map(|expr| expr_span(expr).end)
            .unwrap_or(name.span.end);

        Ok(LetStmt {
            name: name.name,
            ty,
            value,
            mutability,
            span: Span::new(start, end),
        })
    }

    fn parse_return(&mut self) -> Result<ReturnStmt, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let mut values = Vec::new();
        if self.matches(TokenKind::Semi) {
            return Ok(ReturnStmt {
                values,
                span: Span::new(start, self.last_span_end(start)),
            });
        }
        loop {
            values.push(self.parse_expression()?);
            if self.matches(TokenKind::Comma) {
                continue;
            }
            break;
        }
        let end = self.expect(TokenKind::Semi)?.span.end;
        Ok(ReturnStmt {
            values,
            span: Span::new(start, end),
        })
    }

    fn parse_while(&mut self) -> Result<WhileStmt, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        let body_end = body.span.end;
        Ok(WhileStmt {
            condition,
            body,
            span: Span::new(start, body_end),
        })
    }

    fn parse_for_range(&mut self) -> Result<ForRangeStmt, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let binding = self.expect_identifier("Expected loop binding")?;
        self.expect(TokenKind::In)?;
        let range_expr = self.parse_expression()?;
        let range = if let Expr::Range(expr) = range_expr {
            expr
        } else {
            return Err(self.error_here("Expected range expression (start..end)"));
        };
        let body = self.parse_block()?;
        let body_end = body.span.end;
        Ok(ForRangeStmt {
            binding: binding.name,
            range,
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
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.matches(TokenKind::LParen) {
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
                let end = self.expect(TokenKind::RParen)?.span.end;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                    span: Span::new(span_start, end),
                };
                continue;
            }
            if self.matches(TokenKind::Dot) {
                let field = self.expect_identifier("Expected field name after '.'")?;
                let span = expr_span(&expr).union(field.span);
                expr = Expr::FieldAccess {
                    base: Box::new(expr),
                    field: field.name,
                    span,
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
        if !self.suppress_block_literal && self.matches(TokenKind::LBrace) {
            self.rewind();
            let block = self.parse_block()?;
            return Ok(Expr::Block(Box::new(block)));
        }

        match self.peek_kind() {
            Some(TokenKind::Identifier(_)) => self.parse_identifier_expression(),
            Some(TokenKind::Integer(_)) => self.parse_int_literal(),
            Some(TokenKind::Float(_)) => self.parse_float_literal(),
            Some(TokenKind::String(_)) => self.parse_string_literal(),
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
                let end = self.expect(TokenKind::RParen)?.span.end;
                if values.len() == 1 {
                    Ok(values.into_iter().next().unwrap())
                } else {
                    Ok(Expr::Tuple(values, Span::new(start, end)))
                }
            }
            _ => Err(self.error_here("Unexpected token in expression")),
        }
    }

    fn parse_identifier_expression(&mut self) -> Result<Expr, SyntaxError> {
        let ident = self.expect_identifier("Expected identifier")?;
        if self.matches(TokenKind::LBrace) {
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
                        let span = expr_span(&value);
                        named_fields.push(StructLiteralField {
                            name: field.name,
                            value,
                            span,
                        });
                    } else {
                        is_named.get_or_insert(false);
                        positional.push(self.parse_expression()?);
                    }
                    if self.matches(TokenKind::Comma) {
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
        }
        Ok(Expr::Identifier(ident))
    }

    fn parse_match_expression(&mut self) -> Result<Expr, SyntaxError> {
        let start = self
            .previous_span()
            .map(|s| s.start)
            .unwrap_or_else(|| self.current_span_start());
        let prev_flag = self.suppress_block_literal;
        self.suppress_block_literal = true;
        let expr = self.parse_expression()?;
        self.suppress_block_literal = prev_flag;
        self.expect(TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_eof() {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::FatArrow)?;
            let value = self.parse_expression()?;
            let span = expr_span(&value);
            arms.push(MatchArmExpr {
                pattern,
                guard: None,
                value,
                span,
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
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.matches(TokenKind::Else) {
            Some(self.parse_block()?)
        } else {
            None
        };
        let span = Span::new(
            start,
            else_branch
                .as_ref()
                .map(|b| b.span.end)
                .unwrap_or(then_branch.span.end),
        );
        Ok(Expr::If(Box::new(IfExpr {
            condition,
            then_branch,
            else_branch,
            span,
        })))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, SyntaxError> {
        if self.matches(TokenKind::Identifier("_".into())) {
            let span = self.previous_span().unwrap();
            return Ok(Pattern::Wildcard(span));
        }
        if let Some(TokenKind::Identifier(_)) = self.peek_kind() {
            let ident = self.expect_identifier("Expected pattern identifier")?;
            if self.matches(TokenKind::LParen) {
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
                let end = self.expect(TokenKind::RParen)?.span.end;
                return Ok(Pattern::EnumVariant {
                    enum_name: None,
                    variant: ident.name,
                    bindings,
                    span: Span::new(ident.span.start, end),
                });
            }
            return Ok(Pattern::Identifier(ident.name, ident.span));
        }
        if let Some(TokenKind::Integer(_)) = self.peek_kind() {
            let lit = self.parse_int_literal()?;
            return Ok(Pattern::Literal(match lit {
                Expr::Literal(lit) => lit,
                _ => unreachable!(),
            }));
        }
        Err(self.error_here("Unsupported pattern"))
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

    fn rewind(&mut self) {
        self.pos = self.pos.saturating_sub(1);
    }
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(Literal::Int(_, span))
        | Expr::Literal(Literal::Float(_, span))
        | Expr::Literal(Literal::Bool(_, span))
        | Expr::Literal(Literal::String(_, span))
        | Expr::Literal(Literal::Rune(_, span)) => *span,
        Expr::Binary { span, .. } => *span,
        Expr::Unary { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::EnumLiteral { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(expr) => expr.span,
        Expr::Match(expr) => expr.span,
        Expr::Tuple(_, span) => *span,
        Expr::ArrayLiteral(_, span) => *span,
        Expr::Range(range) => range.span,
        Expr::Reference { span, .. } => *span,
        Expr::Deref { span, .. } => *span,
    }
}
