use crate::parser::{BinaryOp, Expr, Program, Statement};

pub fn format_program(program: &Program) -> String {
    let mut output = String::new();
    output.push_str("fn main() {\n");
    for statement in &program.statements {
        output.push_str("  ");
        output.push_str(&format_statement(statement));
        output.push('\n');
    }
    output.push_str("}\n");
    output
}

fn format_statement(statement: &Statement) -> String {
    match statement {
        Statement::Let { name, value } => {
            format!("let int {} = {};", name, format_expr(value))
        }
        Statement::Output(expr) => format!("out({});", format_expr(expr)),
    }
}

fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Integer(value) => value.to_string(),
        Expr::Identifier(name) => name.clone(),
        Expr::Binary { left, op, right } => {
            format!(
                "({} {} {})",
                format_expr(left),
                op_symbol(op),
                format_expr(right)
            )
        }
    }
}

fn op_symbol(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
    }
}
