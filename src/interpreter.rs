use crate::parser::{BinaryOp, Expr, Program, Statement};
use std::collections::HashMap;

pub fn interpret(program: &Program) {
    let mut variables: HashMap<String, i32> = HashMap::new();

    for statement in &program.statements {
        match statement {
            Statement::Let { name, value } => {
                let evaluated = eval_expression(value, &variables);
                variables.insert(name.clone(), evaluated);
            }
            Statement::Output(expr) => {
                let evaluated = eval_expression(expr, &variables);
                println!("{}", evaluated);
            }
        }
    }
}

fn eval_expression(expr: &Expr, variables: &HashMap<String, i32>) -> i32 {
    match expr {
        Expr::Integer(value) => *value,
        Expr::Identifier(name) => variables
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("Undefined variable {}", name)),
        Expr::Binary { left, op, right } => {
            let lhs = eval_expression(left, variables);
            let rhs = eval_expression(right, variables);
            match op {
                BinaryOp::Add => lhs + rhs,
                BinaryOp::Sub => lhs - rhs,
                BinaryOp::Mul => lhs * rhs,
                BinaryOp::Div => {
                    if rhs == 0 {
                        panic!("Division by zero");
                    }
                    lhs / rhs
                }
            }
        }
    }
}
