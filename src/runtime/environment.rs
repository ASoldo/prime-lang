use crate::language::ast::Expr;
use crate::runtime::{error::RuntimeError, value::Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
struct Binding {
    cell: Rc<RefCell<Value>>,
    mutable: bool,
}

#[derive(Clone)]
struct Scope {
    bindings: HashMap<String, Binding>,
    deferred: Vec<Expr>,
}

impl Scope {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            deferred: Vec::new(),
        }
    }
}

pub struct Environment {
    scopes: Vec<Scope>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        if self.scopes.is_empty() {
            self.scopes.push(Scope::new());
        }
    }

    pub fn declare(&mut self, name: &str, value: Value, mutable: bool) -> Result<(), RuntimeError> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.bindings.insert(
                name.to_string(),
                Binding {
                    cell: Rc::new(RefCell::new(value)),
                    mutable,
                },
            );
            Ok(())
        } else {
            Err(RuntimeError::Panic {
                message: format!("No scope available for binding `{}`", name),
            })
        }
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.bindings.get_mut(name) {
                if !binding.mutable {
                    return Err(RuntimeError::ImmutableBinding {
                        name: name.to_string(),
                    });
                }
                *binding.cell.borrow_mut() = value;
                return Ok(());
            }
        }
        Err(RuntimeError::UnknownSymbol {
            name: name.to_string(),
        })
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(binding.cell.borrow().clone());
            }
        }
        None
    }

    pub fn get_cell(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(binding.cell.clone());
            }
        }
        None
    }

    pub fn defer(&mut self, expr: Expr) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.deferred.push(expr);
        }
    }

    pub fn drain_deferred(&mut self) -> Vec<Expr> {
        if let Some(scope) = self.scopes.last_mut() {
            let mut drained = Vec::new();
            std::mem::swap(&mut drained, &mut scope.deferred);
            drained
        } else {
            Vec::new()
        }
    }
}
