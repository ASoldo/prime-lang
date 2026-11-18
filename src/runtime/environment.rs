use crate::language::{ast::Expr, borrows::MutableBorrows};
use crate::runtime::{
    error::RuntimeError,
    value::{FormatRuntimeSegment, Value},
};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
struct Binding {
    cell: Arc<Mutex<Value>>,
    mutable: bool,
}

#[derive(Clone)]
pub struct DropRecord {
    pub binding: String,
    pub type_name: String,
}

#[derive(Clone)]
pub enum CleanupAction {
    Defer(Expr),
    Drop(DropRecord),
}

#[derive(Clone)]
struct Scope {
    bindings: HashMap<String, Binding>,
    cleanups: Vec<CleanupAction>,
}

impl Scope {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            cleanups: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct Environment {
    scopes: Vec<Scope>,
    borrows: MutableBorrows<Arc<Mutex<Value>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self {
            scopes: Vec::new(),
            borrows: MutableBorrows::default(),
        };
        env.push_scope();
        env
    }

    pub fn task_environment(&self, captures: &[String]) -> Self {
        let mut child = Environment::new();
        // Module constants are available to functions invoked by the task.
        child.scopes[0]
            .bindings
            .clone_from(&self.scopes[0].bindings);
        child.push_scope();
        for name in captures {
            if let Some((cell, mutable)) = self.get_cell(name) {
                child.scopes[1]
                    .bindings
                    .insert(name.clone(), Binding { cell, mutable });
            }
        }
        child
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.borrows.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        self.borrows.pop_scope();
        if self.scopes.is_empty() {
            self.push_scope();
        }
    }

    pub fn declare(&mut self, name: &str, value: Value, mutable: bool) -> Result<(), RuntimeError> {
        self.bind(name, value, mutable, false)
    }

    pub(super) fn bind(
        &mut self,
        name: &str,
        value: Value,
        mutable: bool,
        parameter: bool,
    ) -> Result<(), RuntimeError> {
        let scope_index = self.scopes.len().saturating_sub(1);
        let cell = Arc::new(Mutex::new(value));
        {
            let stored = cell.lock().unwrap();
            self.track_reference_borrow_in_scope(&stored, scope_index, parameter)?;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope
                .bindings
                .insert(name.to_string(), Binding { cell, mutable });
            Ok(())
        } else {
            Err(RuntimeError::Panic {
                message: format!("No scope available for binding `{}`", name),
            })
        }
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        for index in (0..self.scopes.len()).rev() {
            if let Some(binding) = self.scopes[index].bindings.get(name) {
                if !binding.mutable {
                    return Err(RuntimeError::ImmutableBinding {
                        name: name.to_string(),
                    });
                }
                let cell = binding.cell.clone();
                self.track_reference_borrow_in_scope(&value, index, false)?;
                {
                    let current = cell.lock().unwrap();
                    self.release_reference_borrow(&current, index);
                }
                *cell.lock().unwrap() = value;
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
                return Some(binding.cell.lock().unwrap().clone());
            }
        }
        None
    }

    pub fn get_cell(&self, name: &str) -> Option<(Arc<Mutex<Value>>, bool)> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some((binding.cell.clone(), binding.mutable));
            }
        }
        None
    }

    pub fn is_mut_borrowed(&self, name: &str) -> bool {
        self.get_cell(name)
            .is_some_and(|(cell, _)| self.borrows.contains(&cell))
    }

    pub fn end_mut_borrow(&mut self, name: &str) {
        if let Some((cell, _)) = self.get_cell(name) {
            for scope in (0..self.scopes.len()).rev() {
                if self.borrows.release(scope, &cell) {
                    break;
                }
            }
        }
    }

    pub fn register_move(&mut self, name: &str) {
        self.end_mut_borrow(name);
    }

    fn track_reference_borrow_in_scope(
        &mut self,
        value: &Value,
        scope_index: usize,
        parameter: bool,
    ) -> Result<(), RuntimeError> {
        match value {
            Value::Reference(reference) if reference.mutable => {
                if let Some(origin) = &reference.origin {
                    self.borrows
                        .acquire(scope_index, &reference.cell, origin, parameter)
                        .map_err(|message| RuntimeError::Panic { message })?;
                }
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.track_reference_borrow_in_scope(named, scope_index, false)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn release_reference_borrow(&mut self, value: &Value, scope: usize) {
        match value {
            Value::Reference(reference) if reference.mutable => {
                self.borrows.release(scope, &reference.cell);
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.release_reference_borrow(named, scope);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn defer(&mut self, expr: Expr) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.cleanups.push(CleanupAction::Defer(expr));
        }
    }

    pub fn schedule_drop(&mut self, record: DropRecord) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.cleanups.push(CleanupAction::Drop(record));
        }
    }

    pub fn drain_cleanups(&mut self) -> Vec<CleanupAction> {
        if let Some(scope) = self.scopes.last_mut() {
            let mut drained = Vec::new();
            std::mem::swap(&mut drained, &mut scope.cleanups);
            drained
        } else {
            Vec::new()
        }
    }
}
