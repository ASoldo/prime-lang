use crate::language::ast::Expr;
use crate::runtime::{
    error::RuntimeError,
    value::{FormatRuntimeSegment, Value},
};
use std::collections::{HashMap, HashSet};
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
    active_mut_borrows: HashSet<String>,
    borrow_frames: Vec<Vec<String>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            active_mut_borrows: HashSet::new(),
            borrow_frames: vec![Vec::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.borrow_frames.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        if let Some(frame) = self.borrow_frames.pop() {
            for name in frame {
                self.active_mut_borrows.remove(&name);
            }
        }
        if self.scopes.is_empty() {
            self.scopes.push(Scope::new());
        }
        if self.borrow_frames.is_empty() {
            self.borrow_frames.push(Vec::new());
        }
    }

    pub fn declare(&mut self, name: &str, value: Value, mutable: bool) -> Result<(), RuntimeError> {
        let scope_index = self.scopes.len().saturating_sub(1);
        let cell = Arc::new(Mutex::new(value));
        {
            let stored = cell.lock().unwrap();
            self.track_reference_borrow_in_scope(&stored, scope_index)?;
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
                self.track_reference_borrow_in_scope(&value, index)?;
                {
                    let current = cell.lock().unwrap();
                    self.release_reference_borrow(&current);
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

    fn begin_mut_borrow_in_scope(
        &mut self,
        name: &str,
        scope_index: usize,
    ) -> Result<(), RuntimeError> {
        if self.active_mut_borrows.contains(name) {
            return Err(RuntimeError::Panic {
                message: format!("`{name}` is already mutably borrowed"),
            });
        }
        self.active_mut_borrows.insert(name.to_string());
        if let Some(frame) = self.borrow_frames.get_mut(scope_index) {
            frame.push(name.to_string());
        }
        Ok(())
    }

    pub fn end_mut_borrow(&mut self, name: &str) {
        self.active_mut_borrows.remove(name);
    }

    pub fn is_mut_borrowed(&self, name: &str) -> bool {
        self.active_mut_borrows.contains(name)
    }

    pub fn register_move(&mut self, name: &str) {
        self.end_mut_borrow(name);
    }

    fn track_reference_borrow_in_scope(
        &mut self,
        value: &Value,
        scope_index: usize,
    ) -> Result<(), RuntimeError> {
        match value {
            Value::Reference(reference) => {
                if reference.mutable {
                    if let Some(origin) = &reference.origin {
                        self.begin_mut_borrow_in_scope(origin, scope_index)?;
                    }
                }
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.track_reference_borrow_in_scope(named, scope_index)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn release_reference_borrow(&mut self, value: &Value) {
        match value {
            Value::Reference(reference) => {
                if reference.mutable {
                    if let Some(origin) = &reference.origin {
                        self.end_mut_borrow(origin);
                    }
                }
            }
            Value::FormatTemplate(template) => {
                for segment in &template.segments {
                    if let FormatRuntimeSegment::Named(named) = segment {
                        self.release_reference_borrow(named);
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
