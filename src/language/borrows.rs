use std::collections::HashSet;

/// Each scope owns its registrations. When a parameter receives a reference,
/// its caller keeps the original loan until the caller's binding goes away.
#[derive(Clone, Default)]
pub(crate) struct MutableBorrows {
    frames: Vec<HashSet<usize>>,
}

impl MutableBorrows {
    pub fn push_scope(&mut self) {
        self.frames.push(HashSet::new());
    }

    pub fn pop_scope(&mut self) {
        self.frames.pop();
    }

    pub fn contains<T>(&self, cell: *const T) -> bool {
        self.frames.iter().any(|frame| frame.contains(&(cell as usize)))
    }

    pub fn acquire<T>(&mut self, scope: usize, cell: *const T, name: &str, parameter: bool) -> Result<(), String> {
        if (!parameter && self.contains(cell)) || !self.frames[scope].insert(cell as usize) {
            return Err(format!("`{name}` is already mutably borrowed"));
        }
        Ok(())
    }

    pub fn release<T>(&mut self, scope: usize, cell: *const T) {
        self.frames[scope].remove(&(cell as usize));
    }
}
