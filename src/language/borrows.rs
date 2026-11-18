use std::{ops::Deref, ptr};

/// A forwarded parameter holds its own registration for the duration of the
/// call. Popping it leaves the caller's registration intact.
#[derive(Clone)]
pub(crate) struct MutableBorrows<Cell> {
    frames: Vec<Vec<Cell>>,
}

impl<Cell> Default for MutableBorrows<Cell> {
    fn default() -> Self {
        Self { frames: Vec::new() }
    }
}

impl<Cell: Clone + Deref> MutableBorrows<Cell> {
    pub fn push_scope(&mut self) {
        self.frames.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        self.frames.pop();
    }

    pub fn contains(&self, cell: &Cell) -> bool {
        self.frames
            .iter()
            .flatten()
            .any(|other| ptr::eq(&**other, &**cell))
    }

    pub fn acquire(
        &mut self,
        scope: usize,
        cell: &Cell,
        name: &str,
        parameter: bool,
    ) -> Result<(), String> {
        // A fresh loan conflicts with any live loan. A function can receive a
        // caller's reference once, even if the two bindings happen to share a name.
        let frames = if parameter {
            &self.frames[scope..=scope]
        } else {
            &self.frames[..]
        };
        if frames
            .iter()
            .flatten()
            .any(|other| ptr::eq(&**other, &**cell))
        {
            return Err(format!("`{name}` is already mutably borrowed"));
        }
        self.frames[scope].push(cell.clone());
        Ok(())
    }

    pub fn release(&mut self, scope: usize, cell: &Cell) -> bool {
        let frame = &mut self.frames[scope];
        if let Some(index) = frame.iter().position(|other| ptr::eq(&**other, &**cell)) {
            frame.swap_remove(index);
            return true;
        }
        false
    }
}
