use crate::{Sexpr, SourceLocation};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct SourceMap {
    mapping: RefCell<HashMap<Sexpr, SourceLocation<()>>>,
}

impl Default for SourceMap {
    fn default() -> Self {
        SourceMap::new()
    }
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            mapping: RefCell::new(HashMap::new()),
        }
    }

    pub fn insert(&self, sexpr: Sexpr, location: SourceLocation<()>) {
        self.mapping.borrow_mut().insert(sexpr, location);
    }

    pub fn get(&self, sexpr: &Sexpr) -> SourceLocation<()> {
        self.mapping
            .borrow()
            .get(sexpr)
            .cloned()
            .unwrap_or_else(SourceLocation::default)
    }
}
