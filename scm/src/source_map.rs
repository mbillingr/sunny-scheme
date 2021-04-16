use crate::scm::HashPtrEq;
use crate::{Scm, SourceLocation};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct SourceMap {
    mapping: RefCell<HashMap<HashPtrEq, SourceLocation<()>>>,
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

    pub fn insert(&self, scm: Scm, location: SourceLocation<()>) {
        self.mapping.borrow_mut().insert(scm.into(), location);
    }

    pub fn get(&self, scm: &Scm) -> SourceLocation<()> {
        self.mapping
            .borrow()
            .get(HashPtrEq::from_ref(scm))
            .cloned()
            .unwrap_or_else(SourceLocation::default)
    }
}
