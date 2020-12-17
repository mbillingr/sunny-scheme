use crate::mem::{Traceable, Tracer};
use crate::Value;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

pub struct Table(HashMap<Value, Value>);

impl Table {
    pub fn new() -> Self {
        Table(HashMap::new())
    }
}

impl Traceable for Table {
    fn trace(&self, gc: &mut Tracer) {
        for (k, v) in &self.0 {
            k.trace(gc);
            v.trace(gc);
        }
    }
}

impl Deref for Table {
    type Target = HashMap<Value, Value>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Table {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
