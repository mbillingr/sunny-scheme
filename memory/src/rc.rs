use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub struct Ref<T>(Rc<T>);

impl<T> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.0
    }
}

pub struct Storage {}

impl Storage {
    pub fn new(_capacity: usize) -> Self {
        Storage {}
    }

    pub fn insert<T>(&mut self, obj: T) -> Option<Ref<T>> {
        Some(Ref(Rc::new(obj)))
    }

    pub fn is_valid<T>(&self, _: &Ref<T>) -> bool {
        true
    }

    pub fn collect_garbage<T>(&mut self, _root: T) {}
}
