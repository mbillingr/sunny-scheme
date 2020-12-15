use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Ref<T: ?Sized>(Rc<T>);

impl<T> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.0
    }
}

impl<T: ?Sized> PartialEq for Ref<T> {
    fn eq(&self, rhs: &Self) -> bool {
        Rc::ptr_eq(&self.0, &rhs.0)
    }
}

pub struct Storage {}

impl Storage {
    pub fn new(_capacity: usize) -> Self {
        Storage {}
    }

    pub fn insert<T>(&mut self, obj: T) -> Result<Ref<T>, T> {
        Ok(Ref(Rc::new(obj)))
    }

    pub fn is_valid<T>(&self, _: &Ref<T>) -> bool {
        true
    }

    pub fn grow(&mut self) {}

    pub fn collect_garbage<T>(&mut self, _root: T) {}

    pub fn begin_garbage_collection(&mut self) -> GarbageCollector {
        GarbageCollector
    }
}

pub trait Traceable {
    fn trace(&self, _: &mut GarbageCollector) {}
}

impl<T: ?Sized> Traceable for Ref<T> {}

impl<T> Traceable for Option<T> {}

impl<T, U: ?Sized> Traceable for (T, U) {}

impl<T> Traceable for Vec<T> {}

impl<T: ?Sized> Traceable for Box<T> {}

impl<T> Traceable for &[T] {}

pub struct GarbageCollector;

impl GarbageCollector {
    pub fn mark(self, _: &impl Traceable) -> Self {
        self
    }

    pub fn sweep(self) {}
}
