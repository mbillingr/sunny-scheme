use crate::gc::{GarbageCollector, Ref, Traceable};
use std::any::Any;

pub struct Storage {
    objects: Vec<Box<dyn Any>>,
}

impl Storage {
    pub fn new(_capacity: usize) -> Self {
        Storage {
            objects: Vec::with_capacity(_capacity),
        }
    }

    pub fn insert<T: 'static>(&mut self, obj: T) -> Result<Ref<T>, T> {
        if self.is_full() {
            return Err(obj);
        }

        let mut boxed = Box::new(obj);
        let ptr = (&mut *boxed) as *mut T;
        self.objects.push(boxed);
        Ok(Ref::new(ptr))
    }

    fn is_full(&self) -> bool {
        self.objects.len() >= self.objects.capacity()
    }

    pub fn is_valid<T>(&self, obj: &Ref<T>) -> bool {
        self.objects
            .iter()
            .map(Box::as_ref)
            .map(|o| o as *const dyn Any)
            .map(|o| o as *const T)
            .any(|ptr| ptr == obj.as_ptr())
    }

    pub unsafe fn collect_garbage(&mut self, root: &impl Traceable) {
        let mut gc = GarbageCollector::new(&mut self.objects);
        gc.mark(root);
        gc.sweep();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cant_insert_into_full_storage() {
        let mut storage = Storage::new(0);
        assert_eq!(storage.insert(123), Err(123));
    }

    #[test]
    fn a_made_up_ref_is_not_valid() {
        let storage = Storage::new(0);
        let obj = Ref::new(0x12345678 as *mut i8);
        assert!(!storage.is_valid(&obj));
    }

    #[test]
    fn gc_without_root_clears_storage() {
        let mut storage = Storage::new(1);
        let obj = storage.insert(0).unwrap();
        unsafe { storage.collect_garbage(&()) }
        assert_eq!(storage.objects.len(), 0);
        assert!(!storage.is_valid(&obj));
    }

    #[test]
    fn gc_preserves_root() {
        let mut storage = Storage::new(1);
        let obj = storage.insert("foo").unwrap();
        unsafe { storage.collect_garbage(&obj) }
        assert_eq!(storage.objects.len(), 1);
        assert!(storage.is_valid(&obj));
    }

    #[test]
    fn gc_preserves_object_chain() {
        let mut storage = Storage::new(4);
        let a = storage.insert("foo").unwrap();
        let b = storage.insert(a.clone()).unwrap();
        let c = storage.insert(b.clone()).unwrap();
        let d = storage.insert(c.clone()).unwrap();
        unsafe { storage.collect_garbage(&c) }

        assert_eq!(storage.objects.len(), 3);

        assert!(storage.is_valid(&a));
        assert!(storage.is_valid(&b));
        assert!(storage.is_valid(&c));
        assert!(!storage.is_valid(&d));

        assert_eq!(***c, "foo");
    }
}
