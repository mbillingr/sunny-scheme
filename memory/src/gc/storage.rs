use crate::gc::{GarbageCollector, Ref, Traceable};
use log::{debug, warn};
use std::any::Any;

/// Managed object storage with garbage collection.
///
/// When the storage is dropped all remaining objects are leaked.
/// To avoid leakage run garbage collection before dropping the storage
/// and make sure no references to any objects in the storage remain.
pub struct Storage {
    objects: &'static mut Vec<Box<dyn Any>>,
}

impl Drop for Storage {
    fn drop(&mut self) {
        if !self.objects.is_empty() {
            warn!(
                "Leaked {} objects from dropped storage. (Hint: did you run garbage collection?)",
                self.objects.len()
            )
        }
    }
}

impl Storage {
    pub fn new(_capacity: usize) -> Self {
        Storage {
            objects: Box::leak(Box::new(Vec::with_capacity(_capacity))),
        }
    }

    pub fn free(&self) -> usize {
        self.objects.capacity() - self.objects.len()
    }
    pub fn used(&self) -> usize {
        self.objects.len()
    }
    fn is_full(&self) -> bool {
        self.objects.len() >= self.objects.capacity()
    }

    pub fn insert<T: 'static>(&mut self, obj: T) -> Result<Ref<T>, T> {
        let boxed = Box::new(obj);
        self.insert_box(boxed).map_err(|x| *x)
    }

    pub fn insert_box<T: 'static>(&mut self, mut obj: Box<T>) -> Result<Ref<T>, Box<T>> {
        if self.is_full() {
            return Err(obj);
        }

        let ptr = (&mut *obj) as *mut T;
        self.objects.push(obj);
        Ok(Ref::new(ptr))
    }

    pub fn find_object<T: 'static>(&mut self, predicate: impl Fn(&T) -> bool) -> Option<Ref<T>> {
        self.objects
            .iter_mut()
            .map(|obj| &mut **obj)
            .filter_map(Any::downcast_mut::<T>)
            .filter(|obj| predicate(obj))
            .next()
            .map(|x| Ref::new(x))
    }

    pub fn is_valid<T>(&self, obj: &Ref<T>) -> bool {
        self.objects
            .iter()
            .map(Box::as_ref)
            .map(|o| o as *const dyn Any)
            .map(|o| o as *const T)
            .any(|ptr| ptr == obj.as_ptr())
    }

    /// Make sure the storage is at most half full
    pub fn grow(&mut self) {
        let n_add = if self.objects.capacity() == 0 {
            1
        } else {
            self.objects.len()
        };
        self.objects.reserve(n_add);
        debug!("Growing storage to {}", self.objects.capacity());
    }

    pub unsafe fn collect_garbage(&mut self, root: &impl Traceable) {
        let gc = self.begin_garbage_collection().mark(root);
        self.finish_garbage_collection(gc);
    }

    pub fn begin_garbage_collection(&mut self) -> GarbageCollector {
        GarbageCollector::new()
    }

    pub unsafe fn finish_garbage_collection(&mut self, gc: GarbageCollector) {
        gc.sweep(&mut self.objects);
        self.grow();
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

    #[test]
    fn growing_zero_storage_results_in_empty_storage() {
        let mut storage = Storage::new(0);
        storage.grow();
        assert!(storage.free() > 0)
    }

    #[test]
    fn growing_empty_storage_does_nothing() {
        let mut storage = Storage::new(5);
        let free_before = storage.free();

        storage.grow();

        assert_eq!(storage.free(), free_before)
    }

    #[test]
    fn growing_full_storage_at_least_doubles_capacity() {
        let mut storage = Storage::new(8);
        for i in 0..storage.free() {
            storage.insert(i).unwrap();
        }

        storage.grow();

        assert!(storage.objects.capacity() >= 8 * 2);
    }

    #[test]
    fn growing_half_full_storage_does_nothing() {
        let mut storage = Storage::new(8);
        storage.insert("a").unwrap();
        storage.insert("b").unwrap();
        storage.insert("c").unwrap();
        storage.insert("d").unwrap();

        let free_before = storage.free();

        storage.grow();

        assert_eq!(storage.free(), free_before)
    }

    #[test]
    fn growing_more_than_half_full_storage_allocates_more_memory() {
        let mut storage = Storage::new(7);
        let cap_before = storage.objects.capacity();
        assert_eq!(cap_before, 7);

        storage.insert("a").unwrap();
        storage.insert("b").unwrap();
        storage.insert("c").unwrap();
        storage.insert("d").unwrap();

        storage.grow();

        assert!(storage.objects.capacity() >= 4 * 2);
    }
}
