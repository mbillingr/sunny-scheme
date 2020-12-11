use std::any::Any;
use std::collections::HashSet;
use std::ops::Deref;

pub trait Traceable {
    fn trace(&self, gc: &mut GarbageCollector);
}

impl Traceable for () {
    fn trace(&self, _: &mut GarbageCollector) {}
}

impl Traceable for &str {
    fn trace(&self, _: &mut GarbageCollector) {}
}

#[derive(Clone)]
pub struct Ref<T> {
    ptr: *mut T,
}

impl<T> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        // This is safe if `Storage::collect_garbage` was never called.
        // It is still safe if the object was accessible through the root when collecting garbage.
        // Thus, we can say dereferencing this pointer is safe, but collecting garbage is unsafe.
        unsafe { &*self.ptr }
    }
}

impl<T: Traceable> Traceable for Ref<T> {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.trace_pointer(self.ptr)
    }
}

impl<T> Eq for Ref<T> {}

impl<T> PartialEq for Ref<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.ptr == rhs.ptr
    }
}

impl<T> std::fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:?}", std::any::type_name::<T>(), self.ptr)
    }
}

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
        Ok(Ref { ptr })
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
            .any(|ptr| ptr == obj.ptr)
    }

    pub unsafe fn collect_garbage(&mut self, root: &impl Traceable) {
        let mut gc = GarbageCollector::new(&mut self.objects);
        gc.mark(root);
        gc.sweep();
    }
}

pub struct GarbageCollector<'a> {
    reachable: HashSet<usize>,
    objects: &'a mut Vec<Box<dyn Any>>,
}

impl<'a> GarbageCollector<'a> {
    fn new(objects: &'a mut Vec<Box<dyn Any>>) -> Self {
        GarbageCollector {
            reachable: HashSet::new(),
            objects,
        }
    }

    fn mark(&mut self, root: &impl Traceable) {
        root.trace(self)
    }

    fn trace_pointer<T: Traceable>(&mut self, ptr: *const T) {
        let addr = ptr as *const u8 as usize;
        let unmarked = self.reachable.insert(addr);
        if unmarked {
            let obj = unsafe { &*ptr };
            obj.trace(self);
        }
    }

    fn sweep(self) {
        let objects = self.objects;
        let reachable = self.reachable;

        let new_objects = objects
            .drain(..)
            .filter(|obj| {
                let addr = &**obj as *const _ as *const u8 as usize;
                reachable.contains(&addr)
            })
            .collect();

        *objects = new_objects;
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
        let obj = Ref {
            ptr: 0x12345678 as *mut i8,
        };
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
