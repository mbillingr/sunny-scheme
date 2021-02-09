use std::any::Any;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

pub struct Ref<T: ?Sized>(Rc<T>);

impl<T> Ref<T> {
    pub fn from_leaked_static(obj: T) -> Self {
        Ref(Rc::new(obj))
    }
}

impl<T: ?Sized> Ref<T> {
    pub fn as_ptr(&self) -> *const T {
        Rc::as_ptr(&self.0)
    }
}

impl<T: ?Sized> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.0
    }
}

impl<T: ?Sized> DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe {
            let ptr = &*self.0 as *const T;
            let mutptr = ptr as *mut T;
            &mut *mutptr
        }
    }
}

impl<T: ?Sized> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref(self.0.clone())
    }
}

impl<T: ?Sized> Eq for Ref<T> {}

impl<T: ?Sized> PartialEq for Ref<T> {
    fn eq(&self, rhs: &Self) -> bool {
        Rc::ptr_eq(&self.0, &rhs.0)
    }
}

impl<T: ?Sized> std::fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:?}", std::any::type_name::<T>(), self.as_ptr())
    }
}

pub struct Storage {
    interned: Vec<Weak<dyn Any>>,
}

impl Storage {
    pub fn new(_capacity: usize) -> Self {
        Storage { interned: vec![] }
    }

    pub fn free(&self) -> usize {
        usize::max_value()
    }

    pub fn insert_interned<T: 'static>(&mut self, obj: T) -> Result<Ref<T>, T> {
        let rc = Rc::new(obj);
        let wc = Rc::downgrade(&rc);
        self.interned.push(wc);
        Ok(Ref(rc))
    }

    pub fn insert<T>(&mut self, obj: T) -> Result<Ref<T>, T> {
        Ok(Ref(Rc::new(obj)))
    }

    pub fn find_interned<T: 'static>(&mut self, predicate: impl Fn(&T) -> bool) -> Option<Ref<T>> {
        let mut i = 0;
        while i < self.interned.len() {
            if let Some(any) = self.interned[i].upgrade() {
                if let Ok(concrete) = any.downcast::<T>() {
                    if predicate(&concrete) {
                        return Some(Ref(concrete));
                    }
                }
                i += 1;
            } else {
                self.interned.swap_remove(i);
            }
        }
        None
    }

    pub fn is_valid<T>(&self, _: &Ref<T>) -> bool {
        true
    }

    pub fn grow(&mut self) {}

    pub fn auto_grow(&mut self) {}

    pub fn ensure(&mut self, _: usize) {}

    pub unsafe fn collect_garbage<T>(&mut self, _root: T) {}

    pub fn begin_garbage_collection(&mut self) -> Tracer {
        Tracer
    }

    pub unsafe fn finish_garbage_collection(&mut self, _: Tracer) {}
}

pub trait Traceable {
    fn trace(&self, _: &mut Tracer) {}
}

impl<T: ?Sized> Traceable for Ref<T> {}

impl<T> Traceable for Option<T> {}

impl<T, U: ?Sized> Traceable for (T, U) {}

impl<T> Traceable for Vec<T> {}

impl<T: ?Sized> Traceable for Box<T> {}

impl<T> Traceable for &[T] {}

pub struct Tracer;

impl Tracer {
    pub fn mark(self, _: &impl Traceable) -> Self {
        self
    }
}
