use crate::gc::{Traceable, Tracer};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

pub struct Ref<T: ?Sized> {
    ptr: *mut T,
}

impl<T> Ref<T> {
    pub fn from_leaked_static(obj: T) -> Self {
        let ptr = Box::leak(Box::new(obj));
        Ref { ptr }
    }
}

impl<T: ?Sized> Ref<T> {
    pub(crate) fn new(ptr: *mut T) -> Self {
        Ref { ptr }
    }

    pub fn as_ptr(&self) -> *mut T {
        self.ptr
    }
}

impl<T: ?Sized> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        // This is safe if `Storage::collect_garbage` was never called.
        // It is still safe if the object was accessible through the root when collecting garbage.
        // Thus, we can say dereferencing this pointer is safe, but collecting garbage is unsafe.
        //trace!("Deref {:p}", self.ptr);
        unsafe { &*self.ptr }
    }
}

impl<T: ?Sized> DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut T {
        // This is actually really unsafe because it allows mutating shared references.
        //trace!("Deref(mut) {:p}", self.ptr);
        unsafe { &mut *self.ptr }
    }
}

impl<T: ?Sized> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref { ptr: self.ptr }
    }
}

impl<T: ?Sized + Traceable> Traceable for Ref<T> {
    fn trace(&self, gc: &mut Tracer) {
        self.ptr.trace(gc)
    }
}

impl<T: ?Sized> Eq for Ref<T> {}

impl<T: ?Sized> PartialEq for Ref<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.ptr == rhs.ptr
    }
}

impl<T: ?Sized> std::fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:?}", std::any::type_name::<T>(), self.ptr)
    }
}

impl<T: ?Sized> Hash for Ref<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state)
    }
}
