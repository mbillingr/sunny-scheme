use std::any::Any;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc;

pub struct Ref<T: ?Sized>(rc::Rc<T>);
pub struct Weak<T: ?Sized>(rc::Weak<T>);

impl<T> Ref<T> {
    pub fn from_leaked_static(obj: T) -> Self {
        Ref(rc::Rc::new(obj))
    }
}

impl<T: ?Sized> Ref<T> {
    pub fn as_ptr(&self) -> *const T {
        rc::Rc::as_ptr(&self.0)
    }

    pub fn downgrade(&self) -> Weak<T> {
        Weak(rc::Rc::downgrade(&self.0))
    }
}

impl<T: ?Sized> Weak<T> {
    pub fn upgrade(&self) -> Option<Ref<T>> {
        self.0.upgrade().map(Ref)
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

impl<T: ?Sized> Clone for Weak<T> {
    fn clone(&self) -> Self {
        Weak(self.0.clone())
    }
}

impl<T: ?Sized> Eq for Ref<T> {}

impl<T: ?Sized> PartialEq for Ref<T> {
    fn eq(&self, rhs: &Self) -> bool {
        rc::Rc::ptr_eq(&self.0, &rhs.0)
    }
}

impl<T: ?Sized> std::fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:?}", std::any::type_name::<T>(), self.as_ptr())
    }
}

impl<T: ?Sized> Hash for Ref<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        rc::Rc::as_ptr(&self.0).hash(state)
    }
}

pub struct Storage {
    interned: Vec<rc::Weak<dyn Any>>,
}

impl Storage {
    pub fn new(_capacity: usize) -> Self {
        Storage { interned: vec![] }
    }

    pub fn insert_interned<T: 'static>(&mut self, obj: T) -> Ref<T> {
        let rc = rc::Rc::new(obj);
        let wc = rc::Rc::downgrade(&rc);
        self.interned.push(wc);
        Ref(rc)
    }

    pub fn insert<T>(&mut self, obj: T) -> Ref<T> {
        Ref(rc::Rc::new(obj))
    }

    pub fn insert_box<T: 'static>(&mut self, obj: Box<T>) -> Ref<T> {
        self.insert(*obj)
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
}
