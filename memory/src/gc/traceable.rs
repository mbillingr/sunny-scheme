use super::{GarbageCollector, Ref};

pub trait Traceable {
    fn trace(&self, gc: &mut GarbageCollector);
}

impl<T: ?Sized + Traceable> Traceable for *const T {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.trace_pointer(*self)
    }
}

impl<T: ?Sized + Traceable> Traceable for *mut T {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.trace_pointer(*self)
    }
}

impl<T: ?Sized + Traceable> Traceable for &T {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.trace_pointer(*self)
    }
}

macro_rules! impl_primitive_tracable {
    ($($t:ty,)*) => {
        $(impl Traceable for $t {
            fn trace(&self, _: &mut GarbageCollector) {}
        })*
    }
}

impl_primitive_tracable!(
    (),
    bool,
    i8,
    i16,
    i32,
    i64,
    i128,
    u8,
    u16,
    u32,
    u64,
    u128,
    &str,
    //str,
    String,
);

impl<T: Traceable> Traceable for Option<T> {
    fn trace(&self, gc: &mut GarbageCollector) {
        if let Some(inner) = self {
            inner.trace(gc)
        }
    }
}

impl<T: Traceable, U: ?Sized + Traceable> Traceable for (T, U) {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.0.trace(gc);
        self.1.trace(gc);
    }
}

impl<T: Traceable> Traceable for Vec<T> {
    fn trace(&self, gc: &mut GarbageCollector) {
        for item in self {
            item.trace(gc);
        }
    }
}

impl<T: Traceable> Traceable for Box<[T]> {
    fn trace(&self, gc: &mut GarbageCollector) {
        for item in self.iter() {
            item.trace(gc);
        }
    }
}

impl<T: Traceable> Traceable for Ref<[T]> {
    fn trace(&self, gc: &mut GarbageCollector) {
        for item in self.iter() {
            item.trace(gc);
        }
    }
}

impl<T: Traceable> Traceable for &[T] {
    fn trace(&self, gc: &mut GarbageCollector) {
        for item in *self {
            item.trace(gc);
        }
    }
}