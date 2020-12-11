use super::GarbageCollector;

pub trait Traceable {
    fn trace(&self, gc: &mut GarbageCollector);
}

macro_rules! impl_atomic_tracable {
    ($($t:ty,)*) => {
        $(impl Traceable for $t {
            fn trace(&self, _: &mut GarbageCollector) {}
        })*
    }
}

impl_atomic_tracable!((), &str,);
