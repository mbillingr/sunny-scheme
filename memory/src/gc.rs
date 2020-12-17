mod tracer;
mod reference;
mod storage;
mod traceable;

pub use tracer::Tracer;
pub use reference::Ref;
pub use storage::Storage;
pub use traceable::Traceable;

trait GcMarker {
    fn is_reachable<T: ?Sized>(&self, ptr: *const T) -> bool;
}
