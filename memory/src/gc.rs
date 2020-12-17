mod reference;
mod storage;
mod traceable;
mod tracer;

pub use reference::Ref;
pub use storage::Storage;
pub use traceable::Traceable;
pub use tracer::Tracer;

trait GcMarker {
    fn is_reachable<T: ?Sized>(&self, ptr: *const T) -> bool;
}
