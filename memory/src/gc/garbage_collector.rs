use super::{GcMarker, Traceable};
use log::{debug, info, trace};
use std::any::Any;
use std::collections::HashSet;

pub struct GarbageCollector {
    reachable: HashSet<*const ()>,
}

impl GcMarker for GarbageCollector {
    fn is_reachable<T: ?Sized>(&self, ptr: *const T) -> bool {
        self.reachable.contains(&normalize(ptr))
    }
}

impl GarbageCollector {
    pub(crate) fn new() -> Self {
        info!("Initializing garbage collecction");
        GarbageCollector {
            reachable: HashSet::new(),
        }
    }

    pub fn mark(mut self, root: &impl Traceable) -> Self {
        root.trace(&mut self);
        debug!("Mark phase: {} reachable addresses", self.reachable.len());
        self
    }

    pub(crate) fn trace_pointer<T: ?Sized + Traceable>(&mut self, ptr: *const T) {
        let unmarked = self.set_reachable(ptr);
        if unmarked {
            trace!("tracing {:p}: diving in...", ptr);
            let obj = unsafe { &*ptr };
            obj.trace(self);
        } else {
            trace!("tracing {:p}: reached before", ptr)
        }
    }

    pub fn set_reachable<T: ?Sized>(&mut self, ptr: *const T) -> bool {
        self.reachable.insert(normalize(ptr))
    }
}

fn normalize<T: ?Sized>(ptr: *const T) -> *const () {
    ptr as *const ()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tracing_a_pointer_marks_address_as_reachable() {
        let mut gc = GarbageCollector::new();

        let ptr = 0x1234 as *const ();

        gc.trace_pointer(ptr);

        assert!(gc.reachable.contains(&ptr))
    }

    #[test]
    fn tracing_a_pointer_triggers_tracing_of_pointed_object() {
        let mut gc = GarbageCollector::new();

        let spy = TraceSpy::new();

        gc.trace_pointer(&spy);

        assert!(spy.was_traced());
        assert!(gc.is_reachable(&spy));
    }

    #[test]
    fn starting_mark_phase_triggers_tracing_of_root() {
        let gc = GarbageCollector::new();

        let spy = TraceSpy::new();

        gc.mark(&spy);

        assert!(spy.was_traced());
    }

    struct TraceSpy(std::cell::Cell<bool>);
    impl Traceable for TraceSpy {
        fn trace(&self, _: &mut GarbageCollector) {
            self.0.set(true)
        }
    }
    impl TraceSpy {
        fn new() -> Self {
            TraceSpy(std::cell::Cell::new(false))
        }
        fn was_traced(&self) -> bool {
            self.0.get()
        }
    }
}
