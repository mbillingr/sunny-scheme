use super::Traceable;
use log::{debug, info, trace};
use std::any::Any;
use std::collections::HashSet;

pub struct GarbageCollector {
    reachable: HashSet<*const ()>,
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

    pub unsafe fn sweep(self, objects: &mut Vec<Box<dyn Any>>) {
        let n_before = objects.len();

        let mut i = 0;
        while i < objects.len() {
            if self.is_reachable(&*objects[i]) {
                i += 1;
            } else {
                objects.swap_remove(i);
            }
        }

        debug!(
            "Sweep phase: collected {} objects",
            n_before - objects.len()
        );
        debug!(
            "Sweep phase: {} live and {} free objects",
            objects.len(),
            objects.capacity() - objects.len()
        );
    }

    pub fn set_reachable<T: ?Sized>(&mut self, ptr: *const T) -> bool {
        self.reachable.insert(normalize(ptr))
    }

    fn is_reachable<T: ?Sized>(&self, ptr: *const T) -> bool {
        self.reachable.contains(&normalize(ptr))
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

    #[test]
    fn sweeping_removes_unreachable_objects() {
        let a = Box::new(1);
        let b = Box::new(2);
        let c = Box::new(3);
        let mut objects: Vec<Box<dyn Any>> = vec![a, b, c];
        let b_ptr = &*objects[1] as *const _ as *const ();
        let mut gc = GarbageCollector::new();

        gc.reachable.insert(b_ptr);

        unsafe {
            gc.sweep(&mut objects);
        }

        assert_eq!(objects.len(), 1);
        assert_eq!(objects[0].downcast_ref::<i32>(), Some(&2));
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
