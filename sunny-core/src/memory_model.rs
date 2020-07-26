pub use memory_model_impl::prelude;

#[cfg(feature = "leaking")]
mod memory_model_impl {
    pub mod prelude {
        pub use super::*;
    }

    use std::cell::Cell;

    pub type Ref<T> = &'static T;

    pub type Mut<T> = Cell<T>;

    #[macro_export]
    macro_rules! make_ref {
        ($x:expr) => {
            Box::leak(Box::new($x))
        };
    }
}

#[cfg(not(feature = "leaking"))]
mod memory_model_impl {
    pub mod prelude {
        pub use super::*;
    }

    use std::cell::RefCell;
    pub use std::rc::Rc;

    pub type Ref<T> = Rc<T>;

    #[derive(Debug)]
    pub struct Mut<T>(RefCell<T>);

    impl<T: Clone> Mut<T> {
        pub fn new(x: T) -> Self {
            Mut(RefCell::new(x))
        }

        pub fn get(&self) -> T {
            (*self.0.borrow()).clone()
        }

        pub fn set(&self, new_value: T) -> T {
            std::mem::replace(&mut *self.0.borrow_mut(), new_value)
        }
    }

    #[macro_export]
    macro_rules! make_ref {
        ($x:expr) => {
            Rc::new($x)
        };
    }
}
