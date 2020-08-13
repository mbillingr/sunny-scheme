pub use memory_model_impl::prelude;
pub use memory_model_impl::KIND;

#[cfg(feature = "leaking")]
mod memory_model_impl {
    pub use leaking::*;
    pub mod prelude {
        pub use super::*;
    }

    mod leaking {
        pub const KIND: &'static str = "leaking";

        use std::cell::Cell;

        pub type Ref<T> = &'static T;

        pub type Mut<T> = Cell<T>;

        #[macro_export]
        macro_rules! make_ref {
            ($x:expr) => {
                Box::leak(Box::new($x))
            };
        }

        pub fn ref_as_ptr<T>(r: &Ref<T>) -> *const T {
            *r
        }

        #[derive(Copy, Clone)]
        pub struct Boxed<T: 'static + Copy>(Ref<Mut<T>>);

        impl<T: 'static + Copy> Boxed<T> {
            pub fn new(value: T) -> Self {
                Boxed(make_ref!(Mut::new(value)))
            }

            pub fn get(&self) -> T {
                self.0.get()
            }

            pub fn set(&self, new_value: T) {
                self.0.set(new_value);
            }
        }
    }
}

#[cfg(not(feature = "leaking"))]
mod memory_model_impl {
    pub use refcnting::*;
    pub mod prelude {
        pub use super::*;
    }

    mod refcnting {
        pub const KIND: &'static str = "reference counting";

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

        pub fn ref_as_ptr<T>(r: &Ref<T>) -> *const T {
            &**r
        }

        #[derive(Clone)]
        pub struct Boxed<T: 'static + Clone>(Ref<Mut<T>>);

        impl<T: 'static + Clone> Boxed<T> {
            pub fn new(value: T) -> Self {
                Boxed(make_ref!(Mut::new(value)))
            }

            pub fn get(&self) -> T {
                self.0.get()
            }

            pub fn set(&self, new_value: T) {
                self.0.set(new_value);
            }
        }
    }
}
