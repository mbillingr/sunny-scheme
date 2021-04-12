type Strong<T> = std::rc::Rc<T>;
type Weak<T> = std::rc::Weak<T>;

pub struct Interner<T: ?Sized> {
    known_objects: Vec<Weak<T>>,
}

impl<T: 'static + ?Sized> Interner<T> {
    pub fn new() -> Self {
        Interner {
            known_objects: vec![],
        }
    }

    pub fn get_interned<Q>(&mut self, value: Q) -> Strong<T>
    where
        Q: Into<Strong<T>>,
        Q: AsRef<T>,
        T: PartialEq,
    {
        if let Some(already_interned) = self.find_value(value.as_ref()) {
            already_interned
        } else {
            let strong = value.into();
            self.insert_ref(&strong);
            strong
        }
    }

    fn insert_ref(&mut self, strong: &Strong<T>) {
        self.known_objects.push(Strong::downgrade(strong));
    }

    fn find_value(&mut self, value: &T) -> Option<Strong<T>>
    where
        T: PartialEq,
    {
        let mut idx = 0;
        while idx < self.known_objects.len() {
            if let Some(obj) = Weak::upgrade(&self.known_objects[idx]) {
                if &*obj == value {
                    return Some(obj);
                }
                idx += 1;
            } else {
                // remove dead references as we encounter them
                self.known_objects.swap_remove(idx);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    #[test]
    fn interner_acceptance_test() {
        let mut interner = Interner::<str>::new();

        let foo1 = interner.get_interned("foo");
        let foo2 = interner.get_interned("foo");
        let bar = interner.get_interned("bar");

        assert!(std::ptr::eq(&*foo1, &*foo2));
        assert!(!std::ptr::eq(&*foo1, &*bar));
    }

    #[test]
    fn new_values_get_interned() {
        let mut interner = Interner::<Internee<_>>::new();
        let foo_spy = InterneeSpy::new("foo");

        let _foo = interner.get_interned(&foo_spy);

        foo_spy.was_interned()
    }

    #[test]
    fn seen_value_gets_not_interned_again() {
        let mut interner = Interner::<Internee<_>>::new();
        let foo_spy = InterneeSpy::new("foo");

        let _foo1 = interner.get_interned(&foo_spy);
        let _foo2 = interner.get_interned(&foo_spy);

        foo_spy.was_interned_once()
    }

    #[test]
    fn different_value_gets_interned_again() {
        let mut interner = Interner::<Internee<_>>::new();
        let foo_spy = InterneeSpy::new("foo");
        let bar_spy = InterneeSpy::new("bar");

        let _foo = interner.get_interned(&foo_spy);
        let _bar = interner.get_interned(&bar_spy);

        foo_spy.was_interned_once();
        bar_spy.was_interned_once();
    }

    #[test]
    fn seen_value_gets_not_interned_again_if_it_has_been_dropped() {
        let mut interner = Interner::<Internee<_>>::new();
        let foo_spy = InterneeSpy::new("foo");

        let foo = interner.get_interned(&foo_spy);
        drop(foo);

        let _foo = interner.get_interned(&foo_spy);

        foo_spy.was_interned_twice()
    }

    #[derive(Default, Copy, Clone, PartialEq)]
    struct Internee<T>(T);

    impl<T> AsRef<Internee<T>> for Internee<T> {
        fn as_ref(&self) -> &Internee<T> {
            &self
        }
    }

    struct InterneeSpy<T> {
        the_internee: Internee<T>,

        intern_count: Cell<usize>,
    }

    impl<T> InterneeSpy<T> {
        fn new(id: T) -> Self {
            InterneeSpy {
                the_internee: Internee(id),
                intern_count: Cell::new(0),
            }
        }

        fn was_interned(&self) {
            assert!(self.intern_count.get() > 0)
        }

        fn was_interned_once(&self) {
            assert_eq!(self.intern_count.get(), 1)
        }

        fn was_interned_twice(&self) {
            assert_eq!(self.intern_count.get(), 2)
        }
    }

    impl<T> AsRef<Internee<T>> for InterneeSpy<T> {
        fn as_ref(&self) -> &Internee<T> {
            &self.the_internee
        }
    }

    impl<T: Copy> Into<Strong<Internee<T>>> for &InterneeSpy<T> {
        fn into(self) -> Strong<Internee<T>> {
            self.intern_count.set(self.intern_count.get() + 1);
            Strong::new(self.the_internee)
        }
    }
}
