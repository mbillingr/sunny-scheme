use std::hash::{Hash, Hasher};

use crate::closure::Closure;
use crate::continuation::Continuation;

#[repr(u8)]
pub enum Value {
    Closure(Closure),
    Continuation(Continuation),

    Values(usize), // mark multiple return values on stack
}

impl Default for Value {
    fn default() -> Self {
        unimplemented!()
    }
}

macro_rules! impl_accessor {
    ($pred:ident, $variant:path) => {
        pub fn $pred(&self) -> bool {
            match self {
                $variant => true,
                _ => false,
            }
        }
    };

    ($pred:ident, $conv:ident, $conv_mut:ident, $variant:path, ref $output:ty) => {
        impl_accessor! {$pred, $conv, $variant, ref $output}
        pub fn $conv_mut(&mut self) -> Option<&mut $output> {
            match self {
                $variant(x) => Some(&mut *x),
                _ => None,
            }
        }
    };

    ($pred:ident, $conv:ident, $variant:path, ref $output:ty) => {
        pub fn $pred(&self) -> bool {
            self.$conv().is_some()
        }
        pub fn $conv(&self) -> Option<&$output> {
            match self {
                $variant(x) => Some(&*x),
                _ => None,
            }
        }
    };

    ($pred:ident, $conv:ident, $variant:path, $output:ty) => {
        pub fn $pred(&self) -> bool {
            self.$conv().is_some()
        }
        pub fn $conv(&self) -> Option<$output> {
            match self {
                $variant(x) => Some(*x),
                _ => None,
            }
        }
    };
}

impl Value {
    impl_accessor!(is_closure, as_closure, Value::Closure, ref Closure);
    impl_accessor!(
        is_continuation,
        as_continuation,
        Value::Continuation,
        ref Continuation
    );
    impl_accessor!(is_values, as_values, Value::Values, ref usize);

    pub fn get_tag(&self) -> u8 {
        use Value::*;
        match self {
            Closure(_) => 8,
            Continuation(_) => 10,
            Values(_) => 254,
        }
    }

    pub fn equals(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Closure(a), Closure(b)) => *a == *b,
            (Continuation(a), Continuation(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        self.eq(rhs)
    }

    pub fn shallow_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Closure(p) => (p as *const Closure).hash(state),
            Value::Continuation(p) => (p as *const Continuation).hash(state),
            Value::Values(n) => n.hash(state),
        }
    }

    pub fn deep_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Closure(_) => unimplemented!(),
            Value::Continuation(_) => unimplemented!(),
            Value::Values(n) => n.hash(state),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Closure(p) => write!(f, "{:?}", p),
            Value::Continuation(p) => write!(f, "{:?}", *p),
            Value::Values(n) => write!(f, "<{} values>", n),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Closure(a), Closure(b)) => a == b,
            (Values(a), Values(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.shallow_hash(state)
    }
}
