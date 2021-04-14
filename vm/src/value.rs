use std::hash::{Hash, Hasher};

use crate::closure::Closure;
use crate::continuation::Continuation;
use crate::mem::Ref;
use crate::number::Number;
use crate::primitive::Primitive;
use std::any::Any;

pub type Symbol = Box<str>;
pub type ConstString = Box<str>;

#[repr(u8)]
pub enum Value {
    Number(Number),
    Symbol(Ref<Symbol>),
    String(Ref<ConstString>),
    Pair(Ref<(Value, Value)>),

    Closure(Closure),
    Primitive(Primitive),
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
    impl_accessor!(is_number, as_number, Value::Number, ref Number);
    impl_accessor!(is_symbol, as_symbol, Value::Symbol, ref Symbol);
    impl_accessor!(is_pair, as_pair, as_mut_pair, Value::Pair, ref (Value, Value));
    impl_accessor!(is_closure, as_closure, Value::Closure, ref Closure);
    impl_accessor!(is_primitive, as_primitive, Value::Primitive, ref Primitive);
    impl_accessor!(
        is_continuation,
        as_continuation,
        Value::Continuation,
        ref Continuation
    );
    impl_accessor!(is_values, as_values, Value::Values, ref usize);

    pub fn number(x: impl Into<Number>) -> Self {
        Value::Number(x.into())
    }

    pub fn get_tag(&self) -> u8 {
        use Value::*;
        match self {
            Number(_) => 4,
            Symbol(_) => 5,
            String(_) => 6,
            Pair(_) => 7,
            Closure(_) => 8,
            Primitive(_) => 9,
            Continuation(_) => 10,
            Values(_) => 254,
        }
    }

    pub fn is_int(&self) -> bool {
        self.as_int().is_some()
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Number(num) => num.as_int(),
            _ => None,
        }
    }

    pub fn car(&self) -> Option<&Value> {
        self.as_pair().map(|(car, _)| car)
    }

    pub fn cdr(&self) -> Option<&Value> {
        self.as_pair().map(|(_, cdr)| cdr)
    }

    pub fn is_procedure(&self) -> bool {
        matches!(
            self,
            Value::Closure(_) | Value::Primitive(_) | Value::Continuation(_)
        )
    }

    pub fn as_obj<T: Any>(&self) -> Option<&T> {
        match self {
            _ => None,
        }
    }

    pub fn as_obj_mut<T: Any>(&self) -> Option<&mut T> {
        match self {
            _ => None,
        }
    }

    pub fn equals(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.equals(b),
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a.0.equals(&b.0) && a.1.equals(&b.1),
            (Closure(a), Closure(b)) => *a == *b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
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
            Value::Number(n) => n.hash(state),
            Value::Symbol(p) => p.as_ptr().hash(state),
            Value::String(p) => p.as_ptr().hash(state),
            Value::Pair(p) => p.as_ptr().hash(state),
            Value::Closure(p) => (p as *const Closure).hash(state),
            Value::Primitive(p) => (p as *const Primitive).hash(state),
            Value::Continuation(p) => (p as *const Continuation).hash(state),
            Value::Values(n) => n.hash(state),
        }
    }

    pub fn deep_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Number(n) => n.hash(state),
            Value::Symbol(p) => p.as_ptr().hash(state),
            Value::String(p) => p.as_ptr().hash(state),
            Value::Pair(p) => {
                p.0.deep_hash(state);
                p.1.deep_hash(state);
            }
            Value::Closure(_) => unimplemented!(),
            Value::Primitive(_) => unimplemented!(),
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
            Value::Number(i) => write!(f, "{}", i),
            Value::Symbol(p) => write!(f, "'{}", **p),
            Value::String(p) => write!(f, "{}", **p),
            Value::Pair(p) => write!(f, "({} . {})", p.0, p.1),
            Value::Closure(p) => write!(f, "{:?}", p),
            Value::Primitive(p) => write!(f, "<primitive {:p}>", p),
            Value::Continuation(p) => write!(f, "{:?}", *p),
            Value::Values(n) => write!(f, "<{} values>", n),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a == b,
            (Closure(a), Closure(b)) => a == b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            (Values(a), Values(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialEq<(Value, Value)> for Value {
    fn eq(&self, rhs: &(Value, Value)) -> bool {
        self.as_pair().map(|lhs| lhs == rhs).unwrap_or(false)
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.shallow_hash(state)
    }
}

impl<T: Into<Number>> From<T> for Value {
    fn from(i: T) -> Self {
        Value::Number(i.into())
    }
}

pub mod arithmetic {
    use super::*;

    impl Value {
        pub fn try_add(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Number(a + b)),
                _ => None,
            }
        }

        pub fn try_sub(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Number(a - b)),
                _ => None,
            }
        }

        pub fn try_mul(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Number(a * b)),
                _ => None,
            }
        }

        pub fn try_div(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Number(a / b)),
                _ => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compound_values_are_compared_by_pointer() {
        let pair1 = Value::Pair(Ref::new((Value::number(1), Value::number(2))));
        let pair2 = Value::Pair(Ref::new((Value::number(1), Value::number(2))));
        assert_ne!(pair1, pair2);
    }

    #[test]
    fn shallow_hashes_of_similar_pairs_are_different() {
        let pair1 = Value::Pair(Ref::new((Value::number(1), Value::number(2))));
        let pair2 = Value::Pair(Ref::new((Value::number(1), Value::number(2))));

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        pair1.shallow_hash(&mut hasher);
        let hash1 = hasher.finish();

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        pair2.shallow_hash(&mut hasher);
        let hash2 = hasher.finish();

        assert_ne!(hash1, hash2);
    }

    #[test]
    fn deep_hashes_of_similar_pairs_are_different() {
        let pair1 = Value::Pair(Ref::new((Value::number(1), Value::number(2))));
        let pair2 = Value::Pair(Ref::new((Value::number(1), Value::number(2))));

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        pair1.deep_hash(&mut hasher);
        let hash1 = hasher.finish();

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        pair2.deep_hash(&mut hasher);
        let hash2 = hasher.finish();

        assert_eq!(hash1, hash2);
    }
}
