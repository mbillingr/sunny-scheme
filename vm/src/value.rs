use std::hash::{Hash, Hasher};

use crate::closure::Closure;
use crate::continuation::Continuation;
use crate::mem::{Ref, Weak};
use crate::number::{Number, WeakNumber};
use crate::primitive::Primitive;
use std::any::Any;
use std::fmt::Debug;

pub type Symbol = Box<str>;
pub type ConstString = Box<str>;

pub trait Object: 'static + Debug {
    fn as_any(&self) -> &dyn Any;

    fn equals(&self, other: &dyn Object) -> bool {
        std::ptr::eq(
            self as *const _ as *const u8,
            other as *const _ as *const u8,
        )
    }
}

impl dyn Object {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }

    pub unsafe fn downcast_mut<T: Any>(&self) -> Option<&mut T> {
        let ptr = self.as_any() as *const dyn Any;
        let ptr = ptr as *mut dyn Any;
        let any = &mut *ptr;
        any.downcast_mut()
    }
}

#[derive(Clone)]
#[repr(u8)]
pub enum Value {
    Void,
    Nil,
    False,
    True,
    Number(Number),
    Symbol(Ref<Symbol>),
    String(Ref<ConstString>),
    Pair(Ref<(Value, Value)>),

    Closure(Ref<Closure>),
    Primitive(Primitive),
    Continuation(Ref<Continuation>),

    Values(usize), // mark multiple return values on stack
    Object(Ref<Box<dyn Object>>),
}

#[derive(Clone)]
#[repr(u8)]
pub enum WeakValue {
    Void,
    Nil,
    False,
    True,
    Number(WeakNumber),
    Symbol(Weak<Symbol>),
    String(Weak<ConstString>),
    Pair(Weak<(Value, Value)>),

    Closure(Weak<Closure>),
    Primitive(Primitive),
    Continuation(Weak<Continuation>),

    Values(usize),
    Object(Weak<Box<dyn Object>>),
}

impl Default for Value {
    fn default() -> Self {
        Value::Void
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
    impl_accessor!(is_void, Value::Void);
    impl_accessor!(is_nil, Value::Nil);
    impl_accessor!(is_number, as_number, Value::Number, ref Number);
    impl_accessor!(is_symbol, as_symbol, Value::Symbol, ref Symbol);
    impl_accessor!(is_pair, as_pair, as_mut_pair, Value::Pair, ref (Value, Value));
    impl_accessor!(is_closure, as_closure, Value::Closure, ref Closure);

    pub fn bool(b: bool) -> Self {
        match b {
            true => Self::True,
            false => Self::False,
        }
    }

    pub fn number(x: impl Into<Number>) -> Self {
        Value::Number(x.into())
    }

    pub fn get_tag(&self) -> u8 {
        use Value::*;
        match self {
            Void => 0,
            Nil => 1,
            False => 2,
            True => 3,
            Number(_) => 4,
            Symbol(_) => 5,
            String(_) => 6,
            Pair(_) => 7,
            Closure(_) => 8,
            Primitive(_) => 9,
            Continuation(_) => 10,
            Values(_) => 254,
            Object(_) => 255,
        }
    }

    pub fn is_like_true(&self) -> bool {
        match self {
            Value::Void => false,
            Value::False => false,
            _ => true,
        }
    }

    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::True => Some(true),
            Value::False => Some(false),
            _ => None,
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
        match self {
            Value::Closure(_) | Value::Primitive(_) | Value::Continuation(_) => true,
            _ => false,
        }
    }

    pub fn as_obj<T: Any>(&self) -> Option<&T> {
        match self {
            Value::Object(obj) => obj.downcast_ref(),
            _ => None,
        }
    }

    pub fn as_obj_mut<T: Any>(&self) -> Option<&mut T> {
        match self {
            Value::Object(obj) => unsafe { obj.downcast_mut() },
            _ => None,
        }
    }

    pub fn equals(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Void, Void) => true,
            (Nil, Nil) => true,
            (False, False) => true,
            (True, True) => true,
            (Number(a), Number(b)) => a.equals(b),
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a.0.equals(&b.0) && a.1.equals(&b.1),
            (Closure(a), Closure(b)) => **a == **b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            (Continuation(a), Continuation(b)) => std::ptr::eq(a, b),
            (Object(a), Object(b)) => a.equals(&***b),
            _ => false,
        }
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        self.eq(rhs)
    }

    pub fn shallow_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Void => {}
            Value::Nil => {}
            Value::False => {}
            Value::True => {}
            Value::Number(n) => n.hash(state),
            Value::Symbol(p) => p.as_ptr().hash(state),
            Value::String(p) => p.as_ptr().hash(state),
            Value::Pair(p) => p.as_ptr().hash(state),
            Value::Closure(p) => p.as_ptr().hash(state),
            Value::Primitive(p) => (*p as *const u8).hash(state),
            Value::Continuation(p) => p.as_ptr().hash(state),
            Value::Values(n) => n.hash(state),
            Value::Object(p) => p.as_ptr().hash(state),
        }
    }

    pub fn deep_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Void => {}
            Value::Nil => {}
            Value::False => {}
            Value::True => {}
            Value::Number(n) => n.hash(state),
            Value::Symbol(p) => p.as_ptr().hash(state),
            Value::String(p) => p.as_ptr().hash(state),
            Value::Pair(p) => {
                p.0.deep_hash(state);
                p.1.deep_hash(state);
            }
            Value::Closure(_) => unimplemented!(),
            Value::Primitive(p) => (*p as *const u8).hash(state),
            Value::Continuation(_) => unimplemented!(),
            Value::Values(n) => n.hash(state),
            Value::Object(_) => unimplemented!(),
        }
    }
}

impl std::fmt::Debug for WeakValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.upgrade() {
            Some(value) => write!(f, "{}", value),
            None => write!(f, "<dead value>"),
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
            Value::Void => write!(f, "<void>"),
            Value::Nil => write!(f, "<nil>"),
            Value::False => write!(f, "#f"),
            Value::True => write!(f, "#t"),
            Value::Number(i) => write!(f, "{}", i),
            Value::Symbol(p) => write!(f, "'{}", **p),
            Value::String(p) => write!(f, "{}", **p),
            Value::Pair(p) => write!(f, "({} . {})", p.0, p.1),
            Value::Closure(p) => write!(f, "{:?}", p),
            Value::Primitive(p) => write!(f, "<primitive {:p}>", p),
            Value::Continuation(p) => write!(f, "{:?}", **p),
            Value::Values(n) => write!(f, "<{} values>", n),
            Value::Object(p) => write!(f, "<object {:p}>", p),
        }
    }
}

impl Eq for WeakValue {}

impl PartialEq for WeakValue {
    fn eq(&self, rhs: &Self) -> bool {
        use WeakValue::*;
        match (self, rhs) {
            (Void, Void) => true,
            (Nil, Nil) => true,
            (False, False) => true,
            (True, True) => true,
            (Number(a), Number(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a == b,
            (Closure(a), Closure(b)) => a == b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            (Values(a), Values(b)) => a == b,
            (Object(a), Object(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Void, Void) => true,
            (Nil, Nil) => true,
            (False, False) => true,
            (True, True) => true,
            (Number(a), Number(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a == b,
            (Closure(a), Closure(b)) => a == b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            (Values(a), Values(b)) => a == b,
            (Object(a), Object(b)) => a == b,
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

impl Hash for WeakValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            WeakValue::Void => {}
            WeakValue::Nil => {}
            WeakValue::False => {}
            WeakValue::True => {}
            WeakValue::Number(n) => n.hash(state),
            WeakValue::Symbol(p) => p.as_ptr().hash(state),
            WeakValue::String(p) => p.as_ptr().hash(state),
            WeakValue::Pair(p) => p.as_ptr().hash(state),
            WeakValue::Closure(p) => p.as_ptr().hash(state),
            WeakValue::Primitive(p) => (*p as *const u8).hash(state),
            WeakValue::Continuation(p) => p.as_ptr().hash(state),
            WeakValue::Values(n) => n.hash(state),
            WeakValue::Object(p) => p.as_ptr().hash(state),
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Number(Number::from(i))
    }
}

impl Value {
    pub fn downgrade(&self) -> WeakValue {
        match self {
            Value::Void => WeakValue::Void,
            Value::Nil => WeakValue::Nil,
            Value::False => WeakValue::False,
            Value::True => WeakValue::True,
            Value::Number(n) => WeakValue::Number(n.downgrade()),
            Value::Symbol(p) => WeakValue::Symbol(p.downgrade()),
            Value::String(p) => WeakValue::String(p.downgrade()),
            Value::Pair(p) => WeakValue::Pair(p.downgrade()),
            Value::Closure(p) => WeakValue::Closure(p.downgrade()),
            Value::Primitive(p) => WeakValue::Primitive(*p),
            Value::Continuation(p) => WeakValue::Continuation(p.downgrade()),
            Value::Values(n) => WeakValue::Values(*n),
            Value::Object(p) => WeakValue::Object(p.downgrade()),
        }
    }
}

impl WeakValue {
    pub fn is_dead(&self) -> bool {
        self.upgrade().is_none()
    }

    pub fn upgrade(&self) -> Option<Value> {
        match self {
            WeakValue::Void => Some(Value::Void),
            WeakValue::Nil => Some(Value::Nil),
            WeakValue::False => Some(Value::False),
            WeakValue::True => Some(Value::True),
            WeakValue::Number(n) => n.upgrade().map(Value::Number),
            WeakValue::Symbol(p) => p.upgrade().map(Value::Symbol),
            WeakValue::String(p) => p.upgrade().map(Value::String),
            WeakValue::Pair(p) => p.upgrade().map(Value::Pair),
            WeakValue::Closure(p) => p.upgrade().map(Value::Closure),
            WeakValue::Primitive(p) => Some(Value::Primitive(*p)),
            WeakValue::Continuation(p) => p.upgrade().map(Value::Continuation),
            WeakValue::Values(n) => Some(Value::Values(*n)),
            WeakValue::Object(p) => p.upgrade().map(Value::Object),
        }
    }

    pub fn get_tag(&self) -> u8 {
        use WeakValue::*;
        match self {
            Void => 0,
            Nil => 1,
            False => 2,
            True => 3,
            Number(_) => 4,
            Symbol(_) => 5,
            String(_) => 6,
            Pair(_) => 7,
            Closure(_) => 8,
            Primitive(_) => 9,
            Continuation(_) => 10,
            Values(_) => 254,
            Object(_) => 255,
        }
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

        pub fn try_is_less(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Value::bool(a < b)),
                _ => None,
            }
        }

        pub fn try_is_greater(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Value::bool(a > b)),
                _ => None,
            }
        }

        pub fn try_is_less_or_equal(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Value::bool(a <= b)),
                _ => None,
            }
        }

        pub fn try_is_greater_or_equal(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Value::bool(a >= b)),
                _ => None,
            }
        }

        pub fn try_is_numeq(&self, other: &Self) -> Option<Self> {
            use Value::*;
            match (self, other) {
                (Number(a), Number(b)) => Some(Value::bool(a == b)),
                _ => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_size_of_value_type() {
        let value_size = std::mem::size_of::<Value>();
        let pointer_size = std::mem::size_of::<usize>();
        assert_eq!(value_size, 2 * pointer_size);
    }

    #[test]
    fn compound_values_are_compared_by_pointer() {
        let pair1 = Value::Pair(Ref::from_leaked_static((
            Value::number(1),
            Value::number(2),
        )));
        let pair2 = Value::Pair(Ref::from_leaked_static((
            Value::number(1),
            Value::number(2),
        )));

        assert_eq!(pair1, pair1);
        assert_eq!(pair2, pair2);
        assert_ne!(pair1, pair2);
    }

    #[test]
    fn shallow_hashes_of_similar_pairs_are_different() {
        let pair1 = Value::Pair(Ref::from_leaked_static((
            Value::number(1),
            Value::number(2),
        )));
        let pair2 = Value::Pair(Ref::from_leaked_static((
            Value::number(1),
            Value::number(2),
        )));

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
        let pair1 = Value::Pair(Ref::from_leaked_static((
            Value::number(1),
            Value::number(2),
        )));
        let pair2 = Value::Pair(Ref::from_leaked_static((
            Value::number(1),
            Value::number(2),
        )));

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        pair1.deep_hash(&mut hasher);
        let hash1 = hasher.finish();

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        pair2.deep_hash(&mut hasher);
        let hash2 = hasher.finish();

        assert_eq!(hash1, hash2);
    }
}
