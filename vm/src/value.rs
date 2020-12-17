use crate::closure::Closure;
use crate::mem::{Tracer, Ref, Traceable};
use crate::storage::ValueStorage;
use crate::Result;

pub type Symbol = Box<str>;

#[derive(Clone)]
pub enum Value {
    Void,
    Nil,
    False,
    True,
    Int(i64),
    Symbol(Ref<Symbol>),
    Pair(Ref<(Value, Value)>),

    Closure(Ref<Closure>),
    Primitive(fn(&mut Vec<Value>, &mut ValueStorage) -> Result<()>),
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
    impl_accessor!(is_int, as_int, Value::Int, i64);
    impl_accessor!(is_symbol, as_symbol, Value::Symbol, ref Symbol);
    impl_accessor!(is_pair, as_pair, Value::Pair, ref (Value, Value));
    impl_accessor!(is_closure, as_closure, Value::Closure, ref Closure);

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
}

impl Traceable for Value {
    fn trace(&self, gc: &mut Tracer) {
        match self {
            Value::Void => {}
            Value::Nil => {}
            Value::False => {}
            Value::True => {}
            Value::Int(_) => {}
            Value::Symbol(p) => p.trace(gc),
            Value::Pair(p) => p.trace(gc),
            Value::Closure(p) => p.trace(gc),
            Value::Primitive(_) => {}
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "<void>"),
            Value::Nil => write!(f, "<nil>"),
            Value::False => write!(f, "#f"),
            Value::True => write!(f, "#t"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Symbol(p) => write!(f, "'{}", **p),
            Value::Pair(p) => write!(f, "{:?}", p),
            Value::Closure(p) => write!(f, "{:?}", p),
            Value::Primitive(p) => write!(f, "<primitive {:p}", p),
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
            (Int(a), Int(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Pair(a), Pair(b)) => a == b,
            (Closure(a), Closure(b)) => a == b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }
}

impl PartialEq<[Value; 2]> for Value {
    fn eq(&self, rhs: &[Value; 2]) -> bool {
        self.as_pair()
            .map(|lhs| lhs.0 == rhs[0] && lhs.1 == rhs[1])
            .unwrap_or(false)
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
}
