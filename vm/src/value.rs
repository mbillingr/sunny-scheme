use crate::closure::Closure;
use crate::mem::{GarbageCollector, Ref, Traceable};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Nil,
    False,
    True,
    Int(i64),
    Pair(Ref<(Value, Value)>),

    Closure(Ref<Closure>),
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
    fn trace(&self, gc: &mut GarbageCollector) {
        match self {
            Value::Void => {}
            Value::Nil => {}
            Value::False => {}
            Value::True => {}
            Value::Int(_) => {}
            Value::Pair(p) => p.trace(gc),
            Value::Closure(p) => p.trace(gc),
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
