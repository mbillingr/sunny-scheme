use crate::mem::{GarbageCollector, Ref, Traceable};
use crate::vm::Closure;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Nil,
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
}

impl Traceable for Value {
    fn trace(&self, gc: &mut GarbageCollector) {
        match self {
            Value::Void => {}
            Value::Nil => {}
            Value::Int(_) => {}
            Value::Pair(p) => p.trace(gc),
            Value::Closure(p) => p.trace(gc),
        }
    }
}
