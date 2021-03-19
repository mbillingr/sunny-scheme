use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::closure::Closure;
use crate::continuation::Continuation;
use crate::mem::{Ref, Traceable, Tracer};
use crate::primitive::Primitive;
use crate::table::Table;

pub type Symbol = Box<str>;

#[derive(Clone)]
#[repr(u8)]
pub enum Value {
    Void,
    Nil,
    False,
    True,
    Number(i64),
    Symbol(Ref<Symbol>),
    Pair(Ref<(Value, Value)>),
    Table(Ref<Table>),

    Closure(Ref<Closure>),
    Primitive(Primitive),
    Continuation(Ref<Continuation>),
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
    impl_accessor!(is_int, as_int, Value::Number, i64);
    impl_accessor!(is_number, as_number, Value::Number, i64);
    impl_accessor!(is_symbol, as_symbol, Value::Symbol, ref Symbol);
    impl_accessor!(is_pair, as_pair, as_mut_pair, Value::Pair, ref (Value, Value));
    impl_accessor!(is_table, as_table, as_mut_table, Value::Table, ref Table);
    impl_accessor!(is_closure, as_closure, Value::Closure, ref Closure);

    pub fn bool(b: bool) -> Self {
        match b {
            true => Self::True,
            false => Self::False,
        }
    }

    pub fn get_tag(&self) -> u8 {
        match self {
            Value::Void => 0,
            Value::Nil => 1,
            Value::False => 2,
            Value::True => 3,
            Value::Number(_) => 4,
            Value::Symbol(_) => 5,
            Value::Pair(_) => 6,
            Value::Table(_) => 7,
            Value::Closure(_) => 8,
            Value::Primitive(_) => 9,
            Value::Continuation(_) => 10,
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

    pub fn car(&self) -> Option<&Value> {
        self.as_pair().map(|(car, _)| car)
    }

    pub fn cdr(&self) -> Option<&Value> {
        self.as_pair().map(|(_, cdr)| cdr)
    }

    pub fn table_set(&mut self, key: Value, value: Value) -> Option<()> {
        self.as_mut_table().map(|table| {
            table.insert(key, value);
        })
    }

    pub fn table_get(&self, key: &Value) -> Option<&Value> {
        self.as_table()
            .map(|table| table.get(key).unwrap_or(&Value::Void))
    }

    pub fn is_procedure(&self) -> bool {
        match self {
            Value::Closure(_) | Value::Primitive(_) => true,
            _ => false,
        }
    }

    pub fn equals(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Void, Void) => true,
            (Nil, Nil) => true,
            (False, False) => true,
            (True, True) => true,
            (Number(a), Number(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Pair(a), Pair(b)) => a.0.equals(&b.0) && a.1.equals(&b.1),
            (Table(a), Table(b)) => ***a == ***b,
            (Closure(a), Closure(b)) => **a == **b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            (Continuation(a), Continuation(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }

    pub fn as_traceable(&self) -> Option<Ref<dyn Traceable>> {
        match self {
            Value::Void | Value::Nil | Value::False | Value::True | Value::Number(_) => None,
            Value::Symbol(s) => Some(s.as_dyn_traceable()),
            Value::Pair(p) => Some(p.as_dyn_traceable()),
            Value::Table(t) => Some(t.as_dyn_traceable()),
            Value::Closure(c) => Some(c.as_dyn_traceable()),
            Value::Primitive(_) => None,
            Value::Continuation(c) => Some(c.as_dyn_traceable()),
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
            Value::Number(_) => {}
            Value::Symbol(p) => p.trace(gc),
            Value::Pair(p) => p.trace(gc),
            Value::Table(p) => p.trace(gc),
            Value::Closure(p) => p.trace(gc),
            Value::Primitive(_) => {}
            Value::Continuation(p) => p.trace(gc),
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
            Value::Number(i) => write!(f, "{}", i),
            Value::Symbol(p) => write!(f, "'{}", **p),
            Value::Pair(p) => write!(f, "{:?}", p),
            Value::Table(p) => write!(f, "{:?}", p),
            Value::Closure(p) => write!(f, "{:?}", p),
            Value::Primitive(p) => write!(f, "<primitive {:p}", p),
            Value::Continuation(p) => write!(f, "{:?}", p),
        }
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
            Value::Pair(p) => write!(f, "({} . {})", p.0, p.1),
            Value::Table(p) => write!(f, "{:?}", **p),
            Value::Closure(p) => write!(f, "{:?}", p),
            Value::Primitive(p) => write!(f, "<primitive {:p}", p),
            Value::Continuation(p) => write!(f, "{:?}", p),
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
            (Pair(a), Pair(b)) => a == b,
            (Table(a), Table(b)) => a == b,
            (Closure(a), Closure(b)) => a == b,
            (Primitive(a), Primitive(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }
}

impl PartialEq<(Value, Value)> for Value {
    fn eq(&self, rhs: &(Value, Value)) -> bool {
        self.as_pair().map(|lhs| lhs == rhs).unwrap_or(false)
    }
}

impl PartialEq<HashMap<Value, Value>> for Value {
    fn eq(&self, rhs: &HashMap<Value, Value>) -> bool {
        self.as_table().map(|lhs| lhs == rhs).unwrap_or(false)
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Void => {}
            Value::Nil => {}
            Value::False => {}
            Value::True => {}
            Value::Number(_) => {}
            Value::Symbol(p) => p.as_ptr().hash(state),
            Value::Pair(p) => p.as_ptr().hash(state),
            Value::Table(p) => p.as_ptr().hash(state),
            Value::Closure(p) => p.as_ptr().hash(state),
            Value::Primitive(p) => (*p as *const u8).hash(state),
            Value::Continuation(p) => p.as_ptr().hash(state),
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Number(i)
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
    fn can_insert_entries_in_table() {
        let mut table = Table::new();
        table.insert(Value::Void, Value::Number(1));
        table.insert(Value::Nil, Value::Number(2));
        table.insert(Value::False, Value::Number(3));
        table.insert(Value::True, Value::Number(4));
        table.insert(Value::Number(0), Value::Number(5));

        assert_eq!(table[&Value::Void], Value::Number(1));
        assert_eq!(table[&Value::Nil], Value::Number(2));
        assert_eq!(table[&Value::False], Value::Number(3));
        assert_eq!(table[&Value::True], Value::Number(4));
        assert_eq!(table[&Value::Number(0)], Value::Number(5));
    }

    #[test]
    fn compound_values_are_compared_by_pointer() {
        let pair1 = Value::Pair(Ref::from_leaked_static((
            Value::Number(1),
            Value::Number(2),
        )));
        let pair2 = Value::Pair(Ref::from_leaked_static((
            Value::Number(1),
            Value::Number(2),
        )));

        assert_eq!(pair1, pair1);
        assert_eq!(pair2, pair2);
        assert_ne!(pair1, pair2);
    }
}
