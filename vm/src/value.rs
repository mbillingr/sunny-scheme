use crate::closure::Closure;
use crate::mem::{Ref, Traceable, Tracer};
use crate::storage::ValueStorage;
use crate::table::Table;
use crate::Result;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

pub type Symbol = Box<str>;

#[derive(Clone)]
#[repr(u8)]
pub enum Value {
    Void,
    Nil,
    False,
    True,
    Int(i64),
    Symbol(Ref<Symbol>),
    Pair(Ref<(Value, Value)>),
    Table(Ref<Table>),

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
    impl_accessor!(is_int, as_int, Value::Int, i64);
    impl_accessor!(is_symbol, as_symbol, Value::Symbol, ref Symbol);
    impl_accessor!(is_pair, as_pair, as_mut_pair, Value::Pair, ref (Value, Value));
    impl_accessor!(is_table, as_table, as_mut_table, Value::Table, ref Table);
    impl_accessor!(is_closure, as_closure, Value::Closure, ref Closure);

    pub fn get_tag(&self) -> u8 {
        match self {
            Value::Void => 0,
            Value::Nil => 1,
            Value::False => 2,
            Value::True => 3,
            Value::Int(_) => 4,
            Value::Symbol(_) => 5,
            Value::Pair(_) => 6,
            Value::Table(_) => 7,
            Value::Closure(_) => 8,
            Value::Primitive(_) => 9,
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
            Value::Table(p) => p.trace(gc),
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
            Value::Table(p) => write!(f, "{:?}", p),
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
            Value::Int(_) => {}
            Value::Symbol(p) => p.as_ptr().hash(state),
            Value::Pair(p) => p.as_ptr().hash(state),
            Value::Table(p) => p.as_ptr().hash(state),
            Value::Closure(p) => p.as_ptr().hash(state),
            Value::Primitive(p) => (*p as *const u8).hash(state),
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
        table.insert(Value::Void, Value::Int(1));
        table.insert(Value::Nil, Value::Int(2));
        table.insert(Value::False, Value::Int(3));
        table.insert(Value::True, Value::Int(4));
        table.insert(Value::Int(0), Value::Int(5));

        assert_eq!(table[&Value::Void], Value::Int(1));
        assert_eq!(table[&Value::Nil], Value::Int(2));
        assert_eq!(table[&Value::False], Value::Int(3));
        assert_eq!(table[&Value::True], Value::Int(4));
        assert_eq!(table[&Value::Int(0)], Value::Int(5));
    }

    #[test]
    fn compound_values_are_compared_by_pointer() {
        let pair1 = Value::Pair(Ref::from_leaked_static((Value::Int(1), Value::Int(2))));
        let pair2 = Value::Pair(Ref::from_leaked_static((Value::Int(1), Value::Int(2))));

        assert_eq!(pair1, pair1);
        assert_eq!(pair2, pair2);
        assert_ne!(pair1, pair2);
    }
}
