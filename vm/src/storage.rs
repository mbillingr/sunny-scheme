use crate::closure::Closure;
use crate::mem::{Ref, Storage};
use crate::value::{ConstString, Symbol};
use crate::Value;
use std::fmt::Debug;
use sunny_sexpr_parser::Scm;

pub struct ValueStorage {
    storage: Storage,
}

impl Default for ValueStorage {
    fn default() -> Self {
        ValueStorage::new(65536)
    }
}

impl ValueStorage {
    pub fn new(capacity: usize) -> Self {
        ValueStorage {
            storage: Storage::new(capacity),
        }
    }

    pub fn scm_to_value(&mut self, scm: &Scm) -> Value {
        if scm.is_null() {
            Value::Nil
        } else if let Some(false) = scm.as_bool() {
            Value::False
        } else if let Some(true) = scm.as_bool() {
            Value::True
        } else if let Some(i) = scm.as_int() {
            Value::Number(i.into())
        } else if let Some(s) = scm.as_symbol() {
            self.interned_symbol(s)
        } else if let Some(s) = scm.as_string() {
            self.interned_string(s)
        } else if let Some(p) = scm.as_pair() {
            let car = self.scm_to_value(&p.0);
            let cdr = self.scm_to_value(&p.1);
            self.cons(car, cdr)
        } else {
            panic!("Don't know how to build Value from {:?}", scm)
        }
    }

    pub fn interned_string(&mut self, content: &str) -> Value {
        let string_ref = if let Some(s) = self
            .storage
            .find_interned(|s: &ConstString| &**s == content)
        {
            s
        } else {
            let s = content.to_string().into_boxed_str();
            self.storage.insert_interned(s)
        };
        Value::String(string_ref)
    }

    pub fn interned_symbol(&mut self, name: &str) -> Value {
        let symbol = if let Some(s) = self.storage.find_interned(|s: &Symbol| &**s == name) {
            s
        } else {
            let symbol = name.to_string().into_boxed_str();
            self.storage.insert_interned(symbol)
        };
        Value::Symbol(symbol)
    }

    pub fn uninterned_symbol(&mut self, name: impl ToString) -> Value {
        let symbol = name.to_string().into_boxed_str();
        let obj = self.insert(symbol);
        Value::Symbol(obj)
    }

    pub fn cons(&mut self, car: impl Into<Value>, cdr: impl Into<Value>) -> Value {
        let pair = (car.into(), cdr.into());
        let obj = self.insert(pair);
        Value::Pair(obj)
    }

    pub fn list(&mut self, items: Vec<impl Into<Value> + Debug>) -> Value {
        items
            .into_iter()
            .rev()
            .fold(Value::Nil, |acc, x| self.cons(x, acc))
    }

    pub fn store_closure(&mut self, cls: Closure) -> Result<Value, Closure> {
        let obj = self.insert(cls);
        Ok(Value::Closure(obj))
    }

    pub fn insert<T: 'static>(&mut self, obj: T) -> Ref<T> {
        self.storage.insert(obj)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn storage_can_cons_values() {
        let mut storage = ValueStorage::new(1);
        let obj = storage.cons(Value::Nil, Value::Nil);
        assert!(obj.is_pair());
    }

    #[test]
    fn interned_symbols_with_same_name_are_equal() {
        let mut storage = ValueStorage::new(2);
        let a = storage.interned_symbol("foo");
        let b = storage.interned_symbol("foo");
        assert_eq!(a, b);
    }

    #[test]
    fn interned_symbols_with_same_name_do_not_take_up_extra_space() {
        let mut storage = ValueStorage::new(1);
        storage.interned_symbol("foo");
        storage.interned_symbol("foo");
    }

    #[test]
    fn uninterned_symbols_with_same_name_are_not_equal() {
        let mut storage = ValueStorage::new(2);
        let a = storage.uninterned_symbol("foo");
        let b = storage.uninterned_symbol("foo");
        assert_ne!(a, b);
    }
}
