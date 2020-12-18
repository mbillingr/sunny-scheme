use crate::closure::Closure;
use crate::mem::{Ref, Storage, Traceable, Tracer};
use crate::table::Table;
use crate::value::Symbol;
use crate::Value;

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

    pub fn interned_symbol(&mut self, name: &str) -> Result<Value, ()> {
        let symbol = if let Some(s) = self.storage.find_interned(|s: &Symbol| &**s == name) {
            s
        } else {
            let symbol = name.to_string().into_boxed_str();
            self.storage.insert_interned(symbol).map_err(|_| ())?
        };
        Ok(Value::Symbol(symbol))
    }

    pub fn uninterned_symbol(&mut self, name: impl ToString) -> Result<Value, Symbol> {
        let symbol = name.to_string().into_boxed_str();
        let obj = self.insert(symbol)?;
        Ok(Value::Symbol(obj))
    }

    pub fn cons(
        &mut self,
        car: impl Into<Value>,
        cdr: impl Into<Value>,
    ) -> Result<Value, (Value, Value)> {
        let pair = (car.into(), cdr.into());
        let obj = self.insert(pair)?;
        Ok(Value::Pair(obj))
    }

    pub fn new_table(&mut self) -> Result<Value, ()> {
        let table = Table::new();
        let obj = self.insert(table).map_err(|_| ())?;
        Ok(Value::Table(obj))
    }

    pub fn store_closure(&mut self, cls: Closure) -> Result<Value, Closure> {
        let obj = self.insert(cls)?;
        Ok(Value::Closure(obj))
    }

    pub fn free(&self) -> usize {
        self.storage.free()
    }

    pub fn insert<T: 'static>(&mut self, obj: T) -> Result<Ref<T>, T> {
        self.storage.insert(obj)
    }

    /// Make sure the storage is at most half full
    pub fn auto_grow(&mut self) {
        self.storage.auto_grow();
    }

    pub fn ensure(&mut self, n: usize) {
        self.storage.ensure(n);
    }

    pub unsafe fn collect_garbage(&mut self, root: &impl Traceable) {
        self.storage.collect_garbage(root)
    }

    pub fn begin_garbage_collection(&mut self) -> Tracer {
        self.storage.begin_garbage_collection()
    }

    pub unsafe fn finish_garbage_collection(&mut self, gc: Tracer) {
        self.storage.finish_garbage_collection(gc)
    }

    pub fn is_valid(&self, value: &Value) -> bool {
        match value {
            Value::Void => true,
            Value::Nil => true,
            Value::False => true,
            Value::True => true,
            Value::Int(_) => true,
            Value::Symbol(p) => self.storage.is_valid(p),
            Value::Pair(p) => self.storage.is_valid(p),
            Value::Table(p) => self.storage.is_valid(p),
            Value::Closure(p) => self.storage.is_valid(p),
            Value::Primitive(_) => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn storage_can_cons_values() {
        let mut storage = ValueStorage::new(1);
        let obj = storage.cons(Value::Nil, Value::Nil).unwrap();
        assert!(obj.is_pair());
    }

    #[test]
    fn consed_values_are_valid() {
        let mut storage = ValueStorage::new(1);
        let obj = storage.cons(Value::Nil, Value::Nil).unwrap();
        assert!(storage.is_valid(&obj));
    }

    #[test]
    fn gc_preserves_nested_conses() {
        let mut storage = ValueStorage::new(5);
        let a = storage.cons(Value::Int(1), Value::Int(2)).unwrap();
        let b = storage.cons(Value::Int(3), Value::Int(4)).unwrap();
        let c = storage.cons(Value::Int(5), Value::Int(6)).unwrap();
        let ab = storage.cons(a.clone(), b.clone()).unwrap();
        let abc = storage.cons(ab.clone(), c.clone()).unwrap();

        unsafe {
            storage.collect_garbage(&abc);
        }

        assert!(storage.is_valid(&abc));
        assert!(storage.is_valid(&ab));
        assert!(storage.is_valid(&a));
        assert!(storage.is_valid(&b));
        assert!(storage.is_valid(&c));
    }

    #[test]
    fn interned_symbols_with_same_name_are_equal() {
        let mut storage = ValueStorage::new(2);
        let a = storage.interned_symbol("foo").unwrap();
        let b = storage.interned_symbol("foo").unwrap();
        assert_eq!(a, b);
    }

    #[test]
    fn interned_symbols_with_same_name_do_not_take_up_extra_space() {
        let mut storage = ValueStorage::new(1);
        storage.interned_symbol("foo").unwrap();
        storage.interned_symbol("foo").unwrap();
    }

    #[test]
    fn uninterned_symbols_with_same_name_are_not_equal() {
        let mut storage = ValueStorage::new(2);
        let a = storage.uninterned_symbol("foo").unwrap();
        let b = storage.uninterned_symbol("foo").unwrap();
        assert_ne!(a, b);
    }

    #[test]
    fn uninterned_symbols_with_same_name_are_not_equal_to_interned_symbol() {
        let mut storage = ValueStorage::new(2);
        let a = storage.uninterned_symbol("foo").unwrap();
        let b = storage.interned_symbol("foo").unwrap();
        assert_ne!(a, b);
    }
}
