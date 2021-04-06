use crate::closure::Closure;
use crate::mem::{Ref, Storage, Traceable, Tracer};
use crate::value::{ConstString, Symbol};
use crate::Value;
use std::collections::HashMap;
use std::fmt::Debug;
use sunny_sexpr_parser::Sexpr;

pub struct ValueStorage {
    storage: Storage,
    permanent_roots: HashMap<Ref<dyn Traceable>, usize>,
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
            permanent_roots: HashMap::new(),
        }
    }

    pub fn sexpr_to_value(&mut self, sexpr: &Sexpr) -> Result<Value, ()> {
        Ok(match sexpr {
            Sexpr::Nil => Value::Nil,
            Sexpr::Bool(false) => Value::False,
            Sexpr::Bool(true) => Value::True,
            Sexpr::Integer(x) => Value::Number((*x).into()),
            Sexpr::Symbol(s) => self.interned_symbol(s)?,
            Sexpr::String(s) => self.interned_string(s)?,
            Sexpr::Pair(p) => {
                let car = self.sexpr_to_value(p.0.get_value())?;
                let cdr = self.sexpr_to_value(p.1.get_value())?;
                self.cons(car, cdr).map_err(|_| ())?
            }
            Sexpr::Object(_) => Err(())?,
        })
    }

    pub fn count_allocations(&mut self, sexpr: &Sexpr) -> usize {
        match sexpr {
            Sexpr::Nil | Sexpr::Bool(_) | Sexpr::Integer(_) => 0,
            Sexpr::Symbol(_) => 1,
            Sexpr::String(_) => 1,
            Sexpr::Pair(p) => {
                let n_car = self.count_allocations(p.0.get_value());
                let n_cdr = self.count_allocations(p.1.get_value());
                1 + n_car + n_cdr
            }
            Sexpr::Object(_) => 0,
        }
    }

    pub fn interned_string(&mut self, content: &str) -> Result<Value, ()> {
        let string_ref = if let Some(s) = self
            .storage
            .find_interned(|s: &ConstString| &**s == content)
        {
            s
        } else {
            let s = content.to_string().into_boxed_str();
            self.storage.insert_interned(s).map_err(|_| ())?
        };
        Ok(Value::String(string_ref))
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

    pub fn list(
        &mut self,
        items: Vec<impl Into<Value> + Debug>,
    ) -> Result<Value, Vec<impl Into<Value> + Debug>> {
        if self.free() < items.len() {
            return Err(items);
        }

        /* let mut list = Value::Nil;
        for x in items.into_iter().rev() {
            list = self.cons(x, list).unwrap();
        }*/

        let list = items
            .into_iter()
            .rev()
            .fold(Value::Nil, |acc, x| self.cons(x, acc).unwrap());

        Ok(list)
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

    pub fn insert_box<T: 'static>(&mut self, obj: Box<T>) -> Result<Ref<T>, Box<T>> {
        self.storage.insert_box(obj)
    }

    /// Make sure the storage is at most half full
    pub fn auto_grow(&mut self) {
        self.storage.auto_grow();
    }

    pub fn ensure(&mut self, n: usize) {
        self.storage.ensure(n);
    }

    pub unsafe fn collect_garbage(&mut self, root: &impl Traceable) {
        let gc = self.begin_garbage_collection().mark(root);
        self.finish_garbage_collection(gc);
    }

    pub fn begin_garbage_collection(&mut self) -> Tracer {
        let mut tracer = self.storage.begin_garbage_collection();
        for root in self.permanent_roots.keys() {
            tracer = tracer.mark(root);
        }
        tracer
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
            Value::Number(_) => true,
            Value::Symbol(p) => self.storage.is_valid(p),
            Value::String(p) => self.storage.is_valid(p),
            Value::Pair(p) => self.storage.is_valid(p),
            Value::Closure(p) => self.storage.is_valid(p),
            Value::Primitive(_) => true,
            Value::Continuation(p) => self.storage.is_valid(p),
            Value::Values(_) => true,
            Value::Object(p) => self.storage.is_valid(p),
        }
    }

    pub fn preserve(&mut self, value: &Ref<impl 'static + Traceable>) {
        *self
            .permanent_roots
            .entry(value.as_dyn_traceable())
            .or_insert(0) += 1;
    }

    pub fn release(&mut self, value: &Ref<impl 'static + Traceable>) {
        let value = value.as_dyn_traceable();
        if let Some(count) = self.permanent_roots.get_mut(&value) {
            *count -= 1;
            if *count == 0 {
                self.permanent_roots.remove(&value);
            }
        }
    }

    pub fn preserve_value(&mut self, value: &Value) {
        if let Some(x) = value.as_traceable() {
            *self.permanent_roots.entry(x).or_insert(0) += 1;
        }
    }

    pub fn release_value(&mut self, value: &Value) {
        if let Some(x) = value.as_traceable() {
            if let Some(count) = self.permanent_roots.get_mut(&x) {
                *count -= 1;
                if *count == 0 {
                    self.permanent_roots.remove(&x);
                }
            }
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
        let a = storage.cons(Value::number(1), Value::number(2)).unwrap();
        let b = storage.cons(Value::number(3), Value::number(4)).unwrap();
        let c = storage.cons(Value::number(5), Value::number(6)).unwrap();
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
    fn preserved_values_remain_valid_after_collection() {
        let mut storage = ValueStorage::new(10);
        let x = storage.interned_symbol("foo").unwrap();
        storage.preserve_value(&x);
        unsafe {
            storage.collect_garbage(&());
        }
        assert!(storage.is_valid(&x));
    }

    #[test]
    fn releasing_allows_a_previously_preserved_value_to_be_collected() {
        let mut storage = ValueStorage::new(10);
        let x = storage.interned_symbol("foo").unwrap();
        storage.preserve_value(&x);
        storage.release_value(&x);
        unsafe {
            storage.collect_garbage(&());
        }
        assert!(!storage.is_valid(&x));
    }

    #[test]
    fn a_repeatedly_preserved_value_must_be_repeatedly_released() {
        let mut storage = ValueStorage::new(10);
        let x = storage.interned_symbol("foo").unwrap();
        storage.preserve_value(&x);
        storage.preserve_value(&x);

        storage.release_value(&x);
        unsafe {
            storage.collect_garbage(&());
        }
        assert!(storage.is_valid(&x));

        storage.release_value(&x);
        unsafe {
            storage.collect_garbage(&());
        }
        assert!(!storage.is_valid(&x));
    }
}
