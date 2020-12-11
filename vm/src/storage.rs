use crate::mem::{Storage, Traceable};
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

    pub fn cons(
        &mut self,
        car: impl Into<Value>,
        cdr: impl Into<Value>,
    ) -> Result<Value, (Value, Value)> {
        let pair = (car.into(), cdr.into());
        let obj = self.storage.insert(pair)?;

        Ok(Value::Pair(obj))
    }

    pub unsafe fn collect_garbage(&mut self, root: &impl Traceable) {
        self.storage.collect_garbage(root)
    }

    pub fn is_valid(&self, value: &Value) -> bool {
        match value {
            Value::Void => true,
            Value::Nil => true,
            Value::Int(_) => true,
            Value::Pair(p) => self.storage.is_valid(p),
            Value::Closure(p) => self.storage.is_valid(p),
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
}
