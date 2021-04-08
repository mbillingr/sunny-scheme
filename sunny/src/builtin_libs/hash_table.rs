use crate::context::Context;
use std::any::Any;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use sunny_vm::{ErrorKind, Object, Result, Value, Vm, WeakValue};

pub fn define_lib_sunny_hash_table(ctx: &mut Context) {
    ctx.define_library("(sunny hash-table)")
        .define_primitive("hash-table?", is_hashtable)
        .define_primitive("make-hash-table", make_hashtable)
        .define_primitive("hash-table-set!", hashtable_set)
        .define_primitive("hash-table-ref/default", hashtable_ref_default)
        .build();
}

primitive! {
    fn is_hashtable(obj: Value) -> Result<Value> {
        Ok(Value::bool(obj.as_obj::<TableKeyStrongEquals>().is_some()))
    }

    fn hashtable_set(obj: Value, key: Value, value: Value) -> Result<Value> {
        let table = obj.as_obj_mut::<TableKeyStrongEquals>().ok_or(ErrorKind::TypeError)?;

        table.insert(key, value);
        Ok(Value::Void)
    }

    fn hashtable_ref_default(obj: Value, key: Value, default: Value) -> Result<Value> {
        let table = obj.as_obj_mut::<TableKeyStrongEquals>().ok_or(ErrorKind::TypeError)?;

        if let Some(value) = table.get(&key) {
            Ok(value.clone())
        } else {
            Ok(default)
        }
    }
}

fn make_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    if n_args != 0 {
        return Err(ErrorKind::TooManyArgs);
    }

    let table: Box<dyn Object> = Box::new(TableKeyStrongEquals::new());
    let obj = vm.borrow_storage().insert(table);

    vm.push_value(Value::Object(obj));
    Ok(())
}

type TableKeyStrongEquals = GenericTable<KeyStrongEquals, Value>;
type TableKeyStrongEq = GenericTable<KeyStrongEq, Value>;
type TableKeyWeakEq = GenericTable<KeyWeakEq, Value>;

#[derive(Debug)]
struct GenericTable<K, V> {
    table: HashMap<K, V>,
}

impl<K: TableKey, V: From<Value>> GenericTable<K, V> {
    pub fn new() -> Self {
        GenericTable {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: impl Into<K>, value: V) -> Option<V> {
        key.into().insert(value, &mut self.table)
    }

    pub fn get<Q: Hash + Eq>(&mut self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        K::get(key, &mut self.table)
    }
}

impl<K: 'static + Debug + PartialEq + Eq + Hash, V: 'static + Debug + PartialEq> Object
    for GenericTable<K, V>
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn Object) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| other.table == self.table)
            .unwrap_or(false)
    }
}

impl<K, V> Deref for GenericTable<K, V> {
    type Target = HashMap<K, V>;
    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl<K, V> DerefMut for GenericTable<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.table
    }
}
/*
trait DropDeadKeys {
    fn drop_dead_keys(&mut self) {
        let dead_keys: Vec<_> = self.table.keys().filter(|k| k.is_dead()).cloned().collect();
        for key in dead_keys {
            self.table.remove(&key);
        }
    }
}*/

trait TableKey: Sized + Eq + Hash {
    fn from_value(key: Value) -> Self;

    fn insert<V>(self, value: V, table: &mut HashMap<Self, V>) -> Option<V>;

    fn get<'a, Q: Hash + Eq, V>(key: &Q, table: &'a mut HashMap<Self, V>) -> Option<&'a V>
    where
        Self: Borrow<Q>;
}

pub struct KeyStrongEquals(Value);

impl From<Value> for KeyStrongEquals {
    fn from(value: Value) -> Self {
        KeyStrongEquals(value)
    }
}

impl Borrow<Value> for KeyStrongEquals {
    fn borrow(&self) -> &Value {
        &self.0
    }
}

impl TableKey for KeyStrongEquals {
    fn from_value(key: Value) -> Self {
        Self::from(key)
    }

    fn insert<V>(self, value: V, table: &mut HashMap<Self, V>) -> Option<V> {
        table.insert(self, value)
    }

    fn get<'a, Q: Hash + Eq, V>(key: &Q, table: &'a mut HashMap<Self, V>) -> Option<&'a V>
    where
        Self: Borrow<Q>,
    {
        table.get(key)
    }
}

impl Eq for KeyStrongEquals {}

impl PartialEq for KeyStrongEquals {
    fn eq(&self, other: &Self) -> bool {
        self.0.equals(&other.0)
    }
}

impl Hash for KeyStrongEquals {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.deep_hash(state)
    }
}

impl Debug for KeyStrongEquals {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct KeyStrongEq(Value);

impl From<Value> for KeyStrongEq {
    fn from(value: Value) -> Self {
        KeyStrongEq(value)
    }
}

impl Borrow<Value> for KeyStrongEq {
    fn borrow(&self) -> &Value {
        &self.0
    }
}

impl TableKey for KeyStrongEq {
    fn from_value(key: Value) -> Self {
        Self::from(key)
    }

    fn insert<V>(self, value: V, table: &mut HashMap<Self, V>) -> Option<V> {
        table.insert(self, value)
    }

    fn get<'a, Q: Hash + Eq, V>(key: &Q, table: &'a mut HashMap<Self, V>) -> Option<&'a V>
    where
        Self: Borrow<Q>,
    {
        table.get(key)
    }
}

impl Eq for KeyStrongEq {}

impl PartialEq for KeyStrongEq {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl Hash for KeyStrongEq {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.shallow_hash(state)
    }
}

impl Debug for KeyStrongEq {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct KeyWeakEq(WeakValue);

impl From<WeakValue> for KeyWeakEq {
    fn from(value: WeakValue) -> Self {
        KeyWeakEq(value)
    }
}

impl From<Value> for KeyWeakEq {
    fn from(value: Value) -> Self {
        KeyWeakEq(value.downgrade())
    }
}

impl From<&Value> for KeyWeakEq {
    fn from(value: &Value) -> Self {
        KeyWeakEq(value.downgrade())
    }
}

impl Borrow<WeakValue> for KeyWeakEq {
    fn borrow(&self) -> &WeakValue {
        &self.0
    }
}

impl TableKey for KeyWeakEq {
    fn from_value(key: Value) -> Self {
        Self::from(key)
    }

    fn insert<V>(self, value: V, table: &mut HashMap<Self, V>) -> Option<V> {
        table.insert(self, value)
    }

    fn get<'a, Q: Hash + Eq, V>(key: &Q, table: &'a mut HashMap<Self, V>) -> Option<&'a V>
    where
        Self: Borrow<Q>,
    {
        if let Some((entry_key, _)) = table.get_key_value(key) {
            if entry_key.0.is_dead() {
                table.remove(key);
            } else {
            }
        }
        table.get(key)
    }
}

impl Eq for KeyWeakEq {}

impl PartialEq for KeyWeakEq {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl Hash for KeyWeakEq {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl Debug for KeyWeakEq {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sunny_vm::ValueStorage;

    #[test]
    fn strong_table_keeps_keys_alive() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEquals::new();
        table.insert(storage.cons(1, 2), Value::True);
        assert_eq!(table.len(), 1);
    }

    #[test]
    fn weak_table_loses_dead_keys() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyWeakEq::new();

        let key = storage.cons(1, 2).downgrade();

        table.insert(key.clone(), Value::True);

        assert_eq!(table.len(), 1);

        assert!(table.get(&key).is_none());

        assert_eq!(table.len(), 0);
    }

    #[test]
    fn table_compares_keys_deeply() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEquals::new();

        table.insert(storage.cons(1, 2), Value::True);
        table.insert(storage.cons(1, 2), Value::False);

        assert_eq!(table.len(), 1);
    }

    #[test]
    fn table_compares_keys_shallowly() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEq::new();

        table.insert(storage.cons(1, 2), Value::True);
        table.insert(storage.cons(1, 2), Value::False);

        assert_eq!(table.len(), 2);
    }
}
