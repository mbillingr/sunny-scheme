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
        .define_primitive("make-strong-eq-hash-table", make_strong_eq_hashtable)
        .define_primitive("make-key-weak-eq-hash-table", make_weak_eq_hashtable)
        .define_primitive("make-equal-hash-table", make_strong_equals_hashtable)
        .define_primitive("hash-table-set!", hashtable_set)
        .define_primitive("hash-table-ref/default", hashtable_ref_default)
        .define_primitive("hash-table-delete!", hashtable_delete)
        .define_primitive("hash-table-clear!", hashtable_clear)
        .build();
}

primitive! {
    fn is_hashtable(obj: Value) -> Result<Value> {
        Ok(Value::bool(obj.as_obj::<Box<dyn Table>>().is_some()))
    }

    fn hashtable_set(obj: Value, key: Value, value: Value) -> Result<Value> {
        let table = obj.as_obj_mut::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;

        table.table_set(key, value);
        Ok(Value::Void)
    }

    fn hashtable_ref_default(obj: Value, key: Value, default: Value) -> Result<Value> {
        let table = obj.as_obj_mut::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;

        if let Some(value) = table.table_ref(key) {
            Ok(value.clone())
        } else {
            Ok(default)
        }
    }

    fn hashtable_delete(obj: Value, key: Value) -> Result<Value> {
        let table = obj.as_obj_mut::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;
        table.table_del(key);
        Ok(Value::Void)
    }

    fn hashtable_clear(obj: Value) -> Result<Value> {
        let table = obj.as_obj_mut::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;
        table.table_clear();
        Ok(Value::Void)
    }
}

fn make_strong_equals_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    make_hashtable(n_args, vm, TableKeyStrongEquals::new)
}

fn make_strong_eq_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    make_hashtable(n_args, vm, TableKeyStrongEq::new)
}

fn make_weak_eq_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    make_hashtable(n_args, vm, TableKeyWeakEq::new)
}

fn make_hashtable<T: 'static + Table>(
    n_args: usize,
    vm: &mut Vm,
    table_constructor: impl Fn() -> T,
) -> Result<()> {
    if n_args != 0 {
        return Err(ErrorKind::TooManyArgs);
    }
    let table: Box<dyn Table> = Box::new(table_constructor());
    let table: Box<dyn Object> = Box::new(table);
    let obj = vm.borrow_storage().insert(table);
    vm.push_value(Value::Object(obj));
    Ok(())
}

type TableKeyStrongEquals = GenericTable<KeyStrongEquals, Value>;
type TableKeyStrongEq = GenericTable<KeyStrongEq, Value>;
type TableKeyWeakEq = GenericTable<KeyWeakEq, Value>;

trait Table: Debug {
    fn table_set(&mut self, key: Value, value: Value) -> Option<Value>;
    fn table_ref(&mut self, key: Value) -> Option<&Value>;
    fn table_del(&mut self, key: Value) -> Option<Value>;
    fn table_clear(&mut self);

    fn is_equal(&self, other: &dyn Any) -> bool {
        std::ptr::eq(
            self as *const Self as *const u8,
            other as *const _ as *const u8,
        )
    }
}

impl Table for TableKeyStrongEquals {
    fn table_set(&mut self, key: Value, value: Value) -> Option<Value> {
        self.insert(key, value)
    }

    fn table_ref(&mut self, key: Value) -> Option<&Value> {
        self.get(&key.into())
    }

    fn table_del(&mut self, key: Value) -> Option<Value> {
        self.del(&KeyStrongEquals::from_value(key))
    }

    fn table_clear(&mut self) {
        self.clear();
    }
}

impl Table for TableKeyStrongEq {
    fn table_set(&mut self, key: Value, value: Value) -> Option<Value> {
        self.insert(key, value)
    }

    fn table_ref(&mut self, key: Value) -> Option<&Value> {
        self.get(&key.into())
    }

    fn table_del(&mut self, key: Value) -> Option<Value> {
        self.del(&KeyStrongEq::from_value(key))
    }

    fn table_clear(&mut self) {
        self.clear();
    }
}

impl Table for TableKeyWeakEq {
    fn table_set(&mut self, key: Value, value: Value) -> Option<Value> {
        self.insert(key, value)
    }

    fn table_ref(&mut self, key: Value) -> Option<&Value> {
        self.get(&key.into())
    }

    fn table_del(&mut self, key: Value) -> Option<Value> {
        self.del(&KeyWeakEq::from_value(key))
    }

    fn table_clear(&mut self) {
        self.clear();
    }
}

impl Object for Box<dyn Table> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn Object) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.is_equal(other))
            .unwrap_or(false)
    }
}

#[derive(Debug)]
struct GenericTable<K, V> {
    table: HashMap<K, V>,
}

impl<K: TableKey, V> GenericTable<K, V> {
    pub fn new() -> Self {
        GenericTable {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: impl Into<K>, value: V) -> Option<V> {
        key.into().insert(value, &mut self.table)
    }

    pub fn get(&mut self, key: &K) -> Option<&V> {
        K::get(key, &mut self.table)
    }

    pub fn del(&mut self, key: &K) -> Option<V> {
        K::del(key, &mut self.table)
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

    fn insert<V>(self, value: V, table: &mut HashMap<Self, V>) -> Option<V> {
        table.insert(self, value)
    }

    fn get<'a, V>(key: &Self, table: &'a mut HashMap<Self, V>) -> Option<&'a V> {
        table.get(key)
    }

    fn del<V>(key: &Self, table: &mut HashMap<Self, V>) -> Option<V> {
        table.remove(key)
    }
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

    fn get<'a, V>(key: &Self, table: &'a mut HashMap<Self, V>) -> Option<&'a V> {
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
        table.table_set(storage.cons(1, 2), Value::True);
        assert_eq!(table.len(), 1);
    }

    #[test]
    fn weak_table_loses_dead_keys() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyWeakEq::new();

        let key = storage.cons(1, 2).downgrade();

        table.insert(key.clone(), Value::True);

        assert_eq!(table.len(), 1);

        assert!(table.get(&key.into()).is_none());

        assert_eq!(table.len(), 0);
    }

    #[test]
    fn table_compares_keys_deeply() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEquals::new();

        table.table_set(storage.cons(1, 2), Value::True);
        table.table_set(storage.cons(1, 2), Value::False);

        assert_eq!(table.len(), 1);
    }

    #[test]
    fn table_compares_keys_shallowly() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEq::new();

        table.table_set(storage.cons(1, 2), Value::True);
        table.table_set(storage.cons(1, 2), Value::False);

        assert_eq!(table.len(), 2);
    }

    #[test]
    fn can_remove_from_table() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEquals::new();

        table.table_set(storage.cons(1, 2), Value::True);
        table.table_set(storage.cons(1, 2), Value::True);

        assert_eq!(table.len(), 1);

        table.table_del(storage.cons(1, 2));

        assert_eq!(table.len(), 0);
    }

    #[test]
    fn can_get_value_from_strong_equals_table() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEquals::new();

        let key = storage.cons(1, 2);
        table.table_set(key.clone(), Value::True);
        assert_eq!(table.table_ref(key), Some(&Value::True));
    }

    #[test]
    fn can_get_value_from_strong_eq_table() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyStrongEq::new();

        let key = storage.cons(1, 2);
        table.table_set(key.clone(), Value::True);
        assert_eq!(table.table_ref(key.clone()), Some(&Value::True));

        table.table_set(storage.cons(1, 2), Value::False);
        assert_eq!(table.table_ref(key), Some(&Value::True));
    }

    #[test]
    fn can_get_value_from_weak_eq_table() {
        let mut storage = ValueStorage::new(1);
        let mut table = TableKeyWeakEq::new();

        let key = storage.cons(1, 2);
        table.table_set(key.clone(), Value::True);
        assert_eq!(table.table_ref(key.clone()), Some(&Value::True));
        drop(key); // make sure key stays alive so we don't lose the table entry
    }

    #[test]
    fn key_strong_equals() {
        let mut storage = ValueStorage::new(1);
        let a_key = KeyStrongEquals(storage.cons(1, 2));
        let copied_key = KeyStrongEquals(a_key.0.clone());
        let a_similar_key = KeyStrongEquals(storage.cons(1, 2));
        let another_key = KeyStrongEquals(storage.cons(2, 1));

        assert_eq!(a_key, copied_key);
        assert_eq!(a_key, a_similar_key);
        assert_ne!(a_key, another_key);

        assert_eq!(hash(&a_key), hash(&copied_key));
        assert_eq!(hash(&a_key), hash(&a_similar_key));
        assert_ne!(hash(&a_key), hash(&another_key));
    }

    #[test]
    fn key_strong_eq() {
        let mut storage = ValueStorage::new(1);
        let a_key = KeyStrongEq(storage.cons(1, 2));
        let copied_key = KeyStrongEq(a_key.0.clone());
        let a_similar_key = KeyStrongEq(storage.cons(1, 2));
        let another_key = KeyStrongEq(storage.cons(2, 1));

        assert_eq!(a_key, copied_key);
        assert_ne!(a_key, a_similar_key);
        assert_ne!(a_key, another_key);

        assert_eq!(hash(&a_key), hash(&copied_key));
        assert_ne!(hash(&a_key), hash(&a_similar_key));
        assert_ne!(hash(&a_key), hash(&another_key));
    }

    fn hash<T: Hash>(obj: &T) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        obj.hash(&mut hasher);
        hasher.finish()
    }
}
