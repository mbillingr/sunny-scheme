use crate::context::Context;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::Deref;
use sunny_sexpr_parser::{HashEqual, HashPtrEq, Scm, ScmHasher, ScmObject, WeakScm};
use sunny_vm::scm_extension::ScmExt;
use sunny_vm::{ErrorKind, Result, Vm};

pub fn define_lib_sunny_hash_table(ctx: &mut Context) {
    ctx.define_library("(sunny hash-table)")
        .define_primitive_fixed_arity("hash-table?", 1, is_hashtable)
        .define_primitive_fixed_arity("make-strong-eq-hash-table", 0, make_strong_eq_hashtable)
        .define_primitive_fixed_arity("make-key-weak-eq-hash-table", 0, make_weak_eq_hashtable)
        .define_primitive_fixed_arity("make-equal-hash-table", 0, make_strong_equals_hashtable)
        .define_primitive_fixed_arity("hash-table-set!", 3, hashtable_set)
        .define_primitive_fixed_arity("hash-table-ref/default", 3, hashtable_ref_default)
        .define_primitive_fixed_arity("hash-table-delete!", 2, hashtable_delete)
        .define_primitive_fixed_arity("hash-table-clear!", 1, hashtable_clear)
        .build();
}

primitive! {
    fn is_hashtable(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.as_type::<MutableTable>().is_some()))
    }

    fn hashtable_set(obj: Scm, key: Scm, value: Scm) -> Result<Scm> {
        let table = obj.as_type::<MutableTable>().ok_or(ErrorKind::TypeError)?;

        table.0.borrow_mut().table_set(key, value);
        Ok(Scm::void())
    }

    fn hashtable_ref_default(obj: Scm, key: Scm, default: Scm) -> Result<Scm> {
        let table = obj.as_type::<MutableTable>().ok_or(ErrorKind::TypeError)?;

        if let Some(value) = table.0.borrow().table_ref(key) {
            Ok(value.clone())
        } else {
            Ok(default)
        }
    }

    fn hashtable_delete(obj: Scm, key: Scm) -> Result<Scm> {
        let table = obj.as_type::<MutableTable>().ok_or(ErrorKind::TypeError)?;
        table.0.borrow_mut().table_del(key);
        Ok(Scm::void())
    }

    fn hashtable_clear(obj: Scm) -> Result<Scm> {
        let table = obj.as_type::<MutableTable>().ok_or(ErrorKind::TypeError)?;
        table.0.borrow_mut().table_clear();
        Ok(Scm::void())
    }
}

fn make_strong_equals_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    make_hashtable(n_args, vm, TableKeyStrongEquals::default)
}

fn make_strong_eq_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    make_hashtable(n_args, vm, TableKeyStrongEq::default)
}

fn make_weak_eq_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    make_hashtable(n_args, vm, TableKeyWeakEq::default)
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
    let table = RefCell::new(table);
    let table = MutableTable(table);
    vm.push_value(Scm::obj(table));
    Ok(())
}

#[derive(Debug, Default)]
struct TableKeyStrongEquals(HashMap<HashEqual, Scm>);

#[derive(Debug, Default)]
struct TableKeyStrongEq(HashMap<HashPtrEq, Scm>);

#[derive(Debug, Default)]
struct TableKeyWeakEq {
    table: HashMap<WeakScm, Scm>,
}

#[derive(Debug)]
struct MutableTable(RefCell<Box<dyn Table>>);

impl Deref for MutableTable {
    type Target = RefCell<Box<dyn Table>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ScmObject for MutableTable {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn equals(&self, _other: &dyn ScmObject) -> bool {
        unimplemented!()
    }
    fn deep_hash(&self, _state: &mut ScmHasher) {
        unimplemented!()
    }
    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}

impl Display for MutableTable {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}", self.0.borrow())
    }
}

trait Table: Debug {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm>;
    fn table_ref(&self, key: Scm) -> Option<&Scm>;
    fn table_del(&mut self, key: Scm) -> Option<Scm>;
    fn table_clear(&mut self);

    fn is_equal(&self, other: &dyn Any) -> bool {
        std::ptr::eq(
            self as *const Self as *const u8,
            other as *const _ as *const u8,
        )
    }
}

impl Table for TableKeyStrongEquals {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm> {
        self.0.insert(key.into(), value)
    }

    fn table_ref(&self, key: Scm) -> Option<&Scm> {
        self.0.get(&HashEqual::from(key))
    }

    fn table_del(&mut self, key: Scm) -> Option<Scm> {
        self.0.remove(&HashEqual::from(key))
    }

    fn table_clear(&mut self) {
        self.0.clear();
    }
}

impl Table for TableKeyStrongEq {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm> {
        self.0.insert(key.into(), value)
    }

    fn table_ref(&self, key: Scm) -> Option<&Scm> {
        self.0.get(&HashPtrEq::from(key))
    }

    fn table_del(&mut self, key: Scm) -> Option<Scm> {
        self.0.remove(&HashPtrEq::from(key))
    }

    fn table_clear(&mut self) {
        self.0.clear();
    }
}

impl Table for TableKeyWeakEq {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm> {
        let capacity_before = self.table.capacity();
        let old_value = self.table.insert(key.downgrade(), value);
        drop(key);
        if self.table.capacity() != capacity_before {
            // I'd prefer to drop dead keys before growing the hash table
            // but checking capacity after the fact is the best solution I could find.
            self.drop_dead_keys();
        }
        old_value
    }

    fn table_ref(&self, key: Scm) -> Option<&Scm> {
        self.table
            .get_key_value(&key.downgrade())
            .filter(|(k, _)| !k.is_dead())
            .map(|(_, v)| v)
    }

    fn table_del(&mut self, key: Scm) -> Option<Scm> {
        self.table.remove(&key.downgrade())
    }

    fn table_clear(&mut self) {
        self.table.clear();
    }
}

impl TableKeyWeakEq {
    fn drop_dead_keys(&mut self) {
        let keys_to_drop: Vec<_> = self
            .table
            .keys()
            .cloned()
            .filter(WeakScm::is_dead)
            .collect();

        for key in keys_to_drop {
            self.table.remove(&key);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sunny_sexpr_parser::Scm;

    #[test]
    fn strong_table_keeps_keys_alive() {
        let mut table = TableKeyStrongEquals::default();
        table.table_set(Scm::cons(1, 2), Scm::bool(true));
        assert_eq!(table.0.len(), 1);
    }

    #[test]
    fn weak_table_eventually_loses_dead_keys() {
        let mut table = TableKeyWeakEq::default();

        // this depends on the hashmap's initial capacity to be 4.
        // after inserting the 5th item all dead keys are purged.
        for _ in 0..5 {
            table.table_set(Scm::cons(1, 2), Scm::bool(true));
        }

        assert_eq!(table.table.len(), 0);
    }

    #[test]
    fn table_compares_keys_deeply() {
        let mut table = TableKeyStrongEquals::default();

        table.table_set(Scm::cons(1, 2), Scm::bool(true));
        table.table_set(Scm::cons(1, 2), Scm::bool(false));

        assert_eq!(table.0.len(), 1);
    }

    #[test]
    fn table_compares_keys_shallowly() {
        let mut table = TableKeyStrongEq::default();

        table.table_set(Scm::cons(1, 2), Scm::bool(true));
        table.table_set(Scm::cons(1, 2), Scm::bool(false));

        assert_eq!(table.0.len(), 2);
    }

    #[test]
    fn can_remove_from_table() {
        let mut table = TableKeyStrongEquals::default();

        table.table_set(Scm::cons(1, 2), Scm::bool(true));

        assert_eq!(table.0.len(), 1);

        table.table_del(Scm::cons(1, 2));

        assert_eq!(table.0.len(), 0);
    }

    #[test]
    fn can_get_value_from_strong_equals_table() {
        let mut table = TableKeyStrongEquals::default();

        let key = Scm::cons(1, 2);
        table.table_set(key.clone(), Scm::bool(true));
        assert_eq!(table.table_ref(key), Some(&Scm::bool(true)));
    }

    #[test]
    fn can_get_value_from_strong_eq_table() {
        let mut table = TableKeyStrongEq::default();

        let key = Scm::cons(1, 2);
        table.table_set(key.clone(), Scm::bool(true));
        assert_eq!(table.table_ref(key.clone()), Some(&Scm::bool(true)));

        table.table_set(Scm::cons(1, 2), Scm::bool(false));
        assert_eq!(table.table_ref(key), Some(&Scm::bool(true)));
    }

    #[test]
    fn can_get_value_from_weak_eq_table() {
        let mut table = TableKeyWeakEq::default();

        let key = Scm::cons(1, 2);
        table.table_set(key.clone(), Scm::bool(true));
        assert_eq!(table.table_ref(key.clone()), Some(&Scm::bool(true)));
        drop(key); // make sure key stays alive so we don't lose the table entry
    }
}
