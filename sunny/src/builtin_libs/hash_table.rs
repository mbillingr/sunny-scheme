use crate::context::Context;
use std::any::Any;
use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use sunny_sexpr_parser::{HashEqual, HashPtrEq, Scm, ScmHasher, ScmObject, WeakScm};
use sunny_vm::scm_extension::ScmExt;
use sunny_vm::{ErrorKind, Object, Result, Vm};

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
        Ok(Scm::bool(obj.as_type::<Box<dyn Table>>().is_some()))
    }

    fn hashtable_set(obj: Scm, key: Scm, value: Scm) -> Result<Scm> {
        //let table = obj.as_type::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;
        let table: &mut dyn Table = unimplemented!();

        table.table_set(key, value);
        Ok(Scm::void())
    }

    fn hashtable_ref_default(obj: Scm, key: Scm, default: Scm) -> Result<Scm> {
        //let table = obj.as_type::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;
        let table: &mut dyn Table = unimplemented!();

        if let Some(value) = table.table_ref(key) {
            Ok(value.clone())
        } else {
            Ok(default)
        }
    }

    fn hashtable_delete(obj: Scm, key: Scm) -> Result<Scm> {
        //let table = obj.as_type::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;
        let table: &mut dyn Table = unimplemented!();
        table.table_del(key);
        Ok(Scm::void())
    }

    fn hashtable_clear(obj: Scm) -> Result<Scm> {
        //let table = obj.as_type::<Box<dyn Table>>().ok_or(ErrorKind::TypeError)?;
        let table: &mut dyn Table = unimplemented!();
        table.table_clear();
        Ok(Scm::void())
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
    //let table: Box<dyn Object> = Box::new(table);
    vm.push_value(Scm::obj(table));
    Ok(())
}

type TableKeyStrongEquals = HashMap<HashEqual, Scm>;
type TableKeyStrongEq = HashMap<HashPtrEq, Scm>;
type TableKeyWeakEq = HashMap<WeakScm, Scm>;

impl ScmObject for Box<dyn Table> {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn equals(&self, other: &dyn ScmObject) -> bool {
        unimplemented!()
    }
    fn deep_hash(&self, state: &mut ScmHasher) {
        unimplemented!()
    }
    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}

impl Display for Box<dyn Table> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}", self)
    }
}

trait Table: Debug {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm>;
    fn table_ref(&mut self, key: Scm) -> Option<&Scm>;
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
        self.insert(key.into(), value)
    }

    fn table_ref(&mut self, key: Scm) -> Option<&Scm> {
        self.get(&HashEqual::from(key))
    }

    fn table_del(&mut self, key: Scm) -> Option<Scm> {
        self.remove(&HashEqual::from(key))
    }

    fn table_clear(&mut self) {
        self.clear();
    }
}

impl Table for TableKeyStrongEq {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm> {
        self.insert(key.into(), value)
    }

    fn table_ref(&mut self, key: Scm) -> Option<&Scm> {
        self.get(&HashPtrEq::from(key))
    }

    fn table_del(&mut self, key: Scm) -> Option<Scm> {
        self.remove(&HashPtrEq::from(key))
    }

    fn table_clear(&mut self) {
        self.clear();
    }
}

impl Table for TableKeyWeakEq {
    fn table_set(&mut self, key: Scm, value: Scm) -> Option<Scm> {
        self.insert(key.downgrade(), value)
    }

    fn table_ref(&mut self, key: Scm) -> Option<&Scm> {
        self.get_key_value(&key.downgrade())
            .filter(|(k, v)| !k.is_dead())
            .map(|(k, v)| v)
    }

    fn table_del(&mut self, key: Scm) -> Option<Scm> {
        self.remove(&key.downgrade())
    }

    fn table_clear(&mut self) {
        self.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sunny_sexpr_parser::Scm;

    #[test]
    fn strong_table_keeps_keys_alive() {
        let mut table = TableKeyStrongEquals::new();
        table.table_set(Scm::cons(1, 2), Scm::bool(true));
        assert_eq!(table.len(), 1);
    }

    #[test]
    fn weak_table_loses_dead_keys() {
        let mut table = TableKeyWeakEq::new();

        let key = Scm::cons(1, 2).downgrade();

        table.insert(key.clone(), Scm::bool(true));

        assert_eq!(table.len(), 1);

        assert!(table.get(&key).is_none());

        assert_eq!(table.len(), 0);
    }

    #[test]
    fn table_compares_keys_deeply() {
        let mut table = TableKeyStrongEquals::new();

        table.table_set(Scm::cons(1, 2), Scm::bool(true));
        table.table_set(Scm::cons(1, 2), Scm::bool(false));

        assert_eq!(table.len(), 1);
    }

    #[test]
    fn table_compares_keys_shallowly() {
        let mut table = TableKeyStrongEq::new();

        table.table_set(Scm::cons(1, 2), Scm::bool(true));
        table.table_set(Scm::cons(1, 2), Scm::bool(false));

        assert_eq!(table.len(), 2);
    }

    #[test]
    fn can_remove_from_table() {
        let mut table = TableKeyStrongEquals::new();

        table.table_set(Scm::cons(1, 2), Scm::bool(true));

        assert_eq!(table.len(), 1);

        table.table_del(Scm::cons(1, 2));

        assert_eq!(table.len(), 0);
    }

    #[test]
    fn can_get_value_from_strong_equals_table() {
        let mut table = TableKeyStrongEquals::new();

        let key = Scm::cons(1, 2);
        table.table_set(key.clone(), Scm::bool(true));
        assert_eq!(table.table_ref(key), Some(&Scm::bool(true)));
    }

    #[test]
    fn can_get_value_from_strong_eq_table() {
        let mut table = TableKeyStrongEq::new();

        let key = Scm::cons(1, 2);
        table.table_set(key.clone(), Scm::bool(true));
        assert_eq!(table.table_ref(key.clone()), Some(&Scm::bool(true)));

        table.table_set(Scm::cons(1, 2), Scm::bool(false));
        assert_eq!(table.table_ref(key), Some(&Scm::bool(true)));
    }

    #[test]
    fn can_get_value_from_weak_eq_table() {
        let mut table = TableKeyWeakEq::new();

        let key = Scm::cons(1, 2);
        table.table_set(key.clone(), Scm::bool(true));
        assert_eq!(table.table_ref(key.clone()), Some(&Scm::bool(true)));
        drop(key); // make sure key stays alive so we don't lose the table entry
    }
}
