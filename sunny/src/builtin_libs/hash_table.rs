use crate::context::Context;
use std::any::Any;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use sunny_vm::mem::{Traceable, Tracer};
use sunny_vm::{ErrorKind, Object, Result, Value, Vm};

pub fn define_lib_sunny_hash_table(ctx: &mut Context) {
    ctx.define_library("(sunny hash-table)")
        .define_primitive("make-hash-table", make_hashtable)
        .define_primitive("hash-table?", is_hashtable)
        .build();
}

primitive! {
    fn is_hashtable(obj: Value) -> Result<Value> {
        Ok(Value::bool(obj.as_obj::<Table>().is_some()))
    }
}

fn make_hashtable(n_args: usize, vm: &mut Vm) -> Result<()> {
    if n_args != 0 {
        return Err(ErrorKind::TooManyArgs);
    }

    unsafe {
        vm.ensure_storage_space(1)?;
    }

    let table: Box<dyn Object> = Box::new(Table::new());
    let obj = vm.borrow_storage().insert(table).unwrap();

    vm.push_value(Value::Object(obj));
    Ok(())
}

#[derive(Debug)]
pub struct Table(HashMap<Value, Value>);

impl Table {
    pub fn new() -> Self {
        Table(HashMap::new())
    }
}

impl Traceable for Table {
    fn trace(&self, gc: &mut Tracer) {
        for (k, v) in &self.0 {
            k.trace(gc);
            v.trace(gc);
        }
    }
}

impl Object for Table {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn Object) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| other.0 == self.0)
            .unwrap_or(false)
    }
}

impl Deref for Table {
    type Target = HashMap<Value, Value>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Table {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
