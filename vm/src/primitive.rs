use crate::{Result, Value, ValueStorage};

pub type Primitive = fn(&mut Vec<Value>, &mut ValueStorage) -> Result<()>;
