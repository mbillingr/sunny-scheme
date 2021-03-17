use crate::{Result, Value, ValueStorage};

pub type Primitive = fn(usize, &mut Vec<Value>, &mut ValueStorage) -> Result<()>;
