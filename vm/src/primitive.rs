use crate::{Result, Vm};

pub type Primitive = fn(usize, &mut Vm) -> Result<()>;
