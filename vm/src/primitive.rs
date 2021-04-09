use crate::{Result, Vm};

pub struct Primitive {
    pub name: &'static str,
    pub min_arity: usize,
    pub max_arity: Option<usize>,
    pub proc: PrimitiveProc,
}

pub type PrimitiveProc = fn(usize, &mut Vm) -> Result<()>;

impl Primitive {
    pub fn fixed_arity(name: &'static str, arity: usize, proc: PrimitiveProc) -> Self {
        Primitive {
            name,
            min_arity: arity,
            max_arity: Some(arity),
            proc,
        }
    }

    pub fn max_arity(
        name: &'static str,
        min_arity: usize,
        max_arity: usize,
        proc: PrimitiveProc,
    ) -> Self {
        Primitive {
            name,
            min_arity,
            max_arity: Some(max_arity),
            proc,
        }
    }

    pub fn vararg(name: &'static str, min_arity: usize, proc: PrimitiveProc) -> Self {
        Primitive {
            name,
            min_arity,
            max_arity: None,
            proc,
        }
    }
}
