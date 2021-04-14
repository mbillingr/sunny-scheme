use crate::{Result, Vm};
use std::any::Any;
use std::collections::HashMap;
use sunny_sexpr_parser::{Scm, ScmHasher, ScmObject};

pub struct Primitive {
    pub name: &'static str,
    pub min_arity: usize,
    pub max_arity: Option<usize>,
    pub proc: PrimitiveProc,
}

pub type PrimitiveProc = fn(usize, &mut Vm) -> Result<()>;

impl std::fmt::Debug for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<primitive {}>", self.name)
    }
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

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

impl ScmObject for Primitive {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| std::ptr::eq(&self.proc, &other.proc))
            .unwrap_or(false)
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(&self.proc, state);
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}
