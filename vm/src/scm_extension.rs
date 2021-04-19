use crate::closure::Closure;
use crate::continuation::Continuation;
use crate::Primitive;
use std::any::Any;
use std::collections::HashMap;
use std::hash::Hash;
use sunny_scm::{Scm, ScmHasher, ScmObject};

pub trait ScmExt {
    fn is_procedure(&self) -> bool;

    fn closure(cls: Closure) -> Scm;
    fn is_closure(&self) -> bool;
    fn as_closure(&self) -> Option<&Closure>;

    fn primitive(pri: Primitive) -> Scm;
    fn is_primitive(&self) -> bool;
    fn as_primitive(&self) -> Option<&Primitive>;

    fn continuation(cnt: Continuation) -> Scm;
    fn is_continuation(&self) -> bool;
    fn as_continuation(&self) -> Option<&Continuation>;

    fn values(n: usize) -> Scm;
    fn as_values(&self) -> Option<usize>;
}

impl ScmExt for Scm {
    fn is_procedure(&self) -> bool {
        self.is_closure() || self.is_primitive() || self.is_continuation()
    }
    fn closure(cls: Closure) -> Scm {
        Scm::obj(cls)
    }

    fn is_closure(&self) -> bool {
        self.as_closure().is_some()
    }

    fn as_closure(&self) -> Option<&Closure> {
        self.as_type::<Closure>()
    }

    fn primitive(pri: Primitive) -> Scm {
        Scm::obj(pri)
    }

    fn is_primitive(&self) -> bool {
        self.as_primitive().is_some()
    }

    fn as_primitive(&self) -> Option<&Primitive> {
        self.as_type::<Primitive>()
    }

    fn continuation(cnt: Continuation) -> Scm {
        Scm::obj(cnt)
    }

    fn is_continuation(&self) -> bool {
        self.as_continuation().is_some()
    }

    fn as_continuation(&self) -> Option<&Continuation> {
        self.as_type::<Continuation>()
    }

    fn values(n: usize) -> Scm {
        Scm::obj(Values(n))
    }

    fn as_values(&self) -> Option<usize> {
        self.as_type::<Values>().map(|values| values.0)
    }
}

#[derive(Debug, Hash)]
struct Values(usize);

impl std::fmt::Display for Values {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{} values>", self.0)
    }
}

impl ScmObject for Values {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.0 == other.0)
            .unwrap_or(false)
    }

    fn value_hash(&self, state: &mut ScmHasher) {
        self.hash(state);
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        self.hash(state);
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}
