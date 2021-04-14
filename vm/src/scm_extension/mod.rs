use crate::closure::Closure;
use crate::continuation::Continuation;
use crate::{Primitive, Value};
use std::any::Any;
use std::collections::HashMap;
use sunny_sexpr_parser::{Scm, ScmHasher, ScmObject};

impl ScmObject for Value {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.equals(other))
            .unwrap_or(false)
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        self.deep_hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}

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
        Scm::obj(Value::Closure(cls.into()))
    }

    fn is_closure(&self) -> bool {
        self.as_closure().is_some()
    }

    fn as_closure(&self) -> Option<&Closure> {
        self.as_type::<Value>().and_then(Value::as_closure)
    }

    fn primitive(pri: Primitive) -> Scm {
        Scm::obj(Value::Primitive(pri.into()))
    }

    fn is_primitive(&self) -> bool {
        self.as_primitive().is_some()
    }

    fn as_primitive(&self) -> Option<&Primitive> {
        self.as_type::<Value>().and_then(Value::as_primitive)
    }

    fn continuation(cnt: Continuation) -> Scm {
        Scm::obj(Value::Continuation(cnt.into()))
    }

    fn is_continuation(&self) -> bool {
        self.as_continuation().is_some()
    }

    fn as_continuation(&self) -> Option<&Continuation> {
        self.as_type::<Value>().and_then(Value::as_continuation)
    }

    fn values(n: usize) -> Scm {
        Scm::obj(Value::Values(n))
    }

    fn as_values(&self) -> Option<usize> {
        self.as_type::<Value>().and_then(Value::as_values).copied()
    }
}
