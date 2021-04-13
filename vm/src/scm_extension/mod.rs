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
    fn void() -> Scm;
    fn closure(cls: Closure) -> Scm;
    fn primitive(pri: Primitive) -> Scm;
    fn continuation(cnt: Continuation) -> Scm;
    fn values(n: usize) -> Scm;

    fn is_like_true(&self) -> bool;
    fn is_procedure(&self) -> bool;

    fn is_void(&self) -> bool;
    fn is_closure(&self) -> bool;
    fn as_closure(&self) -> Option<&Closure>;
    fn is_primitive(&self) -> bool;
    fn as_primitive(&self) -> Option<&Primitive>;
    fn is_continuation(&self) -> bool;
    fn as_continuation(&self) -> Option<&Continuation>;
    fn as_values(&self) -> Option<usize>;
}

impl ScmExt for Scm {
    fn void() -> Scm {
        Scm::obj(Value::Void)
    }

    fn closure(cls: Closure) -> Scm {
        Scm::obj(Value::Closure(cls.into()))
    }

    fn primitive(pri: Primitive) -> Scm {
        Scm::obj(Value::Primitive(pri.into()))
    }

    fn continuation(cnt: Continuation) -> Scm {
        Scm::obj(Value::Continuation(cnt.into()))
    }

    fn values(n: usize) -> Scm {
        Scm::obj(Value::Values(n))
    }

    fn is_like_true(&self) -> bool {
        self.as_bool().unwrap_or(true) && !self.is_void()
    }

    fn is_procedure(&self) -> bool {
        self.is_closure() || self.is_primitive() || self.is_continuation()
    }

    fn is_void(&self) -> bool {
        self.as_type::<Value>().map(Value::is_void).unwrap_or(false)
    }

    fn is_closure(&self) -> bool {
        self.as_closure().is_some()
    }

    fn as_closure(&self) -> Option<&Closure> {
        self.as_type::<Value>().and_then(Value::as_closure)
    }

    fn is_primitive(&self) -> bool {
        self.as_primitive().is_some()
    }

    fn as_primitive(&self) -> Option<&Primitive> {
        self.as_type::<Value>().and_then(Value::as_primitive)
    }

    fn is_continuation(&self) -> bool {
        self.as_continuation().is_some()
    }

    fn as_continuation(&self) -> Option<&Continuation> {
        self.as_type::<Value>().and_then(Value::as_continuation)
    }

    fn as_values(&self) -> Option<usize> {
        self.as_type::<Value>().and_then(Value::as_values).copied()
    }
}