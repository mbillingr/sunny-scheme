use sunny_sexpr_parser::{ScmObject, Scm};
use crate::Value;
use std::any::Any;
use std::collections::HashMap;

impl ScmObject for Value {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn ScmObject) -> bool {
        other.downcast_ref::<Self>().map(|other| self.equals(other)).unwrap_or(false)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}
