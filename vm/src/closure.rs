use crate::activation::Activation;
use crate::bytecode::{CodePointer, Op};
use crate::mem::Ref;
use std::any::Any;
use std::collections::HashMap;
use std::hash::Hash;
use sunny_scm::{Scm, ScmHasher, ScmObject};

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<closure {:p}>", self)
    }
}

impl Closure {
    pub fn new_procedure(code: CodePointer) -> Self {
        Closure { parent: None, code }
    }

    pub fn arity(&self) -> (usize, Option<usize>) {
        let mut n_args = 0;
        loop {
            match self.code.peek() {
                Op::ExtArg(n) => n_args = Op::extend_arg(n, n_args),
                Op::PrepareArgs(n) => {
                    n_args = Op::extend_arg(n, n_args);
                    return (n_args, Some(n_args));
                }
                Op::PrepareVarArgs(n) => {
                    n_args = Op::extend_arg(n, n_args);
                    return (n_args, None);
                }
                op => panic!("Closure unexpectedly starts with {}", op),
            }
        }
    }
}

impl ScmObject for Closure {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| std::ptr::eq(self, other))
            .unwrap_or(false)
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(self, state);
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.code.hash(state);
        self.parent.hash(state);
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}
