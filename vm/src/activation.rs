use crate::bytecode::CodePointer;
use crate::closure::Closure;
use crate::mem::Ref;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use sunny_scm::{Scm, ScmHasher, ScmObject};

#[derive(Clone)]
pub struct Activation {
    caller: Option<Ref<Activation>>,
    parent: Option<Ref<Activation>>,
    return_addr: CodePointer,
    locals: Ref<RefCell<Vec<Scm>>>,
}

impl std::fmt::Debug for Activation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<activation record @ {:p}> -> {:?}", self, self.caller)
    }
}

impl std::fmt::Display for Activation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}>", self)
    }
}

impl Activation {
    pub fn new(
        caller: Option<Ref<Activation>>,
        parent: Option<Ref<Activation>>,
        return_addr: CodePointer,
        locals: Vec<Scm>,
    ) -> Self {
        Activation {
            caller,
            parent,
            return_addr,
            locals: Ref::new(RefCell::new(locals)),
        }
    }

    pub fn from_closure(
        caller: Option<Ref<Activation>>,
        return_addr: CodePointer,
        cls: &Closure,
        args: Vec<Scm>,
    ) -> Self {
        let tmp = std::mem::ManuallyDrop::new(args);
        let args = unsafe { Vec::from_raw_parts(tmp.as_ptr() as _, tmp.len(), tmp.capacity()) };

        Activation {
            caller,
            parent: cls.parent.clone(),
            return_addr,
            locals: Ref::new(RefCell::new(args)),
        }
    }

    pub fn clone_caller(&self) -> Option<Ref<Activation>> {
        self.caller.clone()
    }

    pub fn parent(&self) -> Option<&Activation> {
        self.parent.as_deref()
    }

    pub fn return_addr(&self) -> &CodePointer {
        &self.return_addr
    }

    pub fn push_local(&self, value: Scm) {
        self.locals.borrow_mut().push(value)
    }

    pub fn pop_local(&self) -> Option<Scm> {
        self.locals.borrow_mut().pop()
    }

    pub fn get_local(&self, idx: usize) -> Scm {
        self.locals.borrow_mut()[idx].clone()
    }

    pub fn set_local(&self, idx: usize, value: Scm) {
        let mut locals = self.locals.borrow_mut();
        if idx >= locals.len() {
            locals.resize(idx + 1, Scm::void());
        }
        locals[idx] = value
    }

    pub fn n_locals(&self) -> usize {
        self.locals.borrow().len()
    }
}

impl ScmObject for Activation {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, _other: &dyn ScmObject) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| std::ptr::eq(self, other))
            .unwrap_or(false)
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(self, state);
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(self, state);
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}
