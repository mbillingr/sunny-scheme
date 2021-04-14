use crate::bytecode::CodePointer;
use crate::closure::Closure;
use crate::mem::Ref;
use crate::scm_extension::ScmExt;
use sunny_sexpr_parser::Scm;

#[derive(Clone)]
pub struct Activation {
    pub(crate) caller: Option<Ref<Activation>>,
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
    pub(crate) locals: Vec<Scm>,
}

impl std::fmt::Debug for Activation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<activation record @ {:p}>", self)
    }
}

impl Activation {
    pub fn from_closure(caller: Ref<Activation>, cls: &Closure, args: Vec<Scm>) -> Self {
        let tmp = std::mem::ManuallyDrop::new(args);
        let args = unsafe { Vec::from_raw_parts(tmp.as_ptr() as _, tmp.len(), tmp.capacity()) };

        Activation {
            caller: Some(caller),
            parent: cls.parent.clone(),
            code: cls.code.clone(),
            locals: args,
        }
    }

    pub fn duplicate(&self) -> Self {
        self.clone()
    }

    pub fn push_local(&mut self, value: Scm) {
        self.locals.push(value)
    }

    pub fn pop_local(&mut self) -> Option<Scm> {
        self.locals.pop()
    }

    pub fn get_local(&self, idx: usize) -> &Scm {
        &self.locals[idx]
    }

    pub fn set_local(&mut self, idx: usize, value: Scm) {
        if idx >= self.locals.len() {
            self.locals.resize(idx + 1, Scm::void());
        }
        self.locals[idx] = value
    }
}
