use crate::bytecode::CodePointer;
use crate::closure::Closure;
use crate::mem::Ref;
use crate::Value;
use std::cell::Cell;

pub struct Activation {
    pub(crate) caller: Option<Ref<Activation>>,
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
    pub(crate) locals: Vec<Cell<Value>>,
}

impl std::fmt::Debug for Activation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<activation record @ {:p}>", self)
    }
}

impl Activation {
    pub fn from_closure(caller: Ref<Activation>, cls: &Closure, args: Vec<Value>) -> Self {
        let tmp = std::mem::ManuallyDrop::new(args);
        let args = unsafe { Vec::from_raw_parts(tmp.as_ptr() as _, tmp.len(), tmp.capacity()) };

        Activation {
            caller: Some(caller),
            parent: cls.parent.clone(),
            code: cls.code.clone(),
            locals: args.into(),
        }
    }

    pub fn duplicate(&self) -> Self {
        Activation {
            caller: self.caller.clone(),
            parent: self.parent.clone(),
            code: self.code.clone(),
            locals: self
                .locals
                .iter()
                .map(|cv| {
                    let val = cv.take();
                    cv.set(val.clone());
                    Cell::new(val)
                })
                .collect(),
        }
    }
}
