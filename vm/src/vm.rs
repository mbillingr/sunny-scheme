use crate::{
    mem::{GarbageCollector, Ref, Traceable},
    storage::ValueStorage,
    Value,
};

pub type Result<T> = std::result::Result<T, Error>;

pub enum Error {
    StackUnderflow,
    AllocationError,
    NotCallable,
}

#[derive(Debug, Clone)]
pub struct Closure {
    code: Vec<Op>,
    constants: Vec<Value>,
    free_vars: Vec<Value>,
}

impl Traceable for Closure {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.constants.trace(gc);
        self.free_vars.trace(gc);
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(u16)]
pub enum Op {
    Nop,
    ExtArg(u8),

    Const(u8),

    Cons,
}

pub struct Vm {
    storage: ValueStorage,
    value_stack: Vec<Value>,
    current_closure: Ref<Closure>,

    ip: usize,
}

impl Traceable for Vm {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.value_stack.trace(gc);
        self.current_closure.trace(gc);
    }
}

impl Vm {
    pub fn collect_garbage(&mut self) {
        unsafe {
            // This is safe if all state values (registers, etc.) are accessible through
            // the VM struct.
            self.storage
                .begin_garbage_collection()
                .mark(&self.value_stack)
                .mark(&self.current_closure)
                .sweep()
        }
    }

    fn eval(&mut self, closure: Ref<Closure>) -> Result<()> {
        self.current_closure = closure;
        self.ip = 0;

        loop {
            match self.eval_next_op() {
                Ok(()) => return Ok(()),
                Err(e @ Error::AllocationError) => e,
                Err(e) => return Err(e),
            };

            // Retry last instruction, which triggered the allocation error
            self.ip -= 1;
        }
    }

    fn eval_next_op(&mut self) -> Result<()> {
        let mut arg: usize = 0;
        loop {
            match self.fetch_op() {
                Op::Nop => {}
                Op::ExtArg(a) => {
                    arg = extend_arg(a, arg);
                    continue;
                }
                Op::Const(a) => {
                    let c = self.fetch_constant(extend_arg(a, arg));
                    self.push_value(c);
                }
                Op::Cons => self.cons()?,
            }
            break;
        }
        Ok(())
    }

    fn fetch_op(&mut self) -> Op {
        let op = self.current_closure.code[self.ip];
        self.ip += 1;
        op
    }

    fn fetch_constant(&mut self, index: usize) -> Value {
        self.current_closure.constants[index].clone()
    }

    fn push_value(&mut self, val: Value) {
        self.value_stack.push(val);
    }

    fn pop_value(&mut self) -> Result<Value> {
        self.value_stack.pop().ok_or(Error::StackUnderflow)
    }

    fn cons(&mut self) -> Result<()> {
        let cdr = self.pop_value()?;
        let car = self.pop_value()?;
        match self.storage.cons(car, cdr) {
            Ok(pair) => Ok(self.push_value(pair)),
            Err((car, cdr)) => {
                self.push_value(car);
                self.push_value(cdr);
                Err(Error::AllocationError)
            }
        }
    }
}

#[inline(always)]
fn extend_arg(a: u8, arg: usize) -> usize {
    a as usize + arg * u8::max_value() as usize
}
