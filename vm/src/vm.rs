use crate::{
    mem::{GarbageCollector, Traceable},
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
}

impl Vm {
    fn eval(&mut self, closure: Value) -> Result<()> {
        let cls = closure.as_closure().ok_or(Error::NotCallable)?;
        let code = &cls.code;
        let constants = &cls.constants;

        let mut ip = 0;
        let mut arg: usize = 0;

        /// inner loop
        let mut runner = |this: &mut Self, ip: &mut usize| -> Result<()> {
            loop {
                let op = code[*ip];
                *ip += 1;

                match op {
                    Op::Nop => {}
                    Op::ExtArg(a) => {
                        arg = extend_arg(a, arg);
                        continue;
                    }
                    Op::Const(a) => this.push_value(constants[extend_arg(a, arg)].clone()),
                    Op::Cons => this.cons()?,
                }

                arg = 0;
            }
        };

        /// outer loop, may recover from errors and restart inner loop
        loop {
            let err = match runner(self, &mut ip) {
                Ok(()) => return Ok(()),
                Err(e @ Error::AllocationError) => e,
                Err(e) => return Err(e),
            };

            self.push_value(closure);

            /// This is safe if all state values (registers, etc.) are pushed on the
            /// value stack so they are not collected.
            unsafe {
                self.storage.collect_garbage(&self.value_stack)
            }

            closure = self.pop_value().unwrap();

            // Retry last instruction, which triggered the allocation error
            ip -= 1;
        }
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
