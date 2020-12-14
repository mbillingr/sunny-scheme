use crate::{
    mem::{GarbageCollector, Ref, Traceable},
    storage::ValueStorage,
    Value,
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    StackUnderflow,
    AllocationError,
    NotCallable,
    Halted,
}

#[derive(Debug, Clone)]
pub struct Closure {
    code: Box<[Op]>,
    constants: Box<[Value]>,
    free_vars: Box<[Value]>,
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

    Halt,
    Return,

    Const(u8),

    Cons,
}

#[derive(Clone)] // todo: should be Copy, but Ref is not Copy yet
struct CallStackFrame {
    closure: Ref<Closure>,
    ip: usize,
}

impl Traceable for CallStackFrame {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.closure.trace(gc);
    }
}

pub struct Vm {
    storage: ValueStorage,
    value_stack: Vec<Value>,
    call_stack: Vec<CallStackFrame>,

    current_frame: CallStackFrame,
}

impl Traceable for Vm {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.value_stack.trace(gc);
        self.call_stack.trace(gc);
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
                .mark(&self.call_stack)
                .mark(&self.current_frame)
                .sweep()
        }
    }

    pub fn new(mut storage: ValueStorage) -> Result<Self> {
        let closure = storage
            .insert(Closure {
                code: vec![Op::Halt].into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            })
            .map_err(|_| Error::AllocationError)?;

        Ok(Vm {
            storage,
            value_stack: vec![],
            call_stack: vec![],
            current_frame: CallStackFrame { closure, ip: 0 },
        })
    }

    fn eval(&mut self, closure: Ref<Closure>) -> Result<Value> {
        self.current_frame = CallStackFrame { closure, ip: 0 };

        loop {
            match self.eval_loop() {
                Ok(ret_val) => return Ok(ret_val),
                Err(e @ Error::AllocationError) => e,
                Err(e) => return Err(e),
            };

            // Retry last instruction, which triggered the allocation error
            self.current_frame.ip -= 1;
        }
    }

    fn eval_loop(&mut self) -> Result<Value> {
        let mut arg: usize = 0;
        loop {
            match self.fetch_op() {
                Op::Nop => {}
                Op::ExtArg(a) => {
                    arg = extend_arg(a, arg);
                    continue;
                }
                Op::Halt => return Err(Error::Halted),
                Op::Return => {
                    if let Some(frame) = self.call_stack.pop() {
                        self.current_frame = frame;
                    } else {
                        let ret_val = self.pop_value()?;
                        return Ok(ret_val);
                    }
                }
                Op::Const(a) => {
                    let c = self.fetch_constant(extend_arg(a, arg));
                    self.push_value(c);
                }
                Op::Cons => self.cons()?,
            }
            arg = 0;
        }
    }

    fn fetch_op(&mut self) -> Op {
        let op = self.current_frame.closure.code[self.current_frame.ip];
        self.current_frame.ip += 1;
        op
    }

    fn fetch_constant(&mut self, index: usize) -> Value {
        self.current_frame.closure.constants[index].clone()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_program_halts_immediately() {
        let mut storage = ValueStorage::new(1);
        let mut vm = Vm::new(storage).unwrap();
        let ret = vm.eval_loop();
        assert_eq!(ret, Err(Error::Halted));
    }

    #[test]
    fn evaluating_a_closure_returns_value() {
        let mut storage = ValueStorage::new(2);

        let run_closure = storage
            .insert(Closure {
                code: vec![Op::Const(0), Op::Return].into_boxed_slice(),
                constants: vec![Value::Int(123)].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            })
            .unwrap();

        let mut vm = Vm::new(storage).unwrap();
        let ret = vm.eval(run_closure).unwrap();

        assert_eq!(ret, Value::Int(123));
    }
}
