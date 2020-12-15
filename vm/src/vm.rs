use crate::closure::ClosureBuilder;
use crate::{
    closure::Closure,
    mem::{GarbageCollector, Ref, Traceable},
    opcode::Op,
    storage::ValueStorage,
    Error, Result, Value,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodePointer {
    segment: Ref<Box<[Op]>>,
    position: usize,
}

impl Traceable for CodePointer {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.segment.trace(gc);
    }
}

impl CodePointer {
    pub fn new(segment: Ref<Box<[Op]>>) -> Self {
        CodePointer {
            segment,
            position: 0,
        }
    }

    pub fn at(self, position: usize) -> Self {
        CodePointer { position, ..self }
    }

    pub fn fetch(&mut self) -> Op {
        let op = self.segment[self.position];
        self.position += 1;
        op
    }

    pub fn step_back(&mut self) {
        self.position -= 1;
    }

    pub fn as_slice(&self) -> &[Op] {
        &self.segment[self.position..]
    }
}

#[derive(Clone)] // todo: should be Copy, but Ref is not Copy yet
struct CallStackFrame {
    closure: Closure,
    ip: CodePointer,
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
                .sweep();
            self.storage.grow();
        }
    }

    pub fn new(mut storage: ValueStorage) -> Result<Self> {
        let closure = ClosureBuilder::new()
            .op(Op::Halt)
            .build(&mut storage)
            .map_err(|_| Error::AllocationError)?;

        /*let closure = storage
        .insert(ClosureBuilder::new().op(Op::Halt).build())
        .map_err(|_| Error::AllocationError)?;*/

        Ok(Vm {
            storage,
            value_stack: vec![],
            call_stack: vec![],
            current_frame: CallStackFrame {
                ip: closure.code.clone(),
                closure,
            },
        })
    }

    pub fn eval(&mut self, closure: Closure) -> Result<Value> {
        self.current_frame = CallStackFrame {
            ip: closure.code.clone(),
            closure,
        };

        loop {
            match self.eval_loop() {
                Ok(ret_val) => return Ok(ret_val),
                Err(Error::AllocationError) => {
                    self.collect_garbage();
                }
                Err(e) => return Err(e),
            };

            // Retry last instruction, which triggered the error
            self.current_frame.ip.step_back()
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
                Op::Integer(a) => {
                    //self.push_value(Value::Int(extend_arg(a, arg) as i64))
                    self.push_value(Value::Int(a as i64))
                }
                Op::Const(a) => {
                    //let c = self.fetch_constant(extend_arg(a, arg));
                    let c = self.fetch_constant(a as usize);
                    self.push_value(c);
                }
                Op::Cons => self.cons()?,
            }
            arg = 0;
        }
    }

    fn fetch_op(&mut self) -> Op {
        self.current_frame.ip.fetch()
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
    a as usize + arg << 8 as usize
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::closure::ClosureBuilder;

    struct VmRunner {
        storage_capacity: usize,
        value_stack: Option<Vec<Value>>,
    }

    impl VmRunner {
        fn new() -> Self {
            VmRunner {
                storage_capacity: 1024,
                value_stack: None,
            }
        }

        fn with_capacity(self, capacity: usize) -> Self {
            VmRunner {
                storage_capacity: capacity,
                ..self
            }
        }

        fn with_value_stack(self, values: Vec<Value>) -> Self {
            VmRunner {
                value_stack: Some(values),
                ..self
            }
        }

        fn run_closure(self, cb: ClosureBuilder) -> (Result<Value>, Vm) {
            let additional_capacity = 3 + 3; // required by the vm's default closure and by the closure being run
            let mut storage = ValueStorage::new(self.storage_capacity + additional_capacity);

            let closure = cb.build(&mut storage).unwrap();

            let mut vm = Vm::new(storage).unwrap();

            if let Some(value_stack) = self.value_stack {
                vm.value_stack = value_stack;
            }

            let ret = vm.eval(closure);
            (ret, vm)
        }
    }

    #[test]
    fn default_program_halts_immediately() {
        let storage = ValueStorage::new(1024);
        let mut vm = Vm::new(storage).unwrap();
        let ret = vm.eval_loop();
        assert_eq!(ret, Err(Error::Halted));
    }

    #[test]
    fn op_nop_does_nothing() {
        let (_, vm) = VmRunner::new().run_closure(ClosureBuilder::new().op(Op::Nop).op(Op::Halt));

        assert!(vm.call_stack.is_empty());
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn no_instructions_executed_after_op_halt() {
        let (_, vm) =
            VmRunner::new().run_closure(ClosureBuilder::new().op(Op::Halt).op(Op::Integer(0)));
        assert!(vm.call_stack.is_empty());
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_return_pops_value_stack() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Value::Int(0), Value::Int(1)])
            .run_closure(ClosureBuilder::new().op(Op::Return));

        assert_eq!(ret, Ok(Value::Int(1)));
        assert_eq!(vm.value_stack, vec![Value::Int(0)]);
    }

    #[test]
    fn op_integer_pushes_int_value() {
        let (_, vm) =
            VmRunner::new().run_closure(ClosureBuilder::new().op(Op::Integer(123)).op(Op::Halt));

        assert_eq!(vm.value_stack, vec![Value::Int(123)]);
    }

    #[test]
    fn op_const_pushes_constant() {
        let (_, vm) = VmRunner::new().run_closure(
            ClosureBuilder::new()
                .constant(Value::Void)
                .constant(Value::Nil)
                .constant(Value::Int(0))
                .op(Op::Halt),
        );

        assert_eq!(vm.value_stack, vec![Value::Void, Value::Nil, Value::Int(0)]);
    }

    #[test]
    fn op_cons_pops_two_values_and_pushes_pair() {
        let (_, vm) = VmRunner::new()
            .with_value_stack(vec![Value::Int(0), Value::Nil])
            .run_closure(ClosureBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(vm.value_stack.last().unwrap(), &[Value::Int(0), Value::Nil]);
    }

    #[test]
    fn op_cons_can_trigger_garbage_collection() {
        let (_, vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Value::Int(0), Value::Nil])
            .run_closure(ClosureBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(vm.value_stack.last().unwrap(), &[Value::Int(0), Value::Nil]);
    }
}
