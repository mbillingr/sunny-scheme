use crate::closure::ClosureBuilder;
use crate::code::CodePointer;
use crate::{
    closure::Closure,
    code::Op,
    mem::{GarbageCollector, Ref, Traceable},
    storage::ValueStorage,
    Error, Result, Value,
};

#[derive(Clone)] // todo: should be Copy, but Ref is not Copy yet
struct CallStackFrame {
    free_vars: Ref<Box<[Value]>>,
    args: Box<[Value]>,
    code: CodePointer,
}

impl Traceable for CallStackFrame {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.free_vars.trace(gc);
        self.args.trace(gc);
        self.code.trace(gc);
    }
}

pub struct Vm {
    storage: ValueStorage,
    value_stack: Vec<Value>,
    call_stack: Vec<CallStackFrame>,

    current_frame: CallStackFrame,

    // constants
    empty_value_array: Ref<Box<[Value]>>,
}

impl Traceable for Vm {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.value_stack.trace(gc);
        self.call_stack.trace(gc);
        self.current_frame.trace(gc);
        self.empty_value_array.trace(gc);
    }
}

impl Vm {
    pub fn collect_garbage(&mut self) {
        unsafe {
            // This is safe if all state values (registers, etc.) are marked as roots
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

        let no_values = storage
            .insert(vec![].into_boxed_slice())
            .map_err(|_| Error::AllocationError)?;

        Ok(Vm {
            storage,
            value_stack: vec![],
            call_stack: vec![],
            current_frame: CallStackFrame {
                free_vars: closure.free_vars.clone(),
                args: vec![].into_boxed_slice(),
                code: closure.code.clone(),
            },
            empty_value_array: no_values,
        })
    }

    pub fn eval(&mut self, code: CodePointer) -> Result<Value> {
        let closure = Closure {
            code,
            free_vars: self.empty_value_array.clone(),
        };
        self.eval_closure(&closure)
    }

    pub fn eval_closure(&mut self, closure: &Closure) -> Result<Value> {
        self.load_closure(closure);
        self.eval_gc()
    }

    fn load_closure(&mut self, closure: &Closure) {
        self.current_frame = CallStackFrame {
            free_vars: closure.free_vars.clone(),
            args: vec![].into_boxed_slice(),
            code: closure.code.clone(),
        };
    }

    fn eval_gc(&mut self) -> Result<Value> {
        loop {
            match self.eval_loop() {
                Ok(ret_val) => return Ok(ret_val),
                Err(Error::AllocationError) => {
                    self.collect_garbage();
                }
                Err(e) => return Err(e),
            };

            // Retry last instruction, which triggered the error
            self.current_frame.code.step_back()
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
                Op::Integer(a) => self.push_value(Value::Int(extend_arg(a, arg) as i64)),
                Op::Const(a) => {
                    let c = self.fetch_constant(extend_arg(a, arg));
                    self.push_value(c);
                }
                Op::Cons => self.cons()?,
                Op::MakeClosure(a) => self.make_closure(extend_arg(a, arg))?,
            }
            arg = 0;
        }
    }

    fn fetch_op(&mut self) -> Op {
        self.current_frame.code.fetch()
    }

    fn fetch_constant(&mut self, index: usize) -> Value {
        self.current_frame.code.get_constant(index).clone()
    }

    fn push_value(&mut self, val: Value) {
        self.value_stack.push(val);
    }

    fn pop_value(&mut self) -> Result<Value> {
        self.value_stack.pop().ok_or(Error::StackUnderflow)
    }

    fn cons(&mut self) -> Result<()> {
        if self.storage.free() < 1 {
            return Err(Error::AllocationError);
        }

        let cdr = self.pop_value()?;
        let car = self.pop_value()?;
        let pair = self.storage.cons(car, cdr).unwrap();
        self.push_value(pair);
        Ok(())
    }

    fn make_closure(&mut self, n_free: usize) -> Result<()> {
        if self.storage.free() < 2 {
            return Err(Error::AllocationError);
        }

        let code_offset = self.pop_value()?.as_int().ok_or(Error::TypeError)? as isize;
        let code = self.current_frame.code.offset(code_offset);

        let mut free_vars = Vec::with_capacity(n_free);
        for _ in 0..n_free {
            free_vars.push(self.pop_value()?);
        }
        let free_vars = self.storage.insert(free_vars.into_boxed_slice()).unwrap();

        let closure = Closure { code, free_vars };
        let closure = self.storage.insert(closure).unwrap();

        self.push_value(Value::Closure(closure));
        Ok(())
    }
}

#[inline(always)]
fn extend_arg(a: u8, arg: usize) -> usize {
    a as usize + (arg << 8)
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
            let additional_capacity = 2 + 3; // required by the vm's default closure and by the closure being run
            let mut storage = ValueStorage::new(self.storage_capacity + additional_capacity);

            let closure = cb.build(&mut storage).unwrap();

            let mut vm = Vm::new(storage).unwrap();

            if let Some(value_stack) = self.value_stack {
                vm.value_stack = value_stack;
            }

            vm.load_closure(&closure);
            let ret = vm.eval_loop();
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
    fn op_extarg_allows_large_arguments() {
        let (_, vm) = VmRunner::new().run_closure(
            ClosureBuilder::new()
                .op(Op::ExtArg(0x01))
                .op(Op::ExtArg(0x23))
                .op(Op::ExtArg(0x45))
                .op(Op::ExtArg(0x67))
                .op(Op::ExtArg(0x89))
                .op(Op::ExtArg(0xab))
                .op(Op::ExtArg(0xcd))
                .op(Op::Integer(0xef))
                .op(Op::Halt),
        );

        assert_eq!(vm.value_stack, vec![Value::Int(0x0123456789abcdef)]);
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
    fn op_cons_preserves_state_on_allocation_error() {
        let (ret, vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Value::Int(0), Value::Nil])
            .run_closure(ClosureBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(ret, Err(Error::AllocationError));
        assert_eq!(vm.value_stack, vec![Value::Int(0), Value::Nil]);
    }

    #[test]
    fn op_make_closure_pops_n_free_vars_and_pushes_closure() {
        let (_, mut vm) = VmRunner::new()
            .with_value_stack(vec![Value::Int(2), Value::Int(1), Value::Int(0)])
            .run_closure(ClosureBuilder::new().op(Op::MakeClosure(2)).op(Op::Halt));

        let closure = vm.value_stack.pop().unwrap();
        let closure = closure.as_closure().unwrap();

        assert!(vm.value_stack.is_empty());

        assert_eq!(&**closure.free_vars, vec![Value::Int(1), Value::Int(2)]);
        assert_eq!(closure.code.clone().fetch(), Op::Halt);
    }

    #[test]
    fn op_make_closure_preserves_state_on_memory_error() {
        let (ret, vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Value::Int(0), Value::Int(0)])
            .run_closure(ClosureBuilder::new().op(Op::MakeClosure(1)).op(Op::Halt));

        assert_eq!(ret, Err(Error::AllocationError));
        assert_eq!(vm.value_stack, vec![Value::Int(0), Value::Int(0)]);
    }
}