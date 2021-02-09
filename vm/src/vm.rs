use crate::bytecode::{CodeBuilder, CodePointer};
use crate::{
    bytecode::Op,
    closure::Closure,
    mem::{Ref, Traceable, Tracer},
    storage::ValueStorage,
    Error, ErrorKind, Result, RuntimeResult, Value,
};

#[derive(Debug, Clone)] // todo: should be Copy, but Ref is not Copy yet
struct CallStackFrame {
    free_vars: Ref<Box<[Value]>>,
    args: Box<[Value]>,
    code: CodePointer,
}

impl Traceable for CallStackFrame {
    fn trace(&self, gc: &mut Tracer) {
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
    fn trace(&self, gc: &mut Tracer) {
        self.value_stack.trace(gc);
        self.call_stack.trace(gc);
        self.current_frame.trace(gc);
        self.empty_value_array.trace(gc);
    }
}

impl Vm {
    pub fn new(mut storage: ValueStorage) -> Result<Self> {
        storage.ensure(2);
        let empty_value_array = storage.insert(vec![].into_boxed_slice()).unwrap();

        let code_segment = CodeBuilder::new().op(Op::Halt).build().unwrap();
        let code_ptr = CodePointer::new(storage.insert(code_segment).unwrap());

        let closure = Closure {
            code: code_ptr,
            free_vars: empty_value_array.clone(),
        };

        Ok(Vm {
            storage,
            value_stack: vec![],
            call_stack: vec![],
            current_frame: CallStackFrame {
                free_vars: closure.free_vars.clone(),
                args: vec![].into_boxed_slice(),
                code: closure.code.clone(),
            },
            empty_value_array,
        })
    }

    pub fn eval(&mut self, code: CodePointer) -> RuntimeResult<Value> {
        let closure = Closure {
            code,
            free_vars: self.empty_value_array.clone(),
        };
        self.eval_closure(&closure)
    }

    pub fn eval_closure(&mut self, closure: &Closure) -> RuntimeResult<Value> {
        self.load_closure(closure);
        self.run().map_err(|e| self.add_error_context(e))
    }

    fn load_closure(&mut self, closure: &Closure) {
        self.current_frame = CallStackFrame {
            free_vars: closure.free_vars.clone(),
            args: vec![].into_boxed_slice(),
            code: closure.code.clone(),
        };
    }

    fn add_error_context(&mut self, kind: ErrorKind) -> Error {
        Error {
            kind,
            location: self.current_frame.code.offset(-1),
        }
    }

    fn run(&mut self) -> Result<Value> {
        let mut arg: usize = 0;
        loop {
            match self.fetch_op() {
                Op::Nop => {}
                Op::ExtArg(a) => {
                    arg = extend_arg(a, arg);
                    continue;
                }
                Op::Halt => return Err(ErrorKind::Halted),
                Op::Jump { forward } => self.jump(extend_arg(forward, arg)),
                Op::JumpIfTrue { forward } => self.jump_if_true(extend_arg(forward, arg))?,
                Op::JumpIfVoid { forward } => self.jump_if_void(extend_arg(forward, arg))?,
                Op::Return => {
                    if let Some(frame) = self.call_stack.pop() {
                        self.current_frame = frame;
                    } else {
                        let ret_val = self.pop_value()?;
                        return Ok(ret_val);
                    }
                }
                Op::Call { n_args } => self.call(extend_arg(n_args, arg))?,
                Op::Integer(a) => self.push_value(Value::Int(extend_arg(a, arg) as i64)),
                Op::Const(a) => self.push_const(extend_arg(a, arg))?,
                Op::GetArg(a) => self.push_arg(extend_arg(a, arg))?,
                Op::GetFree(a) => self.push_free(extend_arg(a, arg))?,
                Op::GetStack(a) => self.push_from_stack(extend_arg(a, arg))?,
                Op::Eq => self.eq()?,
                Op::Inc => self.inc()?,
                Op::Dec => self.dec()?,
                Op::Cons => self.cons()?,
                Op::Car => self.car()?,
                Op::Cdr => self.cdr()?,
                Op::Table => self.table()?,
                Op::TableSet => self.table_set()?,
                Op::TableGet => self.table_get()?,
                Op::MakeClosure { n_free } => self.make_closure(extend_arg(n_free, arg))?,
            }
            arg = 0;
        }
    }

    fn fetch_op(&mut self) -> Op {
        self.current_frame.code.fetch()
    }

    fn push_value(&mut self, val: Value) {
        //println!("pushing {:?}", val);
        self.value_stack.push(val);
    }

    fn pop_value(&mut self) -> Result<Value> {
        //println!("popping");
        self.value_stack.pop().ok_or(ErrorKind::StackUnderflow)
        //self.value_stack.pop().ok_or_else(||panic!())
    }

    fn pop_int(&mut self) -> Result<i64> {
        self.pop_value()?.as_int().ok_or(ErrorKind::TypeError)
    }

    fn pop_values(&mut self, n: usize) -> Result<Box<[Value]>> {
        let mut values = Vec::with_capacity(n);
        for _ in 0..n {
            values.push(self.pop_value()?);
        }
        Ok(values.into_boxed_slice())
    }

    fn push_const(&mut self, idx: usize) -> Result<()> {
        let x = self.current_frame.code.get_constant(idx).clone();
        self.push_value(x);
        Ok(())
    }

    fn push_arg(&mut self, idx: usize) -> Result<()> {
        let x = self.current_frame.args[idx].clone();
        self.push_value(x);
        Ok(())
    }

    fn push_free(&mut self, idx: usize) -> Result<()> {
        let x = self.current_frame.free_vars[idx].clone();
        self.push_value(x);
        Ok(())
    }

    fn push_from_stack(&mut self, idx: usize) -> Result<()> {
        let x = self.value_stack[idx].clone();
        self.push_value(x);
        Ok(())
    }

    fn jump_if_true(&mut self, forward: usize) -> Result<()> {
        let condition = self.pop_value()?;
        if condition.is_like_true() {
            self.jump(forward);
        }
        Ok(())
    }

    fn jump_if_void(&mut self, forward: usize) -> Result<()> {
        let condition = self.pop_value()?;
        if condition.is_void() {
            self.jump(forward);
        } else {
            self.push_value(condition)
        }
        Ok(())
    }

    fn jump(&mut self, forward: usize) {
        self.current_frame.code.step_forward(forward)
    }

    fn call(&mut self, n_args: usize) -> Result<()> {
        let func = self.pop_value()?;
        match func {
            Value::Closure(cls) => self.call_closure(cls, n_args),
            Value::Primitive(f) => match f(&mut self.value_stack, &mut self.storage) {
                Ok(()) => Ok(()),
                Err(e) => {
                    self.push_value(func);
                    Err(e)
                }
            },
            _ => Err(ErrorKind::TypeError),
        }
    }

    fn call_closure(&mut self, cls: Ref<Closure>, n_args: usize) -> Result<()> {
        let args = self.pop_values(n_args)?;
        let frame = CallStackFrame {
            code: cls.code.clone(),
            free_vars: cls.free_vars.clone(),
            args,
        };
        let old_frame = std::mem::replace(&mut self.current_frame, frame);
        self.call_stack.push(old_frame);
        Ok(())
    }

    fn eq(&mut self) -> Result<()> {
        let a = self.pop_value()?;
        let b = self.pop_value()?;
        self.push_value(Value::bool(a.eq(&b)));
        Ok(())
    }

    fn inc(&mut self) -> Result<()> {
        let x = self.pop_int()?;
        self.push_value(Value::Int(x + 1));
        Ok(())
    }

    fn dec(&mut self) -> Result<()> {
        unimplemented!()
    }

    fn cons(&mut self) -> Result<()> {
        self.ensure_storage_space(1)?;
        let cdr = self.pop_value()?;
        let car = self.pop_value()?;
        let pair = self.storage.cons(car, cdr).unwrap();
        self.push_value(pair);
        Ok(())
    }

    fn car(&mut self) -> Result<()> {
        self.pop_value()?
            .car()
            .ok_or(ErrorKind::TypeError)
            .map(|x| self.push_value(x.clone()))
    }

    fn cdr(&mut self) -> Result<()> {
        self.pop_value()?
            .cdr()
            .ok_or(ErrorKind::TypeError)
            .map(|x| self.push_value(x.clone()))
    }

    fn table(&mut self) -> Result<()> {
        self.ensure_storage_space(1)?;
        let table = self.storage.new_table().unwrap();
        self.push_value(table);
        Ok(())
    }

    fn table_set(&mut self) -> Result<()> {
        let value = self.pop_value()?;
        let field = self.pop_value()?;
        let mut table = self.pop_value()?;
        table.table_set(field, value).ok_or(ErrorKind::TypeError)?;
        self.push_value(table);
        Ok(())
    }

    fn table_get(&mut self) -> Result<()> {
        let field = self.pop_value()?;
        let table = self.pop_value()?;
        table
            .table_get(&field)
            .ok_or(ErrorKind::TypeError)
            .map(|x| self.push_value(x.clone()))
    }

    fn make_closure(&mut self, n_free: usize) -> Result<()> {
        self.ensure_storage_space(2)?;

        let code_offset = self.pop_value()?.as_int().ok_or(ErrorKind::TypeError)? as isize;
        let code = self.current_frame.code.offset(code_offset);

        let free_vars = self.pop_values(n_free)?;
        let free_vars = self.storage.insert(free_vars).unwrap();

        let closure = Closure { code, free_vars };
        let closure = self.storage.insert(closure).unwrap();

        self.push_value(Value::Closure(closure));
        Ok(())
    }

    fn ensure_storage_space(&mut self, n_objects: usize) -> Result<()> {
        if self.storage.free() < n_objects {
            self.collect_garbage();
            self.storage.ensure(n_objects);
        }
        Ok(())
    }

    fn collect_garbage(&mut self) {
        unsafe {
            // This is safe if all state values (registers, etc.) are marked as roots
            let gc = self.storage.begin_garbage_collection().mark(self);
            self.storage.finish_garbage_collection(gc);
        }
    }
}

#[inline(always)]
fn extend_arg(a: u8, arg: usize) -> usize {
    a as usize + (arg << 8)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::CodeBuilder;
    use maplit::hashmap;

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

        fn run_code(self, cb: CodeBuilder) -> (Result<Value>, Vm) {
            let mut vm = self.prepare_vm(cb);
            let ret = vm.run();
            (ret, vm)
        }

        fn prepare_vm(self, cb: CodeBuilder) -> Vm {
            let additional_capacity = 2 + 2; // required by the vm's default closure and by the closure being run
            let mut storage = ValueStorage::new(self.storage_capacity + additional_capacity);

            let code_segment = cb.build().unwrap();
            let code_ptr = CodePointer::new(storage.insert(code_segment).unwrap());

            let closure = Closure {
                code: code_ptr,
                free_vars: storage.insert(vec![].into_boxed_slice()).unwrap(),
            };

            let mut vm = Vm::new(storage).unwrap();

            if let Some(value_stack) = self.value_stack {
                vm.value_stack = value_stack;
            }

            vm.load_closure(&closure);
            vm
        }
    }

    #[test]
    fn default_program_halts_immediately() {
        let storage = ValueStorage::new(1024);
        let mut vm = Vm::new(storage).unwrap();
        let ret = vm.run();
        assert_eq!(ret, Err(ErrorKind::Halted));
    }

    #[test]
    fn op_nop_does_nothing() {
        let (_, vm) = VmRunner::new().run_code(CodeBuilder::new().op(Op::Nop).op(Op::Halt));

        assert!(vm.call_stack.is_empty());
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn no_instructions_executed_after_op_halt() {
        let (_, vm) = VmRunner::new().run_code(CodeBuilder::new().op(Op::Halt).op(Op::Integer(0)));
        assert!(vm.call_stack.is_empty());
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_return_from_toplevel_pops_value_stack() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Value::Int(0), Value::Int(1)])
            .run_code(CodeBuilder::new().op(Op::Return));

        assert_eq!(ret, Ok(Value::Int(1)));
        assert_eq!(vm.value_stack, vec![Value::Int(0)]);
    }

    #[test]
    fn op_extarg_allows_large_arguments() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
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
            VmRunner::new().run_code(CodeBuilder::new().op(Op::Integer(123)).op(Op::Halt));

        assert_eq!(vm.value_stack, vec![Value::Int(123)]);
    }

    #[test]
    fn op_const_pushes_constant() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
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
            .run_code(CodeBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(vm.value_stack.last().unwrap(), &(Value::Int(0), Value::Nil));
    }

    #[test]
    fn op_cons_succeeds_even_when_storage_is_full() {
        let (_, vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Value::Int(0), Value::Nil])
            .run_code(CodeBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(vm.value_stack.last().unwrap(), &(Value::Int(0), Value::Nil));
    }

    #[test]
    fn op_make_closure_pops_n_free_vars_and_pushes_closure() {
        let (_, mut vm) = VmRunner::new()
            .with_value_stack(vec![Value::Int(2), Value::Int(1), Value::Int(0)])
            .run_code(
                CodeBuilder::new()
                    .op(Op::MakeClosure { n_free: 2 })
                    .op(Op::Halt),
            );

        let closure = vm.value_stack.pop().unwrap();
        let closure = closure.as_closure().unwrap();

        assert!(vm.value_stack.is_empty());

        assert_eq!(&**closure.free_vars, vec![Value::Int(1), Value::Int(2)]);
        assert_eq!(closure.code.clone().fetch(), Op::Halt);
    }

    #[test]
    fn op_make_closure_succeeds_even_when_storage_is_full() {
        let (_, mut vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Value::Int(2), Value::Int(1), Value::Int(0)])
            .run_code(
                CodeBuilder::new()
                    .op(Op::MakeClosure { n_free: 2 })
                    .op(Op::Halt),
            );

        let closure = vm.value_stack.pop().unwrap();
        let closure = closure.as_closure().unwrap();

        assert!(vm.value_stack.is_empty());

        assert_eq!(&**closure.free_vars, vec![Value::Int(1), Value::Int(2)]);
        assert_eq!(closure.code.clone().fetch(), Op::Halt);
    }

    #[test]
    fn op_call_expects_callable_on_top_of_stack() {
        let (ret, _) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Value::Int(0)])
            .run_code(CodeBuilder::new().op(Op::Call { n_args: 0 }));

        assert_eq!(ret, Err(ErrorKind::TypeError));
    }

    #[test]
    fn op_call_executes_closure() {
        let mut storage = ValueStorage::new(1024);

        let code = CodeBuilder::new()
            // main
            .op(Op::Call { n_args: 0 })
            .op(Op::Halt)
            // callee
            .op(Op::Integer(1))
            .op(Op::Halt)
            .build()
            .unwrap();
        let code_segment = storage.insert(code).unwrap();

        let main = Closure {
            code: CodePointer::new(code_segment.clone()).at(0),
            free_vars: storage.insert(vec![].into_boxed_slice()).unwrap(),
        };

        let callee = Closure {
            code: CodePointer::new(code_segment.clone()).at(2),
            free_vars: storage.insert(vec![].into_boxed_slice()).unwrap(),
        };

        let value_stack = vec![storage.store_closure(callee).unwrap()];

        let mut vm = Vm::new(storage).unwrap();

        vm.value_stack = value_stack;

        vm.load_closure(&main);
        let ret = vm.run();

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(vm.call_stack.len(), 1);
        assert_eq!(vm.value_stack, vec![Value::Int(1)]);
    }

    #[test]
    fn op_call_puts_args_in_frame() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // args for call
                .op(Op::Integer(10))
                .op(Op::Integer(11))
                .op(Op::Integer(12))
                // callee code offset
                .op(Op::Integer(2))
                .op(Op::MakeClosure { n_free: 0 })
                .op(Op::Call { n_args: 3 })
                .op(Op::Halt)
                // callee
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(
            &*vm.current_frame.args,
            vec![Value::Int(12), Value::Int(11), Value::Int(10)]
        )
    }

    #[test]
    fn op_getarg_references_args() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // args for call
                .op(Op::Integer(10))
                .op(Op::Integer(11))
                .op(Op::Integer(12))
                .make_closure("callee", 0)
                .op(Op::Call { n_args: 3 })
                .op(Op::Halt)
                .label("callee")
                .op(Op::GetArg(0))
                .op(Op::GetArg(2))
                .op(Op::GetArg(2))
                .op(Op::GetArg(1))
                .op(Op::Halt),
        );

        assert_eq!(
            &*vm.value_stack,
            vec![
                Value::Int(12),
                Value::Int(10),
                Value::Int(10),
                Value::Int(11)
            ]
        )
    }

    #[test]
    fn op_getfree_references_closure_vars() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // free vars
                .op(Op::Integer(10))
                .op(Op::Integer(11))
                .op(Op::Integer(12))
                .make_closure("callee", 3)
                .op(Op::Call { n_args: 0 })
                .op(Op::Halt)
                .label("callee")
                .op(Op::GetFree(0))
                .op(Op::GetFree(2))
                .op(Op::GetFree(2))
                .op(Op::GetFree(1))
                .op(Op::Halt),
        );

        assert_eq!(
            &*vm.value_stack,
            vec![
                Value::Int(12),
                Value::Int(10),
                Value::Int(10),
                Value::Int(11)
            ]
        )
    }

    #[test]
    fn op_return_continues_at_call_site() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // callee code offset
                .make_closure("callee", 0)
                .op(Op::Call { n_args: 0 })
                .op(Op::Integer(34))
                .op(Op::Halt)
                .label("callee")
                .op(Op::Integer(12))
                .op(Op::Return),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(12), Value::Int(34)])
    }

    #[test]
    fn op_zero_jump_is_like_nop() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Jump { forward: 0 })
                .op(Op::Integer(0))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(0)])
    }

    #[test]
    fn op_zero_jump_skips_instructions() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Jump { forward: 2 })
                .op(Op::Integer(0))
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::Integer(3))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(2), Value::Int(3)])
    }

    #[test]
    fn op_jump_conditionally_take_false_branch() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Value::False)
                .branch_if("then")
                .label("else")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("then")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(0)])
    }

    #[test]
    fn op_jump_conditionally_take_true_branch() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Value::True)
                .branch_if("then")
                .label("else")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("then")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(1)])
    }

    #[test]
    fn op_jump_void_falls_through_and_leaves_condition_on_stack() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Value::Int(2))
                .branch_void("is void")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("is void")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(2), Value::Int(0)])
    }

    #[test]
    fn op_jump_void_take_true_branch() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Value::Void)
                .branch_void("is void")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("is void")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Value::Int(1)])
    }

    #[test]
    fn op_call_primitive() {
        use std::sync::atomic::{AtomicBool, Ordering};

        static PRIM_CALLED: AtomicBool = AtomicBool::new(false);
        fn prim(_stack: &mut Vec<Value>, _storage: &mut ValueStorage) -> Result<()> {
            PRIM_CALLED.store(true, Ordering::SeqCst);
            Ok(())
        }

        let (ret, _) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Value::Primitive(prim))
                .op(Op::Call { n_args: 0 })
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(PRIM_CALLED.load(Ordering::SeqCst), true);
    }

    #[test]
    fn op_car_returns_value_error_if_no_pair_on_top() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Value::Nil])
            .run_code(CodeBuilder::new().op(Op::Car));

        assert_eq!(ret, Err(ErrorKind::TypeError));
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_cdr_returns_value_error_if_no_pair_on_top() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Value::Nil])
            .run_code(CodeBuilder::new().op(Op::Cdr));

        assert_eq!(ret, Err(ErrorKind::TypeError));
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_car_returns_first_element_of_pair() {
        let (_, vm) = VmRunner::new().with_value_stack(vec![]).run_code(
            CodeBuilder::new()
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::Cons)
                .op(Op::Car)
                .op(Op::Halt),
        );

        assert_eq!(&*vm.value_stack, vec![Value::Int(1)])
    }

    #[test]
    fn op_cdr_returns_second_element_of_pair() {
        let (_, vm) = VmRunner::new().with_value_stack(vec![]).run_code(
            CodeBuilder::new()
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::Cons)
                .op(Op::Cdr)
                .op(Op::Halt),
        );

        assert_eq!(&*vm.value_stack, vec![Value::Int(2)])
    }

    #[test]
    fn op_table_creates_an_empty_table() {
        let (_, vm) = VmRunner::new()
            .with_value_stack(vec![])
            .run_code(CodeBuilder::new().op(Op::Table).op(Op::Halt));

        assert_eq!(vm.value_stack.last().unwrap(), &hashmap! {});
    }

    #[test]
    fn op_table_succeeds_even_when_storage_is_full() {
        let (_, vm) = VmRunner::new()
            .with_capacity(0)
            .run_code(CodeBuilder::new().op(Op::Table).op(Op::Halt));

        assert_eq!(vm.value_stack.last().unwrap(), &hashmap! {});
    }

    #[test]
    fn op_table_set_pops_three_values_and_return_type_error_if_deepest_is_no_table() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Integer(0))
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::TableSet),
        );

        assert_eq!(ret, Err(ErrorKind::TypeError));
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_table_set_pops_key_and_value_but_updates_table() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Table)
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::TableSet)
                .op(Op::Halt),
        );

        assert_eq!(
            vm.value_stack.last().unwrap(),
            &hashmap! {Value::Int(1) => Value::Int(2)}
        );
    }

    #[test]
    fn op_table_get_pops_two_values_and_return_type_error_if_deeper_is_no_table() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Integer(0))
                .op(Op::Integer(1))
                .op(Op::TableGet),
        );

        assert_eq!(ret, Err(ErrorKind::TypeError));
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_table_get_pops_two_values_and_pushes_void() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Table)
                .op(Op::Integer(1))
                .op(Op::TableGet)
                .op(Op::Halt),
        );

        assert_eq!(&*vm.value_stack, vec![Value::Void])
    }

    #[test]
    fn op_table_get_pops_key_and_table_and_pushes_value() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Table)
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::TableSet)
                .op(Op::Integer(1))
                .op(Op::TableGet)
                .op(Op::Halt),
        );

        assert_eq!(&*vm.value_stack, vec![Value::Int(2)])
    }

    #[test]
    fn op_getstack_references_stack_from_bottom() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Integer(1))
                .op(Op::Integer(2))
                .op(Op::Integer(3))
                .op(Op::GetStack(0))
                .op(Op::GetStack(2))
                .op(Op::GetStack(4))
                .op(Op::Halt),
        );

        assert_eq!(
            &*vm.value_stack,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(1),
                Value::Int(3),
                Value::Int(3)
            ]
        )
    }
}
