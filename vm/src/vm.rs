use crate::activation::Activation;
use crate::bytecode::{CodeBuilder, CodePointer, CodeSegment};
use crate::continuation::Continuation;
use crate::scm_extension::ScmExt;
use crate::{
    bytecode::Op, closure::Closure, mem::Ref, storage::ValueStorage, Error, ErrorKind, Result,
    RuntimeResult,
};
use std::cell::Cell;
use sunny_sexpr_parser::{CxR, Scm};

pub struct Vm {
    storage: ValueStorage,
    value_stack: Vec<Scm>,
    globals: Vec<Scm>,

    current_activation: Ref<Activation>,
}

impl Vm {
    pub fn new(mut storage: ValueStorage) -> Result<Self> {
        let code_segment = CodeBuilder::new().op(Op::Halt).build().unwrap();
        let code_ptr = CodePointer::new(storage.insert(code_segment));

        let root_activation = Activation {
            caller: None,
            parent: None,
            code: code_ptr,
            locals: vec![],
        };
        let root_activation = storage.insert(root_activation);

        Ok(Vm {
            storage,
            value_stack: vec![],
            globals: vec![],
            current_activation: root_activation,
        })
    }

    pub fn borrow_storage(&mut self) -> &mut ValueStorage {
        &mut self.storage
    }

    pub fn build_value(&mut self, scm: &Scm) -> Scm {
        scm.clone()
    }

    pub fn eval_repl(&mut self, code: CodeSegment) -> RuntimeResult<Scm> {
        let mut root_activation = self.current_activation.clone();
        let code = self.storage.insert(code);
        root_activation.code = CodePointer::new(code);

        match self.run() {
            Ok(x) => {
                if !self.value_stack.is_empty() {
                    panic!(
                        "Scm stack should be empty but contains {:?}",
                        self.value_stack
                    )
                }
                Ok(x)
            }
            Err(e) => {
                let ce = self.add_error_context(e);
                self.current_activation = root_activation; // restore root activation
                self.value_stack.clear();
                Err(ce)
            }
        }
    }

    pub fn eval(&mut self, code: CodePointer) -> RuntimeResult<Scm> {
        let closure = Closure { code, parent: None };
        self.eval_closure(&closure)
    }

    pub fn eval_closure(&mut self, closure: &Closure) -> RuntimeResult<Scm> {
        self.load_closure(closure);
        self.run().map_err(|e| self.add_error_context(e))
    }

    fn load_closure(&mut self, closure: &Closure) {
        let activation = Activation {
            caller: None,
            parent: closure.parent.clone(),
            code: closure.code.clone(),
            locals: vec![],
        };
        self.current_activation = self.storage.insert(activation);
    }

    fn add_error_context(&mut self, kind: ErrorKind) -> Error {
        Error {
            kind,
            location: self.current_activation.code.offset(-1),
        }
    }

    fn run(&mut self) -> Result<Scm> {
        let mut arg: usize = 0;
        loop {
            let op = self.fetch_op();
            match op {
                Op::Nop => {}
                Op::ExtArg(a) => {
                    arg = Op::extend_arg(a, arg);
                    continue;
                }
                Op::Inspect(i) => println!(
                    "{:?}",
                    self.value_stack
                        .get(self.value_stack.len() - 1 - i as usize)
                ),
                Op::Halt => return Err(ErrorKind::Halted),
                Op::Jump { forward } => self.jump(Op::extend_arg(forward, arg)),
                Op::JumpIfTrue { forward } => self.jump_if_true(Op::extend_arg(forward, arg))?,
                Op::JumpIfVoid { forward } => self.jump_if_void(Op::extend_arg(forward, arg))?,
                Op::RJump { backward } => self.rjump(Op::extend_arg(backward, arg)),
                Op::RJumpIfTrue { backward } => {
                    self.rjump_if_true(Op::extend_arg(backward, arg))?
                }
                Op::RJumpIfVoid { backward } => {
                    self.rjump_if_void(Op::extend_arg(backward, arg))?
                }
                Op::Return => {
                    if let Some(act) = self.current_activation.caller.take() {
                        self.current_activation = act;
                    } else {
                        let ret_val = self.pop_value()?;
                        return Ok(ret_val);
                    }
                }
                Op::FetchGlobal(a) => self.get_global(Op::extend_arg(a, arg))?,
                Op::StoreGlobal(a) => self.set_global(Op::extend_arg(a, arg))?,
                Op::Peek(i) => self.peek_closure(Op::extend_arg(i, arg))?,
                Op::PushLocal => self.push_local()?,
                Op::DropLocal => {
                    self.drop_local()?;
                }
                Op::Fetch(a) => self.get_local(Op::extend_arg(a, arg))?,
                Op::Store(a) => {
                    let value = self.pop_value()?;
                    self.set_local(Op::extend_arg(a, arg), value);
                }
                Op::Apply => self.apply()?,
                Op::Call { n_args } => self.call(Op::extend_arg(n_args, arg))?,
                Op::TailCall { n_args } => self.tail_call(Op::extend_arg(n_args, arg))?,
                Op::CallDynamic => self.call_dynamic(Self::call)?,
                Op::TailCallDynamic => self.call_dynamic(Self::tail_call)?,
                Op::PrepareArgs(n_args) => self.prepare_args(Op::extend_arg(n_args, arg))?,
                Op::PrepareVarArgs(n_args) => self.prepare_varargs(Op::extend_arg(n_args, arg))?,
                Op::Void => self.push_value(Scm::void()),
                Op::Integer(a) => self.push_value(Scm::int(Op::extend_arg(a, arg) as i64)),
                Op::Const(a) => self.push_const(Op::extend_arg(a, arg)),
                Op::GetStack(a) => self.push_from_stack(Op::extend_arg(a, arg)),
                Op::Dup => self.dup()?,
                Op::Drop => self.drop()?,
                Op::Swap => self.swap()?,
                Op::Eq => self.eq()?,
                Op::Inc => self.inc()?,
                Op::Dec => self.dec()?,
                Op::Cons => self.cons()?,
                Op::Car => self.car()?,
                Op::Cdr => self.cdr()?,
                Op::MakeClosure { offset } => self.make_closure(Op::extend_arg(offset, arg)),
                Op::CaptureContinuation { offset } => {
                    self.capture_continuation(Op::extend_arg(offset, arg))?
                }
            }
            arg = 0;
        }
    }

    fn fetch_op(&mut self) -> Op {
        self.current_activation.code.fetch()
    }

    pub fn push_value(&mut self, val: Scm) {
        //println!("pushing {:?}", val);
        self.value_stack.push(val);
    }

    pub fn pop_value(&mut self) -> Result<Scm> {
        //println!("popping");
        self.value_stack.pop().ok_or(ErrorKind::StackUnderflow)
        //self.value_stack.pop().ok_or_else(||panic!())
    }

    pub fn push_list_items(&mut self, list: &Scm) -> Result<usize> {
        if list.is_null() {
            Ok(0)
        } else if let Some(x) = list.car() {
            let n = self.push_list_items(list.cdr().unwrap())?;
            self.push_value(x.clone());
            Ok(n + 1)
        } else {
            Err(ErrorKind::TypeError)
        }
    }

    fn pop_int(&mut self) -> Result<i64> {
        self.pop_value()?.as_int().ok_or(ErrorKind::TypeError)
    }

    fn pop_values(&mut self, n: usize) -> Result<Vec<Scm>> {
        let mut values = Vec::with_capacity(n);
        for _ in 0..n {
            values.push(self.pop_value()?);
        }
        Ok(values)
    }

    fn push_const(&mut self, idx: usize) {
        let x = self.current_activation.code.get_constant(idx).clone();
        self.push_value(x);
    }

    fn push_from_stack(&mut self, idx: usize) {
        let x = self.value_stack[idx].clone();
        self.push_value(x);
    }

    fn get_global(&mut self, idx: usize) -> Result<()> {
        if idx >= self.globals.len() {
            /*self.globals
            .resize((idx + 1).next_power_of_two(), Scm::Void);*/
            return Err(ErrorKind::UndefinedVariable);
        }
        let x = self.globals[idx].clone();
        if x.is_void() {
            return Err(ErrorKind::UndefinedVariable);
        }
        self.push_value(x);
        Ok(())
    }

    fn set_global(&mut self, idx: usize) -> Result<()> {
        let x = self.pop_value()?;
        self.assign_global(idx, x);
        Ok(())
    }

    pub fn assign_global(&mut self, idx: usize, value: Scm) {
        if idx >= self.globals.len() {
            self.globals
                .resize((idx + 1).next_power_of_two(), Scm::void());
        }
        self.globals[idx] = value;
    }

    fn push_local(&mut self) -> Result<()> {
        let value = self.pop_value()?;
        self.append_local(value);
        Ok(())
    }

    fn append_local(&mut self, value: Scm) {
        self.current_activation.locals.push(Cell::new(value));
    }

    fn drop_local(&mut self) -> Result<Scm> {
        let value = self
            .current_activation
            .locals
            .pop()
            .ok_or(ErrorKind::StackUnderflow)?;
        Ok(value.into_inner())
    }

    fn get_local(&mut self, mut idx: usize) -> Result<()> {
        let mut act = &self.current_activation;
        while idx >= act.locals.len() {
            idx -= act.locals.len();
            act = act.parent.as_ref().ok_or(ErrorKind::UndefinedVariable)?;
        }

        let x = cell_clone(&act.locals[idx]);
        if x.is_void() {
            return Err(ErrorKind::UndefinedVariable);
        }
        self.push_value(x);
        Ok(())
    }

    fn set_local(&mut self, mut idx: usize, value: Scm) {
        let mut act = &mut self.current_activation;
        while idx >= act.locals.len() && act.parent.is_some() {
            idx -= act.locals.len();
            act = act.parent.as_mut().unwrap();
        }

        if idx >= act.locals.len() {
            act.locals
                .resize_with((idx + 1).next_power_of_two(), || Cell::new(Scm::void()));
        }
        act.locals[idx].set(value);
    }

    fn peek_closure(&mut self, idx: usize) -> Result<()> {
        if let Some(cls) = self.pop_value()?.as_closure() {
            let x = cell_clone(&cls.parent.as_ref().unwrap().locals[idx]);
            self.push_value(x);
            Ok(())
        } else {
            Err(ErrorKind::TypeError)
        }
    }

    fn jump(&mut self, amount: usize) {
        self.current_activation.code.jump_forward(amount)
    }

    fn jump_if_true(&mut self, amount: usize) -> Result<()> {
        let condition = self.pop_value()?;
        if condition.is_like_true() {
            self.jump(amount);
        }
        Ok(())
    }

    fn jump_if_void(&mut self, amount: usize) -> Result<()> {
        let condition = self.pop_value()?;
        if condition.is_void() {
            self.jump(amount);
        } else {
            self.push_value(condition)
        }
        Ok(())
    }

    fn rjump(&mut self, amount: usize) {
        self.current_activation.code.jump_backward(amount)
    }

    fn rjump_if_true(&mut self, amount: usize) -> Result<()> {
        let condition = self.pop_value()?;
        if condition.is_like_true() {
            self.rjump(amount);
        }
        Ok(())
    }

    fn rjump_if_void(&mut self, amount: usize) -> Result<()> {
        let condition = self.pop_value()?;
        if condition.is_void() {
            self.rjump(amount);
        } else {
            self.push_value(condition)
        }
        Ok(())
    }

    fn apply(&mut self) -> Result<()> {
        let args = self.pop_value()?;
        let func = self.pop_value()?;

        let n_args = self.push_list_items(&args)?;

        self.push_value(func);
        self.call(n_args)
    }

    fn call(&mut self, n_args: usize) -> Result<()> {
        let func = self.pop_value()?;
        if let Some(cls) = func.as_closure() {
            self.call_closure(cls, n_args)
        } else if let Some(pri) = func.as_primitive() {
            (pri.proc)(n_args, self)
        } else if let Some(cnt) = func.as_continuation() {
            self.call_continuation(cnt, n_args)
        } else {
            Err(ErrorKind::TypeError)
        }
    }

    fn tail_call(&mut self, n_args: usize) -> Result<()> {
        let func = self.pop_value()?;
        if let Some(cls) = func.as_closure() {
            self.tail_call_closure(cls, n_args)
        } else if let Some(pri) = func.as_primitive() {
            (pri.proc)(n_args, self)
        } else if let Some(cnt) = func.as_continuation() {
            self.call_continuation(cnt, n_args)
        } else {
            Err(ErrorKind::TypeError)
        }
    }

    fn call_closure(&mut self, cls: &Closure, n_args: usize) -> Result<()> {
        let args = self.pop_values(n_args)?;
        let act = Activation::from_closure(self.current_activation.clone(), cls, args);
        self.current_activation = self.storage.insert(act);
        Ok(())
    }

    fn tail_call_closure(&mut self, cls: &Closure, n_args: usize) -> Result<()> {
        let args = self.pop_values(n_args)?;
        let caller = self
            .current_activation
            .caller
            .as_ref()
            .unwrap_or(&self.current_activation);
        let act = Activation::from_closure(caller.clone(), &cls, args);
        self.current_activation = self.storage.insert(act);
        Ok(())
    }

    fn call_continuation(&mut self, cnt: &Continuation, n_args: usize) -> Result<()> {
        if n_args < 1 {
            return Err(ErrorKind::TooFewArgs);
        }
        if n_args > 1 {
            return Err(ErrorKind::TooManyArgs);
        }

        let args = self.pop_values(n_args)?;
        self.value_stack = cnt.value_stack.clone();
        self.value_stack.extend(args);
        self.current_activation = self.storage.insert(cnt.activation.duplicate());
        Ok(())
    }

    fn call_dynamic(&mut self, actual_call: fn(&mut Self, usize) -> Result<()>) -> Result<()> {
        let top = self.pop_value()?;
        let n_args = if let Some(n) = top.as_values() {
            n
        } else {
            self.push_value(top);
            1
        };
        self.get_callee_with_values(n_args)?;
        actual_call(self, n_args)
    }

    fn get_callee_with_values(&mut self, n_values: usize) -> Result<()> {
        if n_values >= self.value_stack.len() {
            return Err(ErrorKind::StackUnderflow);
        }
        let callee_idx = self.value_stack.len() - 1 - n_values;
        let callee = self.value_stack.remove(callee_idx);
        self.push_value(callee);
        Ok(())
    }

    fn prepare_args(&mut self, n_args: usize) -> Result<()> {
        if self.current_activation.locals.len() < n_args {
            return Err(ErrorKind::TooFewArgs);
        }

        if self.current_activation.locals.len() > n_args {
            return Err(ErrorKind::TooManyArgs);
        }

        Ok(())
    }

    fn prepare_varargs(&mut self, n_args: usize) -> Result<()> {
        if self.current_activation.locals.len() < n_args {
            return Err(ErrorKind::TooFewArgs);
        }

        let n_varargs = self.current_activation.locals.len() - n_args;

        let mut vararg = Scm::null();
        for _ in 0..n_varargs {
            let x = self.drop_local()?;
            vararg = Scm::cons(x, vararg);
        }

        self.append_local(vararg);

        Ok(())
    }

    fn dup(&mut self) -> Result<()> {
        let x = self.pop_value()?;
        self.push_value(x.clone());
        self.push_value(x);
        Ok(())
    }

    fn drop(&mut self) -> Result<()> {
        self.pop_value()?;
        Ok(())
    }

    fn swap(&mut self) -> Result<()> {
        let a = self.pop_value()?;
        let b = self.pop_value()?;
        self.push_value(a);
        self.push_value(b);
        Ok(())
    }

    fn eq(&mut self) -> Result<()> {
        let a = self.pop_value()?;
        let b = self.pop_value()?;
        self.push_value(Scm::bool(a.eq(&b)));
        Ok(())
    }

    fn inc(&mut self) -> Result<()> {
        let x = self.pop_int()?;
        self.push_value(Scm::number(x + 1));
        Ok(())
    }

    fn dec(&mut self) -> Result<()> {
        let x = self.pop_int()?;
        self.push_value(Scm::number(x - 1));
        Ok(())
    }

    fn cons(&mut self) -> Result<()> {
        let cdr = self.pop_value()?;
        let car = self.pop_value()?;
        let pair = Scm::cons(car, cdr);
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

    fn make_closure(&mut self, code_offset: usize) {
        let code = self.current_activation.code.offset(code_offset as isize);

        let closure = Closure {
            code,
            parent: Some(self.current_activation.clone()),
        };

        self.push_value(Scm::closure(closure));
    }

    fn capture_continuation(&mut self, code_offset: usize) -> Result<()> {
        let mut activation = self.current_activation.duplicate();
        activation.code = activation.code.offset(code_offset as isize);
        let activation = self.storage.insert(activation);

        let mut value_stack = self.value_stack.clone();
        value_stack.pop().ok_or(ErrorKind::StackUnderflow)?; // remove the function called by call/cc

        let continuation = Continuation {
            activation,
            value_stack,
        };
        self.push_value(Scm::continuation(continuation));

        Ok(())
    }
}

fn cell_clone<T: Clone + Default>(cell: &Cell<T>) -> T {
    let x = cell.take();
    cell.set(x.clone());
    x
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::CodeBuilder;
    use crate::Primitive;

    struct VmRunner {
        storage_capacity: usize,
        value_stack: Option<Vec<Scm>>,
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

        fn with_value_stack(self, values: Vec<Scm>) -> Self {
            VmRunner {
                value_stack: Some(values),
                ..self
            }
        }

        fn run_code(self, cb: CodeBuilder) -> (Result<Scm>, Vm) {
            let mut vm = self.prepare_vm(cb);
            let ret = vm.run();
            (ret, vm)
        }

        fn prepare_vm(self, cb: CodeBuilder) -> Vm {
            let additional_capacity = 3 + 2; // required by the vm's default closure and by the closure being run
            let mut storage = ValueStorage::new(self.storage_capacity + additional_capacity);

            let code_segment = cb.build().unwrap();
            let code_ptr = CodePointer::new(storage.insert(code_segment));

            let closure = Closure {
                code: code_ptr,
                parent: None,
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

        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn no_instructions_executed_after_op_halt() {
        let (_, vm) = VmRunner::new().run_code(CodeBuilder::new().op(Op::Halt).op(Op::Integer(0)));
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_return_from_toplevel_pops_value_stack() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Scm::number(0), Scm::number(1)])
            .run_code(CodeBuilder::new().op(Op::Return));

        assert_eq!(ret, Ok(Scm::number(1)));
        assert_eq!(vm.value_stack, vec![Scm::number(0)]);
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

        assert_eq!(vm.value_stack, vec![Scm::number(0x0123456789abcdef_i64)]);
    }

    #[test]
    fn op_integer_pushes_int_value() {
        let (_, vm) =
            VmRunner::new().run_code(CodeBuilder::new().op(Op::Integer(123)).op(Op::Halt));

        assert_eq!(vm.value_stack, vec![Scm::number(123)]);
    }

    #[test]
    fn op_const_pushes_constant() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Scm::void())
                .constant(Scm::null())
                .constant(Scm::number(0))
                .op(Op::Halt),
        );

        assert_eq!(
            vm.value_stack,
            vec![Scm::void(), Scm::null(), Scm::number(0)]
        );
    }

    #[test]
    fn op_cons_pops_two_values_and_pushes_pair() {
        let (_, vm) = VmRunner::new()
            .with_value_stack(vec![Scm::number(0), Scm::null()])
            .run_code(CodeBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(
            vm.value_stack.last().unwrap(),
            &Scm::cons(Scm::number(0), Scm::null())
        );
    }

    #[test]
    fn op_cons_succeeds_even_when_storage_is_full() {
        let (_, vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Scm::number(0), Scm::null()])
            .run_code(CodeBuilder::new().op(Op::Cons).op(Op::Halt));

        assert_eq!(
            vm.value_stack.last().unwrap(),
            &Scm::cons(Scm::number(0), Scm::null())
        );
    }

    #[test]
    fn op_make_closure_pushes_closure() {
        let (_, mut vm) = VmRunner::new().with_value_stack(vec![]).run_code(
            CodeBuilder::new()
                .op(Op::MakeClosure { offset: 0 })
                .op(Op::Halt),
        );

        let closure = vm.value_stack.pop().unwrap();
        let closure = closure.as_closure().unwrap();

        assert!(vm.value_stack.is_empty());

        assert_eq!(closure.code.clone().fetch(), Op::Halt);
    }

    #[test]
    fn op_make_closure_succeeds_even_when_storage_is_full() {
        let (_, mut vm) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![])
            .run_code(
                CodeBuilder::new()
                    .op(Op::MakeClosure { offset: 0 })
                    .op(Op::Halt),
            );

        let closure = vm.value_stack.pop().unwrap();
        let closure = closure.as_closure().unwrap();

        assert!(vm.value_stack.is_empty());

        assert_eq!(closure.code.clone().fetch(), Op::Halt);
    }

    #[test]
    fn op_call_expects_callable_on_top_of_stack() {
        let (ret, _) = VmRunner::new()
            .with_capacity(0)
            .with_value_stack(vec![Scm::number(0)])
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
        let code_segment = storage.insert(code);

        let main = Closure {
            code: CodePointer::new(code_segment.clone()).at(0),
            parent: None,
        };

        let callee = Closure {
            code: CodePointer::new(code_segment).at(2),
            parent: None,
        };

        let value_stack = vec![Scm::closure(callee)];

        let mut vm = Vm::new(storage).unwrap();

        vm.value_stack = value_stack;

        vm.load_closure(&main);
        let ret = vm.run();

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(vm.value_stack, vec![Scm::number(1)]);
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
                .op(Op::MakeClosure { offset: 2 })
                .op(Op::Call { n_args: 3 })
                .op(Op::Halt)
                // callee
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(
            cell_clone(&vm.current_activation.locals[0]),
            Scm::number(12)
        );
        assert_eq!(
            cell_clone(&vm.current_activation.locals[1]),
            Scm::number(11)
        );
        assert_eq!(
            cell_clone(&vm.current_activation.locals[2]),
            Scm::number(10)
        );
    }

    #[test]
    fn op_getarg_references_args() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // args for call
                .op(Op::Integer(10))
                .op(Op::Integer(11))
                .op(Op::Integer(12))
                .make_closure("callee")
                .op(Op::Call { n_args: 3 })
                .op(Op::Halt)
                .label("callee")
                .op(Op::Fetch(0))
                .op(Op::Fetch(2))
                .op(Op::Fetch(2))
                .op(Op::Fetch(1))
                .op(Op::Halt),
        );

        assert_eq!(
            &*vm.value_stack,
            vec![
                Scm::number(12),
                Scm::number(10),
                Scm::number(10),
                Scm::number(11)
            ]
        )
    }

    #[test]
    fn op_reference_closure_vars() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // free vars
                .op(Op::Integer(10))
                .op(Op::PushLocal)
                .op(Op::Integer(11))
                .op(Op::PushLocal)
                .op(Op::Integer(12))
                .op(Op::PushLocal)
                .make_closure("callee")
                .op(Op::Call { n_args: 0 })
                .op(Op::Halt)
                .label("callee")
                .op(Op::Fetch(0))
                .op(Op::Fetch(2))
                .op(Op::Fetch(2))
                .op(Op::Fetch(1))
                .op(Op::Halt),
        );

        assert_eq!(
            &*vm.value_stack,
            vec![
                Scm::number(10),
                Scm::number(12),
                Scm::number(12),
                Scm::number(11)
            ]
        )
    }

    #[test]
    fn op_return_continues_at_call_site() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                // callee code offset
                .make_closure("callee")
                .op(Op::Call { n_args: 0 })
                .op(Op::Integer(34))
                .op(Op::Halt)
                .label("callee")
                .op(Op::Integer(12))
                .op(Op::Return),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Scm::number(12), Scm::number(34)])
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
        assert_eq!(&*vm.value_stack, vec![Scm::number(0)])
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
        assert_eq!(&*vm.value_stack, vec![Scm::number(2), Scm::number(3)])
    }

    #[test]
    fn op_jump_conditionally_take_false_branch() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Scm::bool(false))
                .branch_if("then")
                .label("else")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("then")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Scm::number(0)])
    }

    #[test]
    fn op_jump_conditionally_take_true_branch() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Scm::bool(true))
                .branch_if("then")
                .label("else")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("then")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Scm::number(1)])
    }

    #[test]
    fn op_jump_void_falls_through_and_leaves_condition_on_stack() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Scm::number(2))
                .branch_void("is void")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("is void")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Scm::number(2), Scm::number(0)])
    }

    #[test]
    fn op_jump_void_take_true_branch() {
        let (ret, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Scm::void())
                .branch_void("is void")
                .op(Op::Integer(0))
                .op(Op::Halt)
                .label("is void")
                .op(Op::Integer(1))
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(&*vm.value_stack, vec![Scm::number(1)])
    }

    #[test]
    fn op_call_primitive() {
        use std::sync::atomic::{AtomicBool, Ordering};

        static PRIM_CALLED: AtomicBool = AtomicBool::new(false);
        fn prim(_: usize, _vm: &mut Vm) -> Result<()> {
            PRIM_CALLED.store(true, Ordering::SeqCst);
            Ok(())
        }
        let prim_val = Scm::primitive(Primitive::fixed_arity("", 0, prim));

        let (ret, _) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(prim_val)
                .op(Op::Call { n_args: 0 })
                .op(Op::Halt),
        );

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(PRIM_CALLED.load(Ordering::SeqCst), true);
    }

    #[test]
    fn op_car_returns_value_error_if_no_pair_on_top() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Scm::null()])
            .run_code(CodeBuilder::new().op(Op::Car));

        assert_eq!(ret, Err(ErrorKind::TypeError));
        assert!(vm.value_stack.is_empty());
    }

    #[test]
    fn op_cdr_returns_value_error_if_no_pair_on_top() {
        let (ret, vm) = VmRunner::new()
            .with_value_stack(vec![Scm::null()])
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

        assert_eq!(&*vm.value_stack, vec![Scm::number(1)])
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

        assert_eq!(&*vm.value_stack, vec![Scm::number(2)])
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
                Scm::number(1),
                Scm::number(2),
                Scm::number(3),
                Scm::number(1),
                Scm::number(3),
                Scm::number(3)
            ]
        )
    }

    #[test]
    fn can_reference_locals() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Integer(1))
                .op(Op::PushLocal)
                .op(Op::Integer(2))
                .op(Op::PushLocal)
                .op(Op::Fetch(0))
                .op(Op::Fetch(1))
                .op(Op::Halt),
        );

        assert_eq!(&*vm.value_stack, vec![Scm::number(1), Scm::number(2)])
    }

    #[test]
    fn dropped_locals_can_be_reused() {
        let (_, vm) = VmRunner::new().run_code(
            CodeBuilder::new()
                .op(Op::Integer(1))
                .op(Op::PushLocal)
                .op(Op::Fetch(0))
                .op(Op::Integer(2))
                .op(Op::DropLocal)
                .op(Op::PushLocal)
                .op(Op::Fetch(0))
                .op(Op::Halt),
        );

        assert_eq!(&*vm.value_stack, vec![Scm::number(1), Scm::number(2)])
    }

    #[test]
    fn op_peek_into_closure() {
        let mut storage = ValueStorage::new(1024);

        let code = CodeBuilder::new()
            // main
            .op(Op::Peek(1))
            .op(Op::Halt)
            // closure
            .op(Op::Halt)
            .build()
            .unwrap();
        let code_segment = storage.insert(code);

        let main = Closure {
            code: CodePointer::new(code_segment.clone()).at(0),
            parent: None,
        };

        let act = Activation {
            caller: None,
            parent: None,
            code: CodePointer::new(code_segment.clone()).at(999),
            locals: vec![Cell::new(Scm::number(1)), Cell::new(Scm::number(2))],
        };
        let act = storage.insert(act);

        let callee = Closure {
            code: CodePointer::new(code_segment).at(2),
            parent: Some(act),
        };

        let value_stack = vec![Scm::closure(callee)];

        let mut vm = Vm::new(storage).unwrap();

        vm.value_stack = value_stack;

        vm.load_closure(&main);
        let ret = vm.run();

        assert_eq!(ret, Err(ErrorKind::Halted));
        assert_eq!(vm.value_stack, vec![Scm::number(2)]);
    }

    #[test]
    fn dereferencing_void_is_an_error() {
        let (res, _) = VmRunner::new().run_code(
            CodeBuilder::new()
                .constant(Scm::void())
                .op(Op::PushLocal)
                .op(Op::Fetch(0))
                .op(Op::Halt),
        );

        assert_eq!(res, Err(ErrorKind::UndefinedVariable));
    }
}
