use crate::activation::Activation;
use crate::bytecode::{CodePointer, Op};
use crate::mem::Ref;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
}

impl Closure {
    pub fn arity(&self) -> (usize, Option<usize>) {
        let mut n_args = 0;
        loop {
            match self.code.peek() {
                Op::ExtArg(n) => n_args = Op::extend_arg(n, n_args),
                Op::PrepareArgs(n) => {
                    n_args = Op::extend_arg(n, n_args);
                    return (n_args, Some(n_args));
                }
                Op::PrepareVarArgs(n) => {
                    n_args = Op::extend_arg(n, n_args);
                    return (n_args, None);
                }
                op => panic!("Closure unexpectedly starts with {}", op),
            }
        }
    }
}
