use crate::activation::Activation;
use crate::bytecode::CodePointer;
use sunny_memory::rc_arena::Ref as ARef;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) parent: Option<ARef<Activation>>,
    pub(crate) code: CodePointer,
}
