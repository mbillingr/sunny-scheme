use crate::activation::Activation;
use crate::bytecode::CodePointer;
use crate::mem::Ref;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
}
