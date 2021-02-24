use crate::bytecode::CodeSegment;
use crate::bytecode::Op;

pub fn tail_call_optimization(mut input: CodeSegment) -> CodeSegment {
    let n_ops = input.code_slice().len();
    for i in 0..n_ops {
        if let Op::Call { n_args } = input.code_slice()[i] {
            if let Op::Return = input.code_slice()[i + 1] {
                input.code_slice_mut()[i] = Op::TailCall { n_args }
            }
        }
    }

    input
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tco_does_not_change_non_tail_calls() {
        let input = CodeSegment::new(vec![Op::Call { n_args: 0 }, Op::Nop], vec![]);
        let output = tail_call_optimization(input.clone());
        assert_eq!(output.code_slice(), input.code_slice())
    }

    #[test]
    fn tco_replaces_calls_followed_by_return_with_tail_calls() {
        let input = CodeSegment::new(vec![Op::Call { n_args: 0 }, Op::Return], vec![]);
        let output = tail_call_optimization(input);
        assert_eq!(
            output.code_slice(),
            vec![Op::TailCall { n_args: 0 }, Op::Return]
        )
    }
}
