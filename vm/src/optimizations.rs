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

pub fn simple_jump_optimization(mut code: CodeSegment) -> CodeSegment {
    let n_ops = code.code_slice().len();
    for i in 0..n_ops {
        match code.code_slice()[i] {
            Op::Jump { .. } | Op::RJump { .. } => {
                let jump_target = code.jump_target(i);
                if jump_target.is_terminal() {
                    code.code_slice_mut()[i] = jump_target
                }
            }
            _ => {}
        }
    }
    code
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

    #[test]
    fn jump_resolver_replaces_jump_with_terminal_target() {
        let input = CodeSegment::new(vec![Op::Jump { forward: 0 }, Op::Return], vec![]);
        let output = simple_jump_optimization(input);
        assert_eq!(output.code_slice(), vec![Op::Return, Op::Return])
    }

    #[test]
    fn jump_resolver_does_not_replace_jump_with_nonterminal_target() {
        let code = vec![Op::Jump { forward: 0 }, Op::Nop];
        let input = CodeSegment::new(code.clone(), vec![]);
        let output = simple_jump_optimization(input);
        assert_eq!(output.code_slice(), code)
    }

    #[test]
    fn jump_resolver_replaces_jump_with_indirect_terminal_target() {
        let input = CodeSegment::new(
            vec![Op::Jump { forward: 0 }, Op::Jump { forward: 0 }, Op::Return],
            vec![],
        );
        let output = simple_jump_optimization(input);
        assert_eq!(
            output.code_slice(),
            vec![Op::Return, Op::Return, Op::Return]
        )
    }

    #[test]
    fn jump_resolver_replaces_jump_with_indirect_bacward_terminal_target() {
        let input = CodeSegment::new(
            vec![
                Op::Jump { forward: 1 },
                Op::RJump { backward: 2 },
                Op::Return,
            ],
            vec![],
        );
        let output = simple_jump_optimization(input);
        assert_eq!(
            output.code_slice(),
            vec![Op::Return, Op::Return, Op::Return]
        )
    }
}
