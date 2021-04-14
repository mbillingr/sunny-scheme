use crate::bytecode::{repr, CodeBuilder, CodeSegment, Op};
use sunny_sexpr_parser::parser::{parse_with_map, Error as ParseError};
use sunny_sexpr_parser::Scm;
use sunny_sexpr_parser::SourceLocation;
use sunny_sexpr_parser::{CxR, SourceMap};

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug)]
pub enum Error {
    ParseError(ParseError),
    UnknownSection,
    ExpectedList,
    ExpectedSymbol,
    ExpectedIndex,
    AllocationError,
    UnknownOpcode,
    ExpectedU8,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::ParseError(pe) => pe.fmt(f),
            Error::UnknownSection => write!(f, "Unknown Section"),
            Error::ExpectedList => write!(f, "Expected list"),
            Error::ExpectedSymbol => write!(f, "Expected symbol"),
            Error::ExpectedIndex => write!(f, "Expected index"),
            Error::AllocationError => write!(f, "Allocation Error"),
            Error::UnknownOpcode => write!(f, "Unknown Opcode"),
            Error::ExpectedU8 => write!(f, "Expected integer in range 0..255"),
        }
    }
}

impl From<ParseError> for Error {
    fn from(pe: ParseError) -> Self {
        Error::ParseError(pe)
    }
}

pub fn user_load(src: &str) -> std::result::Result<CodeSegment, String> {
    load_str(src)
        .map_err(|e| e.in_string(src))
        .map_err(|e| e.pretty_fmt())
}

pub fn load_str(src: &str) -> Result<CodeSegment> {
    let mut src_map = SourceMap::new();
    let seq = parse_with_map(src, &mut src_map).map_err(|e| e.convert())?;

    let mut cb = CodeBuilder::new();
    for sexpr in seq.iter() {
        let section_name = sexpr
            .car()
            .ok_or_else(|| error_at(&src_map.get(sexpr), Error::ExpectedList))?
            .as_symbol()
            .ok_or_else(|| error_at(&src_map.get(sexpr), Error::ExpectedSymbol))?;
        let section_body = sexpr.cdr().unwrap();

        match section_name {
            "constants:" => cb = build_constant_section(cb, section_body),
            "code:" => cb = build_code_section(cb, section_body, &src_map)?,
            _ => {
                return Err(error_at(
                    &src_map.get(sexpr.car().unwrap()),
                    Error::UnknownSection,
                ))
            }
        }
    }

    cb.build().map_err(|_| unreachable!())
}

fn build_constant_section(mut cb: CodeBuilder, constants_section: &Scm) -> CodeBuilder {
    for value in constants_section.iter() {
        cb.add_constant(value.clone());
    }
    cb
}

fn build_code_section(mut cb: CodeBuilder, code: &Scm, src_map: &SourceMap) -> Result<CodeBuilder> {
    let mut code_parts = code.iter();
    while let Some(statement) = code_parts.next() {
        let stmt = statement
            .as_symbol()
            .ok_or_else(|| error_at(&src_map.get(&statement), Error::ExpectedSymbol))?;
        match stmt.to_uppercase().as_str() {
            repr::NOP => cb = cb.op(Op::Nop),
            repr::EXTARG => {
                let i = read_u8(&mut code_parts, &statement, src_map)?;
                cb = cb.op(Op::ExtArg(i))
            }
            repr::INSPECT => {
                let i = read_u8(&mut code_parts, &statement, src_map)?;
                cb = cb.op(Op::Inspect(i))
            }
            repr::HALT => cb = cb.op(Op::Halt),
            repr::JUMP => {
                let label = read_symbol(&mut code_parts, &statement, src_map)?;
                cb = cb.jump_to(label);
            }
            repr::JUMPIFTRUE => {
                let label = read_symbol(&mut code_parts, &statement, src_map)?;
                cb = cb.branch_if(label);
            }
            repr::JUMPIFVOID => {
                let label = read_symbol(&mut code_parts, &statement, src_map)?;
                cb = cb.branch_void(label);
            }
            repr::RJUMP => {
                let label = read_symbol(&mut code_parts, &statement, src_map)?;
                cb = cb.jump_to(label);
            }
            repr::RJUMPIFTRUE => {
                let label = read_symbol(&mut code_parts, &statement, src_map)?;
                cb = cb.branch_if(label);
            }
            repr::RJUMPIFVOID => {
                let label = read_symbol(&mut code_parts, &statement, src_map)?;
                cb = cb.branch_void(label);
            }
            repr::RETURN => cb = cb.op(Op::Return),
            repr::CALL => {
                let i = read_index(&mut code_parts, &statement, src_map)?;
                cb = cb.with(|n_args| Op::Call { n_args }, i)
            }
            repr::INTEGER => {
                let i = read_index(&mut code_parts, &statement, src_map)?;
                cb = cb.with(Op::Integer, i)
            }
            repr::CONST => {
                let i = read_index(&mut code_parts, &statement, src_map)?;
                cb = cb.with(Op::Const, i)
            }
            repr::FETCH => {
                let i = read_index(&mut code_parts, &statement, src_map)?;
                cb = cb.with(Op::Fetch, i)
            }
            repr::GETSTACK => {
                let i = read_index(&mut code_parts, &statement, src_map)?;
                cb = cb.with(Op::GetStack, i)
            }
            repr::DUP => cb = cb.op(Op::Dup),
            repr::DROP => cb = cb.op(Op::Drop),
            repr::SWAP => cb = cb.op(Op::Swap),
            repr::EQ => cb = cb.op(Op::Eq),
            repr::INC => cb = cb.op(Op::Inc),
            repr::DEC => cb = cb.op(Op::Dec),
            repr::CONS => cb = cb.op(Op::Cons),
            repr::CAR => cb = cb.op(Op::Car),
            repr::CDR => cb = cb.op(Op::Cdr),
            repr::MAKECLOSURE => {
                let i = read_index(&mut code_parts, &statement, src_map)?;
                cb = cb.with(|offset| Op::MakeClosure { offset }, i)
            }
            _ if is_label(stmt) => cb = cb.label(label_name(stmt).unwrap()),
            _ => return Err(error_at(&src_map.get(&statement), Error::UnknownOpcode)),
        }
    }
    Ok(cb)
}

fn is_label(stmt: &str) -> bool {
    stmt.ends_with(':')
}

fn label_name(stmt: &str) -> Option<&str> {
    stmt.strip_suffix(':')
}

fn read_index<'a, 'b: 'a>(
    sexpr_iter: &mut impl Iterator<Item = &'a Scm>,
    previous: &Scm,
    src_map: &SourceMap,
) -> Result<usize> {
    let i = sexpr_iter
        .next()
        .ok_or_else(|| error_after(&src_map.get(previous), Error::ExpectedIndex))?;
    i.as_usize()
        .ok_or_else(|| error_at(&src_map.get(&i), Error::ExpectedIndex))
}

fn read_u8<'a, 'b: 'a>(
    sexpr_iter: &mut impl Iterator<Item = &'a Scm>,
    previous: &Scm,
    src_map: &SourceMap,
) -> Result<u8> {
    let i = sexpr_iter
        .next()
        .ok_or_else(|| error_after(&src_map.get(previous), Error::ExpectedIndex))?;
    let value = i
        .as_usize()
        .ok_or_else(|| error_at(&src_map.get(&i), Error::ExpectedIndex))?;
    if value > u8::MAX as usize {
        Err(error_at(&src_map.get(&i), Error::ExpectedU8))
    } else {
        Ok(value as u8)
    }
}

fn read_symbol<'a, 'b: 'a>(
    sexpr_iter: &mut impl Iterator<Item = &'a Scm>,
    previous: &Scm,
    src_map: &SourceMap,
) -> Result<&'a str> {
    let i = sexpr_iter
        .next()
        .ok_or_else(|| error_after(&src_map.get(previous), Error::ExpectedSymbol))?;
    i.as_symbol()
        .ok_or_else(|| error_at(&src_map.get(&i), Error::ExpectedSymbol))
}

fn error_at<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map_value(error.into())
}

fn error_after<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map_after(error.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bytecode_loader() {
        let source = "
(constants:
    42 foo (1 2 3))

(code:
    nop nop nop
    const 2
    jump the-end
    const 1
the-end:
    return)
        ";
        let code = match user_load(source) {
            Ok(code) => code,
            Err(e) => panic!("{}", e),
        };

        assert_eq!(
            code.code_slice(),
            &[
                Op::Nop,
                Op::Nop,
                Op::Nop,
                Op::Const(2),
                Op::Jump { forward: 1 },
                Op::Const(1),
                Op::Return
            ]
        );

        assert!(code.constant_slice()[0].is_equal(&Scm::number(42)));
        assert!(code.constant_slice()[1].is_equal(&Scm::symbol("foo")));
        assert!(code.constant_slice()[2].is_equal(&Scm::list(vec![1, 2, 3].into_iter())));
    }
}
