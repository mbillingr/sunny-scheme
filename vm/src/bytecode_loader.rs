use crate::bytecode::{repr, CodeBuilder, CodeSegment, Op};
use crate::storage::ValueStorage;
use sunny_sexpr_parser::{parse_str, Context, CxR, Error as ParseError, Sexpr};

pub type Result<T> = std::result::Result<T, Context<Error>>;

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

pub fn user_load(
    src: &str,
    storage: &mut ValueStorage,
) -> std::result::Result<CodeSegment, String> {
    load_str(src, storage)
        .map_err(|e| e.in_string(src))
        .map_err(|e| e.pretty_fmt())
}

pub fn load_str(src: &str, storage: &mut ValueStorage) -> Result<CodeSegment> {
    let seq = parse_str(src).map_err(|e| e.convert())?;

    let mut cb = CodeBuilder::new();
    for sexpr in seq.iter() {
        let section_name = sexpr
            .car()
            .ok_or_else(|| error_at(sexpr, Error::ExpectedList))?
            .as_symbol()
            .ok_or_else(|| error_at(sexpr, Error::ExpectedSymbol))?;
        let section_body = sexpr.cdr().unwrap();

        match section_name {
            "constants:" => cb = build_constant_section(cb, section_body, storage)?,
            "code:" => cb = build_code_section(cb, section_body)?,
            _ => return Err(error_at(sexpr.car().unwrap(), Error::UnknownSection)),
        }
    }

    cb.build().map_err(|_| unreachable!())
}

fn build_constant_section(
    mut cb: CodeBuilder,
    constants_section: &Context<Sexpr>,
    storage: &mut ValueStorage,
) -> Result<CodeBuilder> {
    for value in constants_section.iter() {
        let c = storage
            .sexpr_to_value(value)
            .map_err(|_| Error::AllocationError)?;
        cb.add_constant(c);
    }
    Ok(cb)
}

fn build_code_section(mut cb: CodeBuilder, code: &Context<Sexpr>) -> Result<CodeBuilder> {
    let mut code_parts = code.iter();
    while let Some(statement) = code_parts.next() {
        let stmt = statement
            .as_symbol()
            .ok_or_else(|| error_at(statement, Error::ExpectedSymbol))?;
        match stmt.to_uppercase().as_str() {
            repr::NOP => cb = cb.op(Op::Nop),
            repr::EXTARG => {
                let i = read_u8(&mut code_parts, statement)?;
                cb = cb.op(Op::ExtArg(i))
            }
            repr::INSPECT => {
                let i = read_u8(&mut code_parts, statement)?;
                cb = cb.op(Op::Inspect(i))
            }
            repr::HALT => cb = cb.op(Op::Halt),
            repr::JUMP => {
                let label = read_symbol(&mut code_parts, statement)?;
                cb = cb.jump_to(label);
            }
            repr::JUMPIFTRUE => {
                let label = read_symbol(&mut code_parts, statement)?;
                cb = cb.branch_if(label);
            }
            repr::JUMPIFVOID => {
                let label = read_symbol(&mut code_parts, statement)?;
                cb = cb.branch_void(label);
            }
            repr::RJUMP => {
                let label = read_symbol(&mut code_parts, statement)?;
                cb = cb.jump_to(label);
            }
            repr::RJUMPIFTRUE => {
                let label = read_symbol(&mut code_parts, statement)?;
                cb = cb.branch_if(label);
            }
            repr::RJUMPIFVOID => {
                let label = read_symbol(&mut code_parts, statement)?;
                cb = cb.branch_void(label);
            }
            repr::RETURN => cb = cb.op(Op::Return),
            repr::CALL => {
                let i = read_index(&mut code_parts, statement)?;
                cb = cb.with(|n_args| Op::Call { n_args }, i)
            }
            repr::INTEGER => {
                let i = read_index(&mut code_parts, statement)?;
                cb = cb.with(Op::Integer, i)
            }
            repr::CONST => {
                let i = read_index(&mut code_parts, statement)?;
                cb = cb.with(Op::Const, i)
            }
            repr::FETCHLOCAL => {
                let i = read_index(&mut code_parts, statement)?;
                cb = cb.with(Op::FetchLocal, i)
            }
            repr::FETCH => {
                let i = read_index(&mut code_parts, statement)?;
                cb = cb.with(Op::Fetch, i)
            }
            repr::GETSTACK => {
                let i = read_index(&mut code_parts, statement)?;
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
            repr::TABLE => cb = cb.op(Op::Table),
            repr::TABLEGET => cb = cb.op(Op::TableGet),
            repr::TABLESET => cb = cb.op(Op::TableSet),
            repr::MAKECLOSURE => {
                let i = read_index(&mut code_parts, statement)?;
                cb = cb.with(|offset| Op::MakeClosure { offset }, i)
            }
            _ if is_label(stmt) => cb = cb.label(label_name(stmt).unwrap()),
            _ => return Err(error_at(statement, Error::UnknownOpcode)),
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

fn read_index<'a, 'b: 'a, T>(
    sexpr_iter: &mut impl Iterator<Item = &'a Context<Sexpr<'b>>>,
    previous: &Context<T>,
) -> Result<usize> {
    let i = sexpr_iter
        .next()
        .ok_or_else(|| error_after(previous, Error::ExpectedIndex))?;
    i.as_usize()
        .ok_or_else(|| error_at(i, Error::ExpectedIndex))
}

fn read_u8<'a, 'b: 'a, T>(
    sexpr_iter: &mut impl Iterator<Item = &'a Context<Sexpr<'b>>>,
    previous: &Context<T>,
) -> Result<u8> {
    let i = sexpr_iter
        .next()
        .ok_or_else(|| error_after(previous, Error::ExpectedIndex))?;
    let value = i
        .as_usize()
        .ok_or_else(|| error_at(i, Error::ExpectedIndex))?;
    if value > u8::MAX as usize {
        return Err(error_at(i, Error::ExpectedU8));
    } else {
        Ok(value as u8)
    }
}

fn read_symbol<'a, 'b: 'a, T>(
    sexpr_iter: &mut impl Iterator<Item = &'a Context<Sexpr<'b>>>,
    previous: &Context<T>,
) -> Result<&'a str> {
    let i = sexpr_iter
        .next()
        .ok_or_else(|| error_after(previous, Error::ExpectedSymbol))?;
    i.as_symbol()
        .ok_or_else(|| error_at(i, Error::ExpectedSymbol))
}

fn error_at<T>(sexpr: &Context<T>, error: impl Into<Error>) -> Context<Error> {
    sexpr.map(error.into())
}

fn error_after<T>(sexpr: &Context<T>, error: impl Into<Error>) -> Context<Error> {
    sexpr.map_after(error.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Value;

    #[test]
    fn bytecode_loader() {
        let source = "
((constants:
    42 foo (1 2 3))

(code:
    nop nop nop
    const 2
    jump the-end
    const 1
the-end:
    return))
        ";
        let mut storage = ValueStorage::new(1024);
        let code = match user_load(source, &mut storage) {
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

        assert!(code.constant_slice()[0].equals(&Value::Int(42)));
        assert!(code.constant_slice()[1].equals(&storage.interned_symbol("foo").unwrap()));
        assert!(code.constant_slice()[2].equals(&storage.list(vec![1, 2, 3]).unwrap()));
    }
}
