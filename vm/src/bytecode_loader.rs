use crate::bytecode::{CodeBuilder, CodeSegment, Op};
use crate::storage::ValueStorage;
use std::collections::HashMap;
use sunny_sexpr_parser::{parse_str, Context, CxR, Error as ParseError};

pub type Result<T> = std::result::Result<T, Context<Error>>;

#[derive(Debug)]
pub enum Error {
    ParseError(ParseError),
    ExpectedList,
    ExpectedSymbol,
    ExpectedIndex,
    AllocationError,
    UnknownOpcode,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::ParseError(pe) => pe.fmt(f),
            Error::ExpectedList => write!(f, "Expected list"),
            Error::ExpectedSymbol => write!(f, "Expected symbol"),
            Error::ExpectedIndex => write!(f, "Expected index"),
            Error::AllocationError => write!(f, "Allocation Error"),
            Error::UnknownOpcode => write!(f, "Unknown Opcode"),
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
    let mut sections = HashMap::new();
    for sexpr in seq.iter() {
        let section_name = sexpr
            .car()
            .ok_or_else(|| sexpr.map(Error::ExpectedList))?
            .as_symbol()
            .ok_or_else(|| sexpr.map(Error::ExpectedSymbol))?;
        let section_body = sexpr.cdr().unwrap();
        sections.insert(section_name, section_body);
    }

    let mut cb = CodeBuilder::new();

    let mut constants = vec![];
    for value in sections["constants:"].iter() {
        let c = storage
            .sexpr_to_value(value)
            .map_err(|_| Error::AllocationError)?;
        constants.push(c.clone());
        cb.add_constant(c);
    }

    let mut code_parts = sections["code:"].iter();
    while let Some(statement) = code_parts.next() {
        let stmt = statement
            .as_symbol()
            .ok_or_else(|| statement.map(Error::ExpectedSymbol))?;
        match stmt {
            "nop" => cb = cb.op(Op::Nop),
            "jump" => {
                let label = code_parts
                    .next()
                    .ok_or_else(|| statement.map_after(Error::ExpectedSymbol))?;
                let label = label
                    .as_symbol()
                    .ok_or_else(|| label.map(Error::ExpectedSymbol))?;
                cb = cb.jump_to(label);
            }
            "const" => {
                let i = code_parts
                    .next()
                    .ok_or_else(|| statement.map_after(Error::ExpectedIndex))?;
                let i = i.as_usize().ok_or_else(|| i.map(Error::ExpectedIndex))?;
                cb = cb.constant(constants[i].clone())
            }
            "return" => cb = cb.op(Op::Return),
            _ if stmt.ends_with(':') => cb = cb.label(stmt.strip_suffix(':').unwrap()),
            _ => return Err(statement.map(Error::UnknownOpcode)),
        }
    }

    cb.build().map_err(|_| unreachable!())
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
