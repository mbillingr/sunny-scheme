use crate::bytecode::{CodeBuilder, CodeSegment, Op};
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
        match stmt {
            "nop" => cb = cb.op(Op::Nop),
            "jump" => {
                let label = code_parts
                    .next()
                    .ok_or_else(|| error_after(statement, Error::ExpectedSymbol))?;
                let label = label
                    .as_symbol()
                    .ok_or_else(|| error_at(label, Error::ExpectedSymbol))?;
                cb = cb.jump_to(label);
            }
            "const" => {
                let i = code_parts
                    .next()
                    .ok_or_else(|| error_after(statement, Error::ExpectedIndex))?;
                let i = i
                    .as_usize()
                    .ok_or_else(|| error_at(i, Error::ExpectedIndex))?;
                cb = cb.with(Op::Const, i)
            }
            "return" => cb = cb.op(Op::Return),
            _ if stmt.ends_with(':') => cb = cb.label(stmt.strip_suffix(':').unwrap()),
            _ => return Err(error_at(statement, Error::UnknownOpcode)),
        }
    }
    Ok(cb)
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
