use crate::bytecode::CodeSegment;
use crate::storage::ValueStorage;
use sunny_sexpr_parser::{parse_str, Context, Error as ParseError};

pub type Result<T> = std::result::Result<T, Context<Error>>;

#[derive(Debug)]
pub enum Error {
    ParseError(ParseError),
}

impl From<ParseError> for Error {
    fn from(pe: ParseError) -> Self {
        Error::ParseError(pe)
    }
}

pub fn load_str(src: &str, _storage: &mut ValueStorage) -> Result<CodeSegment> {
    let seq = parse_str(src).map_err(|e| e.convert())?;
    for sexpr in seq.iter() {
        println!("s: {}", sexpr);
    }

    /*for x in seq[0].get_value().iter() {
        println!("{:?}", x);
    }*/
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bytecode_loader() {
        let source = "
((constants:
    42 foo (1 2 3))

(code:
    nop nop nop
    const 2
the-end:
    return))
        ";
        let mut storage = ValueStorage::new(1024);
        let _code = load_str(source, &mut storage).unwrap();
        unimplemented!()
    }
}
