use crate::bytecode::CodeSegment;
use crate::storage::ValueStorage;
use sunny_sexpr_parser::parse_str;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    line: usize,
    kind: ErrorKind,
}

#[derive(Debug)]
enum ErrorKind {}

pub fn load_str(src: &str, _storage: &mut ValueStorage) -> Result<CodeSegment> {
    let sexpr = parse_str(src).unwrap();
    println!("s: {}", sexpr);
    unimplemented!()
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
the-end:
    return)
        ";
        let mut storage = ValueStorage::new(1024);
        let _code = load_str(source, &mut storage).unwrap();
        unimplemented!()
    }
}
