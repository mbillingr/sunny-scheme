use std::fs::File;
use std::io::Read;
use std::path::Path;
use sunny_scm::parser::{parse_with_map, Error as ParseError};
use sunny_scm::{Scm, SourceLocation, SourceMap};

#[derive(Default)]
pub struct FileSystem {}

impl FileSystem {
    pub fn read_with_map(&self, filename: &Path, src_map: &SourceMap) -> Result<Scm, Error> {
        let mut src = String::new();
        File::open(filename)?.read_to_string(&mut src)?;

        let sexprs = parse_with_map(src.clone(), src_map)
            .map_err(|e| Error::ParseError(e.in_string(src.clone()).in_file(filename)))?;

        Ok(Scm::list(sexprs.into_iter()))
    }
}

pub enum Error {
    ParseError(SourceLocation<ParseError>),
    IoError(std::io::Error),
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::IoError(e)
    }
}
