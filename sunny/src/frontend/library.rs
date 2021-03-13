use crate::frontend::environment::EnvBinding;
use std::rc::Rc;
use sunny_sexpr_parser::Sexpr;

pub fn libname_to_string(libname: &Sexpr) -> String {
    libname.to_string()
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub binding: EnvBinding,
    pub export_name: Rc<str>,
}

impl Export {
    pub fn new(export_name: impl Into<Rc<str>>, binding: EnvBinding) -> Self {
        Export {
            binding,
            export_name: export_name.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LibraryBinding {
    exports: Rc<Vec<Export>>,
}

impl LibraryBinding {
    pub fn new(exports: Vec<Export>) -> Self {
        LibraryBinding {
            exports: Rc::new(exports),
        }
    }

    pub fn exports(&self) -> impl Iterator<Item = &Export> {
        self.exports.iter()
    }
}
