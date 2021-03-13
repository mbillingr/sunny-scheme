use std::rc::Rc;
use sunny_sexpr_parser::Sexpr;

pub fn libname_to_string(libname: &Sexpr) -> String {
    libname.to_string()
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub fully_qualified_name: Rc<str>,
    pub export_name: Rc<str>,
}

impl Export {
    pub fn new(export_name: impl Into<Rc<str>>, fully_qualified_name: impl Into<Rc<str>>) -> Self {
        Export {
            fully_qualified_name: fully_qualified_name.into(),
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
