use std::cell::RefCell;
use std::rc::Rc;

use sunny_sexpr_parser::{Scm, SourceLocation, SourceMap};

use crate::frontend::error::error_after;
use crate::frontend::library::{Export, LibraryBinding};
use crate::frontend::{
    error::{error_at, Error, Result},
    SyntaxExpander,
};
use crate::library_filesystem::LibraryFileSystem;
use std::collections::HashMap;
use sunny_sexpr_parser::parser::parse_with_map;

#[derive(Clone)]
pub enum EnvBinding {
    Variable,
    Intrinsic(&'static str, usize, Box<EnvBinding>),
    Global(Rc<str>),
    Syntax(Rc<dyn SyntaxExpander>),
}

impl EnvBinding {
    pub fn global(fully_qualified_name: impl Into<Rc<str>>) -> Self {
        EnvBinding::Global(fully_qualified_name.into())
    }

    pub fn syntax(expander: impl SyntaxExpander + 'static) -> Self {
        EnvBinding::Syntax(Rc::new(expander))
    }

    pub fn is_variable(&self) -> bool {
        match self {
            EnvBinding::Variable => true,
            EnvBinding::Global(_) => true,
            EnvBinding::Intrinsic(_, _, _) => false,
            EnvBinding::Syntax(_) => false,
        }
    }

    pub fn is_intrinsic(&self) -> bool {
        self.as_intrinsic().is_some()
    }

    pub fn as_intrinsic(&self) -> Option<(&str, usize, &EnvBinding)> {
        match self {
            EnvBinding::Variable => None,
            EnvBinding::Intrinsic(name, n_params, reified) => Some((name, *n_params, reified)),
            EnvBinding::Global(_) => None,
            EnvBinding::Syntax(_) => None,
        }
    }

    pub fn is_global(&self) -> bool {
        self.as_global().is_some()
    }

    pub fn as_global(&self) -> Option<&str> {
        match self {
            EnvBinding::Variable => None,
            EnvBinding::Intrinsic(_, _, _) => None,
            EnvBinding::Global(name) => Some(name),
            EnvBinding::Syntax(_) => None,
        }
    }

    pub fn is_syntax(&self) -> bool {
        match self {
            EnvBinding::Variable => false,
            EnvBinding::Intrinsic(_, _, _) => false,
            EnvBinding::Global(_) => false,
            EnvBinding::Syntax(_) => true,
        }
    }

    pub fn as_syntax(&self) -> Option<&Rc<dyn SyntaxExpander>> {
        match self {
            EnvBinding::Variable => None,
            EnvBinding::Intrinsic(_, _, _) => None,
            EnvBinding::Global(_) => None,
            EnvBinding::Syntax(x) => Some(x),
        }
    }
}

impl<T: 'static + SyntaxExpander> From<T> for EnvBinding {
    fn from(se: T) -> Self {
        EnvBinding::Syntax(Rc::new(se))
    }
}

impl From<Rc<dyn SyntaxExpander>> for EnvBinding {
    fn from(se: Rc<dyn SyntaxExpander>) -> Self {
        EnvBinding::Syntax(se)
    }
}

impl std::fmt::Debug for EnvBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            EnvBinding::Variable => write!(f, "var"),
            EnvBinding::Intrinsic(name, n_params, _) => write!(f, "{}/{}", name, n_params),
            EnvBinding::Global(fqn) => write!(f, "@{}", fqn),
            EnvBinding::Syntax(exp) => write!(f, "{}", exp.description()),
        }
    }
}

impl PartialEq for EnvBinding {
    fn eq(&self, other: &Self) -> bool {
        use EnvBinding::*;
        match (self, other) {
            (Variable, Variable) => true,
            (Intrinsic(name1, _, _), Intrinsic(name2, _, _)) => name1 == name2,
            (Global(fqn1), Global(fqn2)) => fqn1 == fqn2,
            (Syntax(exp1), Syntax(exp2)) => {
                std::ptr::eq(Rc::as_ptr(exp1) as *const u8, Rc::as_ptr(exp2) as *const u8)
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    name: String,
    library_filesystem: LibraryFileSystem,
    libraries: Rc<RefCell<HashMap<String, LibraryBinding>>>,
    global: RefCell<Environment>,
    lexical: Environment,
}

impl Env {
    pub fn new(name: String, global: Environment, lexical: Environment) -> Self {
        Env {
            name,
            library_filesystem: LibraryFileSystem::default(),
            libraries: Rc::new(RefCell::new(HashMap::new())),
            global: RefCell::new(global),
            lexical,
        }
    }

    pub fn set_libfs(&mut self, libfs: LibraryFileSystem) {
        self.library_filesystem = libfs;
    }

    pub fn use_libraries_from(&mut self, other: &Env) {
        self.libraries = other.libraries.clone();
    }

    pub fn add_global_binding(&self, name: impl ToString, binding: impl Into<EnvBinding>) {
        let mut globals = self.global.borrow_mut();
        *globals = globals.add_binding(name, binding);
    }

    pub fn add_binding(&self, name: impl ToString, binding: impl Into<EnvBinding>) {
        let mut globals = self.global.borrow_mut();
        *globals = globals.add_binding(name, binding);
    }

    pub fn extend_from_sexpr(&self, vars: &Scm, src_map: &SourceMap) -> Result<Self> {
        let mut names = vec![];
        for v in vars.iter() {
            let name = v
                .as_symbol()
                .ok_or_else(|| error_at(&src_map.get(&v), Error::ExpectedSymbol))?
                .to_string();

            names.push(name);
        }

        Ok(self.extend_vars(names.into_iter()))
    }

    pub fn extend_vars<T: ToString>(&self, names: impl DoubleEndedIterator<Item = T>) -> Env {
        let mut env = self.clone();
        for name in names.rev() {
            env.lexical = env.lexical.add_binding(name, EnvBinding::Variable);
        }

        env
    }

    pub fn ensure_variable(&self, name: &str) {
        if self.lexical.find(name).is_some() {
            return;
        }

        self.ensure_global_variable(name)
    }

    pub fn ensure_global_variable(&self, name: &str) {
        if self
            .global
            .borrow()
            .lookup(name)
            .filter(|binding| binding.is_variable())
            .is_some()
        {
            return;
        }

        let full_name = format!("{}.{}", self.name, name);

        self.add_global_binding(name, EnvBinding::Global(full_name.into()))
    }

    pub fn lookup_syntax(&self, name: &str) -> Option<Rc<dyn SyntaxExpander>> {
        self.lexical
            .lookup_syntax(name)
            .cloned()
            .or_else(|| self.global.borrow().lookup_syntax(name).cloned())
    }

    pub fn lookup_variable(&self, name: &str) -> Option<EnvBinding> {
        self.lexical
            .lookup(name)
            .cloned()
            .or_else(|| self.global.borrow().lookup(name).cloned())
    }

    pub fn lookup_global_variable(&self, name: &str) -> Option<EnvBinding> {
        self.global.borrow().lookup(name).cloned()
    }

    pub fn lookup_variable_index(&self, name: &str) -> Option<usize> {
        if let Some(index) = self.lexical.lookup_variable_index(name) {
            return Some(index);
        }

        self.lookup_global_variable_index(name)
    }

    pub fn lookup_global_variable_index(&self, name: &str) -> Option<usize> {
        self.global
            .borrow()
            .lookup_variable_index(name)
            .map(|index| index + self.lexical.len())
    }

    pub fn define_library(&self, name: impl ToString, exports: Vec<Export>) {
        self.libraries
            .borrow_mut()
            .insert(name.to_string(), LibraryBinding::new(exports));
    }

    pub fn find_library(&self, name: &str) -> Option<LibraryBinding> {
        self.libraries.borrow().get(name).cloned()
    }

    pub fn parse_library(&self, name: &str, src_map: &SourceMap) -> Result<Option<Scm>> {
        let path = self.library_filesystem.map_libname_to_path(name);
        let src = self.library_filesystem.load_string(&path);
        if src.is_none() {
            return Ok(None);
        }
        let src = src.unwrap();

        parse_with_map(&src, src_map)
            .map_err(|e| e.map(|pe| Error::ParseError(pe.clone())))
            .and_then(|mut sexprs| match sexprs.len() {
                0 => Err(SourceLocation::new(Error::InvalidForm)),
                1 => Ok(Some(sexprs.pop().unwrap())),
                _ => Err(error_after(&src_map.get(&sexprs[0]), Error::InvalidForm)),
            })
            .map_err(|e| e.in_file(path))
    }
}

#[derive(Clone)]
pub enum Environment {
    Empty,
    Entry(Rc<(String, EnvBinding, Environment)>),
}

impl Environment {
    pub fn add_binding(&self, name: impl ToString, binding: impl Into<EnvBinding>) -> Self {
        Environment::Entry(Rc::new((name.to_string(), binding.into(), self.clone())))
    }

    pub fn get_name(&self) -> Option<&str> {
        match self {
            Environment::Empty => None,
            Environment::Entry(entry) => Some(&entry.0),
        }
    }

    pub fn get_binding(&self) -> Option<&EnvBinding> {
        match self {
            Environment::Empty => None,
            Environment::Entry(entry) => Some(&entry.1),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Environment::Empty)
    }

    pub fn len(&self) -> usize {
        match self {
            Environment::Empty => 0,
            Environment::Entry(entry) => 1 + entry.2.len(),
        }
    }

    pub fn lookup_variable_index(&self, name: &str) -> Option<usize> {
        match self {
            Environment::Empty => None,
            Environment::Entry(entry) if entry.0 == name => Some(0),
            Environment::Entry(entry) => {
                let index = entry.2.lookup_variable_index(name)?;
                if entry.1.is_variable() {
                    Some(index + 1)
                } else {
                    Some(index)
                }
            }
        }
    }

    pub fn lookup_syntax(&self, name: &str) -> Option<&Rc<dyn SyntaxExpander>> {
        self.find(name)
            .and_then(Environment::get_binding)
            .and_then(EnvBinding::as_syntax)
    }

    pub fn lookup(&self, name: &str) -> Option<&EnvBinding> {
        self.find(name).and_then(Environment::get_binding)
    }

    pub fn find(&self, name: &str) -> Option<&Self> {
        match self {
            Environment::Empty => None,
            Environment::Entry(entry) if entry.0 == name => Some(self),
            Environment::Entry(entry) => entry.2.find(name),
        }
    }
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Environment::Empty => write!(f, "--"),
            Environment::Entry(entry) if entry.1.is_variable() => {
                write!(f, "{}, {:?}", entry.0, entry.2)
            }
            Environment::Entry(entry) if entry.1.is_intrinsic() => {
                write!(f, "#{}, {:?}", entry.0, entry.2)
            }
            Environment::Entry(entry) if entry.1.is_syntax() => {
                write!(f, "<{}>, {:?}", entry.0, entry.2)
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Variable {}
