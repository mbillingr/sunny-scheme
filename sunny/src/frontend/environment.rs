use crate::frontend::{
    error::{error_at, Error, Result},
    SyntaxExpander,
};
use std::cell::RefCell;
use std::rc::Rc;
use sunny_sexpr_parser::{Sexpr, SourceLocation};

#[derive(Clone)]
pub enum EnvBinding {
    Variable,
    Syntax(Rc<dyn SyntaxExpander>),
}

impl EnvBinding {
    pub fn syntax(expander: impl SyntaxExpander + 'static) -> Self {
        EnvBinding::Syntax(Rc::new(expander))
    }

    pub fn is_variable(&self) -> bool {
        match self {
            EnvBinding::Variable => true,
            EnvBinding::Syntax(_) => false,
        }
    }

    pub fn is_syntax(&self) -> bool {
        match self {
            EnvBinding::Variable => false,
            EnvBinding::Syntax(_) => true,
        }
    }

    pub fn as_syntax(&self) -> Option<&Rc<dyn SyntaxExpander>> {
        match self {
            EnvBinding::Variable => None,
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

#[derive(Debug, Clone)]
pub struct Env {
    global: RefCell<Environment>,
    lexical: Environment,
}

impl Env {
    pub fn new(global: Environment, lexical: Environment) -> Self {
        Env {
            global: RefCell::new(global),
            lexical,
        }
    }

    pub fn empty() -> Self {
        Env {
            global: RefCell::new(Environment::Empty),
            lexical: Environment::Empty,
        }
    }

    pub fn add_global_binding(&self, name: impl ToString, binding: impl Into<EnvBinding>) {
        let mut globals = self.global.borrow_mut();
        *globals = globals.add_binding(name, binding);
    }

    pub fn extend(&self, vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut names = vec![];
        for v in vars.iter() {
            let name = v
                .as_symbol()
                .ok_or_else(|| error_at(v, Error::ExpectedSymbol))?
                .to_string();

            names.push(name);
        }

        let mut env = self.clone();
        for name in names.into_iter().rev() {
            env.lexical = env.lexical.add_binding(name, EnvBinding::Variable);
        }

        Ok(env)
    }

    pub fn ensure_variable(&self, name: &str) {
        if let Some(_) = self.lexical.find(name) {
            return;
        }

        self.ensure_global_variable(name)
    }

    pub fn ensure_global_variable(&self, name: &str) {
        if let Some(_) = self.global.borrow().find(name) {
            return;
        }

        self.add_global_binding(name, EnvBinding::Variable)
    }

    pub fn lookup_syntax(&self, name: &str) -> Option<Rc<dyn SyntaxExpander>> {
        self.lexical
            .lookup_syntax(name)
            .cloned()
            .or_else(|| self.global.borrow().lookup_syntax(name).cloned())
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

    pub fn len(&self) -> usize {
        match self {
            Environment::Empty => 0,
            Environment::Entry(entry) => 1 + entry.2.len(),
        }
    }

    pub fn find(&self, name: &str) -> Option<&Self> {
        match self {
            Environment::Empty => None,
            Environment::Entry(entry) if entry.0 == name => Some(self),
            Environment::Entry(entry) => entry.2.find(name),
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
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Environment::Empty => write!(f, "--"),
            Environment::Entry(entry) if entry.1.is_variable() => {
                write!(f, "{}, {:?}", entry.0, entry.2)
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
