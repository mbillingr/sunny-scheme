use crate::frontend::{
    ast::{Ast, AstNode},
    error::{error_at, Error, Result},
    SyntaxExpander,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
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

    /*pub fn expand_reference(&self, context: SourceLocation<()>, depth: usize) -> Result<AstNode> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::fetch(context, depth, *idx)),
            EnvBinding::Syntax(_) => Err(context.map_value(Error::SyntaxAsValue)),
        }
    }

    pub fn expand_assignment(
        &self,
        context: SourceLocation<()>,
        depth: usize,
        value: AstNode,
    ) -> Result<AstNode> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::store(context, depth, *idx, value)),
            EnvBinding::Syntax(_) => Err(context.map_value(Error::SyntaxAsValue)),
        }
    }

    pub fn index(&self) -> Option<usize> {
        match self {
            EnvBinding::Variable(idx) => Some(*idx),
            EnvBinding::Syntax(_) => None,
        }
    }*/

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
        let mut env = self.clone();

        for v in vars.iter() {
            let name = v
                .as_symbol()
                .ok_or_else(|| error_at(v, Error::ExpectedSymbol))?
                .to_string();

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
    /*pub fn add_syntax(&self, name: impl ToString, expander: impl SyntaxExpander + 'static) -> Self {
        self.add_binding(name, EnvBinding::syntax(expander))
    }

    pub fn add_variable(&self, name: impl ToString) -> Self {
        self.add_binding(name, EnvBinding::Variable)
    }*/

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

/*
#[derive(Default)]
pub struct Environment {
    parent: Option<Env>,
    variables: RefCell<Vec<(String, EnvBinding)>>,
}

impl Environment {
    fn from_sexpr(vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut variables = vec![];

        for v in vars.iter() {
            let name = v
                .as_symbol()
                .ok_or_else(|| error_at(v, Error::ExpectedSymbol))?
                .to_string();

            let idx = variables.len();
            variables.push((name, EnvBinding::Variable(idx)));
        }

        Ok(Environment {
            parent: None,
            variables: RefCell::new(variables),
        })
    }

    pub fn find(&self, name: &str) -> Option<usize> {
        self.variables
            .borrow()
            .iter()
            .rev()
            .position(|(n, _)| n == name)
    }

    pub fn get(&self, name: &str) -> Option<EnvBinding> {
        self.variables
            .borrow()
            .iter()
            .rev()
            .filter(|(n, _)| n == name)
            .map(|(_, b)| b.clone())
            .next()
    }

    pub fn insert_syntax_static(
        &self,
        name: impl AsRef<str> + ToString,
        expander: impl SyntaxExpander + 'static,
    ) {
        self.insert_syntax(name, EnvBinding::syntax(expander))
    }

    pub fn insert_syntax_shared(
        &self,
        name: impl AsRef<str> + ToString,
        expander: Rc<dyn SyntaxExpander>,
    ) {
        self.insert_syntax(name, EnvBinding::Syntax(expander))
    }

    fn insert_syntax(&self, name: impl AsRef<str> + ToString, binding: EnvBinding) {
        if let Some(idx) = self.find(name.as_ref()) {
            let mut old_binding = &mut self.variables.borrow_mut()[idx].1;
            if old_binding.is_syntax() {
                *old_binding = binding;
                return;
            }
        }

        self.variables
            .borrow_mut()
            .push((name.to_string(), binding))
    }

    pub fn lookup(&self, name: &str) -> Option<(usize, EnvBinding)> {
        self.get(name).map(|b| (0, b)).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.lookup(name))
                .map(|(depth, b)| (1 + depth, b))
        })
    }

    pub fn lookup_syntax(&self, name: &str) -> Option<Rc<dyn SyntaxExpander>> {
        self.lookup(name).map(|(_, b)| b).and_then(|b| {
            if let EnvBinding::Syntax(sx) = b {
                Some(sx.clone())
            } else {
                None
            }
        })
    }

    pub fn lookup_or_insert_global(&self, name: &str) -> (usize, EnvBinding) {
        let var = self.lookup(name.as_ref()).map(|(d, b)| (d, b.clone()));
        var.unwrap_or_else(|| self.add_global(name.to_string()))
    }

    pub fn ensure_global(&self, name: impl AsRef<str> + ToString) -> (usize, EnvBinding) {
        let var = self
            .outermost_env()
            .lookup(name.as_ref())
            .map(|(d, b)| (d, b.clone()));
        var.unwrap_or_else(|| self.add_global(name.to_string()))
    }

    pub fn ensure_global_variable(&self, name: impl AsRef<str> + ToString) -> (usize, EnvBinding) {
        let entry = self
            .outermost_env()
            .lookup(name.as_ref())
            .map(|(d, b)| (d, b.clone()));
        match entry {
            Some((d, b)) if b.is_variable() => (d, b),
            _ => self.add_global(name.to_string()),
        }
    }

    pub fn add_global(&self, name: String) -> (usize, EnvBinding) {
        if let Some(p) = &self.parent {
            let (depth, idx) = p.add_global(name);
            return (1 + depth, idx);
        }

        let idx = self
            .variables
            .borrow()
            .iter()
            .filter(|(_, b)| b.is_variable())
            .count();
        self.variables
            .borrow_mut()
            .push((name.clone(), EnvBinding::Variable(idx)));
        (0, self.get(&name).unwrap())
    }

    pub fn outermost_env(&self) -> &Self {
        if let Some(ref p) = self.parent {
            p.outermost_env()
        } else {
            self
        }
    }
}
*/
