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
    Variable(usize),
    Syntax(Rc<dyn SyntaxExpander>),
}

impl EnvBinding {
    pub fn syntax(expander: impl SyntaxExpander + 'static) -> Self {
        EnvBinding::Syntax(Rc::new(expander))
    }

    pub fn expand_reference(&self, context: SourceLocation<()>, depth: usize) -> Result<AstNode> {
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
    }

    pub fn is_variable(&self) -> bool {
        match self {
            EnvBinding::Variable(_) => true,
            EnvBinding::Syntax(_) => false,
        }
    }

    pub fn is_syntax(&self) -> bool {
        match self {
            EnvBinding::Variable(_) => false,
            EnvBinding::Syntax(_) => true,
        }
    }

    pub fn as_syntax(&self) -> Option<&dyn SyntaxExpander> {
        match self {
            EnvBinding::Variable(_) => None,
            EnvBinding::Syntax(x) => Some(&**x),
        }
    }
}

impl<T: 'static + SyntaxExpander> From<T> for EnvBinding {
    fn from(se: T) -> Self {
        EnvBinding::Syntax(Rc::new(se))
    }
}

#[derive(Clone)]
pub struct Env {
    global: Environment,
    lexical: Environment,
}

impl Env {
    pub fn new(global: Environment, lexical: Environment) -> Self {
        Env {
            global,
            lexical,
        }
    }

    pub fn empty() -> Self {
        Env {
            global: Environment::Empty,
            lexical: Environment::Empty,
        }
    }

    pub fn add_global_syntax(&self, name: impl ToString, expander: impl SyntaxExpander + 'static) -> Self {
        Env {
            global: self.global.add_syntax(name, expander),
            lexical: self.lexical.clone(),
        }
    }

    pub fn add_global_variable(&self, name: impl ToString) -> Self {
        Env {
            global: self.global.add_variable(name, unimplemented!()),
            lexical: self.lexical.clone(),
        }
    }

    pub fn extend(&self, vars: &SourceLocation<Sexpr>) -> Result<Env> {
        /*let mut env = Environment::from_sexpr(vars)?;
        env.parent = Some(self.clone());
        Ok(Env(Rc::new(env)))*/
        unimplemented!()
    }

    pub fn ensure_variable(&self, name: &str) -> EnvBinding {
        if let Some(b) = self.lookup(name) {
            return entry.get_binding().unwrap()
        }

        // TODO:
        //   - implement functional environment
        //   - get rid of depth/offset variable indexing... just an offset should be enough, right?

        self.add_global_variable(name)
    }

    pub fn lookup_syntax(&self, name: &str) -> Option<&dyn SyntaxExpander> {
        self.lookup(name)
            .and_then(Environment::get_binding)
            .and_then(EnvBinding::as_syntax)
    }

    pub fn lookup(&self, name: &str) -> Option<&Environment> {
        unimplemented!()
    }
}

#[derive(Clone)]
pub enum Environment {
    Empty,
    Entry(Rc<(String, EnvBinding, Environment)>),
}

impl Environment {
    pub fn add_syntax(&self, name: impl ToString, expander: impl SyntaxExpander + 'static) -> Self {
        Environment::Entry(Rc::new((name.to_string(), EnvBinding::syntax(expander), self.clone())))
    }

    pub fn get_binding(&self) -> Option<&EnvBinding> {
        match self {
            Environment::Empty => None,
            Environment::Entry(entry) => Some(&entry.1),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Variable {

}

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