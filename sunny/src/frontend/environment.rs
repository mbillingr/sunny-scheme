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

    pub fn expand_reference<'src>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::fetch(context, depth, *idx)),
            EnvBinding::Syntax(_) => Err(context.map(Error::SyntaxAsValue)),
        }
    }

    pub fn expand_assignment<'src>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
        value: AstNode<'src>,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::store(context, depth, *idx, value)),
            EnvBinding::Syntax(_) => Err(context.map(Error::SyntaxAsValue)),
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
}

impl<T: 'static + SyntaxExpander> From<T> for EnvBinding {
    fn from(se: T) -> Self {
        EnvBinding::Syntax(Rc::new(se))
    }
}

#[derive(Clone)]
pub struct Env(Rc<Environment>);

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(Environment {
            parent: None,
            variables: RefCell::new(HashMap::new()),
        }))
    }

    pub fn extend(&self, vars: &SourceLocation<Sexpr>) -> Result<Env> {
        let mut env = Environment::from_sexpr(vars)?;
        env.parent = Some(self.clone());
        Ok(Env(Rc::new(env)))
    }
}

impl Deref for Env {
    type Target = Environment;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

#[derive(Default)]
pub struct Environment {
    parent: Option<Env>,
    variables: RefCell<HashMap<String, EnvBinding>>,
}

impl Environment {
    fn from_sexpr(vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut variables = HashMap::new();

        for v in vars.iter() {
            let name = v
                .as_symbol()
                .ok_or_else(|| error_at(v, Error::ExpectedSymbol))?
                .to_string();

            let idx = variables.len();
            variables.insert(name, EnvBinding::Variable(idx));
        }

        Ok(Environment {
            parent: None,
            variables: RefCell::new(variables),
        })
    }

    pub fn insert_syntax(&self, name: impl ToString, expander: impl SyntaxExpander + 'static) {
        self.variables
            .borrow_mut()
            .insert(name.to_string(), EnvBinding::syntax(expander));
    }

    pub fn lookup(&self, name: &str) -> Option<(usize, EnvBinding)> {
        self.variables
            .borrow()
            .get(name)
            .map(|b| (0, b.clone()))
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|p| p.lookup(name))
                    .map(|(depth, b)| (1 + depth, b.clone()))
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
            .values()
            .filter(|b| b.is_variable())
            .count();
        self.variables
            .borrow_mut()
            .insert(name.clone(), EnvBinding::Variable(idx));
        (0, self.variables.borrow().get(&name).unwrap().clone())
    }

    pub fn outermost_env(&self) -> &Self {
        if let Some(ref p) = self.parent {
            p.outermost_env()
        } else {
            self
        }
    }
}
