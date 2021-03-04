use crate::frontend::{
    ast::{Ast, AstNode},
    error::{error_at, Error, Result},
    SyntaxExpander,
};
use std::collections::HashMap;
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

    pub fn meaning_reference<'src>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::fetch(context, depth, *idx)),
            EnvBinding::Syntax(_) => Err(context.map(Error::SyntaxAsValue)),
        }
    }

    pub fn meaning_assignment<'src>(
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

    pub fn expand_syntax<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(_) => panic!("Attempt to expand variable as syntax"),
            EnvBinding::Syntax(x) => x.expand(sexpr, further),
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

#[derive(Default)]
pub struct Env {
    parent: Option<Box<Env>>,
    variables: HashMap<String, EnvBinding>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            parent: None,
            variables: HashMap::new(),
        }
    }

    pub fn insert_syntax(&mut self, name: impl ToString, expander: impl SyntaxExpander + 'static) {
        self.variables
            .insert(name.to_string(), EnvBinding::syntax(expander));
    }

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

        Ok(Env {
            parent: None,
            variables,
        })
    }

    pub fn extend(self, vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut env = Env::from_sexpr(vars)?;
        env.parent = Some(Box::new(self));
        Ok(env)
    }

    pub fn lookup(&self, name: &str) -> Option<(usize, &EnvBinding)> {
        self.variables.get(name).map(|b| (0, b)).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.lookup(name))
                .map(|(depth, b)| (1 + depth, b))
        })
    }

    pub fn add_global(&mut self, name: String) -> (usize, &EnvBinding) {
        if let Some(p) = &mut self.parent {
            let (depth, idx) = p.add_global(name);
            return (1 + depth, idx);
        }

        let idx = self.variables.values().filter(|b| b.is_variable()).count();
        self.variables
            .insert(name.clone(), EnvBinding::Variable(idx));
        (0, self.variables.get(&name).unwrap())
    }

    pub fn outermost_env(&self) -> &Env {
        if let Some(ref p) = self.parent {
            p.outermost_env()
        } else {
            self
        }
    }

    pub fn pop_scope(&mut self) {
        *self = *self.parent.take().unwrap();
    }
}
