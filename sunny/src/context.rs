use std::rc::Rc;

use crate::backend::{ByteCodeBackend, GlobalTable};
use crate::frontend::ast::Ast;
use crate::frontend::environment::{Env, EnvBinding};
use crate::frontend::library::Export;
use crate::frontend::syntax_forms::Expression;
use crate::frontend::{base_environment, error, SyntaxExpander};
use crate::library_filesystem::LibraryFileSystem;
use sunny_sexpr_parser::parser::{parse_with_map, Error as ParseError};
use sunny_sexpr_parser::{Scm, SharedStr, SourceLocation, SourceMap};
use sunny_vm::optimizations::tail_call_optimization;
use sunny_vm::scm_extension::ScmExt;
use sunny_vm::{ErrorKind, ValueStorage, Vm};
use sunny_vm::{Primitive, PrimitiveProc};

pub struct Context {
    env: Env,
    vm: Vm,
    globals: GlobalTable,
    src_map: SourceMap,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        let globals = GlobalTable::new();
        let storage = ValueStorage::new(11);
        let vm = Vm::new(storage).unwrap();

        Context {
            env: base_environment("main"),
            vm,
            globals,
            src_map: SourceMap::default(),
        }
    }

    pub fn eval(&mut self, src: SharedStr) -> Result<Scm, Error> {
        let sexprs =
            parse_with_map(src.clone(), &mut self.src_map).map_err(|e| e.in_string(src.clone()))?;

        if sexprs.is_empty() {
            return Ok(Scm::void());
        }

        //println!("{:#?}", self.env);

        let mut ast_parts = vec![];
        for sx in sexprs {
            let part = Expression
                .expand(&sx, &self.src_map, &self.env)
                .map_err(|e| e.in_string(src.clone()))?;
            ast_parts.push(part)
        }
        let last_part = ast_parts.pop().unwrap();
        let ast = ast_parts
            .into_iter()
            .rfold(last_part, |ast, part| Ast::sequence(part, ast));

        println!("{}", ast);

        let mut backend = ByteCodeBackend::new(&mut self.globals);

        let ir = ast.build(&mut backend);
        ir.return_from();

        let code = ir.build_segment();
        let code = tail_call_optimization(code);
        println!("{}", code);

        let result = self.vm.eval_repl(code)?;

        Ok(result)
    }

    pub fn set_libfs(&mut self, lfs: LibraryFileSystem) {
        self.env.set_libfs(lfs);
    }

    pub fn define_library(&mut self, libname: impl ToString) -> LibDefiner {
        LibDefiner::new(libname, self)
    }

    pub fn env(&self) -> &Env {
        &self.env
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    ParseError(SourceLocation<ParseError>),
    FrontendError(SourceLocation<error::Error>),
    VmError(sunny_vm::Error),
    VmErrorKind(ErrorKind),
}

impl From<SourceLocation<ParseError>> for Error {
    fn from(cpe: SourceLocation<ParseError>) -> Self {
        Self::ParseError(cpe)
    }
}

impl From<SourceLocation<error::Error>> for Error {
    fn from(fe: SourceLocation<error::Error>) -> Self {
        Self::FrontendError(fe)
    }
}

impl From<sunny_vm::Error> for Error {
    fn from(vme: sunny_vm::Error) -> Self {
        Self::VmError(vme)
    }
}

impl From<ErrorKind> for Error {
    fn from(vme: ErrorKind) -> Self {
        Self::VmErrorKind(vme)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::ParseError(e) => write!(f, "{}", e.pretty_fmt()),
            Error::FrontendError(e) => write!(f, "{}", e.pretty_fmt()),
            Error::VmErrorKind(e) => write!(f, "{}", e),
            Error::VmError(e) => write!(f, "{}", e),
        }
    }
}

pub struct LibDefiner<'c> {
    libname: String,
    context: &'c mut Context,
    exports: Vec<Export>,
}

impl<'c> LibDefiner<'c> {
    pub fn new(libname: impl ToString, context: &'c mut Context) -> Self {
        LibDefiner {
            libname: libname.to_string(),
            context,
            exports: vec![],
        }
    }

    pub fn build(self) {
        self.context.env.define_library(self.libname, self.exports);
    }

    pub fn define_syntax(mut self, name: &str, syntax: impl SyntaxExpander + 'static) -> Self {
        let binding = EnvBinding::from(syntax);
        self.exports.push(Export::new(name, binding));
        self
    }

    pub fn define_intrinsic(mut self, name: &'static str, n_params: usize) -> Self {
        let binding = EnvBinding::Intrinsic(name, n_params);
        self.exports.push(Export::new(name, binding));
        self
    }

    pub fn define_primitive_fixed_arity(
        self,
        name: &'static str,
        arity: usize,
        proc: PrimitiveProc,
    ) -> Self {
        let primitive = Primitive::fixed_arity(name, arity, proc);
        self.define_value(name, |_| Scm::primitive(primitive))
    }

    pub fn define_primitive_max_arity(
        self,
        name: &'static str,
        min_arity: usize,
        max_arity: usize,
        proc: PrimitiveProc,
    ) -> Self {
        let primitive = Primitive::max_arity(name, min_arity, max_arity, proc);
        self.define_value(name, |_| Scm::primitive(primitive))
    }

    pub fn define_primitive_vararg(
        self,
        name: &'static str,
        min_arity: usize,
        proc: PrimitiveProc,
    ) -> Self {
        let primitive = Primitive::vararg(name, min_arity, proc);
        self.define_value(name, |_| Scm::primitive(primitive))
    }

    pub fn define_value(mut self, name: &str, f: impl FnOnce(&mut ValueStorage) -> Scm) -> Self {
        let fqn = self.fully_qualified_name(name);

        let idx = self.runtime_globals().determine_index(&fqn);
        let value = f(self.storage());
        self.vm().assign_global(idx, value);

        let binding = EnvBinding::global(fqn);
        self.exports.push(Export::new(name, binding));
        self
    }

    pub fn fully_qualified_name(&self, name: &str) -> Rc<str> {
        format!("{}.{}", self.libname, name).into()
    }

    fn runtime_globals(&mut self) -> &mut GlobalTable {
        &mut self.context.globals
    }

    fn storage(&mut self) -> &mut ValueStorage {
        self.context.vm.borrow_storage()
    }

    fn vm(&mut self) -> &mut Vm {
        &mut self.context.vm
    }
}
