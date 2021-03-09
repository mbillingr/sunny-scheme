use sunny_sexpr_parser::parser::{parse_str, Error as ParseError};
use sunny_sexpr_parser::SourceLocation;
use sunny_vm::optimizations::tail_call_optimization;
use sunny_vm::{ErrorKind, Value, ValueStorage, Vm};

use crate::backend::{ByteCodeBackend, GlobalTable};
use crate::frontend::environment::Env;
use crate::frontend::syntax_forms::Expression;
use crate::frontend::{base_environment, error, SyntaxExpander};

pub struct Context {
    env: Env,
    vm: Vm,
    globals: GlobalTable,
}

impl Context {
    pub fn new() -> Self {
        let mut globals = GlobalTable::new();
        let modvar = globals.add_variable("*modules*");

        let mut storage = ValueStorage::new(11);
        let module_table = storage.new_table().unwrap();

        let mut vm = Vm::new(storage).unwrap();
        vm.assign_global(modvar, module_table);

        Context {
            env: base_environment("main"),
            vm,
            globals,
        }
    }

    pub fn eval(&mut self, src: &str) -> Result<Value, Error> {
        let sexpr = parse_str(src).map_err(|e| e.in_string(src))?;

        let ast = Expression
            .expand(&sexpr, &mut self.env)
            .map_err(|e| e.in_string(src))?;
        println!("{}", ast);

        let mut backend = ByteCodeBackend::new(self.vm.borrow_storage(), &mut self.globals);

        let ir = ast.build(&mut backend);
        ir.return_from();

        let code = ir.build_segment();
        let code = tail_call_optimization(code);
        println!("{}", code);

        let result = self.vm.eval_repl(code)?;

        Ok(result)
    }

    pub fn preserve(&mut self, value: &Value) {
        self.vm.borrow_storage().preserve_value(value);
    }

    pub fn release(&mut self, value: &Value) {
        self.vm.borrow_storage().release_value(value);
    }

    pub fn symbol(&mut self, name: &str) -> Value {
        let storage = self.vm.borrow_storage();
        storage.ensure(1);
        storage.interned_symbol(name).unwrap()
    }

    pub fn cons(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let storage = self.vm.borrow_storage();
        storage.ensure(1);
        storage.cons(a, b).unwrap()
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
