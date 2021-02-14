mod backend;
mod frontend;

use log::LevelFilter;
use rustyline::{error::ReadlineError, Editor};
use simple_logger::SimpleLogger;
use std::fs::File;
use std::io::Read;
use structopt::StructOpt;
use sunny_sexpr_parser::{parse_str, Context, Error as ParseError};
use sunny_vm::bytecode::{CodeBuilder, CodePointer, Op};
use sunny_vm::bytecode_loader::user_load;
use sunny_vm::{Error as VmError, ErrorKind as VmErrorKind, Value, ValueStorage, Vm};

#[derive(StructOpt)]
enum Cli {
    Bytecode {
        #[structopt(parse(from_os_str))]
        file: std::path::PathBuf,
    },

    Repl,
}

fn main() {
    let args = Cli::from_args();

    SimpleLogger::new()
        .with_module_level("rustyline", LevelFilter::Warn)
        .init()
        .unwrap();

    match args {
        Cli::Bytecode { file } => run_bytecode(&file),
        Cli::Repl => run_repl(),
    }
}

fn run_bytecode(path: &std::path::Path) {
    let mut source = String::new();
    File::open(path)
        .unwrap()
        .read_to_string(&mut source)
        .unwrap();

    let mut storage = ValueStorage::new(1024);

    let code = match user_load(&source, &mut storage) {
        Ok(code) => code,
        Err(e) => panic!("{}", e),
    };
    let code = storage.insert(code).unwrap();

    let cp = CodePointer::new(code);

    let mut vm = Vm::new(storage).unwrap();
    let result = vm.eval(cp).unwrap();
    println!("Result: {}", result);
}

fn run_repl() {
    let mut rl = Editor::<()>::new();

    let storage = ValueStorage::new(5);
    let mut vm = Vm::new(storage).unwrap();

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                match eval(line, &mut vm) {
                    Ok(result) => println!("{}", result),
                    Err(e) => report_error(e),
                }
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn eval(src: &str, vm: &mut Vm) -> Result<Value, ReplError> {
    let sexpr = parse_str(src)?;

    let expr = vm.build_value(sexpr.get_value())?;

    // todo: this is just a silly placeholder.
    //       we need to translate the input instead.
    let code = CodeBuilder::new()
        .constant(expr)
        .op(Op::Return)
        .build()
        .unwrap();

    let result = vm.eval_raw(code)?;
    Ok(result)
}

#[derive(Debug)]
enum ReplError {
    ParseError(Context<ParseError>),
    VmError(VmError),
    VmErrorKind(VmErrorKind),
}

impl From<Context<ParseError>> for ReplError {
    fn from(cpe: Context<ParseError>) -> Self {
        Self::ParseError(cpe)
    }
}

impl From<VmError> for ReplError {
    fn from(vme: VmError) -> Self {
        Self::VmError(vme)
    }
}

impl From<VmErrorKind> for ReplError {
    fn from(vme: VmErrorKind) -> Self {
        Self::VmErrorKind(vme)
    }
}

fn report_error(err: ReplError) {
    println!("Error: {:?}", err)
}
