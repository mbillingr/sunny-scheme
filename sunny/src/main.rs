use std::fs::File;
use std::io::Read;

use log::LevelFilter;
use rustyline::{error::ReadlineError, Editor};
use simple_logger::SimpleLogger;
use structopt::StructOpt;

use crate::stdlib::define_standard_libraries;
use context::{Context, Error};
use sunny_vm::bytecode::CodePointer;
use sunny_vm::bytecode_loader::user_load;
use sunny_vm::{ValueStorage, Vm};

pub mod backend;
pub mod context;
pub mod frontend;
#[cfg(test)]
mod language_tests;
pub mod stdlib;

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

    let mut context = Context::new();
    define_standard_libraries(&mut context);

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                match context.eval(line) {
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

fn report_error(err: Error) {
    println!("Error: {}", err)
}
