use std::fs::File;
use std::io::Read;

use log::LevelFilter;
use rustyline::{error::ReadlineError, Editor};
use simple_logger::SimpleLogger;
use structopt::StructOpt;

use crate::builtin_libs::define_standard_libraries;
use crate::frontend::syntax_forms::Import;
use crate::library_filesystem::LibraryFileSystem;
use context::{Context, Error};
use std::path::{Path, PathBuf};
use sunny_vm::bytecode::CodePointer;
use sunny_vm::bytecode_loader::user_load;
use sunny_vm::{ValueStorage, Vm};

pub mod backend;
pub mod builtin_libs;
pub mod context;
pub mod frontend;
#[cfg(test)]
mod language_tests;
mod library_filesystem;

const LINE_PROMPT: &str = ">> ";
const MULTI_PROMPT: &str = " ... ";

#[derive(StructOpt)]
enum Cli {
    Bytecode {
        #[structopt(parse(from_os_str))]
        file: std::path::PathBuf,
    },

    Repl,

    Run {
        #[structopt(parse(from_os_str))]
        file: std::path::PathBuf,
    },

    Compile {
        /// Scheme program to compile
        #[structopt(parse(from_os_str))]
        _input: std::path::PathBuf,

        /// Bytecode output file. Default: name of input file with extension '.sbc'
        #[structopt(parse(from_os_str))]
        _output: Option<std::path::PathBuf>,
    },
}

fn main() {
    let args = Cli::from_args();

    SimpleLogger::new()
        .with_module_level("rustyline", LevelFilter::Warn)
        .with_level(LevelFilter::Warn)
        .init()
        .unwrap();

    match args {
        Cli::Bytecode { file } => run_bytecode(&file),
        Cli::Repl => main_repl(),
        Cli::Run { file } => main_program(file),
        Cli::Compile { .. } => unimplemented!(),
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
    let code = storage.insert(code);

    let cp = CodePointer::new(code);

    let mut vm = Vm::new(storage).unwrap();
    let result = vm.eval(cp).unwrap();
    println!("Result: {}", result);
}

fn main_program(path: PathBuf) {
    let context = Context::new();
    let context = prepare_program(context, &path);
    run_program(context, &path);
}

fn main_repl() {
    let context = Context::new();
    let context = prepare_repl(context);
    run_repl(context);
}

fn prepare_program(mut context: Context, file: &Path) -> Context {
    define_standard_libraries(&mut context);

    let libfs = LibraryFileSystem::new(vec![
        file.parent().unwrap().to_str().unwrap(),
        std::env::current_exe()
            .unwrap()
            .parent()
            .unwrap()
            .join("sunny-libs")
            .to_str()
            .unwrap(),
    ]);
    context.set_libfs(libfs);

    context
}

fn prepare_repl(mut context: Context) -> Context {
    define_standard_libraries(&mut context);
    Import::import_all("(scheme base)", context.env());

    let libfs = LibraryFileSystem::new(vec![
        std::env::current_dir()
            .unwrap()
            .join("sunny-libs")
            .as_os_str()
            .to_str()
            .unwrap(),
        std::env::current_dir()
            .unwrap()
            .as_os_str()
            .to_str()
            .unwrap(),
        std::env::current_exe()
            .unwrap()
            .parent()
            .unwrap()
            .join("sunny-libs")
            .to_str()
            .unwrap(),
    ]);
    context.set_libfs(libfs);

    context
}

fn run_program(mut context: Context, path: &Path) {
    let mut src = String::new();
    File::open(path)
        .unwrap_or_else(|e| panic!("Error opening {:?}: {}", path, e))
        .read_to_string(&mut src)
        .unwrap_or_else(|e| panic!("Error reading {:?}: {}", path, e));

    match context.eval(&src) {
        Ok(_) => {}
        Err(e) => report_error(e),
    }
}

fn run_repl(mut context: Context) {
    let mut rl = Editor::<()>::new();
    'repl: loop {
        let mut src = String::new();
        let mut prompt = LINE_PROMPT;
        'multiline: loop {
            match rl.readline(prompt) {
                Ok(line) => {
                    src.push_str(&line);
                    src.push('\n');

                    if line.trim().is_empty() {
                        continue;
                    }

                    rl.add_history_entry(line);

                    match context.eval(&src) {
                        Ok(result) => println!("{}", result),
                        Err(Error::ParseError(e)) if e.is_eof() => {
                            prompt = MULTI_PROMPT;
                            continue 'multiline;
                        }
                        Err(e) => report_error(e),
                    }
                    break 'multiline;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break 'repl;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break 'repl;
                }
            }
        }
    }
}

fn report_error(err: Error) {
    println!("Error: {}", err)
}
