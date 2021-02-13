use rustyline::{error::ReadlineError, Editor};
use std::fs::File;
use std::io::Read;
use structopt::StructOpt;
use sunny_vm::bytecode::{CodeBuilder, CodePointer, Op};
use sunny_vm::bytecode_loader::user_load;
use sunny_vm::{ValueStorage, Vm};

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
                // todo: this is just a silly placeholder.
                //       we need to translate the input instead.
                let code = CodeBuilder::new()
                    .op(Op::Integer(42))
                    .op(Op::Return)
                    .build()
                    .unwrap();
                match vm.eval_raw(code) {
                    Ok(r) => println!("{}", r),
                    Err(e) => println!("Error: {:?}", e),
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
