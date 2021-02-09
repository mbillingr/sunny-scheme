use std::env;
use std::fs::File;
use std::io::Read;
use sunny_vm::bytecode::CodePointer;
use sunny_vm::bytecode_loader::user_load;
use sunny_vm::{ValueStorage, Vm};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage:\n> sunny <bytecode-file>");
        return;
    }

    let filename = &args[1];
    let mut source = String::new();
    File::open(filename)
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
