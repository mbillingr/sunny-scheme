macro_rules! primitive {
    ($($kind:tt $name:ident($($args:tt)*) -> Result<Value> $body:block)*) => {
        $(
            primitive!{def $kind $name($($args)*) -> Result<Value> $body}
        )*
    };

    (def fn $name:ident($($a:ident: Value),*) -> Result<Value> $body:block) => {
        fn $name(n_args: usize, vm: &mut sunny_vm::Vm) -> Result<()> {
            let n_expect = 0;
            $(let $a; let n_expect = n_expect + 1;)*

            match n_args.cmp(&n_expect) {
                std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::TooFewArgs),
                std::cmp::Ordering::Greater => return Err(sunny_vm::ErrorKind::TooManyArgs),
                std::cmp::Ordering::Equal => {}
            }

            $($a = vm.pop_value().unwrap();)*
            // wrap body in closure, so `return`ing from the body works as expected
            let ret = (||$body)();
            vm.push_value(ret?);
            Ok(())
        }
    };

    (def varfn $name:ident($($a:ident: Value,)* [$vararg:ident]) -> Result<Value> $body:block) => {
        fn $name(n_args: usize, vm: &mut sunny_vm::Vm) -> Result<()> {
            let n_expect = 0;
            $(let $a; let n_expect = n_expect + 1;)*

            match n_args.cmp(&n_expect) {
                std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::TooFewArgs),
                _ => {}
            }

            $($a = vm.pop_value().unwrap();)*

            let n_varargs = n_args - n_expect;
            let mut $vararg = vec![];
            for _ in 0..n_varargs {
                $vararg.push(vm.pop_value().unwrap());
            }

            // wrap body in closure, so `return`ing from the body works as expected
            let ret = (||$body)();
            vm.push_value(ret?);
            Ok(())
        }
    };
}