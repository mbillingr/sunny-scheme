macro_rules! pipe {
    ($f:tt) =>  { $f };
    ($f:tt $g:ident) => { |x| $g(&[$f(x)]) };
    ($f:tt $g:ident $($rest:ident)*) => { pipe![ (|x| $g(&[$f(x)])) $($rest)*] };
}

macro_rules! pipe_fn {
    ($f:ident($i:ty) -> $r:ty = $($g:ident)*) => {
        pub fn $f(x: $i) -> $r { pipe![$($g)*](x) }
    };
}

macro_rules! wrap_fn1 {
    ($name:ident = $f:path) => {
        wrap_fn1!{stringify!($name), $name = $f}
    };

    ($scm:expr, $name:ident = $f:path) => {
        pub fn $name(args: &[Scm]) -> Scm {
            match args {
                [a] => $f(a).into(),
                _ => panic!("({} a) called with {} arguments", $scm, args.len()),
            }
        }
    };
}

macro_rules! wrap_fn2 {
    ($name:ident = $f:path) => {
        wrap_fn2!{stringify!($name), $name = $f}
    };

    ($scm:expr, $name:ident = $f:path) => {
        pub fn $name(args: &[Scm]) -> Scm {
            match args {
                [a, b] => $f(a, b).into(),
                _ => panic!("({} a b) called with {} arguments", $scm, args.len()),
            }
        }
    };
}

macro_rules! wrap_fn {
    ($name:ident() $body:tt) => {
        wrap_fn!{stringify!($name), $name() $body}
    };

    ($name:ident($a:ident) $body:tt) => {
        wrap_fn!{stringify!($name), $name($a) $body}
    };

    ($name:ident($($args:ident),*) $body:tt) => {
        wrap_fn!{stringify!($name), $name($($a),*) $body}
    };

    ($scm:expr, $name:ident() $body:tt) => {
        pub fn $name(args: &[Scm]) -> Scm {
            match args {
                [] => {$body}
                _ => panic!("({}) called with {} arguments", $scm, args.len()),
            }
        }
    };

    ($scm:expr, $name:ident($a:ident) $body:tt) => {
        pub fn $name(args: &[Scm]) -> Scm {
            match args {
                [$a] => {$body}
                _ => panic!("({} {}) called with {} arguments", $scm, stringify!($a), args.len()),
            }
        }
    };

    ($scm:expr, $name:ident($($args:ident),*) $body:tt) => {
        pub fn $name(args: &[Scm]) -> Scm {
            match args {
                [$($args),*] => {$body}
                _ => panic!("({} {}) called with {} arguments", $scm, stringify!($($args)*), args.len()),
            }
        }
    };
}

pub mod native;
