macro_rules! pipe {
    ($f:tt) =>  { $f };
    ($f:tt $g:ident) => { |x| $g(&[$f(x)]) };
    ($f:tt $g:ident $($rest:ident)*) => { pipe![ (|x| $g(&[$f(x)])) $($rest)*] };
}

pub mod native;
