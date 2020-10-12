#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::utils::exports::*;
}

pub mod exports {
    pub use super::rustify_minus_identifier;
    pub use super::rustify_minus_libname;
    pub use super::rustify_minus_testname;
}

pub fn rustify_minus_identifier(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__260 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (cond ((same-name? name (quote args)) "args_") ((same-name? name (quote fn)) "fn_") ((same-name? name (quote loop)) "loop_") ((same-name? name (quote let)) "let_") ((same-name? name (quote mut)) "mut_") ((same-name? name (quote ref)) "ref_") ((same-name? name (quote self)) "self_") (else (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (cond ((same-name? name (quote args)) "args_") ((same-name? name (quote fn)) "fn_") ((same-name? name (quote loop)) "loop_") ((same-name? name (quote let)) "let_") ((same-name? name (quote mut)) "mut_") ((same-name? name (quote ref)) "ref_") ((same-name? name (quote self)) "self_") (else (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name))))))))
                {
                    let [char_minus_map__256, append_minus_all__258] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all__258 = append_minus_all__258.into_boxed();
                        {
                            let char_minus_map__256 = char_minus_map__256.into_boxed();
                            {
                                char_minus_map__256.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch__257 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\?)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('?')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_p")
                                            } else if ({
                                                // (eq? ch #\!)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('!')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_i")
                                            } else if ({
                                                // (eq? ch #\<)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('<')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_l_")
                                            } else if ({
                                                // (eq? ch #\>)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('>')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_g_")
                                            } else if ({
                                                // (eq? ch #\=)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('=')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_e_")
                                            } else if ({
                                                // (eq? ch #\-)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_minus_")
                                            } else if ({
                                                // (eq? ch #\+)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('+')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_plus_")
                                            } else if ({
                                                // (eq? ch #\*)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('*')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_star_")
                                            } else if ({
                                                // (eq? ch #\/)
                                                imports::eq_p(&[ch__257.clone(), Scm::char('/')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_slash_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch__257.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all__258.set({
                                    // Closure
                                    let append_minus_all__258 = append_minus_all__258.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs__259 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs__259.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append(&[
                                                    {
                                                        // (car strs)
                                                        imports::car(&[strs__259.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all__258.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs__259.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (cond ...)
                                    if ({
                                        // (same-name? name (quote args))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("args"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("args_")
                                    } else if ({
                                        // (same-name? name (quote fn))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("fn"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("fn_")
                                    } else if ({
                                        // (same-name? name (quote loop))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("loop"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("loop_")
                                    } else if ({
                                        // (same-name? name (quote let))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("let"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("let_")
                                    } else if ({
                                        // (same-name? name (quote mut))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("mut"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("mut_")
                                    } else if ({
                                        // (same-name? name (quote ref))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("ref"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("ref_")
                                    } else if ({
                                        // (same-name? name (quote self))
                                        imports::same_minus_name_p(&[
                                            name__260.clone(),
                                            Scm::symbol("self"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("self_")
                                    } else {
                                        {
                                            // (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name))))
                                            append_minus_all__258.get().invoke(&[{
                                                // (map char-map (string->list (if (symbol? name) (symbol->string name) name)))
                                                imports::map(&[char_minus_map__256.get(), {
                                                    // (string->list (if (symbol? name) (symbol->string name) name))
                                                    imports::string_minus__g_list(&[
                                                        if ({
                                                            // (symbol? name)
                                                            imports::symbol_p(&[name__260.clone()])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (symbol->string name)
                                                                imports::symbol_minus__g_string(&[
                                                                    name__260.clone(),
                                                                ])
                                                            }
                                                        } else {
                                                            name__260.clone()
                                                        },
                                                    ])
                                                }])
                                            }])
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn rustify_minus_libname(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__266 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))))
                {
                    let [char_minus_map__261, append_minus_all__263] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all__263 = append_minus_all__263.into_boxed();
                        {
                            let char_minus_map__261 = char_minus_map__261.into_boxed();
                            {
                                char_minus_map__261.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch__262 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                imports::eq_p(&[ch__262.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\-)
                                                imports::eq_p(&[ch__262.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch__262.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all__263.set({
                                    // Closure
                                    let append_minus_all__263 = append_minus_all__263.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs__264 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs__264.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append(&[
                                                    {
                                                        // (car strs)
                                                        imports::car(&[strs__264.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all__263.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs__264.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))
                                    {
                                        let name__265 = if ({
                                            // (symbol? name)
                                            imports::symbol_p(&[name__266.clone()])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (symbol->string name)
                                                imports::symbol_minus__g_string(
                                                    &[name__266.clone()],
                                                )
                                            }
                                        } else {
                                            name__266.clone()
                                        };
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? name (quote fn))
                                                imports::eq_p(&[
                                                    name__265.clone(),
                                                    Scm::symbol("fn"),
                                                ])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("fn_")
                                            } else {
                                                {
                                                    // (append-all (map char-map (string->list name)))
                                                    append_minus_all__263.get().invoke(&[{
                                                        // (map char-map (string->list name))
                                                        imports::map(&[
                                                            char_minus_map__261.get(),
                                                            {
                                                                // (string->list name)
                                                                imports::string_minus__g_list(&[
                                                                    name__265.clone(),
                                                                ])
                                                            },
                                                        ])
                                                    }])
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn rustify_minus_testname(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__271 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (append-all (filter (lambda (x) x) (map char-map (string->list name)))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (append-all (filter (lambda (x) x) (map char-map (string->list name))))))
                {
                    let [char_minus_map__267, append_minus_all__269] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all__269 = append_minus_all__269.into_boxed();
                        {
                            let char_minus_map__267 = char_minus_map__267.into_boxed();
                            {
                                char_minus_map__267.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch__268 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\ )
                                                imports::eq_p(&[ch__268.clone(), Scm::char(' ')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else if ({
                                                // (eq? ch #\')
                                                imports::eq_p(&[
                                                    ch__268.clone(),
                                                    Scm::char_apostrophe(),
                                                ])
                                            })
                                            .is_true()
                                            {
                                                Scm::False
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch__268.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all__269.set({
                                    // Closure
                                    let append_minus_all__269 = append_minus_all__269.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs__270 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs__270.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append(&[
                                                    {
                                                        // (car strs)
                                                        imports::car(&[strs__270.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all__269.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs__270.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (append-all (filter (lambda (x) x) (map char-map (string->list name))))
                                    append_minus_all__269.get().invoke(&[{
                                        // (filter (lambda (x) x) (map char-map (string->list name)))
                                        imports::filter(&[
                                            {
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x__272 = args[0].clone();
                                                    x__272.clone()
                                                })
                                            },
                                            {
                                                // (map char-map (string->list name))
                                                imports::map(&[char_minus_map__267.get(), {
                                                    // (string->list name)
                                                    imports::string_minus__g_list(&[
                                                        name__271.clone()
                                                    ])
                                                }])
                                            },
                                        ])
                                    }])
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::utils::initialize();
    {
        (/*NOP*/);
        {
            // (define (rustify-identifier name) ...)
            (/*NOP*/)
        };
        {
            // (define (rustify-libname name) ...)
            (/*NOP*/)
        };
        {
            // (define (rustify-testname name) ...)
            (/*NOP*/)
        }
    };
}
