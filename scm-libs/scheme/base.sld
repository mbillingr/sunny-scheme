(define-library (scheme base)
  (export
    = > < - +
    car caar cadr
    cdr cdar cddr
    char? close-port cons
    eof-object?
    eq? equal?
    error
    list->string
    null?
    pair?
    set-car! set-cdr!
    string->list string-append string<?
    symbol? symbol->string)

  (import (native base)))
