(define-library (scheme base)
  (export
    = > < - +
    append
    apply
    assq
    car caar cadr
    cdr cdar cddr
    char? close-port cons
    eof-object?
    eq? equal?
    error
    for-each
    length
    list
    list?
    list->string
    list-copy
    map
    memq
    not
    null?
    pair?
    procedure?
    reverse
    set-car! set-cdr!
    string->list string=? string-append string<?
    symbol? symbol->string symbol=?)

  (import (native base))

  (begin

    ; ====== logic =======================

    (define (not x)
      (if x #f #t))

    ; ====== lists =======================

    (define (list . x) x)

    (define (list? seq)
      (cond ((null? seq) #t)
            ((pair? seq) (list? (cdr seq)))
            (else #f)))

    (define (length seq)
      (fold-left (lambda (acc _) (+ acc 1)) 0 seq))

    (define (reverse seq)
      (fold-left (lambda (acc x) (cons x acc)) '() seq))

    (define (list-copy seq)
      (fold-right cons '() seq))

    (define (assq obj seq)
      (if (pair? seq)
          (if (eq? obj (caar seq))
              (car seq)
              (assq obj (cdr seq)))
          #f))

    (define (memq obj seq)
      (if (pair? seq)
          (if (eq? obj (car seq))
              seq
              (memq obj (cdr seq)))
          #f))

    (define (append . seq)
      (fold-right append2 '() seq))

    (define (append2 list1 list2)
      (if (null? list1)
          list2
          (cons (car list1)
                (append2 (cdr list1) list2))))


    ; ====== symbols =======================

    (define (symbol=? s1 . args)
      (all? (lambda (s) (eq? s1 s)) args))

    ; ====== strings =======================

    (define (string=? s1 . args)
      (all? (lambda (s) (equal? s1 s)) args))

    (define (string-append s1 . args)
      (fold-left string-cons s1 args))

    ; ====== control features =======================

    (define (for-each proc . seq*)
      (_for-each proc seq*))

    (define (map func . seq*)
      (_map func seq*))

    (define (_for-each proc seq*)
      (if (any? null? seq*)
          '()
          (begin (apply proc (map-1 car seq*))
                 (_for-each proc (map-1 cdr seq*)))))

    (define (_map func seq*)
      (if (any? null? seq*)
          '()
          (cons (apply func (map-1 car seq*))
                (_map func (map-1 cdr seq*)))))

    ; ====== low-level building blocks =======================

    (define (all? pred seq)
      (cond ((null? seq) #t)
            ((pred (car seq)) (all? pred (cdr seq)))
            (else #f)))

    (define (any? pred seq)
      (cond ((null? seq) #f)
            ((pred (car seq)) #t)
            (else (any? pred (cdr seq)))))

    (define (map-1 func seq)
      (fold-right (lambda (x acc) (cons (func x) acc)) '() seq))

    (define (fold-right op init seq)
      (if (null? seq)
          init
          (op (car seq)
              (fold-right op init (cdr seq)))))

    (define (fold-left op init seq)
      (if (null? seq)
          init
          (fold-left op
                     (op init (car seq))
                     (cdr seq))))))
