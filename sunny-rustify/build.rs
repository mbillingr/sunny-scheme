use std::env;
use std::fs;
use std::io::prelude::*;
use std::path::Path;
use std::process::{Command, Stdio};

fn main() {
    let mut cmd = Command::new("chibi-scheme")
        .arg("scm2rs.scm")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Could not call Scheme interpreter");

    cmd.stdin
        .as_mut()
        .unwrap()
        .write_all(
            b"
(import (scheme base)
        (scheme write))

(define foo 40)

(define (println x) (display x) (newline))

(println ((lambda (x) (+ x 2)) foo))

(let ((x 1) (y 2))
  (assert-eq (+ x y) 3)

  (let ((x y) (y x))
    (assert-eq x 2)
    (assert-eq y 1))

  (let* ((x y) (y x))
    (assert-eq x 2)
    (assert-eq y 2)))

(define (gen x)
  (lambda () x))

(letrec ((even? (lambda (x)
                  (if (= x 0)
                      #t
                      (odd? (- x 1)))))
         (odd? (lambda (x)
                 (if (= x 0)
                     #f
                     (even? (- x 1))))))
   (assert-eq (odd? 42) #f)
   (assert-eq (even? 42) #t))

(define (foo x)
  (set! x 123)
  (println x))
(foo 0)

(define (even? x)
    (if (= x 0)
        #t
        (odd? (- x 1))))

(define (odd? x)
    (if (= x 0)
        #f
        (even? (- x 1))))

(assert-eq (even? 3007) #f)

(define (outer)
    (define (inner)
        0
    )
    inner
)
(println (outer))
(println ((outer)))

(println 'foo)
(println (cons 1 (cons 2 3)))
(println (cons 1 (cons 2 (cons 3 '()))))
(println '(1 2 3 . 4))

(assert-eq (car (cons 1 2)) 1)
(assert-eq (cdr (cons 1 2)) 2)

(define (sign x)
    (cond ((= x 0) 0)
          ((< x 0) -1)
          (else 1)))
(assert-eq (sign 0) 0)
(assert-eq (sign -2) -1)
(assert-eq (sign 3) 1)

(println 7531902468)
",
        )
        .unwrap();

    let output = cmd.wait_with_output().unwrap();

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");

    fs::write(&dest_path, output.stdout).expect("Error writing destination file");

    if !output.status.success() {
        panic!("{}", String::from_utf8(output.stderr).unwrap());
    }

    let _ = Command::new("rustfmt").arg(&dest_path).output();
}
