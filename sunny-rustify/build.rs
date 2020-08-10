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
(define foo 40)

(define (println x) (display x) (newline))

(println ((lambda (x) (+ x 2)) foo))

(let ((x 1) (y 2))
  (println (+ x y)))

(letrec ((even? (lambda (x)
                  (if (= x 0)
                      #t
                      (if (= x 1)
                          #f
                          (odd? (- x 1))))))
         (odd? (lambda (x)
                 (if (= x 0)
                     #f
                     (if (= x 1)
                         #t
                         (even? (- x 1)))))))
   (println (odd? 42)))

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
