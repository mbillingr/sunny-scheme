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
        .unwrap();

    cmd.stdin
        .as_mut()
        .unwrap()
        .write_all(b"(display ((lambda (x) (+ x 2)) 40))")
        .unwrap();

    let output = cmd.wait_with_output().unwrap();

    if !output.status.success() {
        panic!("{}", String::from_utf8(output.stderr).unwrap());
    }

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");

    fs::write(&dest_path, output.stdout).unwrap();

    let _ = Command::new("rustfmt")
        .arg(&dest_path)
        .output();
}
