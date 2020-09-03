#!/bin/sh

cd src
chibi-scheme -A ../scheme/lib ../scheme/scm2rs.scm ../scheme/scm2rs.scm scm2rs.rs
cd ..
cargo fmt
