#!/bin/sh

cd scheme
chibi-scheme scm2rs.scm scm2rs.scm ../src/scm2rs.rs
cd ..
cargo fmt
