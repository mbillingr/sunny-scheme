#!/bin/sh

chibi-scheme src/scm/scm2rs.scm src/scm/scm2rs.scm src/scm2rs.rs
cargo fmt

