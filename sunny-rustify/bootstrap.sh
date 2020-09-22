#!/bin/sh

chibi-scheme -I scheme/lib scheme/scm2rs.scm scheme/scm2rs.scm src/scm2rs

cargo fmt
