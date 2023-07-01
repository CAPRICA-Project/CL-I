#!/usr/bin/env bash

set -euo pipefail

opam switch create . ocaml-base-compiler.4.14.1 --deps --locked
opam pin -y base v0.15.1
opam pin -y git+https://github.com/bensmrs/ppx_inline_module#0.1.0
opam pin -y git+https://github.com/bensmrs/ppx_map#0.2.1
opam pin -y git+https://github.com/bensmrs/ppx_macro#0.1.0
opam install -y ppx_string.v0.15.0 tsort.2.1.0 visitors.20210608 z3.4.12.2-1 menhir.20230608

eval $(opam env) && dune build
