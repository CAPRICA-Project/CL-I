FROM ocamlpro/ocaml

RUN opam switch create . ocaml-system --deps --locked
RUN opam pin base v0.15.1
RUN opam pin git+https://github.com/bensmrs/ppx_inline_module
RUN opam pin git+https://github.com/bensmrs/ppx_map
RUN opam pin git+https://github.com/bensmrs/ppx_macro
RUN opam install ppx_string
RUN opam install tsort
RUN opam install visitors
RUN opam install z3
RUN opam install menhir

WORKDIR /home/ocaml
RUN git clone https://github.com/CAPRICA-Project/CL-I.git cl-i
WORKDIR cl-i
RUN git checkout sefm2023
RUN eval $(opam env) && dune build
