default: all

all:
	dune build

clean:
	dune clean

lock:
	opam lock *.opam

init:
	opam install . --deps-only --locked

fmt:
	dune build @fmt --auto-promote

.PHONY: default all clean lock init
