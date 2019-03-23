default: all

all:
	dune build

clean:
	dune clean

lock:
	opam lock *.opam

init:
	opam install . --deps-only --locked

.PHONY: default all clean lock init
