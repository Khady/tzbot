default: all

all:
	dune build

clean:
	dune clean

lock:
	opam lock *.opam

.PHONY: default all clean lock
