
.PHONY: default all clean

default: all

all:
	dune build

clean:
	dune clean
