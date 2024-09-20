
.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/tests.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f monopoly.zip
	zip -r monopoly.zip . 

doc:
	dune build @doc
