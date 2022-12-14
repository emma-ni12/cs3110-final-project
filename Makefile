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
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f cs3110-final-project.zip
	zip -r cs3110-final-project.zip .

doc:
	dune build @doc

cloc:
	dune clean
	cloc --by-file --include-lang=OCaml .

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean
	rm -f cs3110-final-project.zip
