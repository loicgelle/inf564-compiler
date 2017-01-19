all: compiler

compiler:
	ocamlbuild main.native

clean:
	rm -rf _build
	rm -f main.native

tests1:
	cd tests; ./run -1 ../main.native
