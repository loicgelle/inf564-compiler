all: compiler tests2

compiler:
	ocamlbuild main.native

clean:
	rm -rf _build
	rm -f main.native

tests1:
	cd tests; ./run -1 ../main.native

tests2:
	cd tests; ./run -2 ../main.native
