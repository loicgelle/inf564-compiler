all: compiler

compiler:
	ocamlbuild main.native -Is mini-c -Is pass3_utils

clean-tests:
	rm -rf tests/exec/*.s
	rm -rf tests/exec-fail/*.s

clean: clean-tests
	rm -rf _build
	rm -f main.native

tests1:
	cd tests; ./run -1 ../main.native

tests2:
	cd tests; ./run -2 ../main.native

tests3:
	cd tests; ./run -3 ../main.native

docker-init:
	docker build -t compiler-base docker-base

docker-init-with-proxy:
	docker build -t compiler-base docker-base-with-proxy

docker-tests: clean clean-tests
	docker build --no-cache=true -t compiler .
	docker run compiler
