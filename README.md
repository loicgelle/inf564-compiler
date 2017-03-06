# Mini-C Compiler in OCaml

## Dependencies

- gcc
- ocaml
- ocamlbuild (can be installed using opam)
- menhir (can be installed using opam)
- Docker (see https://www.docker.com/ for installation)

## Installation

1. First, clone the project
```
git clone git@github.com:loicgelle/inf564-compiler.git
```

2. The project can then be natively compiled using the command
```
make
```

## Testing

The first two levels of tests do not depend on the host architecture; therefore they can be performed using the compiled binary with the command:

```
make tests1
```

and

```
make tests2
```

The final level of tests depend on the host architecture. To ensure consistency between the development and the testing environment, the tests are encapsulated and performed in a Docker container. The base container has to be built before the tests are launched:

```
make docker-init
```

If you are behind Polytechnique proxy, you should launch
```
make docker-init-with-proxy
```

instead. The building phase of the base container can take a few minutes since packages are installed in the container.

The tests can then be launched with:
```
make docker-tests
```

Note that the previous command rebuilds the project before performing the tests. Therefore, you do not have to rebuild the project before launching the tests with Docker.
