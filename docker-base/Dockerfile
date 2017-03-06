FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y build-essential gcc ocaml opam m4 && \
    opam init && \
    opam config env && \
    opam install ocamlbuild menhir
