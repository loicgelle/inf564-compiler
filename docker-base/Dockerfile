FROM ubuntu:latest

ENV http_proxy 'http://kuzh.polytechnique.fr:8080'
ENV https_proxy 'http://kuzh.polytechnique.fr:8080'
RUN apt-get update && \
    apt-get install -y build-essential gcc ocaml opam m4 && \
    opam init && \
    opam config env && \
    opam install ocamlbuild menhir
