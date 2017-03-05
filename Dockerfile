FROM compiler-base

COPY ./ /compiler/
WORKDIR /compiler
ENV PATH="/root/.opam/system/bin:${PATH}"

CMD echo $PATH && \
    ls /root/.opam/system/bin && \
    make && \
    make tests3
    #./main.native tests/exec/uminus1.c && \
    #gcc -g -O0 tests/exec/uminus1.s -o test && \
    #./test
