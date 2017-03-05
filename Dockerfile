FROM compiler-base

COPY ./ /compiler/
WORKDIR /compiler
ENV PATH="/root/.opam/system/bin:${PATH}"

CMD echo $PATH && \
    ls /root/.opam/system/bin && \
    make && \
    make tests3
    #./main.native testprg.c && \
    #gcc -g -O0 testprg.s -o test && \
    #./test
