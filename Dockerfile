FROM ocaml/opam2:alpine

RUN opam install dune

COPY . /src
WORKDIR /src

RUN . ~/.profile \
  && sudo dune build bin/mlmlc.exe

FROM alpine

COPY --from=0 /src/_build/default/bin/mlmlc.exe /usr/local/bin/mlmlc
ENTRYPOINT ["/usr/local/bin/mlmlc"]
