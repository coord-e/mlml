FROM ocaml/opam2:debian-stable

RUN opam install dune bisect_ppx
RUN sudo cp $HOME/.profile /root
RUN sudo apt-get update
RUN sudo apt-get install -y inotify-tools

VOLUME /src
WORKDIR /src

ENV DUNE_BUILD_DIR $HOME/_build
ENV MLML_STDLIB_DIR /src/test/test_stdlib

CMD ["dune", "runtest", "-w"]
