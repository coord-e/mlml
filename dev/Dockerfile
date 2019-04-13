FROM ocaml/opam2:alpine

RUN opam install dune
RUN sudo apk --update --no-cache add inotify-tools

VOLUME /src
WORKDIR /src

CMD ["dune", "runtest", "-w"]
