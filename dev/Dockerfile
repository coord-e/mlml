FROM ocaml/opam2:alpine

RUN opam install dune bisect_ppx
RUN sudo apk --update --no-cache add inotify-tools

VOLUME /src
WORKDIR /src

CMD ["sudo", "dune", "runtest", "-w"]
