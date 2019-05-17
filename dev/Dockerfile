FROM ocaml/opam2:debian-stable

ARG LOCAL_UID
ARG LOCAL_GID

RUN sudo usermod -u ${LOCAL_UID} -o opam
RUN sudo groupmod -g ${LOCAL_GID} -o opam

RUN opam install dune bisect_ppx
RUN sudo apt-get update
RUN sudo apt-get install -y inotify-tools libgc-dev

ENV SRC_DIR=${HOME}/src

VOLUME ${SRC_DIR}
WORKDIR ${SRC_DIR}

ENV MLML_STDLIB_DIR ${SRC_DIR}/stdlib

CMD ["dune", "runtest", "-w"]
