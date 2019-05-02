#!/bin/bash

[ -z "$(sudo docker image ls -q mlml-dev)" ] && sudo docker build "$(dirname $0)" -t mlml-dev --build-arg LOCAL_UID=$(id -u $USER) --build-arg LOCAL_GID=$(id -g $USER)

function exec_docker() {
  sudo docker run --rm -t -v $(pwd):/home/opam/src mlml-dev "$@"
}
