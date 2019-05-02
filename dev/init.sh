#!/bin/bash

set -euo pipefail

readonly IMAGE_NAME=mlml-dev

[ -z "$(sudo docker image ls -q ${IMAGE_NAME})" ] && sudo docker build "$(dirname $0)" -t ${IMAGE_NAME} --build-arg LOCAL_UID=$(id -u $USER) --build-arg LOCAL_GID=$(id -g $USER)

function exec_docker() {
  # TRAVIS_JOB_ID is for CI environment
  sudo docker run --rm -t -v $(pwd):/home/opam/src -e TRAVIS_JOB_ID=${TRAVIS_JOB_ID:-} ${IMAGE_NAME} "$@"
}
