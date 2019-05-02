#!/bin/bash

set -euo pipefail

readonly IMAGE_NAME=mlml-dev

if docker version &> /dev/null; then
  readonly DOCKER=docker
else
  readonly DOCKER="sudo docker"
fi

# Build if the image is not found
[ -z "$($DOCKER image ls -q ${IMAGE_NAME})" ] \
  && $DOCKER build "$(dirname $0)"            \
       -t ${IMAGE_NAME}                       \
       --build-arg LOCAL_UID=$(id -u $USER)   \
       --build-arg LOCAL_GID=$(id -g $USER)

function exec_docker() {
  # TRAVIS_JOB_ID is for CI environment
  $DOCKER run --rm -t -v $(pwd):/home/opam/src -e TRAVIS_JOB_ID=${TRAVIS_JOB_ID:-} ${IMAGE_NAME} "$@"
}
