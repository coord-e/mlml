#!/bin/bash

set -euo pipefail

readonly WORKDIR="${1:-$(mktemp -d)}"

function info () {
  >&2 echo "$(tput setaf 2)=> $(tput sgr0)$(tput bold)$@$(tput sgr0)"
}

function warn () {
  >&2 echo "$(tput setaf 3)$(tput bold)WARN $(tput sgr0)$@"
}

function cmd () {
  >&2 echo "$(tput setaf 5)$ $@$(tput sgr0)"
  eval $@
}

function check_environment () {
  function check_stack_size () {
    local max_stack=$(ulimit -s)
    [ $max_stack -gt 1000000 ] \
      || warn "Compilation might fail with segfault due to small maximum stack size"
  }

  function check_docker () {
    # thanks to Henk Lengeveld from StackOverflow
    # https://stackoverflow.com/questions/23513045/
    cat /proc/1/cgroup | grep -q "/docker" \
      || warn "It seems that this script is running outside of the development docker container"
  }

  check_stack_size
  check_docker
}

function compile () {
  local ASMOUT="$WORKDIR/mlml.s"
  local EXECOUT="$1"
  shift

  cmd "$@ bin/mlmlc.ml > \"$ASMOUT\""
  cmd "gcc \"$ASMOUT\" -lgc -o \"$EXECOUT\""
  md5sum "$ASMOUT" | cut -d' ' -f1
}

function main () {
  readonly GEN1="$WORKDIR/mlml1"
  readonly GEN2="$WORKDIR/mlml2"
  readonly GEN3="$WORKDIR/mlml3"

  info "Compiling mlml with ocaml"
  readonly GEN1_HASH=$(compile $GEN1 dune exec bin/mlmlc.exe)
  info "Successfully compiled 1st-gen compiler: $GEN1 (${GEN1_HASH:0:7})"

  info "Compiling mlml with mlml (1st generation)"
  readonly GEN2_HASH=$(compile $GEN2 $GEN1)
  info "Successfully compiled 2nd-gen compiler: $GEN2 (${GEN2_HASH:0:7})"

  info "Compiling mlml with mlml (2nd generation)"
  readonly GEN3_HASH=$(compile $GEN3 $GEN2)
  info "Successfully compiled 3rd-gen compiler: $GEN3 (${GEN3_HASH:0:7})"

  if [ "$GEN2_HASH" = "$GEN3_HASH" ]; then
    info "SELF HOSTING SUCCESSFUL"
  else
    info "Self hosting failed (output mismatch between 2nd and 3rd)"
  fi
}

check_environment
main
