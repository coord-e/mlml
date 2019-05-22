#!/bin/bash

set -euo pipefail

readonly WORKDIR="$(mktemp -d)"

function info () {
  echo "$(tput setaf 2)=> $(tput sgr0)$(tput bold)$@$(tput sgr0)"
}

function cmd () {
  echo "$(tput setaf 5)$ $@$(tput sgr0)"
  eval $@
}

function compile () {
  readonly ASMOUT="$WORKDIR/mlml.s"
  readonly EXECOUT="$1"
  shift

  cmd "$@ bin/mlmlc.ml > \"$ASMOUT\""
  cmd "gcc \"$ASMOUT\" -lgc -o \"$EXECOUT\""
}

function main () {
  readonly GEN1="$WORKDIR/mlml1"
  readonly GEN2="$WORKDIR/mlml2"

  info "Compiling mlml with ocaml"
  compile $GEN1 dune exec bin/mlmlc.exe
  info "Successfully compiled 1st-gen compiler: $GEN1"

  info "Compiling mlml with mlml"
  compile $GEN2 $GEN1
  info "Successfully compiled 2nd-gen compiler: $GEN2"
}

main
