#!/bin/bash

source "$(dirname $0)/init.sh"

docker run --rm -t -v $(pwd):/src mlml-dev sudo "$@"
