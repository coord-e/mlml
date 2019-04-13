#!/bin/bash

docker build "$(dirname $0)" -t mlml-test
docker run --rm -t -v $(pwd):/src mlml-test dune runtest
