#!/bin/bash

[ -z "$(docker image ls -q mlml-dev)" ] && docker build "$(dirname $0)" -t mlml-dev
