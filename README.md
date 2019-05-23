# mlml

[![Build Status](https://travis-ci.com/coord-e/mlml.svg?branch=develop)](https://travis-ci.com/coord-e/mlml)
[![Coverage Status](https://coveralls.io/repos/github/coord-e/mlml/badge.svg)](https://coveralls.io/github/coord-e/mlml)
![Docker Cloud Automated build](https://img.shields.io/docker/cloud/automated/coorde/mlml.svg)
![Docker Cloud Build Status](https://img.shields.io/docker/cloud/build/coorde/mlml.svg)
[![MicroBadger Image](https://images.microbadger.com/badges/image/coorde/mlml.svg)](https://microbadger.com/images/coorde/mlml)

a compiler for a tiny subset of OCaml, which is written just for fun.

## roadmap

- [x] basic arithmetic
- [x] variables
- [x] if-then-else
- [x] functions
  - [x] recursion
  - [x] mutual recursion
  - [x] closure
  - [x] currying
- [x] tuples
- [x] variants
- [x] records
- [x] pattern matching
- [x] structual comparison
- [x] primitive types
  - [x] string
  - [x] list
  - [x] bytes
  - [x] array
- [x] formatted output with `Printf`
- [x] modules
  - [x] definition
  - [x] aliases
  - [x] `open`
  - [x] importing from other files
- [x] **self-hosting!**
- [ ] exceptions
- [ ] type checker & type inference

## self hosting

mlml is self-hosted. i.e. mlml can compile itself.

```shell
./dev/exec.sh ./dev/self_host.sh
```

To obtain build artifacts, pass a path to local directory as below. You will see compiled binaries under `./self_host`.

```shell
mkdir self_host
./dev/exec.sh ./dev/self_host.sh ./self_host
```

## limitations

- `external` definitions are only available for functions
- all modules and paths are statically-resolved
- all custom oeprators are left-associative
- `function` keyword does not make an expression

## development

If you have docker installed in your system, simply run

```shell
./dev/start.sh
```

to start the development.

You can run tests manually by running the following command:

```shell
./dev/exec.sh dune runtest
```

## thanks

The code and algorithm in parser and lexer is strongly inspired by [ushitora-anqou/aqaml](https://github.com/ushitora-anqou/aqaml)
