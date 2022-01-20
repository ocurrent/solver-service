# Solver Service

*Status: WIP & Experimental*

A standalone, OCurrent service for solving opam dependencies extracted and modified from [OCaml-CI](https://github.com/ocurrent/ocaml-ci).

## Example

The `./example` directory contains a small CLI tool for testing the solver service over a TCP connection. It requires a package name and version to solve for (note it will use `opam` to fetch the opam file information). To test it, you first must install and run the solver service. It spawns workers by recursively calling itself (using `Sys.argv.(0)`) so it is important to run it using its proper name rather than with `dune exec --`.

```sh
$ solver-service --address=tcp:127.0.0.1:7000
Solver service running at: <capnp-address>
```

Copy the `<capnp-address>` and run the example binary passing in the address.

```sh
$ dune exec -- ./example/main.exe --package=yaml --version=3.0.0 <capnp-address>
```