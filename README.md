# Solver Service

*Status: WIP & Experimental*

A standalone, OCurrent service for solving opam dependencies extracted and modified from [OCaml-CI](https://github.com/ocurrent/ocaml-ci). This repostory contains:

 - `src/solver-service-api`: The Capnp solver service API, the defines the format for sending requests and receiving responses from a service.
 - `src/solver-service`: The core functionality of completing a solver request using `opam-0install` is contained here. There are binaries for running a `solver-service` which can communicate over `stdin/stdout` or over the network.
 - `src/solver-worker`: An OCluster worker that can solve requests being submitted to OCluster as a custom job specification.

## Example

### A solver service

The `./examples` directory contains a small CLI tool for testing the solver service over a TCP connection. It requires a package name and version to solve for (note it will use `opam` to fetch the opam file information). To test it, you first must install and run the solver service. It spawns workers by recursively calling itself (using `Sys.argv.(0)`) so it is important to run it using its proper name rather than with `dune exec --`.

```sh
$ solver-service --address=tcp:127.0.0.1:7000
Solver service running at: <capnp-address>
```

Copy the `<capnp-address>` and run the example binary passing in the address.

```sh
$ dune exec -- ./examples/main.exe --package=yaml --version=3.0.0 <capnp-address>
```

### A solver worker

The solver worker can join an existing pool on a scheduler and solve request jobs sent as a custom job type. The example [`submit.ml`](examples/submit.ml) will send such a job to a scheduler (you need to supply the submission cap file).

To try this example locally, first get the scheduler up and running.

```
$ mkdir capnp-secrets
$ ocluster-scheduler --capnp-secret-key-file=capnp-secrets/key.cap --capnp-listen-address=tcp:127.0.0.1:9000 --pools=solver --state-dir=var --verbosity=info
```

This will write an `admin.cap` file into `capnp-secrets`. We can use this to add a new client and get a submission capability file.

```
$ ocluster-admin --connect ./capnp-secrets/admin.cap add-client solver > capnp-secrets/submission.cap
```

Now, we need to connect our solver worker to the pool. In a new terminal connect the worker using the pool registration cap file.

```
$ dune exec -- solver-worker --connect=capnp-secrets/pool-solver.cap --name=solver-1 --state-dir=var --verbosity=info
```

With this running we can use the example submission pipeline to solve the dependencies for the [obuilder](https://github.com/ocurrent/obuilder) repository on Github.

```
$ dune exec -- examples/submit.exe --submission-service=capnp-secrets/submission.cap
```

You should then be able to watch the pipeline in action at `http://localhost:8080`.
