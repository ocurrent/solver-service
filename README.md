# Solver Service

A standalone, OCurrent service for solving opam dependencies extracted and modified from [OCaml-CI](https://github.com/ocurrent/ocaml-ci). This repostory contains:

 - `api`: The Capnp solver service API, the defines the format for sending requests and receiving responses from a service.
 - `service`: The core functionality of completing a solver request using `opam-0install` is contained here.
 - `worker`: An OCluster worker that can solve requests being submitted to OCluster as a custom job specification.
 - `bin`:  Binaries for running as either a stand-alone service or as an OCluster worker.

## Example

### A solver service

The `./examples` directory contains a small CLI tool for testing the solver service over a TCP connection. It requires a package name and version to solve for (note it will use `opam` to fetch the opam file information). To test it, you first must install and run the solver service.

```sh
$ mkdir capnp-secrets
$ dune exec -- solver-service run-service \
    --cache-dir=./cache \
    --capnp-secret-key-file=server.pem \
    --capnp-listen-address=tcp:127.0.0.1:7000 \
    --cap-file=./capnp-secrets/solver.cap
Wrote solver service's address to "./capnp-secrets/solver.cap"
```

Then run the example client to test it:

```sh
$ dune exec -- ./examples/main.exe --package=yaml --version=3.0.0 ./capnp-secrets/solver.cap
```

For a more thorough test, you can use the stress tester:

```sh
$ dune exec -- ./stress/stress.exe service ./capnp-secrets/solver.cap --count=10
Solved warm-up requests in: 9.85s
Running another 10 solves...
10/10 complete
Solved 10 requests in 4.79s (0.48s/iter)
```

### An OCluster worker

The solver worker can join an existing pool on a scheduler and solve request jobs sent as a custom job type. The example [`submit.ml`](examples/submit.ml) will send such a job to a scheduler (you need to supply the submission cap file).

To try this example locally, first get the scheduler up and running.

```
$ ocluster-scheduler --capnp-secret-key-file=capnp-secrets/scheduler-key.cap --capnp-listen-address=tcp:127.0.0.1:9000 --pools=solver --state-dir=var --default-clients=demo
```

This will write a `submit-demo.cap` file into `capnp-secrets`.

Now, we need to connect our solver worker to the pool. In a new terminal connect the worker using the pool registration cap file.

```sh
$ dune exec -- solver-service run-cluster \
    --connect=capnp-secrets/pool-solver.cap \
    --name=solver-1 \
    --cache-dir=./cache \
    --verbosity=info
```

With this running we can use the example submission pipeline to solve the dependencies for the [obuilder](https://github.com/ocurrent/obuilder) repository on Github.

You should then be able to watch the pipeline in action at `http://localhost:8080`.

```sh
$ dune exec -- examples/submit.exe --submission-service=capnp-secrets/submit-demo.cap -v
```

You can also run the stress tests against the cluster:

```sh
$ dune exec -- ./stress/stress.exe cluster ./capnp-secrets/submit-demo.cap --count=10
Solved warm-up requests in: 10.01s
Running another 10 solves...
10/10 complete
Solved 10 requests in 5.19s (0.52s/iter)
```

### A solver worker by docker-compose

Start a solver-worker connected to a scheduler.

```
$ docker-compose -f docker-compose.yml up
```

Get the `Mountpoint` by inspecting the capnp-secrets volume.
```
$ docker volume inspect solver-service_capnp-secrets
[
    {
        "CreatedAt": "2023-06-02T17:00:23+02:00",
        "Driver": "local",
        "Labels": {
            "com.docker.compose.project": "solver-service",
            "com.docker.compose.version": "1.29.2",
            "com.docker.compose.volume": "capnp-secrets"
        },
        "Mountpoint": "/var/lib/docker/volumes/solver-service_capnp-secrets/_data",
        "Name": "solver-service_capnp-secrets",
        "Options": null,
        "Scope": "local"
    }
]
```

Be root to get the `submit-docker.cap` file from the `Mountpoint` path:

```
$ mkdir capnp-secrets
$ sudo cat /var/lib/docker/volumes/solver-service_capnp-secrets/_data/submit-docker.cap > ./capnp-secrets/submit-docker.cap
```

To reach the scheduler,
you can either edit the `.cap` file to replace `scheduler` with `127.0.0.1`, or
you can add `scheduler` to your `/etc/hosts` file:
```
$ cat /etc/hosts
127.0.0.1   localhost
127.0.0.1   scheduler
```

We can use the `submit-docker.cap` file to submit jobs:

```sh
$ dune exec -- ./stress/stress.exe cluster ./capnp-secrets/submit-docker.cap --count=10
Solved warm-up requests in: 23.68s
Running another 10 solves...
10/10 complete
Solved 10 requests in 17.85s (1.79s/iter) (8.96 solves/s)
```

### Testing

The `test` directly contains various tests of the solver.
Running `dune test` runs them and compares the output with the good copy in `./test/test.expected`.

For more realistic testing, the stress tester can also run in `local` mode to run the solver in-process:

```sh
$ dune exec -- ./stress/stress.exe local --cache-dir=./cache --count=10
Solved warm-up requests in: 21.46s
Running another 10 solves...
10/10 complete
Solved 10 requests in 17.87s (1.79s/iter) (8.95 solves/s)
```
