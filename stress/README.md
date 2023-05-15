# Stress Test

The stress test communicates using the solver-service over a TCP connection, but it is still testing the underlying solver library that the worker also uses. To run the test, first start up the solver-service at some local address:

```
$ dune exec -- solver-service --address=tcp:127.0.0.1:9090
```

This should give you a capnp address to copy. Then run the stress test passing in the address:

```
$ dune exec -- stress/stress.exe capnp://...
```

Use this to submit stress tests to a possibly remote scheduler handling a solver-worker.
By default the number of request is limited to 30 (--limit). Make sure, the
cache of the solver-worker is reset. It means using `rm -r var/solver` by finding where `var` directory is
stored if solver-worker use it(`--state-dir=var`) and restart solver-worker. It is also possible to
variate the opam-repository commit without removing solver-worker cache each time.

```
$ dune exec -- stress/stress_submit.exe --submission-service submission.cap --limit N [--opam-repository-commit COMMIT]
```

To stress test the solver-worker against different opam-repository commits changing in time: almost same as previous
the difference is the timing (by default `--time=80` seconds) of a particular opam-repository (how long it remains before change)
and the list of opam-repository commits by default there's a list of 3 different commits.

```
$ dune exec -- stress/stress_auto_cancel_submit.exe --submission-service submission.cap --limit N --timing N [--opam-repository-commits=COMMITS]
```
