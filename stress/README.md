# Stress Test

The stress test communicates using the solver-service over a TCP connection, but it is still testing the underlying solver library that the worker also uses. To run the test, first start up the solver-service at some local address:

```
$ dune exec -- solver-service --address=tcp:127.0.0.1:9090
```

This should give you a capnp address to copy. Then run the stress test passing in the address:

```
$ dune exec -- stress/stress.exe capnp://...
```

To submit the stress tests to a scheduler that handle a solver-worker, the scheduler could be a
remote. by the default the number of request is limited to 30 (--limit). Make sure, the
cache of the solver-worker is reset.

```
$ dune exec -- stress/stress_submit.exe --submission-service submission.cap --limit N
```
