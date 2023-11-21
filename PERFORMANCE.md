# Performance

The script `test.sh` sets up the solver pool with a given number of instances and workers per instance.  The advertised capacity of each worker was set to the number of worker threads.  Advertising a higher capacity doesn't alter performance.  Advertising a lower capacity underutilises the worker.  For each setup, a number of jobs are submitted, resulting in a log file in the format:

```
run-$INSTANCES-$WORKERS-$WORKERS-$JOBS
```

The resulting files can be greped.  See `anaylsis.csv`:

```
INSTANCES	CAPACITY	WORKERS		JOBS		SOLVES/SEC
48		1		1		232		41.14
40		1		1		232		40.33
48		1		1		184		39.92
... etc
```
