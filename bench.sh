#!/bin/sh
set -eu
count=3
dune build -- ./stress/stress.exe
echo Workers, Solves/s
for n in 1 2 3 4 5 6 7 8; do
	rate=$(./_build/default/stress/stress.exe local --cache-dir=./cache --count=$count --internal-workers=$n -q)
	echo $n, $rate
done
