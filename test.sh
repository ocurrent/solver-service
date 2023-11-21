#!/bin/sh 

eval $(opam env)

#host=c2-4.equinix.ci.dev
host=arm64-jade-1.equinix.ci.dev

for JOBS in 1 $(seq 8 16 480) ; do
min=$(( JOBS < 160 ? JOBS : 160 ))
for INSTANCES in 1 $(seq 8 8 $min) ; do
for WORKERS in 1 $(seq 2 2 10) ; do

output="run-$INSTANCES-$WORKERS-$WORKERS-$JOBS"

echo Run $output

if ! [ -f $output ] ; then

rm *.service

for NAM in $(seq 1 $INSTANCES) ; do
sed "s/NAM/$NAM/g;s/CAP/$WORKERS/g;s/WOR/$WORKERS/g;" solver-service > solver-service-$NAM.service
done

ssh $host 'cd /etc/systemd/system ; for s in solver-service-* ; do systemctl stop $s ; done'
ssh $host rm /etc/systemd/system/solver-service-*
scp *.service $host:/etc/systemd/system
ssh $host systemctl daemon-reload
ssh $host 'cd /etc/systemd/system ; for s in solver-service-* ; do systemctl start $s ; done'

dune exec -- stress/stress.exe cluster ~/mtelvers.cap --count=$JOBS --solver-pool=solver-test > $output

fi

done
done
done

grep "Solved [0-9]* " run-* | awk ' { split($1, x, "[-:]") ; split($7, y, "(") ; print x[2] "\t" x[3] "\t" x[4] "\t" x[5] "\t" y[2] }' | sort -rg -k 5
