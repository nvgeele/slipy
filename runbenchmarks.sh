#!/bin/bash

for f in ./benchmarks/*.scm
do
    echo $f
    ../rpislip/slip -l $f | tail -n +3 | head -n 20 > _tmpA
    ./targetslipy-c $f | tail -n +2 | head -n 20 > _tmpB
done
