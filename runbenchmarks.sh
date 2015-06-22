#!/bin/bash

for f in ./benchmarks/*.scm
do
    echo $f
    ../rpislip/slip -l $f | tee $f.slip
    ./targetslipy-c $f | tee $f.slipy
done
