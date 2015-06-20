#!/bin/bash

for f in ./benchmarks/*.scm
do
    echo $f
    ./targetslipy-c $f
    ../rpislip/slip -l $f
done
