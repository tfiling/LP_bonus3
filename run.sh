#!/bin/bash

for i in $(seq 2 8); 
do
    start=`date +%s`
    printf 'fraction #%d\n' $i
    prolog -G10g -L10g -T10g -s bonus.pl -t 'bonus:solve(fraction('$i'), Sol),writeln(Sol).'
    printf '\n exit code: %s\n' $?
    end=`date +%s`
    runtime=$((end-start))
    printf 'time = %d seconds' $runtime
    printf '\n\n-------------------------------------------\n\n'
done


