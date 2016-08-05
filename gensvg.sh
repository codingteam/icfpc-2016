#!/bin/bash
cd src
stack build
for input in ../problems/*.txt; do
    filename=`basename "$input"`
    stack exec visualize -- "$input" -o "../svg/${filename%%txt}svg" -w 500
done
