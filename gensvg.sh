#!/bin/bash
cd src
for input in ../problems/*.txt; do
    filename=`basename "$input"`
    ./visualize -- "$input" -o "../svg/${filename%%txt}svg" -w 500
done
