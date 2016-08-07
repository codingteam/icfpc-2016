#!/bin/bash
cd src
for input in ../problems/*.txt; do
    filename=`basename "$input"`
    svg=${filename%%txt}svg
    if [ ! -e ../svg/$svg ]
    then ./visualize -- "$input" -o "../svg/$svg" -w 500
    fi
done
