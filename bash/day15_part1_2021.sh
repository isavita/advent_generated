#!/bin/bash

# Read the input file
while IFS= read -r line; do
    echo "$line"
done < input.txt