#!/bin/bash

# Read the input from input.txt, process it, and calculate the sum of unique 'yes' answers for each group
awk 'BEGIN { FS = "\n"; RS = ""; total = 0 }
{
    questions = ""; 
    for (i = 1; i <= NF; i++) {
        questions = questions $i;
    }
    split(questions, arr, "");
    delete seen;
    for (j in arr) {
        seen[arr[j]] = 1;
    }
    total += length(seen);
}
END { print total }' input.txt