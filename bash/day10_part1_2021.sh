#!/bin/bash

awk '
BEGIN {
    pairings[")"]="("
    pairings["]"]="["
    pairings["}"]="{"
    pairings[">"]="<"

    scores[")"]=3
    scores["]"]=57
    scores["}"]=1197
    scores[">"]=25137
}

function check_line(line) {
    delete stack
    for (i = 1; i <= length(line); i++) {
        char = substr(line, i, 1)
        if (char in pairings) {
            if (length(stack) == 0 || stack[length(stack)] != pairings[char]) {
                return scores[char]
            }
            delete stack[length(stack)]
        } else {
            stack[length(stack)+1] = char
        }
    }
    return 0
}

{
    total_score += check_line($0)
}

END {
    print total_score
}' input.txt