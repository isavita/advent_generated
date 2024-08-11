#!/bin/bash

awk '{
    split($0, words, " ")
    for (i in words) {
        if (seen[words[i]]++) {
            valid = 0
            break
        }
        valid = 1
    }
    if (valid) count++
    delete seen
} END { print count }' input.txt