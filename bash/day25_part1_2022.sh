#!/bin/bash

fromSnafu() {
    local s=$1
    local n=0
    for (( i=0; i<${#s}; i++ )); do
        n=$((n * 5))
        case "${s:i:1}" in
            "=")
                n=$((n - 2))
                ;;
            "-")
                n=$((n - 1))
                ;;
            *)
                n=$((n + ${s:i:1}))
                ;;
        esac
    done
    echo $n
}

toSnafu() {
    local n=$1
    local b=""
    while [[ $n -gt 0 ]]; do
        case $((n % 5)) in
            3)
                n=$((n + 5))
                b="=${b}"
                ;;
            4)
                n=$((n + 5))
                b="-${b}"
                ;;
            *)
                b="$(($n % 5))${b}"
                ;;
        esac
        n=$((n / 5))
    done
    echo $b
}

main() {
    local sum=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        sum=$((sum + $(fromSnafu "$line")))
    done < input.txt
    toSnafu $sum
}

main