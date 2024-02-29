#!/bin/bash

filename="input.txt"

readIPRanges() {
    ranges=()
    while IFS='-' read -r start end; do
        ranges+=("$start-$end")
    done < "$filename"
}

findUnblockedIP() {
    currentIP=0
    for range in "${ranges[@]}"; do
        IFS='-' read -r start end <<< "$range"
        if (( start > currentIP )); then
            echo "$currentIP"
            exit 0
        fi
        if (( end >= currentIP )); then
            currentIP=$((end + 1))
        fi
    done
    echo "$currentIP"
}

readIPRanges
IFS=$'\n' sorted=($(sort -t- -nk1 <<<"${ranges[*]}"))
ranges=("${sorted[@]}")

findUnblockedIP