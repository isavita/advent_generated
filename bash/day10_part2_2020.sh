#!/bin/bash

adapters=(0)

while IFS= read -r line || [ -n "$line" ]; do
    adapters+=( $line )
done < input.txt

IFS=$'\n' sorted=($(sort -n <<<"${adapters[*]}"))
adapters=("${sorted[@]}" $((sorted[${#sorted[@]}-1]+3)))

countArrangements() {
    declare -A ways
    ways[0]=1

    for (( i=1; i<${#adapters[@]}; i++ )); do
        currentJoltage=${adapters[i]}
        ways[$currentJoltage]=0
        for diff in 1 2 3; do
            if [ -n "${ways[$((currentJoltage-diff))]}" ]; then
                ways[$currentJoltage]=$((ways[$currentJoltage]+ways[$((currentJoltage-diff))]))
            fi
        done
    done

    echo ${ways[${adapters[${#adapters[@]}-1]}]}
}

countArrangements