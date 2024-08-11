#!/bin/bash

input=($(< input.txt))
earliest_timestamp=${input[0]}
bus_ids=(${input[1]//,/ })

# Part One
min_wait=999999999999999
best_bus=0

for bus in "${bus_ids[@]}"; do
    if [[ $bus != "x" ]]; then
        wait_time=$((bus - (earliest_timestamp % bus)))
        if (( wait_time < min_wait )); then
            min_wait=$wait_time
            best_bus=$bus
        fi
    fi
done

echo "Part One: $((best_bus * min_wait))"

# Part Two
timestamp=0
increment=1

for i in "${!bus_ids[@]}"; do
    bus=${bus_ids[i]}
    if [[ $bus != "x" ]]; then
        while (( (timestamp + i) % bus != 0 )); do
            timestamp=$((timestamp + increment))
        done
        increment=$((increment * bus))
    fi
done

echo "Part Two: $timestamp"