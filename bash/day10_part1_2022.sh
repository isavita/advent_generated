#!/bin/bash

X=1
cycle=0
signal_strengths=()

while IFS= read -r line; do
    case $line in
        "noop")
            cycle=$((cycle + 1))
            ;;
        "addx "*)
            cycle=$((cycle + 1))
            if ((cycle % 40 == 20)); then
                signal_strengths+=($((cycle * X)))
            fi
            cycle=$((cycle + 1))
            if ((cycle % 40 == 20)); then
                signal_strengths+=($((cycle * X)))
            fi
            X=$((X + ${line#* }))
            ;;
    esac
done < input.txt

sum=0
for strength in "${signal_strengths[@]}"; do
    sum=$((sum + strength))
done

echo "$sum"