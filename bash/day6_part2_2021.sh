#!/bin/bash

# Read from input.txt
IFS=',' read -r -a fishAges < input.txt

# Initialize lanternFishCounts array
lanternFishCounts=(0 0 0 0 0 0 0 0 0)

# Populate lanternFishCounts array with initial ages
for age in "${fishAges[@]}"; do
    ((lanternFishCounts[age]++))
done

# Iterate for 256 days
for ((day=0; day<256; day++)); do
    newLanternFish="${lanternFishCounts[0]}"
    for ((i=0; i<8; i++)); do
        lanternFishCounts[i]="${lanternFishCounts[i+1]}"
    done
    lanternFishCounts[6]=$((lanternFishCounts[6] + newLanternFish))
    lanternFishCounts[8]="$newLanternFish"
done

# Calculate and print the sum
sum=0
for count in "${lanternFishCounts[@]}"; do
    sum=$((sum + count))
done

echo "$sum"