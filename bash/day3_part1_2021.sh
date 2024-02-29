#!/bin/bash

# Check if the file exists
if [ ! -f "input.txt" ]; then
    echo "File 'input.txt' not found!"
    exit 1
fi

# Initialize arrays
declare -a counts
for ((i=0; i<12; i++)); do
    counts[$i]=0
done

# Read the file line by line
while IFS= read -r line; do
    for ((i=0; i<${#line}; i++)); do
        if [[ ${line:$i:1} -eq 1 ]]; then
            counts[$i]=$((counts[$i]+1))
        fi
    done
done < "input.txt"

# Calculate gammaRate and epsilonRate
gammaRate=0
epsilonRate=0
totalLines=$(wc -l < "input.txt")
for ((i=0; i<12; i++)); do
    if (( counts[$i] > totalLines - counts[$i] )); then
        gammaRate=$((gammaRate | (1 << (11 - i))))
        epsilonRate=$((epsilonRate & ~(1 << (11 - i)) ))
    else
        gammaRate=$((gammaRate & ~(1 << (11 - i)) ))
        epsilonRate=$((epsilonRate | (1 << (11 - i))))
    fi
done

# Print the result
echo $((gammaRate * epsilonRate))

exit 0