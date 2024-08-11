#!/bin/bash

# Define the criteria based on the MFCSAM output
declare -A criteria=(
    [children]=3
    [cats]=7
    [samoyeds]=2
    [pomeranians]=3
    [akitas]=0
    [vizslas]=0
    [goldfish]=5
    [trees]=3
    [cars]=2
    [perfumes]=1
)

# Read input file and process each line
while IFS=: read -r aunt line; do
    # Extract the aunt number
    aunt_number=${aunt##* }
    # Initialize a flag to check if the aunt matches
    match=true
    
    # Check each criterion
    for key in "${!criteria[@]}"; do
        if [[ $line =~ $key:\ ([0-9]+) ]]; then
            value=${BASH_REMATCH[1]}
            if [[ ${criteria[$key]} -ne $value ]]; then
                match=false
                break
            fi
        fi
    done
    
    # Print the aunt number if it matches
    if $match; then
        echo "$aunt_number"
    fi
done < input.txt