#!/bin/bash

# Function to calculate the sum of yes counts for part one
count_yes_anyone() {
    awk 'BEGIN { sum = 0 } 
         { if (NF == 0) { print count; sum += count; count = 0; next } 
           for (i = 1; i <= length($0); i++) { answers[substr($0, i, 1)] = 1 } 
           count = length(answers) } 
         END { print count; sum += count; print sum }' "$1" | tail -n 1
}

# Function to calculate the sum of yes counts for part two
count_yes_everyone() {
    awk 'BEGIN { sum = 0 } 
         { if (NF == 0) { count = 0; for (q in answers) if (answers[q] == people) count++; sum += count; delete answers; people = 0; next } 
           for (i = 1; i <= length($0); i++) { answers[substr($0, i, 1)]++ } 
           people++ } 
         END { count = 0; for (q in answers) if (answers[q] == people) count++; sum += count; print sum }' "$1" | tail -n 1
}

input_file="input.txt"
part_one_sum=$(count_yes_anyone "$input_file")
part_two_sum=$(count_yes_everyone "$input_file")

echo "Part One: $part_one_sum"
echo "Part Two: $part_two_sum"