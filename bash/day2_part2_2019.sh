#!/bin/bash

run_intcode() {
    local program=("$@")
    local pointer=0

    while :; do
        case ${program[pointer]} in
            1) program[${program[pointer + 3]}]=$(( ${program[${program[pointer + 1]}]} + ${program[${program[pointer + 2]}]} )) ;;
            2) program[${program[pointer + 3]}]=$(( ${program[${program[pointer + 1]}]} * ${program[${program[pointer + 2]}]} )) ;;
            99) break ;;
            *) echo "Error: Unknown opcode ${program[pointer]}"; exit 1 ;;
        esac
        ((pointer += 4))
    done
    echo ${program[0]}
}

# Read the input file and convert it to an array
IFS=',' read -r -a original_program < input.txt

# Part 1
program=("${original_program[@]}")
program[1]=12
program[2]=2
output1=$(run_intcode "${program[@]}")
echo "Part 1 Output: $output1"

# Part 2
target_output=19690720
for noun in {0..99}; do
    for verb in {0..99}; do
        program=("${original_program[@]}")
        program[1]=$noun
        program[2]=$verb
        output=$(run_intcode "${program[@]}")
        if [[ $output -eq $target_output ]]; then
            echo "Part 2 Result: $(( 100 * noun + verb ))"
            exit 0
        fi
    done
done