#!/bin/bash

input_file="input.txt"
instructions=()

while IFS= read -r line; do
    instructions+=("$line")
done < "$input_file"

for i in "${!instructions[@]}"; do
    op=${instructions[i]%% *}
    arg=${instructions[i]##* }

    if [[ $op == "acc" ]]; then
        continue
    fi

    modified_instructions=("${instructions[@]}")
    if [[ $op == "jmp" ]]; then
        modified_instructions[i]="nop $arg"
    else
        modified_instructions[i]="jmp $arg"
    fi

    accumulator=0
    visited=()
    current_instruction=0
    terminated=true

    while (( current_instruction < ${#modified_instructions[@]} )); do
        if [[ " ${visited[*]} " == *" $current_instruction "* ]]; then
            terminated=false
            break
        fi
        visited+=("$current_instruction")
        op=${modified_instructions[current_instruction]%% *}
        arg=${modified_instructions[current_instruction]##* }

        case $op in
            acc)
                (( accumulator += arg ))
                (( current_instruction++ ))
                ;;
            jmp)
                (( current_instruction += arg ))
                ;;
            nop)
                (( current_instruction++ ))
                ;;
        esac
    done

    if $terminated; then
        echo "$accumulator"
        break
    fi
done