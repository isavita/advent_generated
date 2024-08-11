#!/bin/bash

declare -a registers=(0 0 0 0 0 0)
instruction_pointer=0
declare -a instructions=()

while IFS= read -r line; do
    if [[ $line == \#ip\ * ]]; then
        instruction_pointer=${line##* }
    else
        instructions+=("$line")
    fi
done < input.txt

execute() {
    local name="${1}"
    local a="${2}"
    local b="${3}"
    local c="${4}"
    
    case $name in
        addr) registers[$c]=$((registers[$a] + registers[$b])) ;;
        addi) registers[$c]=$((registers[$a] + b)) ;;
        mulr) registers[$c]=$((registers[$a] * registers[$b])) ;;
        muli) registers[$c]=$((registers[$a] * b)) ;;
        banr) registers[$c]=$((registers[$a] & registers[$b])) ;;
        bani) registers[$c]=$((registers[$a] & b)) ;;
        borr) registers[$c]=$((registers[$a] | registers[$b])) ;;
        bori) registers[$c]=$((registers[$a] | b)) ;;
        setr) registers[$c]=${registers[$a]} ;;
        seti) registers[$c]=$a ;;
        gtir) registers[$c]=$((a > registers[$b] ? 1 : 0)) ;;
        gtri) registers[$c]=$((registers[$a] > b ? 1 : 0)) ;;
        gtrr) registers[$c]=$((registers[$a] > registers[$b] ? 1 : 0)) ;;
        eqir) registers[$c]=$((a == registers[$b] ? 1 : 0)) ;;
        eqri) registers[$c]=$((registers[$a] == b ? 1 : 0)) ;;
        eqrr) registers[$c]=$((registers[$a] == registers[$b] ? 1 : 0)) ;;
    esac
}

while :; do
    if (( registers[instruction_pointer] >= ${#instructions[@]} )); then
        break
    fi
    inst=${instructions[registers[instruction_pointer]]}
    read -r name a b c <<< "$inst"
    execute "$name" "$a" "$b" "$c"
    ((registers[instruction_pointer]++))
    if (( registers[instruction_pointer] == 28 )); then
        break
    fi
done

echo "${registers[5]}"