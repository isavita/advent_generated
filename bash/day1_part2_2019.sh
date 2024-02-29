#!/bin/bash

calc_fuel_mass() {
    local mass=$1
    local fuel=$((mass / 3 - 2))

    if [ $fuel -le 0 ]; then
        echo 0
    else
        echo $((fuel + $(calc_fuel_mass $fuel)))
    fi
}

get_total() {
    local total=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        local mass=$(echo $line | xargs)
        if ! [[ $mass =~ ^[0-9]+$ ]]; then
            echo "Error parsing line" >&2
            continue
        fi
        total=$((total + $(calc_fuel_mass $mass)))
    done < "input.txt"
    echo $total
}

get_total