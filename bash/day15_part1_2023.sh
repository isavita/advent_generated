#!/bin/bash

hashString() {
    local str="$1"
    local res=0
    for (( i=0; i<${#str}; i++ )); do
        local char="${str:i:1}"
        res=$(( (res + $(printf "%d" "'$char")) * 17 % 256 ))
    done
    echo $res
}

parseStep() {
    local stepStr="$1"
    local label="${stepStr%%[=-]*}"
    local operation="${stepStr:${#label}:1}"
    local number=0

    if [[ "$operation" == "=" ]]; then
        number="${stepStr:${#label}+1}"
    fi

    echo "$label $operation $number"
}

solve() {
    local line="$1"
    IFS=',' read -ra steps <<< "$line"
    local res=0
    for step in "${steps[@]}"; do
        res=$(( res + $(hashString "$step") ))
    done
    echo $res
}

input=$(<input.txt)
result=$(solve "$input")
echo "$result"