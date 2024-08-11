#!/bin/bash

input=$(<input.txt)

find_marker() {
    local length=$1
    local str=$2
    for ((i=0; i<=${#str}-length; i++)); do
        substring=${str:i:length}
        if [[ $(echo "$substring" | fold -w1 | sort -u | wc -l) -eq $length ]]; then
            echo $((i + length))
            return
        fi
    done
}

start_packet=$(find_marker 4 "$input")
start_message=$(find_marker 14 "$input")

echo "Start of packet marker after character: $start_packet"
echo "Start of message marker after character: $start_message"