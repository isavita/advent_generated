#!/bin/bash

file="input.txt"
maxSeatID=0

while IFS= read -r pass; do
    row=$(echo "$pass" | cut -c1-7 | tr 'FB' '01')
    column=$(echo "$pass" | cut -c8-10 | tr 'LR' '01')
    seatID=$((2#$row * 8 + 2#$column))
    if (( seatID > maxSeatID )); then
        maxSeatID=$seatID
    fi
done < "$file"

echo $maxSeatID