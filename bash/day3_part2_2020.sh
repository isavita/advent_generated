
#!/bin/bash

file="input.txt"
lines=()
while IFS= read -r line; do
    lines+=("$line")
done < "$file"

slopes=(
    "1 1"
    "3 1"
    "5 1"
    "7 1"
    "1 2"
)

product=1
for slope in "${slopes[@]}"; do
    IFS=' ' read -r -a slopeArr <<< "$slope"
    treeCount=0
    pos=0
    for (( i=0; i<${#lines[@]}; i+=${slopeArr[1]} )); do
        if [ "${lines[$i]:pos:1}" == "#" ]; then
            ((treeCount++))
        fi
        pos=$(( (pos + ${slopeArr[0]}) % ${#lines[$i]} ))
    done
    product=$((product * treeCount))
done

echo "$product"
