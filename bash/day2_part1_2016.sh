#!/bin/bash

file="input.txt"

if [ ! -f "$file" ]; then
    echo "File not found!"
    exit 1
fi

instructions=($(<"$file"))

keypad=( [0]=1 [1]=2 [2]=3 [3]=4 [4]=5 [5]=6 [6]=7 [7]=8 [8]=9 )
x=1
y=1
code=""

for instruction in "${instructions[@]}"; do
    for (( i=0; i<${#instruction}; i++ )); do
        move=${instruction:$i:1}
        case $move in
            U)
                if [ $x -gt 0 ]; then
                    ((x--))
                fi
                ;;
            D)
                if [ $x -lt 2 ]; then
                    ((x++))
                fi
                ;;
            L)
                if [ $y -gt 0 ]; then
                    ((y--))
                fi
                ;;
            R)
                if [ $y -lt 2 ]; then
                    ((y++))
                fi
                ;;
        esac
    done
    code+="${keypad[(x*3)+y]}"
done

echo "$code"
exit 0