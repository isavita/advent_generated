#!/bin/bash

input=$(cat input.txt)

for (( i=3; i<${#input}; i++ )); do
    if [ "${input:$i:1}" != "${input:$((i-1)):1}" ] && [ "${input:$i:1}" != "${input:$((i-2)):1}" ] && [ "${input:$i:1}" != "${input:$((i-3)):1}" ] && [ "${input:$((i-1)):1}" != "${input:$((i-2)):1}" ] && [ "${input:$((i-1)):1}" != "${input:$((i-3)):1}" ] && [ "${input:$((i-2)):1}" != "${input:$((i-3)):1}" ]; then
        echo "$((i+1))"
        break
    fi
done