#!/bin/bash

reindeers=()
while IFS= read -r line; do
    parts=($line)
    speed=${parts[3]}
    flyTime=${parts[6]}
    restTime=${parts[13]}
    reindeers+=("${speed} ${flyTime} ${restTime} 0 true 0")
done < "input.txt"

for ((i=0; i<2503; i++)); do
    for ((j=0; j<${#reindeers[@]}; j++)); do
        reindeer=(${reindeers[j]})
        speed=${reindeer[0]}
        flyTime=${reindeer[1]}
        restTime=${reindeer[2]}
        distance=${reindeer[3]}
        flying=${reindeer[4]}
        timeInMode=${reindeer[5]}
        
        if [ "$flying" == "true" ]; then
            ((distance += speed))
            ((timeInMode++))
            if [ "$timeInMode" == "$flyTime" ]; then
                flying="false"
                timeInMode=0
            fi
        else
            ((timeInMode++))
            if [ "$timeInMode" == "$restTime" ]; then
                flying="true"
                timeInMode=0
            fi
        fi
        
        reindeers[j]="${speed} ${flyTime} ${restTime} ${distance} ${flying} ${timeInMode}"
    done
done

maxDistance=0
for reindeer in "${reindeers[@]}"; do
    distance=$(echo "$reindeer" | cut -d' ' -f4)
    if [ "$distance" -gt "$maxDistance" ]; then
        maxDistance=$distance
    fi
done

echo "$maxDistance"