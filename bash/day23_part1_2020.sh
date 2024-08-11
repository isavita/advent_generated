#!/bin/bash

input=$(<input.txt)
length=${#input}
declare -A cups

for ((i=0; i<length; i++)); do
    cup=${input:i:1}
    if (( i < length - 1 )); then
        nextCup=${input:i+1:1}
        cups[$cup]=$nextCup
    fi
done
cups[${input: -1}]=${input:0:1}

currentCup=${input:0:1}

for ((i=0; i<100; i++)); do
    pickup1=${cups[$currentCup]}
    pickup2=${cups[$pickup1]}
    pickup3=${cups[$pickup2]}
    cups[$currentCup]=${cups[$pickup3]}

    destinationCup=$((currentCup - 1))
    if ((destinationCup < 1)); then destinationCup=$length; fi
    while [[ $destinationCup == $pickup1 || $destinationCup == $pickup2 || $destinationCup == $pickup3 ]]; do
        destinationCup=$((destinationCup - 1))
        if ((destinationCup < 1)); then destinationCup=$length; fi
    done

    cups[$pickup3]=${cups[$destinationCup]}
    cups[$destinationCup]=$pickup1
    currentCup=${cups[$currentCup]}
done

cup=${cups[1]}
while [[ $cup != 1 ]]; do
    printf "%s" "$cup"
    cup=${cups[$cup]}
done
echo