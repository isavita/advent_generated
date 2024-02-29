
#!/bin/bash

input=$(<input.txt)
IFS=',' read -ra startingNumbers <<< "$input"

declare -A lastSpoken
lastNumber=0
nextNumber=0

for (( turn=1; turn<=2020; turn++ )); do
    if (( turn-1 < ${#startingNumbers[@]} )); then
        lastNumber=${startingNumbers[turn-1]}
        lastSpoken[$lastNumber]=$turn
        continue
    fi
    if [[ -n ${lastSpoken[$lastNumber]} && ${lastSpoken[$lastNumber]} -ne $((turn-1)) ]]; then
        nextNumber=$((turn-1-${lastSpoken[$lastNumber]}))
    else
        nextNumber=0
    fi
    lastSpoken[$lastNumber]=$((turn-1))
    lastNumber=$nextNumber
done

echo $lastNumber
