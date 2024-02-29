
#!/bin/bash

maxCalories=0
currentCalories=0

while IFS= read -r line || [ -n "$line" ]; do
    if [ -z "$line" ]; then
        if [ $currentCalories -gt $maxCalories ]; then
            maxCalories=$currentCalories
        fi
        currentCalories=0
        continue
    fi

    currentCalories=$((currentCalories + line))
done < "input.txt"

if [ $currentCalories -gt $maxCalories ]; then
    maxCalories=$currentCalories
fi

echo $maxCalories
