
#!/bin/bash

caloriesList=()
currentCalories=0

while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ -z "$line" ]]; then
        caloriesList+=("$currentCalories")
        currentCalories=0
        continue
    fi

    calories=$((line))
    currentCalories=$((currentCalories + calories))
done < "input.txt"

caloriesList+=("$currentCalories")
IFS=$'\n' sorted=($(sort -nr <<<"${caloriesList[*]}"))

topThreeSum=0
for ((i=0; i < 3 && i < ${#sorted[@]}; i++)); do
    topThreeSum=$((topThreeSum + sorted[i]))
done

echo $topThreeSum
exit 0
