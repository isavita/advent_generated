
#!/bin/bash

totalFish=0
for fish in $(cat input.txt | tr ',' ' '); do
    fishes[$fish]=$((${fishes[$fish]} + 1))
done

for ((day=1; day<=80; day++)); do
    newFish=${fishes[0]}
    for ((i=1; i<9; i++)); do
        fishes[$i-1]=${fishes[$i]}
    done
    fishes[6]=$((${fishes[6]} + newFish))
    fishes[8]=$newFish
done

for fish in "${fishes[@]}"; do
    totalFish=$(($totalFish + $fish))
done

echo $totalFish
