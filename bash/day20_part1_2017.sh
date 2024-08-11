#!/bin/bash

declare -A particles

while IFS=, read -r p v a; do
    p=(${p//< /})
    p=(${p//>/})
    v=(${v//< /})
    v=(${v//>/})
    a=(${a//< /})
    a=(${a//>/})
    
    particles+=([0]="${p[0]},${p[1]},${p[2]}")
    particles+=([1]="${v[0]},${v[1]},${v[2]}")
    particles+=([2]="${a[0]},${a[1]},${a[2]}")
done < input.txt

closest_particle=-1
min_distance=999999999

for i in "${!particles[@]}"; do
    IFS=',' read -r x y z <<< "${particles[$i]}"
    x=$(($x + $v[0] + $a[0]))
    y=$(($y + $v[1] + $a[1]))
    z=$(($z + $v[2] + $a[2]))
    
    distance=$((${x#-} + ${y#-} + ${z#-}))
    
    if (( distance < min_distance )); then
        min_distance=$distance
        closest_particle=$i
    fi
done

echo "The closest particle is: $closest_particle"