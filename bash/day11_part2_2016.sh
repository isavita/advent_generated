
#!/bin/bash

f1_content=$(sed -n '1p' input.txt)
f2_content=$(sed -n '2p' input.txt)
f3_content=$(sed -n '3p' input.txt)

n1=$(echo "$f1_content" | grep -oE "generator|microchip" | wc -l)
n2=$(echo "$f2_content" | grep -oE "generator|microchip" | wc -l)
n3=$(echo "$f3_content" | grep -oE "generator|microchip" | wc -l)

n1=$((n1 + 4))

steps=0
current_sum=$n1
steps=$((steps + 2 * current_sum - 3))

current_sum=$((current_sum + n2))
steps=$((steps + 2 * current_sum - 3))

current_sum=$((current_sum + n3))
steps=$((steps + 2 * current_sum - 3))

echo $steps
