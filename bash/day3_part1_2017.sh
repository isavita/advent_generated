#!/bin/bash

# Step 1: Read input
target=$(<input.txt)

# Step 2: Find the square
sideLength=$(( $(echo "scale=0; sqrt($target)" | bc -l | awk '{print int($1+0.5)}') ))
(( $sideLength % 2 == 0 )) && sideLength=$(( $sideLength + 1 ))

# Step 3: Find distance to the nearest middle point
maxValue=$(( $sideLength * $sideLength ))
stepsFromEdge=$(( ($sideLength - 1) / 2 ))
distanceToMiddle=$(( $maxValue - $stepsFromEdge ))

for (( i=0; i<4; i++ ))
do
    middlePoint=$(( $maxValue - $stepsFromEdge - ($sideLength-1)*$i ))
    distance=$(( $(echo "scale=0; $target - $middlePoint" | bc -l | awk '{print int($1<0?-$1:$1)}') ))
    (( $distance < $distanceToMiddle || $i == 0 )) && distanceToMiddle=$distance
done

# Step 4: Calculate Manhattan Distance
manhattanDistance=$(( $stepsFromEdge + $distanceToMiddle ))

echo $manhattanDistance