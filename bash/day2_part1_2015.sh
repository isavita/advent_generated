
#!/bin/bash

total=0
while IFS='x' read -r l w h || [ -n "$l" ]; do
    side1=$((l * w))
    side2=$((w * h))
    side3=$((h * l))

    smallest=$side1
    if [ $side2 -lt $smallest ]; then
        smallest=$side2
    fi
    if [ $side3 -lt $smallest ]; then
        smallest=$side3
    fi

    total=$((total + 2*side1 + 2*side2 + 2*side3 + smallest))
done < input.txt

echo $total
exit 0
