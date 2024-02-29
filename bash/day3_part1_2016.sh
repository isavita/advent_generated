
#!/bin/bash

validTriangles=0

while read line; do
    sides=($line)
    if [ ${#sides[@]} -ne 3 ]; then
        echo "Invalid input format"
        continue
    fi

    a=${sides[0]}
    b=${sides[1]}
    c=${sides[2]}

    if [ $((a + b)) -gt $c ] && [ $((a + c)) -gt $b ] && [ $((b + c)) -gt $a ]; then
        ((validTriangles++))
    fi

done < input.txt

echo $validTriangles
