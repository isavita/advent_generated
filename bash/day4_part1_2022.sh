
#!/bin/bash

count=0

while IFS=, read range1 range2; do
    IFS=- read start1 end1 <<< "$range1"
    IFS=- read start2 end2 <<< "$range2"

    if (( (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1) )); then
        ((count++))
    fi
done < input.txt

echo $count
exit 0
