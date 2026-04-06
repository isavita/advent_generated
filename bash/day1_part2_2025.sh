
#!/bin/bash

p=50
h=0

while read -r l || [[ -n $l ]]; do
    l=${l//[[:space:]]/}
    [[ -z $l ]] && continue
    
    d=${l:0:1}
    a=${l:1}

    if [[ $d == "R" ]]; then
        ((h += (p + a) / 100))
        ((p = (p + a) % 100))
    elif [[ $d == "L" ]]; then
        n1=$((p - 1))
        q1=$(( n1 < 0 ? (n1 - 99) / 100 : n1 / 100 ))
        n2=$((p - a - 1))
        q2=$(( n2 < 0 ? (n2 - 99) / 100 : n2 / 100 ))
        
        ((h += q1 - q2))
        ((p = (p - a) % 100))
        ((p < 0 && (p += 100)))
    fi
done < input.txt

echo $h
