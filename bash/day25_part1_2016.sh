
#!/bin/bash

n=$(grep -oE '[0-9]+' input.txt | head -n 2 | awk 'NR==1{p=$1} NR==2{p*=$1} END {print p}')

v=2
while [ $v -le $n ]; do
    v=$((v * 4 + 2))
done

echo $((v - n))
