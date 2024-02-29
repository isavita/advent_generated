
#!/bin/bash

input=$(<input.txt)

lengths=()
for ((i=0; i<${#input}; i++)); do
    lengths+=($(printf "%d" "'${input:$i:1}"))
done
lengths+=(17 31 73 47 23)

list=($(seq 0 255))
currentPosition=0
skipSize=0

for round in {1..64}; do
    for length in "${lengths[@]}"; do
        for ((i=0; i<length/2; i++)); do
            start=$(( (currentPosition + i) % 256 ))
            end=$(( (currentPosition + length - 1 - i) % 256 ))
            temp=${list[$start]}
            list[$start]=${list[$end]}
            list[$end]=$temp
        done
        currentPosition=$(( (currentPosition + length + skipSize) % 256 ))
        ((skipSize++))
    done
done

denseHash=""
for ((i=0; i<256; i+=16)); do
    xor=0
    for ((j=0; j<16; j++)); do
        ((xor ^= list[i+j]))
    done
    denseHash+=$(printf "%02x" $xor)
done

echo $denseHash
