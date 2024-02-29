
#!/bin/bash

while IFS= read -r line
do
    cardPublicKey=$line
    read -r line
    doorPublicKey=$line
done < "input.txt"

findLoopSize() {
    value=1
    loopSize=0
    while [ $value -ne $1 ]
    do
        value=$((value * 7 % 20201227))
        loopSize=$((loopSize + 1))
    done
    echo $loopSize
}

transform() {
    value=1
    for ((i=0; i<$2; i++))
    do
        value=$((value * $1 % 20201227))
    done
    echo $value
}

cardLoopSize=$(findLoopSize $cardPublicKey)
encryptionKey=$(transform $doorPublicKey $cardLoopSize)

echo $encryptionKey
