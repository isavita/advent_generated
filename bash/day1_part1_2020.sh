
#!/bin/bash

sum=2020
numbers=($(cat input.txt))

for ((i=0; i<${#numbers[@]}-1; i++)); do
    for ((j=i+1; j<${#numbers[@]}; j++)); do
        if ((numbers[i] + numbers[j] == sum)); then
            echo $((numbers[i] * numbers[j]))
            exit 0
        fi
    done
done
