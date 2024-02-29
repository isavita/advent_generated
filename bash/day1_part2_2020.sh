
#!/bin/bash

sum=2020
expenses=($(cat input.txt))

for ((i=0; i<${#expenses[@]}; i++)); do
    for ((j=i+1; j<${#expenses[@]}; j++)); do
        for ((k=j+1; k<${#expenses[@]}; k++)); do
            if [ $((expenses[i] + expenses[j] + expenses[k])) -eq $sum ]; then
                echo $((expenses[i] * expenses[j] * expenses[k]))
                exit 0
            fi
        done
    done
done
