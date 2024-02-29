
#!/bin/bash

sum=0
min=0
max=0

while IFS= read -r line
do
    numbers+=("$line")
done < input.txt

for (( i=0; i<${#numbers[@]}; i++ )); do
    sum=${numbers[i]}
    min=${numbers[i]}
    max=${numbers[i]}
    for (( j=i+1; j<${#numbers[@]}; j++ )); do
        sum=$((sum + numbers[j]))
        if (( numbers[j] < min )); then
            min=${numbers[j]}
        fi
        if (( numbers[j] > max )); then
            max=${numbers[j]}
        fi
        if (( sum == 14360655 )); then
            echo $((min + max))
            exit 0
        elif (( sum > 14360655 )); then
            break
        fi
    done
done
