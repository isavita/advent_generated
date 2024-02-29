
#!/bin/bash

data=$(<input.txt)
input=$(echo $data | tr -d '\n')
halfway=$(( ${#input} / 2 ))
sum=0

for (( i=0; i<${#input}; i++ )); do
    next=$(( (i + halfway) % ${#input} ))
    if [ "${input:i:1}" == "${input:next:1}" ]; then
        sum=$(( sum + ${input:i:1} ))
    fi
done

echo $sum
exit 0
