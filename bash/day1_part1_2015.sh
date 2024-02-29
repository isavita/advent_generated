
#!/bin/bash

input=$(<input.txt)
floor=0

for (( i=0; i<${#input}; i++ )); do
    char=${input:i:1}
    if [ $char == "(" ]; then
        ((floor++))
    elif [ $char == ")" ]; then
        ((floor--))
    fi
done

echo $floor
exit 0
