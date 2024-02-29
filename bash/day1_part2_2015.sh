
#!/bin/bash

input=$(<input.txt)
floor=0
position=0

for (( i=0; i<${#input}; i++ )); do
    c=${input:$i:1}
    if [ "$c" == "(" ]; then
        ((floor++))
    elif [ "$c" == ")" ]; then
        ((floor--))
    fi
    if [ $floor -eq -1 ]; then
        position=$((i+1))
        break
    fi
done

echo $position
exit 0
