
#!/bin/bash

declare -i nice=0
declare -i vowels=0

while IFS= read -r line
do
    vowels=0
    hasDouble=0
    disallowPattern='ab|cd|pq|xy'

    # Count vowels
    vowels=$(echo "$line" | tr -cd 'aeiou' | wc -c)

    # Check for double letters
    for ((i=0; i<${#line}-1; i++)); do
        if [ "${line:$i:1}" == "${line:$i+1:1}" ]; then
            hasDouble=1
            break
        fi
    done

    # Check for disallowed patterns
    if ! echo "$line" | grep -qE "$disallowPattern"; then
        if [ $vowels -ge 3 ] && [ $hasDouble -eq 1 ]; then
            nice+=1
        fi
    fi
done < "input.txt"

echo $nice
