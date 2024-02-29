
#!/bin/bash

while IFS= read -r line; do
    lines+=("$line")
done < input.txt

for (( i=0; i<${#lines[@]}-1; i++ )); do
    for (( j=i+1; j<${#lines[@]}; j++ )); do
        diff=0
        for (( k=0; k<${#lines[i]}; k++ )); do
            if [ "${lines[i]:$k:1}" != "${lines[j]:$k:1}" ]; then
                diff=$((diff+1))
                if [ $diff -gt 1 ]; then
                    break
                fi
            fi
        done
        if [ $diff -eq 1 ]; then
            common=""
            for (( k=0; k<${#lines[i]}; k++ )); do
                if [ "${lines[i]:$k:1}" == "${lines[j]:$k:1}" ]; then
                    common+="${lines[i]:$k:1}"
                fi
            done
            echo "$common"
            exit 0
        fi
    done
done
