
#!/bin/bash

input=$(<input.txt)
nice=0

passesRule1() {
    line=$1
    for ((i=0; i<${#line}-2; i++)); do
        toMatch=${line:$i:2}
        for ((j=i+2; j<${#line}-1; j++)); do
            if [[ ${line:$j:2} == $toMatch ]]; then
                return 0
            fi
        done
    done
    return 1
}

while IFS= read -r line; do
    passesRule1 "$line"
    rule1=$?

    rule2=0
    for ((i=0; i<${#line}-2; i++)); do
        if [[ ${line:$i:1} == ${line:$i+2:1} ]]; then
            rule2=1
            break
        fi
    done

    if [[ $rule1 -eq 0 && $rule2 -eq 1 ]]; then
        ((nice++))
    fi
done <<< "$input"

echo $nice
