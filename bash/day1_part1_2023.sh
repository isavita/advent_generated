
#!/bin/bash

sum=0

while IFS= read -r line || [ -n "$line" ]; do
    if [ -z "$line" ]; then
        continue
    fi

    firstDigit=-1
    lastDigit=-1

    for (( i=0; i<${#line}; i++ )); do
        char="${line:$i:1}"
        if [[ $char =~ [0-9] ]]; then
            if [ $firstDigit -eq -1 ]; then
                firstDigit=$char
            fi
            lastDigit=$char
        fi
    done

    if [ $firstDigit -ne -1 ] && [ $lastDigit -ne -1 ]; then
        value=$(( $firstDigit * 10 + $lastDigit ))
        sum=$(( $sum + $value ))
    fi
done < input.txt

echo $sum
exit 0
