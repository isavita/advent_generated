
#!/bin/bash

validCount=0

while IFS= read -r line
do
    policy=$(echo $line | cut -d ':' -f 1)
    password=$(echo $line | cut -d ':' -f 2 | cut -c 2-)
    
    min=$(echo $policy | cut -d '-' -f 1)
    max=$(echo $policy | cut -d '-' -f 2 | cut -d ' ' -f 1)
    char=$(echo $policy | cut -d ' ' -f 2)
    
    count=$(echo $password | grep -o $char | wc -l)
    
    if [ $count -ge $min ] && [ $count -le $max ]
    then
        validCount=$((validCount+1))
    fi
done < input.txt

echo $validCount
