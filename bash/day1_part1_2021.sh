
#!/bin/bash

prev=0
count=0

while IFS= read -r line; do
    current=$line
    if [ $prev -ne 0 ] && [ $current -gt $prev ]; then
        count=$((count+1))
    fi
    prev=$current
done < input.txt

echo $count
