
#!/bin/bash

masses=()
total=0

while IFS= read -r line || [ -n "$line" ]; do
    masses+=("$line")
done < input.txt

for mass in "${masses[@]}"; do
    total=$(echo "$total + $(echo "($mass / 3) - 2" | bc)" | bc)
done

echo "$total"
