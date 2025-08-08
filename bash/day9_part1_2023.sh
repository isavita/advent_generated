
#!/usr/bin/env bash

total=0

while IFS= read -r line || [[ -n $line ]]; do
    # Skip empty lines
    [[ -z $line ]] && continue

    # Read numbers into an array
    read -a arr <<< "$line"
    sum=0

    while true; do
        # Last element of the current series
        last=${arr[${#arr[@]}-1]}
        sum=$((sum + last))

        # Check if all elements are zero
        all_zero=1
        for v in "${arr[@]}"; do
            if (( v != 0 )); then
                all_zero=0
                break
            fi
        done

        # If all zeros, stop
        if (( all_zero )); then
            break
        fi

        # Compute next differences
        new=()
        for ((i=0; i<${#arr[@]}-1; i++)); do
            diff=$((arr[i+1] - arr[i]))
            new+=("$diff")
        done
        arr=("${new[@]}")
    done

    total=$((total + sum))
done < input.txt

echo "$total"
