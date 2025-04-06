
#!/bin/bash

main() {
    local enabled=1
    local total_sum=0
    local line num1 num2 result

    # Regex matching the Python version, extracting only relevant parts
    local regex='(mul\([0-9]{1,3},[0-9]{1,3}\))|(do\(\))|(don'\''t\(\))'

    # Read matches from input.txt one by one
    while IFS= read -r line; do
        # Check for mul(x,y) pattern
        if [[ $line =~ ^mul\(([0-9]{1,3}),([0-9]{1,3})\)$ ]]; then
            if (( enabled )); then
                num1="${BASH_REMATCH[1]}"
                num2="${BASH_REMATCH[2]}"
                result=$(( num1 * num2 ))
                total_sum=$(( total_sum + result ))
                echo "Adding $num1 * $num2 = $result"
            fi
        # Check for do()
        elif [[ $line == "do()" ]]; then
            enabled=1
            echo "Multiplication enabled"
        # Check for don't()
        elif [[ $line == "don't()" ]]; then
            enabled=0
            echo "Multiplication disabled"
        fi
    # Use grep -oE to extract matches and process substitution to feed the loop
    done < <(grep -oE "$regex" input.txt)

    echo "Total sum: $total_sum"
}

# Check if input file exists
if [[ ! -f "input.txt" ]]; then
    echo "Error: input.txt not found." >&2
    exit 1
fi

main
