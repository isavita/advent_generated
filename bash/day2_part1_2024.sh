
#!/bin/bash

# Function to check if a report is safe
# Input: array representing the report levels
is_safe() {
    local report=("$@")
    local n=${#report[@]}

    # Reports with 0 or 1 element are considered safe by Python's logic
    # because the 'all()' checks on empty ranges return True.
    if (( n < 2 )); then
        return 0 # 0 indicates true (success) in Bash functions
    fi

    local is_increasing=1
    local is_decreasing=1
    local diff_ok=1
    local i current next diff abs_diff

    for (( i=0; i < n - 1; i++ )); do
        current=${report[i]}
        next=${report[i+1]}

        # Check difference constraint (1 <= abs(diff) <= 3)
        diff=$(( next - current ))
        abs_diff=${diff#-} # Bash way to get absolute value for integers

        if (( abs_diff < 1 || abs_diff > 3 )); then
            diff_ok=0
            break # No need to check further if difference is wrong
        fi

        # Check monotonicity (strictly increasing or decreasing)
        if (( current >= next )); then
            is_increasing=0
        fi
        if (( current <= next )); then
            is_decreasing=0
        fi

        # Optimization: if neither strictly increasing nor decreasing is possible, break
        if (( is_increasing == 0 && is_decreasing == 0 )); then
            break
        fi
    done

    # Return true (0) if difference is ok AND (it was increasing OR it was decreasing)
    if (( diff_ok == 1 && (is_increasing == 1 || is_decreasing == 1) )); then
        return 0 # Safe
    else
        return 1 # Not safe
    fi
}

# Main function
main() {
    local safe_count=0
    local line
    local report

    # Check if input file exists and is readable
    if [[ ! -f "input.txt" || ! -r "input.txt" ]]; then
        echo "Error: input.txt not found or not readable." >&2
        exit 1
    fi

    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip empty lines
        [[ -z "$line" ]] && continue

        # Read line into an array 'report'
        read -r -a report <<< "$line"

        # Check if the report is safe using the function
        if is_safe "${report[@]}"; then
            (( safe_count++ ))
        fi
    done < "input.txt"

    echo "$safe_count"
}

# Call the main function
main
