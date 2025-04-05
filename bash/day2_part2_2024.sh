
#!/bin/bash

# --- Day 2: Red-Nosed Reports ---
#
# Checks reports (lines of numbers) for safety based on two criteria:
# 1. Strictly monotonic (all increasing or all decreasing).
# 2. Absolute difference between adjacent numbers is between 1 and 3.
# Part 1: Count reports satisfying both criteria.
# Part 2: Count reports that are safe OR become safe after removing exactly one number.

# Function to check if a sequence of numbers (passed as arguments) is safe
# according to the rules of Part 1.
# Returns 0 (Bash success) if safe, 1 (Bash failure) if unsafe.
is_safe() {
    # Capture all function arguments into a local array 'levels'
    local -a levels=("$@")
    local n=${#levels[@]}
    local diff trend abs_diff current next current_trend i

    # A report with 0 or 1 level is trivially safe (no adjacent pairs)
    if (( n < 2 )); then
        return 0
    fi

    # Calculate the difference between the first two levels
    diff=$(( levels[1] - levels[0] ))
    # Calculate absolute difference using parameter expansion (removes leading '-')
    abs_diff=${diff#-}

    # Check initial difference: must not be zero (strictly monotonic)
    # and absolute difference must be between 1 and 3 inclusive.
    if (( diff == 0 || abs_diff < 1 || abs_diff > 3 )); then
        return 1 # Unsafe: Plateau or difference out of range [1, 3]
    fi

    # Determine the initial trend: 1 for increasing, -1 for decreasing
    trend=$(( diff > 0 ? 1 : -1 ))

    # Iterate through the rest of the adjacent pairs (from index 1 to n-2)
    for (( i = 1; i < n - 1; i++ )); do
        current=${levels[i]}
        next=${levels[i+1]}
        diff=$(( next - current ))
        abs_diff=${diff#-}

        # Check current difference validity (same criteria as the first pair)
        if (( diff == 0 || abs_diff < 1 || abs_diff > 3 )); then
            return 1 # Unsafe: Plateau or difference out of range [1, 3]
        fi

        # Determine the trend of the current pair
        current_trend=$(( diff > 0 ? 1 : -1 ))

        # Check if the trend has changed
        if (( current_trend != trend )); then
            return 1 # Unsafe: Trend changed (e.g., increasing then decreasing)
        fi
    done

    # If all checks passed, the report is safe
    return 0
}

# Main function to process the input file and count safe reports for Part 2
main() {
    local input_file="input.txt"
    local safe_count_part2=0
    local line
    local -a levels temp_levels # Declare arrays locally
    local n i found_safe_removal

    # Check if input file exists and is readable
    if [[ ! -f "$input_file" || ! -r "$input_file" ]]; then
        echo "Error: Cannot read input file '$input_file'" >&2
        exit 1
    fi

    # Read the input file line by line
    # Using process substitution and mapfile/readarray is often more robust
    # than `while read`, especially with potential edge cases, but `while read`
    # is standard and fine here. Using `|| [[ -n "$line" ]]` handles files
    # without a trailing newline.
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip empty lines if any
        [[ -z "$line" ]] && continue

        # Read the space-separated numbers from the line into the 'levels' array
        read -r -a levels <<< "$line"
        n=${#levels[@]}

        # First, check if the original report is safe (Part 1 check)
        if is_safe "${levels[@]}"; then
            ((safe_count_part2++))
            # If safe originally, it counts for Part 2, move to the next line
            continue
        fi

        # If not safe originally, try removing one level at a time (Part 2 check)
        found_safe_removal=false # Flag to track if removal resulted in safety
        for (( i = 0; i < n; i++ )); do
            # Create a temporary array 'temp_levels' by removing the element at index 'i'
            # This uses Bash array slicing: take elements before 'i' and after 'i'
            temp_levels=("${levels[@]:0:i}" "${levels[@]:i+1}")

            # Check if the modified report (with one level removed) is safe
            if is_safe "${temp_levels[@]}"; then
                ((safe_count_part2++))
                found_safe_removal=true
                # Found a way to make it safe by removing one level,
                # no need to check other removals for this line.
                break
            fi
        done
        # The loop finishes, either because a safe removal was found (and break was called)
        # or because all possible single removals were checked and none worked.
        # The count is incremented only if 'found_safe_removal' became true.

    done < "$input_file"

    # Print the final count for Part 2 to standard output
    echo "$safe_count_part2"
}

# --- Script Entry Point ---
# Execute the main function
main
