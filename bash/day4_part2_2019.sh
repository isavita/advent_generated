
#!/bin/bash

# Function to check if digits never decrease
never_decreases() {
    local num_str="$1"
    local len="${#num_str}"
    for (( j=0; j<len-1; j++ )); do
        local d1="${num_str:j:1}"
        local d2="${num_str:j+1:1}"
        # Use arithmetic comparison for single digits
        if [[ "$d1" -gt "$d2" ]]; then
            return 1 # false (decreases)
        fi
    done
    return 0 # true (never decreases)
}

# Function to check for at least one adjacent pair using grep (optimized)
has_adjacent() {
    # Check if any digit is followed by itself
    if echo "$1" | grep -q -E '([0-9])\1'; then
        return 0 # true (found adjacent)
    else
        return 1 # false (no adjacent)
    fi
}

# Function to check for a pair of exactly two adjacent digits using awk (optimized)
has_only_two_adjacent() {
    # Use awk to count consecutive digits and check for groups of exactly 2
    if echo "$1" | awk '
        BEGIN { FS=""; found_exact_pair=0 }
        {
            count = 1
            for (i = 1; i < NF; i++) {
                if ($i == $(i+1)) {
                    count++
                } else {
                    if (count == 2) { found_exact_pair=1; break }
                    count = 1
                }
            }
            # Check the group at the very end of the string
            if (count == 2) { found_exact_pair=1 }

            # Exit with 0 if an exact pair was found (success for shell), 1 otherwise (failure for shell)
            exit !found_exact_pair
        }'; then
         return 0 # true (awk exited 0)
    else
         return 1 # false (awk exited non-zero)
    fi
}

main() {
    # Read start and end from input.txt, splitting on '-'
    IFS='-' read start end < "input.txt"

    local count_part1=0
    local count_part2=0

    # Loop through the range
    for (( i=start; i<=end; i++ )); do
        # Convert number to string (Bash treats it as string implicitly here)
        local num="$i"

        # Check common condition first: never decreases
        if never_decreases "$num"; then
            # Part 1 check: has adjacent
            if has_adjacent "$num"; then
                (( count_part1++ ))
            fi
            # Part 2 check: has only two adjacent
            if has_only_two_adjacent "$num"; then
                 (( count_part2++ ))
            fi
        fi
    done

    # Print the results
    echo "$count_part1"
    echo "$count_part2"
}

# Run the main function
main
