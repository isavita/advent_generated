
#!/bin/bash

# --- Day 7: The Treachery of Whales ---
# Part 1: Minimize fuel cost where cost = |target_pos - current_pos|

# Function to calculate the total fuel cost for a given alignment position
# This function implements the mathematical property that the sum of
# absolute differences |x_i - p| is minimized when p is the median of x_i.
calculate_min_fuel() {
    local input_file="$1"
    local line
    local positions_str
    local -a positions # Declare as array
    local -a sorted_positions
    local n
    local median_index
    local median_pos
    local total_fuel=0
    local crab_pos
    local diff

    # Check if input file exists
    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        return 1
    fi

    # Read the single line from the input file
    read -r line < "$input_file"
    if [[ -z "$line" ]]; then
        echo "Error: Input file '$input_file' is empty." >&2
        return 1
    fi

    # Replace commas with spaces to prepare for array creation
    positions_str="${line//,/ }"

    # Read the space-separated numbers into the positions array
    # shellcheck disable=SC2207 # Word splitting is intended here
    positions=($positions_str)

    # Check if array is empty (e.g., input was just commas or whitespace)
    if [[ ${#positions[@]} -eq 0 ]]; then
        echo "Error: No valid positions found in input." >&2
        return 1
    fi

    # Sort the positions numerically to find the median
    # Using process substitution and sort -n
    # shellcheck disable=SC2207 # Word splitting is intended here
    sorted_positions=($(printf "%s\n" "${positions[@]}" | sort -n))

    # Calculate the number of crabs
    n=${#sorted_positions[@]}

    # Find the median position. For an array sorted 0..n-1:
    # If n is odd, median index is (n-1)/2
    # If n is even, any position between index n/2-1 and n/2 minimizes cost.
    # Integer division (n-1)/2 conveniently gives the lower-middle index for even n
    # and the middle index for odd n.
    median_index=$(( (n - 1) / 2 ))
    median_pos=${sorted_positions[$median_index]}

    # Calculate the total fuel cost to align at the median position
    for crab_pos in "${sorted_positions[@]}"; do
        # Calculate absolute difference
        diff=$(( crab_pos - median_pos ))
        if (( diff < 0 )); then
            diff=$(( -diff ))
        fi
        total_fuel=$(( total_fuel + diff ))
    done

    # Print the minimum total fuel cost
    echo "$total_fuel"
    return 0
}

# Main entry point
main() {
    local input_filename="input.txt"
    calculate_min_fuel "$input_filename"
}

# Execute the main function
main

