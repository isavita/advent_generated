
#!/bin/bash

# Reads disc configurations from input.txt and finds the first time
# a capsule can fall through all discs.

main() {
    local -a total_positions=()
    local -a start_positions=()
    local regex='Disc #[0-9]+ has ([0-9]+) positions; at time=0, it is at position ([0-9]+).'
    local i=0

    # Read and parse input file
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ $line =~ $regex ]]; then
            total_positions[i]="${BASH_REMATCH[1]}"
            start_positions[i]="${BASH_REMATCH[2]}"
            ((i++))
        fi
    done < "input.txt"

    local num_discs=${#total_positions[@]}
    local time=0

    # Simulate time steps
    while true; do
        local all_aligned=1 # Assume all discs align for this time

        # Check each disc's position
        for ((i = 0; i < num_discs; i++)); do
            local total="${total_positions[i]}"
            local start="${start_positions[i]}"
            
            # Calculate position at time + i + 1
            # (start + time + disc_index + 1) % total_positions must be 0
            if (( (start + time + i + 1) % total != 0 )); then
                all_aligned=0 # Misaligned
                break        # No need to check further discs for this time
            fi
        done

        # If all discs were aligned, print time and exit
        if (( all_aligned == 1 )); then
            echo "$time"
            exit 0
        fi

        ((time++)) # Increment time
    done
}

# Call the main function
main
