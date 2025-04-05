
#!/bin/bash

# Reads depth and target coordinates from input.txt
# Calculates the total risk level of the cave system area up to the target.

shopt -s expand_aliases
alias bc='bc -l'

main() {
    # Read input from input.txt
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    local depth
    local target_x
    local target_y

    depth=$(awk -F': ' '/depth/ {print $2}' input.txt)
    IFS=',' read target_x target_y < <(awk -F': |,' '/target/ {print $2","$3}' input.txt)

    # Declare an indexed array to simulate the 2D cave
    declare -a cave
    local risk_level=0
    local width=$((target_x + 1)) # Width of the grid for 1D indexing

    # Iterate through the grid, calculate erosion levels, and sum risk
    for (( y=0; y <= target_y; y++ )); do
        for (( x=0; x <= target_x; x++ )); do
            local geo_idx
            local idx=$((y * width + x)) # 1D index for cave[y][x]

            if (( x == 0 && y == 0 )); then
                geo_idx=0
            elif (( x == target_x && y == target_y )); then
                geo_idx=0
            elif (( y == 0 )); then
                geo_idx=$((x * 16807))
            elif (( x == 0 )); then
                geo_idx=$((y * 48271))
            else
                # Get erosion levels from neighbours (left and up)
                local idx_left=$((y * width + x - 1))
                local idx_up=$(((y - 1) * width + x))
                local erosion_left=${cave[$idx_left]}
                local erosion_up=${cave[$idx_up]}
                geo_idx=$((erosion_left * erosion_up))
            fi

            # Calculate erosion level and store it
            local erosion=$(((geo_idx + depth) % 20183))
            cave[$idx]=$erosion

            # Add to total risk level (erosion level % 3)
            risk_level=$((risk_level + erosion % 3))
        done
    done

    echo "$risk_level"
}

# Run the main function
main
