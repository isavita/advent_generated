
#!/bin/bash

# Function to calculate fuel cost for a given target position
# Reads the global 'positions' array
calculate_fuel() {
    local target=$1
    local total_fuel=0
    local pos diff fuel_cost

    for pos in "${positions[@]}"; do
        # Calculate absolute difference
        if (( pos > target )); then
            diff=$((pos - target))
        else
            diff=$((target - pos))
        fi
        # Calculate fuel cost: diff * (diff + 1) / 2
        fuel_cost=$((diff * (diff + 1) / 2))
        total_fuel=$((total_fuel + fuel_cost))
    done
    echo "$total_fuel"
}

main() {
    # Read positions from input.txt, replacing commas with spaces
    # Use process substitution and read into an array
    read -r -a positions <<< "$(tr ',' ' ' < input.txt)"

    local sum=0
    local count=0
    local pos

    # Calculate sum and count of positions
    for pos in "${positions[@]}"; do
        sum=$((sum + pos))
        count=$((count + 1))
    done

    # Optimization: The optimal target minimizing this fuel cost
    # is provably near the mean (average) position.
    # We only need to check floor(mean) and ceil(mean).
    # Bash integer division gives floor(mean).
    local target1=$((sum / count))
    local target2=$((target1 + 1)) # ceil(mean) if mean wasn't integer

    # Calculate fuel cost for the two potential optimal targets
    local fuel1
    fuel1=$(calculate_fuel "$target1")
    local fuel2
    fuel2=$(calculate_fuel "$target2")

    # Determine the minimum fuel cost
    local min_fuel
    if (( fuel1 < fuel2 )); then
        min_fuel=$fuel1
    else
        min_fuel=$fuel2
    fi

    # Print the minimum fuel cost
    echo "$min_fuel"
}

# Execute the main function
main
