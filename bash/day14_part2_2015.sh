
#!/bin/bash

# Function to calculate distance traveled by a reindeer at a given time
distance() {
    local speed=$1 fly_time=$2 rest_time=$3 total_time=$4
    local cycle_time fly_in_remainder dist full_cycles remaining_time

    cycle_time=$((fly_time + rest_time))
    # Handle division by zero if cycle_time is somehow 0
    if (( cycle_time == 0 )); then
        # If cycle_time is 0, it implies continuous flying (or static)
        # Assuming fly_time > 0 and speed > 0 if cycle_time is 0
        if (( fly_time > 0 && speed > 0 )); then
           dist=$(( total_time * speed ))
        else
           dist=0 # Cannot move if speed or fly_time is 0
        fi
        echo "$dist"
        return
    fi

    full_cycles=$((total_time / cycle_time))
    remaining_time=$((total_time % cycle_time))

    dist=$((full_cycles * fly_time * speed))

    # Calculate fly time within the remaining part of the cycle
    fly_in_remainder=$remaining_time
    if (( fly_in_remainder > fly_time )); then
        fly_in_remainder=$fly_time
    fi

    dist=$((dist + fly_in_remainder * speed))
    echo "$dist"
}

main() {
    local speeds=() fly_times=() rest_times=()
    local speed fly_time rest_time
    local total_time=2503
    local i num_reindeer

    # Read input using awk for robust parsing
    # Assumes numbers are the 4th, 7th, and 14th fields
    while read -r speed fly_time rest_time; do
        # Basic validation if needed
        if [[ -z "$speed" || -z "$fly_time" || -z "$rest_time" ]]; then
             echo "Warning: Skipping invalid line in input.txt" >&2
             continue
        fi
        speeds+=("$speed")
        fly_times+=("$fly_time")
        rest_times+=("$rest_time")
    done < <(awk '{print $4, $7, $14}' input.txt 2>/dev/null) # Suppress awk errors if format differs

    num_reindeer=${#speeds[@]}
    if (( num_reindeer == 0 )); then
        echo "Error: No valid reindeer data found in input.txt" >&2
        exit 1
    fi

    # Part 1: Max distance after total_time
    local max_dist=0 dist
    for ((i=0; i<num_reindeer; i++)); do
        dist=$(distance "${speeds[i]}" "${fly_times[i]}" "${rest_times[i]}" "$total_time")
        if (( dist > max_dist )); then
            max_dist=$dist
        fi
    done
    echo "$max_dist"

    # Part 2: Max points awarded second by second
    local points=() current_distances=()
    local t leading_distance max_points=0

    # Initialize points array
    for ((i=0; i<num_reindeer; i++)); do
        points[i]=0
    done

    # Simulate second by second
    for ((t=1; t<=total_time; t++)); do
        current_distances=()
        leading_distance=0
        # Calculate distances at time t and find the maximum distance
        for ((i=0; i<num_reindeer; i++)); do
            dist=$(distance "${speeds[i]}" "${fly_times[i]}" "${rest_times[i]}" "$t")
            current_distances[i]=$dist
            if (( dist > leading_distance )); then
                leading_distance=$dist
            fi
        done

        # Award points to leaders at time t
        # Only award points if leading_distance is greater than 0
        if (( leading_distance > 0 )); then
             for ((i=0; i<num_reindeer; i++)); do
                 if (( current_distances[i] == leading_distance )); then
                     (( points[i]++ ))
                 fi
             done
        fi
    done

    # Find max points
    for ((i=0; i<num_reindeer; i++)); do
        if (( points[i] > max_points )); then
            max_points=${points[i]}
        fi
    done
    echo "$max_points"
}

# Check if input file exists and is readable
if [[ ! -r "input.txt" ]]; then
    echo "Error: input.txt not found or not readable." >&2
    exit 1
fi

# Run the main logic
main

