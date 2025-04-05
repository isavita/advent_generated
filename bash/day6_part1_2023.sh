
#!/bin/bash

# Function to calculate the number of winning scenarios for a single race
number_of_ways_to_win() {
    local time=$1
    local record=$2
    local ways=0
    local hold_time
    local travel_time
    local distance

    for (( hold_time=1; hold_time<time; hold_time++ )); do
        travel_time=$((time - hold_time))
        distance=$((hold_time * travel_time))
        if (( distance > record )); then
            (( ways++ ))
        fi
    done
    echo "$ways"
}

main() {
    # Read times and distances from input.txt, skipping the first word ("Time:", "Distance:")
    read -r _ times_str <<< "$(head -n 1 input.txt)"
    read -r _ distances_str <<< "$(tail -n 1 input.txt)"

    # Create arrays from the numbers
    read -a times <<< "$times_str"
    read -a distances <<< "$distances_str"

    local total_ways=1
    local num_races=${#times[@]}
    local i
    local ways_for_race

    for (( i=0; i<num_races; i++ )); do
        ways_for_race=$(number_of_ways_to_win "${times[i]}" "${distances[i]}")
        # If any race has 0 ways, the total product is 0
        if (( ways_for_race == 0 )); then
            total_ways=0
            break
        fi
        (( total_ways *= ways_for_race ))
    done

    echo "$total_ways"
}

# Check if input file exists
if [[ ! -f "input.txt" ]]; then
    echo "Error: input.txt not found." >&2
    exit 1
fi

main
