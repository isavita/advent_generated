
#!/bin/bash

# --- Data Storage ---
# Use indexed arrays to store ingredient properties
declare -a names
declare -a capacities
declare -a durabilities
declare -a flavors
declare -a textures
declare -a calories
num_ingredients=0

# --- Input Reading ---
read_ingredients() {
    local filename="$1"
    local i=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Use read -ra to split line into parts array
        read -ra parts <<< "$line"
        # Basic validation
        if [[ "${#parts[@]}" -lt 11 ]]; then
            continue
        fi
        # Extract properties, removing trailing commas where needed
        names[i]="${parts[0]%?}" # Remove trailing :
        capacities[i]="${parts[2]%,}"
        durabilities[i]="${parts[4]%,}"
        flavors[i]="${parts[6]%,}"
        textures[i]="${parts[8]%,}"
        calories[i]="${parts[10]}"
        ((i++))
    done < "$filename"
    num_ingredients=$i
}

# --- Score Calculation ---
score() {
    local -a teaspoons=("${@}") # Pass array elements as arguments
    local capacity=0
    local durability=0
    local flavor=0
    local texture=0
    local i

    for (( i=0; i<num_ingredients; i++ )); do
        (( capacity += capacities[i] * teaspoons[i] ))
        (( durability += durabilities[i] * teaspoons[i] ))
        (( flavor += flavors[i] * teaspoons[i] ))
        (( texture += textures[i] * teaspoons[i] ))
    done

    # Clamp negative values to 0
    (( capacity < 0 )) && capacity=0
    (( durability < 0 )) && durability=0
    (( flavor < 0 )) && flavor=0
    (( texture < 0 )) && texture=0

    echo $(( capacity * durability * flavor * texture ))
}

# --- Calorie Calculation ---
calculate_calories() {
    local -a teaspoons=("${@}") # Pass array elements as arguments
    local total_calories=0
    local i

    for (( i=0; i<num_ingredients; i++ )); do
        (( total_calories += calories[i] * teaspoons[i] ))
    done
    echo "$total_calories"
}

# --- Recursive Calculation ---
# Arguments: index, remaining_teaspoons, current_teaspoons (space separated string)
calculate_max_score() {
    local index=$1
    local remaining=$2
    # Collect teaspoons passed as separate args starting from $3
    shift 2
    local -a current_teaspoons=("$@")
    local target_calories=$global_target_calories # Use global target

    # Base case: Last ingredient
    if (( index == num_ingredients - 1 )); then
        current_teaspoons[index]=$remaining
        local current_calories
        current_calories=$(calculate_calories "${current_teaspoons[@]}")

        if (( current_calories == target_calories )); then
            score "${current_teaspoons[@]}"
        else
            echo 0
        fi
        return
    fi

    local max_score=0
    local i
    local current_score

    for (( i=0; i<=remaining; i++ )); do
        current_teaspoons[index]=$i
        # Recursively call, passing current teaspoons array elements
        current_score=$(calculate_max_score "$((index + 1))" "$((remaining - i))" "${current_teaspoons[@]}")
        if (( current_score > max_score )); then
            max_score=$current_score
        fi
    done
    echo "$max_score"
}

# --- Main Execution ---
main() {
    local input_file="input.txt"
    local total_teaspoons=100
    # Make target calories global for easier access in recursion
    global_target_calories=500

    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        exit 1
    fi

    read_ingredients "$input_file"

    if (( num_ingredients == 0 )); then
        echo "Error: No ingredients read from '$input_file'." >&2
        exit 1
    fi

    # Initialize an empty array for teaspoons to pass to the first call
    local -a initial_teaspoons=()
    local final_max_score
    final_max_score=$(calculate_max_score 0 "$total_teaspoons" "${initial_teaspoons[@]}")
    echo "$final_max_score"
}

# --- Entry Point ---
main
