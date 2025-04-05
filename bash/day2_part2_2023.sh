
#!/usr/bin/env bash

# Day 2: Cube Conundrum
# Solves both Part 1 and Part 2 of the Advent of Code 2023 Day 2 challenge.
# Reads game data from input.txt and prints results to standard output.

INPUT_FILE="input.txt"

# --- Part 1 Configuration ---
# Maximum allowed cubes for a game to be possible
declare -r MAX_RED=12
declare -r MAX_GREEN=13
declare -r MAX_BLUE=14

# --- Main Function ---
main() {
    # Check if input file exists and is readable
    if [[ ! -f "$INPUT_FILE" || ! -r "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found or not readable." >&2
        exit 1
    fi

    local total_possible_ids=0 # Sum of IDs for Part 1
    local total_power_sum=0    # Sum of powers for Part 2

    local line
    local game_id
    local sets_part
    local reveal
    local count
    local color

    # Process each game (line) from the input file
    # Use process substitution with grep for efficient extraction of cube counts
    # Read line by line to keep track of game ID and manage state per game
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Extract Game ID
        # Remove "Game " prefix, then remove suffix starting from ":"
        game_id_part="${line#Game }"
        game_id="${game_id_part%%:*}"

        # Extract the part of the line containing the cube sets
        sets_part="${line#*: }"

        # --- Per-Game Variables ---
        local is_game_possible=1 # Assume possible (1=true, 0=false for arithmetic)
        local min_red=0          # Minimum red cubes needed for this game (Part 2)
        local min_green=0        # Minimum green cubes needed for this game (Part 2)
        local min_blue=0         # Minimum blue cubes needed for this game (Part 2)

        # Use grep -oE to extract all "count color" pairs from the sets part
        # Process substitution avoids creating temporary files and pipes efficiently
        while IFS=' ' read -r count color; do
            # Sanitize color (remove potential trailing comma/whitespace if grep includes it)
            # Though the pattern '[0-9]+ (red|green|blue)' should be quite specific
            color="${color//[^a-z]/}" # Keep only letters

             # Ensure count is treated as a number
            if ! [[ "$count" =~ ^[0-9]+$ ]]; then
                 echo "Warning: Invalid count '$count' found in Game $game_id. Skipping reveal." >&2
                 continue
            fi
            count=$((count)) # Force arithmetic interpretation

            # --- Part 1 Check: Is this reveal impossible? ---
            case "$color" in
                red)
                    if (( count > MAX_RED )); then is_game_possible=0; fi
                    ;;
                green)
                    if (( count > MAX_GREEN )); then is_game_possible=0; fi
                    ;;
                blue)
                    if (( count > MAX_BLUE )); then is_game_possible=0; fi
                    ;;
                *)
                    # Should not happen with the grep pattern, but good practice
                    echo "Warning: Unknown color '$color' found in Game $game_id. Skipping reveal." >&2
                    continue
                    ;;
            esac

            # --- Part 2 Update: Track maximum count seen for each color ---
            case "$color" in
                red)
                    if (( count > min_red )); then min_red=$count; fi
                    ;;
                green)
                    if (( count > min_green )); then min_green=$count; fi
                    ;;
                blue)
                    if (( count > min_blue )); then min_blue=$count; fi
                    ;;
            esac

        # Feed the extracted "count color" lines into the inner while loop
        done < <(grep -oE '[0-9]+ (red|green|blue)' <<< "$sets_part")


        # --- Accumulate Results ---

        # Part 1: Add ID if game was possible
        if (( is_game_possible == 1 )); then
            total_possible_ids=$((total_possible_ids + game_id))
        fi

        # Part 2: Calculate power of minimum set and add to sum
        local power=$((min_red * min_green * min_blue))
        total_power_sum=$((total_power_sum + power))

    done < "$INPUT_FILE" # Read lines from the input file

    # Print the final results
    echo "Sum of IDs of possible games (Part 1): $total_possible_ids"
    echo "Sum of the power of minimum sets (Part 2): $total_power_sum"
}

# --- Script Entry Point ---
# Execute the main function
main
