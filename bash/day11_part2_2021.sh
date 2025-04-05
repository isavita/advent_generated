
#!/usr/bin/env bash

# --- Day 11: Dumbo Octopus ---
#
# Solves the Advent of Code 2021 Day 11 challenge in Bash.
# Reads input from input.txt and prints Part 1 and Part 2 answers.

set -euo pipefail # Enable strict mode

# --- Constants ---
readonly GRID_SIZE=10
readonly TOTAL_CELLS=$(( GRID_SIZE * GRID_SIZE ))
readonly MAX_STEPS_PART1=100
readonly INPUT_FILE="input.txt"

# --- Function to check if coordinates are within bounds ---
is_valid() {
    local r=$1
    local c=$2
    [[ $r -ge 0 && $r -lt $GRID_SIZE && $c -ge 0 && $c -lt $GRID_SIZE ]]
}

# --- Main Function ---
main() {
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi

    # --- Data Structures ---
    # Use a flat array to represent the 10x10 grid.
    # Index = row * GRID_SIZE + col
    declare -a grid
    # Temporary array to track which octopuses flashed in the current step
    declare -a flashed

    # --- Read Input ---
    local row=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ ${#line} -ne $GRID_SIZE ]]; then
             echo "Error: Invalid line length in input file (expected $GRID_SIZE): $line" >&2
             exit 1
        fi
        for (( col=0; col < GRID_SIZE; col++ )); do
            local index=$(( row * GRID_SIZE + col ))
            grid[index]=${line:col:1}
            if ! [[ "${grid[index]}" =~ ^[0-9]$ ]]; then
                 echo "Error: Invalid character in input file at row $row, col $col: ${grid[index]}" >&2
                 exit 1
            fi
        done
        ((row++))
    done < "$INPUT_FILE"

    if [[ ${#grid[@]} -ne $TOTAL_CELLS ]]; then
        echo "Error: Input grid dimensions are incorrect (expected ${GRID_SIZE}x${GRID_SIZE})." >&2
        exit 1
    fi


    # --- Simulation ---
    local total_flashes=0
    local step=0
    local first_sync_step=-1
    local part1_flashes=-1

    while true; do
        ((step++))
        local step_flashes=0
        # Reset flashed status for the new step
        for (( i=0; i < TOTAL_CELLS; i++ )); do
            flashed[i]=0
        done

        # 1. Increase energy levels by 1
        for (( i=0; i < TOTAL_CELLS; i++ )); do
            ((grid[i]++))
        done

        # 2. Handle flashes (cascade)
        local new_flash_occurred=1 # Use 1 for true, 0 for false
        while [[ $new_flash_occurred -eq 1 ]]; do
            new_flash_occurred=0 # Assume no new flashes this iteration
            for (( r=0; r < GRID_SIZE; r++ )); do
                for (( c=0; c < GRID_SIZE; c++ )); do
                    local index=$(( r * GRID_SIZE + c ))

                    # Check if octopus needs to flash and hasn't already
                    if [[ ${grid[index]} -gt 9 && ${flashed[index]} -eq 0 ]]; then
                        flashed[index]=1
                        ((step_flashes++))
                        new_flash_occurred=1 # A flash occurred, may need another pass

                        # Increase energy of neighbors
                        for dr in -1 0 1; do
                            for dc in -1 0 1; do
                                # Skip the octopus itself
                                if [[ $dr -eq 0 && $dc -eq 0 ]]; then
                                    continue
                                fi

                                local nr=$(( r + dr ))
                                local nc=$(( c + dc ))

                                # Check bounds and increase neighbor energy if valid
                                if is_valid "$nr" "$nc"; then
                                    local n_index=$(( nr * GRID_SIZE + nc ))
                                    # Only increment neighbours that haven't flashed yet
                                    # (Their energy doesn't matter anymore if they have flashed)
                                    if [[ ${flashed[n_index]} -eq 0 ]]; then
                                        ((grid[n_index]++))
                                    fi
                                fi
                            done # dc
                        done # dr
                    fi # end if needs to flash
                done # c
            done # r
        done # while new_flash_occurred

        # Add step flashes to total
        total_flashes=$(( total_flashes + step_flashes ))

        # Part 1: Record flashes after 100 steps
        if [[ $step -eq $MAX_STEPS_PART1 ]]; then
            part1_flashes=$total_flashes
        fi

        # 3. Reset energy of flashed octopuses to 0
        for (( i=0; i < TOTAL_CELLS; i++ )); do
            if [[ ${flashed[i]} -eq 1 ]]; then
                grid[i]=0
            fi
        done

        # Part 2: Check for synchronization
        if [[ $step_flashes -eq $TOTAL_CELLS && $first_sync_step -eq -1 ]]; then
            first_sync_step=$step
        fi

        # Exit condition: Stop when both parts are solved
        if [[ $part1_flashes -ne -1 && $first_sync_step -ne -1 ]]; then
            break
        fi

        # Safety break - avoid infinite loops in case of unexpected input/logic errors
        if [[ $step -gt 10000 ]]; then
             echo "Warning: Simulation exceeded 10000 steps. Stopping." >&2
             # If Part 1 wasn't found yet (unlikely), set it to current total
             if [[ $part1_flashes -eq -1 ]]; then part1_flashes=$total_flashes; fi
             break
        fi

    done # step loop

    # --- Output Results ---
    echo "Part 1: Total flashes after $MAX_STEPS_PART1 steps: $part1_flashes"
    if [[ $first_sync_step -ne -1 ]]; then
        echo "Part 2: First step where all octopuses flash: $first_sync_step"
    else
        echo "Part 2: All octopuses did not synchronize within the simulated steps."
    fi
}

# --- Program Entry Point ---
# Ensures the script logic runs when executed.
main "$@"
