
#!/usr/bin/env bash

set -euo pipefail

readonly ROWS=6
readonly COLS=50
readonly SIZE=$((ROWS * COLS))
declare -a screen

# Initialize screen (flattened array)
initialize_screen() {
    screen=() # Ensure it's empty before filling
    for ((i=0; i<SIZE; i++)); do
        screen[i]=0
    done
}

# rect AxB
handle_rect() {
    local a=$1
    local b=$2
    for ((r=0; r<b && r<ROWS; r++)); do
        for ((c=0; c<a && c<COLS; c++)); do
            local index=$((r * COLS + c))
            screen[index]=1
        done
    done
}

# rotate row y=Y by B
handle_rotate_row() {
    local y=$1
    local b=$2
    local b_mod=$(( b % COLS ))
    if (( b_mod == 0 )); then return; fi

    local temp_row=()
    local start_index=$(( y * COLS ))

    # Copy row to temp array
    for ((c=0; c<COLS; c++)); do
        temp_row[c]=${screen[start_index + c]}
    done

    # Rotate and write back
    for ((c=0; c<COLS; c++)); do
        local src_col=$(( (c - b_mod + COLS) % COLS ))
        screen[start_index + c]=${temp_row[src_col]}
    done
}

# rotate column x=X by B
handle_rotate_col() {
    local x=$1
    local b=$2
    local b_mod=$(( b % ROWS ))
     if (( b_mod == 0 )); then return; fi

    local temp_col=()

    # Copy column to temp array
    for ((r=0; r<ROWS; r++)); do
        temp_col[r]=${screen[r * COLS + x]}
    done

    # Rotate and write back
    for ((r=0; r<ROWS; r++)); do
        local src_row=$(( (r - b_mod + ROWS) % ROWS ))
        screen[r * COLS + x]=${temp_col[src_row]}
    done
}

main() {
    initialize_screen

    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" =~ ^rect[[:space:]]+([0-9]+)x([0-9]+)$ ]]; then
            handle_rect "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
        elif [[ "$line" =~ ^rotate[[:space:]]+row[[:space:]]+y=([0-9]+)[[:space:]]+by[[:space:]]+([0-9]+)$ ]]; then
            handle_rotate_row "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
        elif [[ "$line" =~ ^rotate[[:space:]]+column[[:space:]]+x=([0-9]+)[[:space:]]+by[[:space:]]+([0-9]+)$ ]]; then
            handle_rotate_col "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
        fi
    done < "input.txt"

    # Count lit pixels
    local lit_pixels=0
    for ((i=0; i<SIZE; i++)); do
        if [[ "${screen[i]}" -eq 1 ]]; then
            lit_pixels=$((lit_pixels + 1))
        fi
    done

    echo "$lit_pixels"
}

main
