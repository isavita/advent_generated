
#!/usr/bin/env bash

# --- Day 2: Bathroom Security ---
# Reads movement instructions from input.txt and calculates two bathroom codes
# based on two different keypad layouts.

# --- Configuration ---
readonly INPUT_FILE="input.txt"

# --- Keypad Logic ---

# Part 1 Keypad:
# 1 2 3
# 4 5 6
# 7 8 9
# Args: $1 = current button, $2 = move (U/D/L/R)
# Output: next button
move_part1() {
    local current="$1"
    local move="$2"
    local next="$current" # Stay put if move is invalid or leads off the grid

    case "$current" in
        1) case "$move" in D) next=4 ;; R) next=2 ;; esac ;;
        2) case "$move" in D) next=5 ;; L) next=1 ;; R) next=3 ;; esac ;;
        3) case "$move" in D) next=6 ;; L) next=2 ;; esac ;;
        4) case "$move" in U) next=1 ;; D) next=7 ;; R) next=5 ;; esac ;;
        5) case "$move" in U) next=2 ;; D) next=8 ;; L) next=4 ;; R) next=6 ;; esac ;;
        6) case "$move" in U) next=3 ;; D) next=9 ;; L) next=5 ;; esac ;;
        7) case "$move" in U) next=4 ;; R) next=8 ;; esac ;;
        8) case "$move" in U) next=5 ;; L) next=7 ;; R) next=9 ;; esac ;;
        9) case "$move" in U) next=6 ;; L) next=8 ;; esac ;;
    esac
    echo "$next"
}

# Part 2 Keypad:
#     1
#   2 3 4
# 5 6 7 8 9
#   A B C
#     D
# Args: $1 = current button, $2 = move (U/D/L/R)
# Output: next button
move_part2() {
    local current="$1"
    local move="$2"
    local next="$current" # Stay put if move is invalid or leads off the grid

    case "$current" in
        1) case "$move" in D) next=3 ;; esac ;;
        2) case "$move" in D) next=6 ;; R) next=3 ;; esac ;;
        3) case "$move" in U) next=1 ;; D) next=7 ;; L) next=2 ;; R) next=4 ;; esac ;;
        4) case "$move" in D) next=8 ;; L) next=3 ;; esac ;;
        5) case "$move" in R) next=6 ;; esac ;;
        6) case "$move" in U) next=2 ;; D) next=A ;; L) next=5 ;; R) next=7 ;; esac ;;
        7) case "$move" in U) next=3 ;; D) next=B ;; L) next=6 ;; R) next=8 ;; esac ;;
        8) case "$move" in U) next=4 ;; D) next=C ;; L) next=7 ;; R) next=9 ;; esac ;;
        9) case "$move" in L) next=8 ;; esac ;;
        A) case "$move" in U) next=6 ;; R) next=B ;; esac ;;
        B) case "$move" in U) next=7 ;; D) next=D ;; L) next=A ;; R) next=C ;; esac ;;
        C) case "$move" in U) next=8 ;; L) next=B ;; esac ;;
        D) case "$move" in U) next=B ;; esac ;;
    esac
    echo "$next"
}

# --- Main Program Logic ---
main() {
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi

    local current_button_p1='5'
    local code_p1=''
    local current_button_p2='5'
    local code_p2=''
    local line
    local move
    local i

    # Process each line of instructions from the input file
    # Using process substitution ensures variables modified inside the loop
    # persist after the loop finishes in Bash.
    while IFS= read -r line || [[ -n "$line" ]]; do # Handle files with or without trailing newline
        # Use temporary variables for the current line's processing
        local temp_button_p1="$current_button_p1"
        local temp_button_p2="$current_button_p2"

        # Process each move character in the line
        for (( i=0; i<${#line}; i++ )); do
            move="${line:i:1}"
            temp_button_p1=$(move_part1 "$temp_button_p1" "$move")
            temp_button_p2=$(move_part2 "$temp_button_p2" "$move")
        done

        # Update the starting button for the next line
        current_button_p1="$temp_button_p1"
        current_button_p2="$temp_button_p2"

        # Append the final button for this line to the respective codes
        code_p1+="$current_button_p1"
        code_p2+="$current_button_p2"

    done < "$INPUT_FILE"

    # Print the results
    echo "Part 1 Code: $code_p1"
    echo "Part 2 Code: $code_p2"
}

# --- Program Entry Point ---
main
