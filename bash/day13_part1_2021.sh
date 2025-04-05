
#!/bin/bash

# --- Day 13: Transparent Origami ---
# Part 1: Count dots after the first fold.

# Input file name
INPUT_FILE="input.txt"

# Function to perform the fold operation on coordinates
# Takes coordinates (stdin), fold axis (arg1), and fold value (arg2)
# Outputs folded coordinates (stdout)
perform_fold() {
    local fold_axis="$1"
    local fold_value="$2"

    awk -F, -v axis="$fold_axis" -v val="$fold_value" '
    {
        x = $1
        y = $2

        # Apply the fold transformation based on the axis
        if (axis == "y" && y > val) {
            # Fold up: new_y = fold_line - (old_y - fold_line) = 2*fold_line - old_y
            y = 2 * val - y
        } else if (axis == "x" && x > val) {
            # Fold left: new_x = fold_line - (old_x - fold_line) = 2*fold_line - old_x
            x = 2 * val - x
        }

        # Print the potentially transformed coordinates
        print x "," y
    }
    '
}

# Main execution logic
main() {
    # Check if input file exists
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi

    # Extract coordinates (lines containing only digits and comma)
    # Use grep to filter lines that look like coordinates (e.g., "123,456")
    # This avoids accidentally matching fold lines if they contained numbers.
    local coordinates
    coordinates=$(grep '^[0-9]*,[0-9]*$' "$INPUT_FILE")

    # Extract the *first* fold instruction
    local first_fold_line
    first_fold_line=$(grep 'fold along' "$INPUT_FILE" | head -n 1)

    # Check if a fold instruction was found
    if [[ -z "$first_fold_line" ]]; then
        echo "Error: No fold instructions found in '$INPUT_FILE'." >&2
        exit 1
    fi

    # Parse the first fold instruction
    # Remove "fold along " prefix
    local fold_spec="${first_fold_line#fold along }"
    # Extract axis (before '=')
    local fold_axis="${fold_spec%=*}"
    # Extract value (after '=')
    local fold_value="${fold_spec#*=}"

    # --- Processing ---
    # 1. Feed the initial coordinates into the perform_fold function
    # 2. Sort the resulting folded coordinates
    # 3. Use uniq to keep only unique coordinates
    # 4. Count the number of unique lines (dots)
    local unique_dot_count
    unique_dot_count=$(echo "$coordinates" | perform_fold "$fold_axis" "$fold_value" | sort -u | wc -l)
    
    # Alternatively using sort | uniq instead of sort -u
    # unique_dot_count=$(echo "$coordinates" | perform_fold "$fold_axis" "$fold_value" | sort | uniq | wc -l)


    # Print the final count
    echo "$unique_dot_count"
}

# Call the main function
main
