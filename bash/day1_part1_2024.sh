
#!/bin/bash

# --- Day 1: Historian Hysteria ---
# Calculates the total distance between two lists of numbers from input.txt
# The distance is found by sorting both lists numerically, pairing corresponding
# elements (smallest left with smallest right, etc.), calculating the absolute
# difference for each pair, and summing these differences.

INPUT_FILE="input.txt"

# Check if input file exists and is readable
if [[ ! -f "$INPUT_FILE" ]]; then
    echo "Error: Input file '$INPUT_FILE' not found." >&2
    exit 1
fi
 if [[ ! -r "$INPUT_FILE" ]]; then
    echo "Error: Input file '$INPUT_FILE' not readable." >&2
    exit 1
fi

# Main function to perform the calculation
main() {
    # 1. Extract the first column ($1) from the input file using awk.
    # 2. Sort the first column numerically (-n).
    # 3. Use process substitution <(...) to treat the output as a file for paste.
    # 4. Do the same for the second column ($2).
    # 5. Use 'paste' to combine the two sorted columns side-by-side, separated by a tab.
    # 6. Pipe the paired, sorted numbers to awk for the final calculation.
    paste <(awk '{print $1}' "$INPUT_FILE" | sort -n) \
          <(awk '{print $2}' "$INPUT_FILE" | sort -n) |
    awk '
    BEGIN {
        # Initialize the total distance sum
        total_distance = 0
    }
    {
        # $1 is the number from the sorted left list
        # $2 is the number from the sorted right list
        # Calculate the difference
        diff = $1 - $2

        # Calculate the absolute value of the difference
        if (diff < 0) {
            diff = -diff
        }

        # Add the absolute difference to the total sum
        total_distance += diff
    }
    END {
        # Print the final total distance
        print total_distance
    }
    '
}

# Execute the main function
main
