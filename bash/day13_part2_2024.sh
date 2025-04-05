
#!/bin/bash

# --- Day 13: Claw Contraption ---
# Part Two: Calculate the minimum total tokens to win all possible prizes
# with adjusted coordinates.

# Define the large offset for Part Two
readonly OFFSET=10000000000000

# Main function to process the input and calculate the total tokens
main() {
    local total_tokens=0
    local line_a line_b line_p dummy # Variables to hold input lines
    local ax ay bx by px_orig py_orig # Parsed values
    local px py # Adjusted prize coordinates
    local det da db a b cost # Variables for calculation

    # Regular expressions for parsing
    local regex_a="^Button A: X\+([0-9]+), Y\+([0-9]+)$"
    local regex_b="^Button B: X\+([0-9]+), Y\+([0-9]+)$"
    local regex_p="^Prize: X=([0-9]+), Y=([0-9]+)$"

    # Read input file line by line, processing 4 lines at a time (A, B, Prize, blank)
    while read -r line_a && \
          read -r line_b && \
          read -r line_p && \
          read -r dummy # Read the blank line or handle EOF gracefully
    do
        # Parse Button A line
        if [[ "$line_a" =~ $regex_a ]]; then
            ax=${BASH_REMATCH[1]}
            ay=${BASH_REMATCH[2]}
        else
            echo "Error parsing Button A line: $line_a" >&2
            continue # Skip this machine
        fi

        # Parse Button B line
        if [[ "$line_b" =~ $regex_b ]]; then
            bx=${BASH_REMATCH[1]}
            by=${BASH_REMATCH[2]}
        else
            echo "Error parsing Button B line: $line_b" >&2
            continue # Skip this machine
        fi

        # Parse Prize line
        if [[ "$line_p" =~ $regex_p ]]; then
            px_orig=${BASH_REMATCH[1]}
            py_orig=${BASH_REMATCH[2]}
        else
            echo "Error parsing Prize line: $line_p" >&2
            continue # Skip this machine
        fi

        # Apply the large offset for Part Two
        px=$(( px_orig + OFFSET ))
        py=$(( py_orig + OFFSET ))

        # Solve the system of linear equations:
        # ax * a + bx * b = px
        # ay * a + by * b = py
        # Using Cramer's rule: a = da / det, b = db / det

        # Calculate the determinant of the coefficient matrix
        det=$(( ax * by - ay * bx ))

        # If determinant is zero, buttons move colinearly.
        # A unique solution exists only if the target is also on the same line.
        # For simplicity and robustness (avoiding division by zero),
        # we skip if det is 0. In practice, integer solutions are unlikely here.
        if (( det == 0 )); then
            continue # No unique solution or infinite solutions, skip
        fi

        # Calculate determinants for 'a' and 'b'
        da=$(( px * by - py * bx ))
        db=$(( ax * py - ay * px ))

        # Check if 'a' and 'b' are integers (da and db must be divisible by det)
        if (( da % det != 0 || db % det != 0 )); then
            continue # Not an integer solution, skip
        fi

        # Calculate 'a' and 'b'
        a=$(( da / det ))
        b=$(( db / det ))

        # Check if 'a' and 'b' are non-negative
        if (( a < 0 || b < 0 )); then
            continue # Need non-negative button presses, skip
        fi

        # If we reach here, a valid non-negative integer solution (a, b) exists.
        # Calculate the cost for this machine
        cost=$(( 3 * a + b ))

        # Add the cost to the total tokens
        total_tokens=$(( total_tokens + cost ))

    # Read from the input file specified as argument or default to input.txt
    done < "${1:-input.txt}"

    # Print the final result to standard output
    echo "$total_tokens"
}

# Ensure the script is run directly and call the main function
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
