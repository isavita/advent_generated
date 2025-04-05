
#!/bin/bash

# Reads input from input.txt, extracts row and column, calculates the code, and prints it.
# Optimized for bash built-ins where possible.

main() {
    local input_file="input.txt"
    local data
    local regex="row ([0-9]+), column ([0-9]+)"
    local row col
    local pos
    local code=20151125
    local multiplier=252533
    local modulus=33554393
    local i

    # Check if input file exists
    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        exit 1
    fi

    # Read the entire file content
    data=$(<"$input_file")

    # Extract row and column using bash regex matching
    if [[ "$data" =~ $regex ]]; then
        row=${BASH_REMATCH[1]}
        col=${BASH_REMATCH[2]}
    else
        echo "Error: Could not find row and column numbers in '$input_file'." >&2
        exit 1
    fi

    # Calculate the position using bash arithmetic expansion
    pos=$(( (row + col - 2) * (row + col - 1) / 2 + col ))

    # Calculate the code using a C-style for loop and arithmetic expansion
    # Bash performs integer arithmetic, including modulo.
    # Bash uses 64-bit signed integers for arithmetic, which is sufficient here.
    for (( i=1; i<pos; i++ )); do
        code=$(( (code * multiplier) % modulus ))
    done

    # Print the final code
    echo "$code"
}

# Call the main function
main
