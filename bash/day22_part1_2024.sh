
#!/bin/bash

# --- Day 22: Monkey Market ---
# Calculates the sum of the 2000th generated secret number for each buyer.

# --- Configuration ---
readonly INPUT_FILE="input.txt"
readonly ITERATIONS=2000
readonly MOD=16777216 # 2^24

# --- Function Definitions ---

# Simulates one step of generating the next secret number.
# Takes the current secret number as an argument.
# Prints the next secret number.
# Uses arithmetic expansion for efficiency.
next_secret() {
    local secret=$1

    # Step 1: Multiply by 64, mix, prune
    secret=$(( (secret ^ (secret * 64)) % MOD ))

    # Step 2: Divide by 32, mix, prune
    secret=$(( (secret ^ (secret / 32)) % MOD ))

    # Step 3: Multiply by 2048, mix, prune
    secret=$(( (secret ^ (secret * 2048)) % MOD ))

    echo "$secret"
}

# Main logic of the script
main() {
    # Check if input file exists
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi

    local total_sum=0
    local initial_secret
    local secret
    local i

    # Read each initial secret number from the input file
    while IFS= read -r initial_secret || [[ -n "$initial_secret" ]]; do
        # Ensure the line read is a valid number (optional, but good practice)
        if ! [[ "$initial_secret" =~ ^[0-9]+$ ]]; then
             echo "Warning: Skipping invalid line in $INPUT_FILE: '$initial_secret'" >&2
             continue
        fi

        secret="$initial_secret"

        # Generate ITERATIONS new secret numbers
        for ((i = 0; i < ITERATIONS; i++)); do
            # Using process substitution or command substitution here adds overhead.
            # Direct arithmetic expansion within the loop is faster.

            # Step 1: Multiply by 64, mix, prune
            secret=$(( (secret ^ (secret * 64)) % MOD ))
            # Step 2: Divide by 32, mix, prune
            secret=$(( (secret ^ (secret / 32)) % MOD ))
            # Step 3: Multiply by 2048, mix, prune
            secret=$(( (secret ^ (secret * 2048)) % MOD ))
        done

        # Add the 2000th secret number to the total sum
        total_sum=$(( total_sum + secret ))

    done < "$INPUT_FILE"

    # Print the final sum
    echo "$total_sum"
}

# --- Script Execution ---

# Call the main function, passing any command-line arguments (though none are used here)
main "$@"
