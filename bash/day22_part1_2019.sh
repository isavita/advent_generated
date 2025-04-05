
#!/bin/bash

# --- Day 22: Slam Shuffle ---
# Part 1: Find the final position of card 2019 after shuffling a deck of 10007 cards.

# Optimization: Instead of simulating the entire deck, we track the position
# of the target card (2019) through the shuffles.

# Constants
readonly DECK_SIZE=10007
readonly TARGET_CARD=2019
readonly INPUT_FILE="input.txt"

# --- Function Definitions ---

# Applies the 'deal into new stack' operation to the position.
# pos' = deck_size - 1 - pos
deal_new_stack() {
    local pos=$1
    local size=$2
    echo $(( (size - 1 - pos) % size ))
}

# Applies the 'cut N' operation to the position.
# pos' = (pos - N) mod size
# Handles negative N correctly.
cut_cards() {
    local pos=$1
    local n=$2
    local size=$3
    # Add size before modulo to handle potential negative results correctly in bash
    echo $(( (pos - n + size) % size ))
}

# Applies the 'deal with increment N' operation to the position.
# pos' = (pos * N) mod size
deal_increment() {
    local pos=$1
    local n=$2
    local size=$3
    # Bash arithmetic doesn't handle large numbers needed for Part 2,
    # but is sufficient for Part 1 deck size and operations.
    echo $(( (pos * n) % size ))
}

# Main function to process the shuffle instructions
main() {
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi

    local current_pos=$TARGET_CARD
    local value # Variable to hold the numeric argument for cut/deal increment

    # Read instructions line by line
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip empty lines
        [[ -z "$line" ]] && continue

        if [[ "$line" == "deal into new stack" ]]; then
            current_pos=$(deal_new_stack "$current_pos" "$DECK_SIZE")
        elif [[ "$line" == cut* ]]; then
            # Extract the number after "cut "
            value=${line##* }
             # Ensure value is treated numerically, handles potential leading '+'
            value=$((value))
            current_pos=$(cut_cards "$current_pos" "$value" "$DECK_SIZE")
        elif [[ "$line" == deal*with*increment* ]]; then
            # Extract the number after "deal with increment "
            value=${line##* }
            value=$((value))
            current_pos=$(deal_increment "$current_pos" "$value" "$DECK_SIZE")
        else
             echo "Warning: Unknown instruction '$line'" >&2
        fi
         # Optional: uncomment to trace position changes
         # echo "'$line' -> pos = $current_pos" >&2
    done < "$INPUT_FILE"

    # Print the final position of the target card
    echo "$current_pos"
}

# --- Program Execution ---

# Call the main function
main
