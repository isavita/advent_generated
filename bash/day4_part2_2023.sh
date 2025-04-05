
#!/bin/bash

set -euo pipefail

# Function to calculate matches for a single card
# Input: winning_numbers_string given_numbers_string
# Output: number_of_matches
calculate_matches() {
    local wn_str=" $1 " # Pad with spaces for whole word matching
    local gn_str="$2"
    local matches=0
    local num
    for num in $gn_str; do
        if grep -q -w "$num" <<< "$wn_str"; then
        # Alternative using bash pattern matching (often faster)
        # if [[ "$wn_str" == *" $num "* ]]; then
            ((matches++))
        fi
    done
    echo "$matches"
}

main() {
    local input_file="input.txt"
    declare -a winning_numbers
    declare -a given_numbers
    declare -a card_counts

    local card_idx=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue

        local line_data="${line#*: }"
        local winning_part="${line_data% | *}"
        local given_part="${line_data#* | }"

        winning_numbers[card_idx]="$winning_part"
        given_numbers[card_idx]="$given_part"
        card_counts[card_idx]=1 # Initialize count for the original card

        ((card_idx++))
    done < "$input_file"

    local num_cards=$card_idx

    for (( i=0; i<num_cards; i++ )); do
        local matches
        matches=$(calculate_matches "${winning_numbers[i]}" "${given_numbers[i]}")

        if (( matches > 0 )); then
            local current_card_count=${card_counts[i]}
            for (( j=1; j<=matches; j++ )); do
                local next_card_idx=$((i + j))
                if (( next_card_idx < num_cards )); then
                    ((card_counts[next_card_idx] += current_card_count))
                fi
            done
        fi
    done

    local total_cards=0
    for count in "${card_counts[@]}"; do
        ((total_cards += count))
    done

    echo "$total_cards"
}

main
