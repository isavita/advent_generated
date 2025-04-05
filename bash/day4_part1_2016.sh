
#!/bin/bash

# Calculates the checksum for a given name string
# Input: $1 = name string (without hyphens)
# Output: Echoes the calculated 5-character checksum
calculate_checksum() {
    local name_no_hyphens="$1"
    # fold: put each char on a new line
    # sort: sort alphabetically (needed for tie-breaking)
    # uniq -c: count occurrences, output format: "  count char"
    # sort -k1,1nr -k2,2: sort numerically reverse by count (field 1), then alphabetically by char (field 2)
    # head -n 5: take the top 5 most frequent characters
    # awk '{printf "%s", $2}': print only the character (field 2) without newline
    echo "$name_no_hyphens" | fold -w1 | sort | uniq -c | sort -k1,1nr -k2,2 | head -n 5 | awk '{printf "%s", $2}'
}

main() {
    local total_sum=0
    local line name name_no_hyphens sector_id checksum calculated_checksum base

    # Read input file line by line
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Extract checksum: remove everything up to last '[' and the final ']'
        checksum="${line##*[}"
        checksum="${checksum%]}"

        # Extract base part (name and sector id): remove checksum part
        base="${line%[*}"

        # Extract sector id: remove everything up to the last '-'
        sector_id="${base##*-}"

        # Extract name: remove the sector id part
        name="${base%-*}"

        # Remove hyphens from name using bash parameter expansion (faster than tr)
        name_no_hyphens="${name//-/}"

        # Calculate the expected checksum based on the name
        calculated_checksum=$(calculate_checksum "$name_no_hyphens")

        # If calculated checksum matches the provided checksum, add sector id to sum
        if [[ "$calculated_checksum" == "$checksum" ]]; then
            total_sum=$((total_sum + sector_id))
        fi
    done < "input.txt"

    # Print the final sum
    echo "$total_sum"
}

# Execute the main function
main
