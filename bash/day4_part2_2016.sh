
#!/bin/bash

# --- Day 4: Security Through Obscurity ---
# Reads room data from input.txt, calculates the sum of sector IDs for real rooms (Part 1),
# and finds the sector ID of the room containing "northpole object storage" (Part 2).

# Input file
INPUT_FILE="input.txt"

# Check if input file exists
if [[ ! -f "$INPUT_FILE" ]]; then
    echo "Error: Input file '$INPUT_FILE' not found." >&2
    exit 1
fi

# --- Function to calculate the checksum for a given name ---
calculate_checksum() {
    local name_no_dashes=$1
    local calculated_checksum

    # Count character frequencies, sort by count (desc) then char (asc), take top 5 chars
    # 1. fold -w1: Put each character on a new line
    # 2. sort: Sort characters alphabetically (groups identical chars together)
    # 3. uniq -c: Count consecutive identical lines (characters)
    # 4. sort -k1,1nr -k2,2: Sort primarily by count (numeric, reverse), secondarily by char (alphabetic)
    # 5. head -n 5: Take the top 5 lines (most frequent chars)
    # 6. awk '{printf "%s", $2}': Print only the character (second field) without a newline
    calculated_checksum=$(echo -n "$name_no_dashes" | fold -w1 | sort | uniq -c | sort -k1,1nr -k2,2 | head -n 5 | awk '{printf "%s", $2}')

    echo "$calculated_checksum"
}

# --- Function to decrypt a name using Caesar shift ---
decrypt_name() {
    local encrypted_name=$1
    local sector_id=$2
    local shift_amount
    local alpha="abcdefghijklmnopqrstuvwxyz"
    local shifted_alpha
    local decrypted_name

    # Calculate effective shift (modulo 26)
    shift_amount=$(( sector_id % 26 ))

    # Create the shifted alphabet
    # Example: shift=1 -> bcdefghijklmnopqrstuvwxyza
    # Example: shift=3 -> defghijklmnopqrstuvwxyzabc
    shifted_alpha="${alpha:${shift_amount}}${alpha:0:${shift_amount}}"

    # Translate characters: letters using the shifted alphabet, dashes to spaces
    # Using -- ensures that $alpha- is treated as the FROM set, even if it starts with '-'
    decrypted_name=$(echo "$encrypted_name" | tr -- "${alpha}-" "${shifted_alpha} ")

    echo "$decrypted_name"
}

# --- Main processing logic ---
main() {
    local total_sector_id_sum=0
    local north_pole_room_sector_id=0
    local line
    local encrypted_name_part
    local sector_id
    local provided_checksum
    local name_no_dashes
    local calculated_checksum
    local decrypted_name

    # Read the input file line by line
    # Using process substitution and regex matching for efficient parsing
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Use Bash regex to parse the line: ([a-z-]+)-([0-9]+)\[([a-z]{5})\]
        if [[ "$line" =~ ^([a-z-]+)-([0-9]+)\[([a-z]{5})\]$ ]]; then
            encrypted_name_part="${BASH_REMATCH[1]}"
            sector_id="${BASH_REMATCH[2]}"
            provided_checksum="${BASH_REMATCH[3]}"

            # --- Part 1 Logic ---
            # Remove dashes from the name part for frequency calculation
            name_no_dashes=$(tr -d '-' <<< "$encrypted_name_part") # Use here-string

            # Calculate the expected checksum based on frequency
            calculated_checksum=$(calculate_checksum "$name_no_dashes")

            # Check if the room is real
            if [[ "$calculated_checksum" == "$provided_checksum" ]]; then
                # Add sector ID to the sum for Part 1
                (( total_sector_id_sum += sector_id ))

                # --- Part 2 Logic ---
                # Only decrypt real rooms
                # Check if we haven't found the North Pole room yet (optimization)
                if [[ $north_pole_room_sector_id -eq 0 ]]; then
                    decrypted_name=$(decrypt_name "$encrypted_name_part" "$sector_id")
                    # Check if the decrypted name matches the target
                    if [[ "$decrypted_name" == "northpole object storage" ]]; then
                        north_pole_room_sector_id=$sector_id
                        # Optional: If you are *sure* there's only one such room and
                        # you only need the final Part 1 sum, you could potentially
                        # add a check here to break early if both parts are solved.
                        # However, we need the full Part 1 sum, so we continue processing.
                    fi
                fi
            fi
        else
             echo "Warning: Skipping malformed line: $line" >&2
        fi
    done < "$INPUT_FILE"

    # --- Output Results ---
    echo "Part 1: $total_sector_id_sum"
    if [[ $north_pole_room_sector_id -ne 0 ]]; then
        echo "Part 2: $north_pole_room_sector_id"
    else
        echo "Part 2: North Pole object storage room not found." >&2
    fi
}

# --- Script Entry Point ---
main
