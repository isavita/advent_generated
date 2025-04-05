
#!/bin/bash

# Simple implementation without optimization (reads file multiple times)

main() {
    local input_file="input.txt"
    local message=""
    local first_line
    local message_length
    local i
    local col_index
    local most_common_char

    if [[ ! -f "$input_file" ]]; then
        echo "Error: $input_file not found." >&2
        exit 1
    fi

    # Read the first line to determine the message length
    read -r first_line < "$input_file"
    # Handle empty file case
    if [[ -z "$first_line" ]]; then
         echo ""
         exit 0
    fi
    message_length=${#first_line}


    for (( i=0; i<message_length; i++ )); do
        # Bash indices are 0-based, cut indices are 1-based
        col_index=$((i + 1))

        # Extract the column, sort characters, count occurrences,
        # sort by count (descending), take the top one, extract the character.
        most_common_char=$(cut -c "$col_index" < "$input_file" | sort | uniq -c | sort -nr | head -n 1 | awk '{print $2}')

        # Append the most common character for this column
        message+="${most_common_char}"
    done

    echo "$message"
}

# Call the main function
main
