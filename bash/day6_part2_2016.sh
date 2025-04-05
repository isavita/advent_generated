
#!/bin/bash

main() {
    # Ensure input file exists
    if [[ ! -f "input.txt" ]]; then
        # Print empty lines for non-existent file to match potential edge case behavior
        echo ""
        echo ""
        exit 1 # Indicate error, though output matches empty file
    fi

    # Read the first line to determine message length
    read -r first_line < "input.txt"
    local message_len=${#first_line}

    # Handle empty input file or empty first line
    if [[ "$message_len" -eq 0 ]]; then
        echo ""
        echo ""
        exit 0
    fi

    local message1=""
    local message2=""
    local i
    local column_chars
    local counts_sorted_desc
    local counts_sorted_asc
    local most_common_char
    local least_common_char

    # Process each character position (column)
    for (( i=0; i<message_len; i++ )); do
        # Extract the (i+1)-th character from each line (cut is 1-based)
        # Output is one character per line already
        column_chars=$(cut -c $((i+1)) "input.txt")

        # Count character occurrences and sort
        # Sort 1: by count descending (-k1,1nr), then character ascending (-k2,2) for most common
        counts_sorted_desc=$(echo "$column_chars" | sort | uniq -c | sort -k1,1nr -k2,2)
        # Sort 2: by count ascending (-k1,1n), then character ascending (-k2,2) for least common
        counts_sorted_asc=$(echo "$column_chars" | sort | uniq -c | sort -k1,1n -k2,2)

        # Get the character from the first line of each sorted list
        # awk '{print $2}' extracts the second field (the character)
        most_common_char=$(echo "$counts_sorted_desc" | head -n 1 | awk '{print $2}')
        least_common_char=$(echo "$counts_sorted_asc" | head -n 1 | awk '{print $2}')

        # Append characters to the messages
        message1="${message1}${most_common_char}"
        message2="${message2}${least_common_char}"
    done

    # Print the resulting messages
    echo "$message1"
    echo "$message2"
}

# Execute the main function
main
