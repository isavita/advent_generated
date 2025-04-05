
#!/bin/bash

# Main function to process the input file
main() {
    # Check if input file exists
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    # Use jq to recursively find all numbers and sum them.
    # '..' recursively descends into the JSON structure.
    # 'numbers' filters out only the numeric values.
    # The stream of numbers is piped to awk for summation.
    # This avoids creating a potentially large intermediate array in jq like '[..|numbers]|add' would.
    jq '.. | numbers' input.txt | awk '{s+=$1} END {print s+0}'

    # Alternative using jq's internal 'add' after collecting numbers into an array.
    # This might be less memory efficient for huge inputs than the awk approach.
    # jq '[.. | numbers] | add' input.txt
}

# Call the main function
main
