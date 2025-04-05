
#!/bin/bash

# Define the main function
main() {
    # Use awk to process the input file
    # NR is the record (line) number
    # $1 is the first field (location_id)
    # $2 is the second field (similarity)
    awk '
    {
        # Store the location ID from the first column, keyed by line number
        location_ids[NR] = $1
        # Increment the count for the similarity value (second column) in an associative array
        similarity_counts[$2]++
    }
    END {
        # After processing all lines, calculate the total
        total = 0
        # Iterate through all the line numbers we processed
        for (i = 1; i <= NR; i++) {
            # Get the location_id for this line
            id = location_ids[i]
            # Get the count of how many times this id appeared in the similarity column
            # Add 0 to handle cases where an id never appeared as a similarity (count becomes 0 instead of empty string)
            count = similarity_counts[id] + 0
            # Add the product to the total
            total += id * count
        }
        # Print the final total
        print total
    }
    ' < input.txt # Redirect the input file to awk
}

# Execute the main function
main
