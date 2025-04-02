
# Function to check if string length is even
function even_digits(s) {
    return length(s) % 2 == 0
}

# Function to trim leading zeros, return "0" if result is empty
function trim_leading_zeros(s,    _tmp) {
    _tmp = s
    sub(/^0+/, "", _tmp)
    return (_tmp == "" ? "0" : _tmp)
}

# AWK's main processing happens in BEGIN, pattern-action blocks, or END.
# We use END block to ensure all processing happens after potential (but unused) line reading.
# We read the file explicitly within the END block.
END {
    # Read the single line from input.txt
    if ((getline line < "input.txt") <= 0) {
        # Handle error: print to stderr and exit if file cannot be read or is empty
        print "Error: Cannot read from input.txt or file is empty." > "/dev/stderr"
        exit 1
    }
    close("input.txt") # Close the file

    # Split the line into the initial stones array
    # split returns the number of elements found
    num_stones = split(line, stones, " ")

    # Main loop: 25 iterations
    for (iter = 1; iter <= 25; iter++) {
        # Temporary array for the next generation of stones
        delete next_stones # Clear array from previous iteration
        next_idx = 0      # Index for next_stones array

        # Iterate over current stones
        for (i = 1; i <= num_stones; i++) {
            s = stones[i]
            if (s == "0") {
                next_idx++
                next_stones[next_idx] = "1"
            } else if (even_digits(s)) {
                mid = int(length(s) / 2)
                # AWK substr is 1-based: substr(string, start, length)
                left = substr(s, 1, mid)
                right = substr(s, mid + 1)

                left_trimmed = trim_leading_zeros(left)
                right_trimmed = trim_leading_zeros(right)

                next_idx++
                next_stones[next_idx] = left_trimmed
                next_idx++
                next_stones[next_idx] = right_trimmed
            } else {
                # AWK automatically handles string-to-number conversion for arithmetic
                # and number-to-string for assignment/printing
                next_idx++
                next_stones[next_idx] = s * 2024
            }
        }

        # Update stones array for the next iteration
        delete stones # Clear the old stones array
        num_stones = next_idx # Get the count of new stones
        for (i = 1; i <= num_stones; i++) {
            stones[i] = next_stones[i] # Copy elements from next_stones to stones
        }
        # No need to delete next_stones here, it's cleared at the start of the loop
    }

    # Print the final count of stones
    print num_stones
}
