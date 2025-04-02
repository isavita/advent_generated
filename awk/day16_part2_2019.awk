
#!/usr/bin/awk -f

# Main entry point simulated using the BEGIN block
BEGIN {
    # Read the entire first line from input.txt
    if ((getline input < "input.txt") <= 0) {
        print "Error reading input.txt or file is empty" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Extract offset (first 7 digits)
    offset = substr(input, 1, 7) + 0 # +0 forces numeric conversion

    n = length(input)
    total_len = n * 10000
    relevant_len = total_len - offset # We only care about digits from offset onwards

    # Optimization: We only need to store and process digits from the offset
    # to the end of the conceptual repeated list.
    # Initialize the array 'digits' with the relevant segment.
    # AWK arrays are 1-indexed. digits[1] corresponds to index 'offset'.
    for (i = 1; i <= relevant_len; ++i) {
        # Calculate the corresponding index in the original input string (0-based)
        original_input_idx_0based = (offset + i - 1) % n
        # Get the digit using 1-based substr
        digits[i] = substr(input, original_input_idx_0based + 1, 1)
    }

    # Perform 100 phases
    for (p = 0; p < 100; ++p) {
        total = 0
        # Iterate backwards through the relevant digits for cumulative sum % 10
        # This matches the optimized logic derived from the Python code's
        # specific calculation for large offsets.
        for (i = relevant_len; i >= 1; --i) {
            total = (total + digits[i]) % 10
            digits[i] = total # Update in place
        }
    }

    # Construct the result string from the first 8 digits after the offset
    result = ""
    limit = (8 < relevant_len) ? 8 : relevant_len # Handle cases where relevant_len < 8
    for (i = 1; i <= limit; ++i) {
        result = result digits[i]
    }

    # Print the final 8-digit result
    print result

    # Exit cleanly after BEGIN block processing is complete
    exit
}

# No main processing block needed as all logic is in BEGIN
