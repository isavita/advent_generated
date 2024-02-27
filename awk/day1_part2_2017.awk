
# Read the input from the file "input.txt"
BEGIN {
    while ((getline line < "input.txt") > 0) {
        for (i = 1; i <= length(line); i++) {
            # Part 1: Check if the current digit matches the next one
            if (substr(line, i, 1) == substr(line, (i + 1) % length(line), 1)) {
                sum1 += substr(line, i, 1)
            }
            # Part 2: Check if the current digit matches the one halfway around the circular list
            if (substr(line, i, 1) == substr(line, (i + length(line) / 2) % length(line), 1)) {
                sum2 += substr(line, i, 1)
            }
        }
    }
    # Print the results
    print "Part 1: ", sum1
    print "Part 2: ", sum2
}
