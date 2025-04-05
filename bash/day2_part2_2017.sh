
#!/bin/bash

# Solution using awk for efficiency

main() {
    awk '
    {
        # Part 1: Calculate min/max difference for the current line
        min = $1
        max = $1
        for (i = 2; i <= NF; i++) {
            if ($i < min) min = $i
            if ($i > max) max = $i
        }
        # Add difference to checksum1
        checksum1 += (max - min)

        # Part 2: Find the evenly divisible pair for the current line
        found_pair = 0 # Flag to process only the first found pair per line
        for (i = 1; i <= NF && !found_pair; i++) {
            for (j = i + 1; j <= NF && !found_pair; j++) {
                 # Check if $i is divisible by $j
                 if ($j != 0 && $i % $j == 0) {
                     checksum2 += int($i / $j)
                     found_pair = 1 # Mark pair as found for this line
                 }
                 # Check if $j is divisible by $i
                 else if ($i != 0 && $j % $i == 0) {
                     checksum2 += int($j / $i)
                     found_pair = 1 # Mark pair as found for this line
                 }
            }
        }
    }
    END {
        # Print the final results after processing all lines
        print checksum1
        print checksum2
    }
    ' input.txt # Read input directly from the file
}

# Execute the main function
main
