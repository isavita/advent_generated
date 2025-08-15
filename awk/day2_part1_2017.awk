
#!/usr/bin/awk -f

# ------------------------------------------------------------
#  Day 2 – Corruption Checksum
#  Reads the spreadsheet from a file named "input.txt" and
#  prints the checksum (sum of max‑min for each row) to
#  standard output.
# ------------------------------------------------------------

# Main entry point – executed before any input is read.
BEGIN {
    checksum = 0

    # Open the required file.
    while ((getline line < "input.txt") > 0) {
        # Split the line into fields (space or tab separated).
        n = split(line, fields, /[ \t]+/)

        # Initialise min and max with the first value.
        min = max = fields[1]

        # Scan the rest of the fields to find min and max.
        for (i = 2; i <= n; i++) {
            if (fields[i] > max) max = fields[i]
            if (fields[i] < min) min = fields[i]
        }

        # Add the row's difference to the running total.
        checksum += max - min
    }

    # Output the final checksum.
    print checksum

    # Explicitly exit to avoid processing any further input.
    exit
}
