#!/usr/bin/awk -f

BEGIN {
    # Read the first row from the input file
    getline row < "input.txt"
    close("input.txt")

    # Initialize the count of safe tiles
    safe_tiles = 0

    # Count the safe tiles in the first row
    for (i = 1; i <= length(row); i++) {
        if (substr(row, i, 1) == ".") {
            safe_tiles++
        }
    }

    # Initialize the previous row
    prev_row = row

    # Process the remaining rows
    for (i = 2; i <= 400000; i++) {
        # Initialize the current row
        curr_row = ""

        # Process each tile in the current row
        for (j = 1; j <= length(prev_row); j++) {
            # Determine the left, center, and right tiles
            left = (j == 1) ? "." : substr(prev_row, j - 1, 1)
            center = substr(prev_row, j, 1)
            right = (j == length(prev_row)) ? "." : substr(prev_row, j + 1, 1)

            # Apply the rules to determine the new tile
            if ((left == "^" && center == "^" && right != "^") ||
                (center == "^" && right == "^" && left != "^") ||
                (left == "^" && center != "^" && right != "^") ||
                (right == "^" && left != "^" && center != "^")) {
                curr_row = curr_row "^"
            } else {
                curr_row = curr_row "."
                safe_tiles++
            }
        }

        # Update the previous row
        prev_row = curr_row
    }

    # Print the total count of safe tiles
    print safe_tiles
}