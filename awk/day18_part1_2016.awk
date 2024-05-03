#!/usr/bin/awk

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

    # Generate the remaining 39 rows
    for (i = 1; i <= 39; i++) {
        new_row = ""
        for (j = 1; j <= length(row); j++) {
            left = (j == 1) ? "." : substr(row, j - 1, 1)
            center = substr(row, j, 1)
            right = (j == length(row)) ? "." : substr(row, j + 1, 1)

            if ((left == "^" && center == "^" && right != "^") ||
                (center == "^" && right == "^" && left != "^") ||
                (left == "^" && center != "^" && right != "^") ||
                (right == "^" && left != "^" && center != "^")) {
                new_row = new_row "^"
            } else {
                new_row = new_row "."
                safe_tiles++
            }
        }
        row = new_row
    }

    # Print the total count of safe tiles
    print safe_tiles
}