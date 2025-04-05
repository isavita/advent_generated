
#!/usr/bin/env bash

# --- Day 5: Hydrothermal Venture ---
#
# Reads vent line coordinates from input.txt (format: x1,y1 -> x2,y2)
# Calculates the number of points where at least two lines overlap.
# Part 1 considers only horizontal and vertical lines.
# Part 2 considers horizontal, vertical, and 45-degree diagonal lines.

# --- Configuration ---
INPUT_FILE="input.txt"

# --- Helper Functions ---

# Checks if a string is a valid non-negative integer
is_int() {
    [[ "$1" =~ ^[0-9]+$ ]]
}

# --- Main Logic ---
main() {
    # Check if input file exists and is readable
    if [[ ! -r "${INPUT_FILE}" ]]; then
        echo "Error: Input file '${INPUT_FILE}' not found or not readable." >&2
        exit 1
    fi

    # Use awk for efficient processing.
    # -F ' -> |,' sets the field separator to " -> " or ",".
    # This means for "x1,y1 -> x2,y2":
    # $1 = x1, $2 = y1, $3 = x2, $4 = y2
    #
    # The awk script:
    # 1. Initializes an associative array `grid` to store point counts.
    # 2. Reads each line, parsing coordinates.
    # 3. Skips lines with invalid coordinates.
    # 4. Determines line type (horizontal, vertical, diagonal).
    # 5. Iterates through points on the line segment.
    # 6. Increments the count for each point in the `grid` array.
    # 7. In the END block, iterates through the `grid` and counts points with value >= 2.
    # 8. Prints the final count.
    awk '
    function abs(v) {
        return (v < 0 ? -v : v)
    }

    function sgn(v) {
        if (v < 0) return -1;
        if (v > 0) return 1;
        return 0;
    }

    BEGIN {
        FS = " -> |," # Set field separators
        overlap_count_part1 = 0
        overlap_count_part2 = 0
    }

    # Validate input format - ensure 4 numeric fields
    NF != 4 {
        # print "Skipping invalid line (wrong field count): " $0 > "/dev/stderr"
        next
    }
    !($1 ~ /^[0-9]+$/ && $2 ~ /^[0-9]+$/ && $3 ~ /^[0-9]+$/ && $4 ~ /^[0-9]+$/) {
        # print "Skipping invalid line (non-numeric coords): " $0 > "/dev/stderr"
        next
    }

    {
        x1 = $1; y1 = $2; x2 = $3; y2 = $4

        # Determine step direction for iteration
        dx = sgn(x2 - x1)
        dy = sgn(y2 - y1)

        # Check line type
        is_horizontal = (y1 == y2)
        is_vertical = (x1 == x2)
        # Check for exactly 45-degree diagonal
        is_diagonal = (abs(x1 - x2) == abs(y1 - y2))

        # Only process horizontal/vertical for Part 1
        if (is_horizontal || is_vertical) {
            x = x1
            y = y1
            while (1) {
                point = x "," y
                grid_part1[point]++
                grid_part2[point]++ # Part 2 also counts these

                if (x == x2 && y == y2) break # Reached end
                x += dx
                y += dy
            }
        }
        # Process diagonals only for Part 2
        else if (is_diagonal) {
             x = x1
             y = y1
             while (1) {
                point = x "," y
                grid_part2[point]++ # Only count diagonals for Part 2

                if (x == x2 && y == y2) break # Reached end
                x += dx
                y += dy
            }
        }
        # else { # Skip non-hv or non-45-diag lines
            # print "Skipping non-HV/Diagonal line: " $0 > "/dev/stderr"
        # }
    }

    END {
        # Calculate Part 1 overlaps
        for (point in grid_part1) {
            if (grid_part1[point] >= 2) {
                overlap_count_part1++
            }
        }
        print "Part 1 Overlap Count: " overlap_count_part1

        # Calculate Part 2 overlaps
        for (point in grid_part2) {
            if (grid_part2[point] >= 2) {
                overlap_count_part2++
            }
        }
        print "Part 2 Overlap Count: " overlap_count_part2
    }
    ' "${INPUT_FILE}"
}

# --- Script Execution ---
main "$@"

# Example input.txt content:
# 0,9 -> 5,9
# 8,0 -> 0,8
# 9,4 -> 3,4
# 2,2 -> 2,1
# 7,0 -> 7,4
# 6,4 -> 2,0
# 0,9 -> 2,9
# 3,4 -> 1,4
# 0,0 -> 8,8
# 5,5 -> 8,2

# Expected output for example:
# Part 1 Overlap Count: 5
# Part 2 Overlap Count: 12
