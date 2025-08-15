#!/usrusr/awk -f
# Advent of Code 2017 – Day 22 (Sporifica Virus)
# Reads the puzzle input from a file named “input.txt” in the current directory.
# Prints the number of bursts that cause a node to become infected for
#   • Part 1 (10000 bursts, simple infection model)
#   • Part 2 (10 000 000 bursts, evolved infection model)

# ----------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------
function key(x, y) {
    # Returns a string that can be used as an associative‑array index.
    return x SUBSEP y
}

# ----------------------------------------------------------------------
# Main entry point
# ----------------------------------------------------------------------
BEGIN {
    # ------------------------------------------------------------------
    # 1. Load the map from input.txt
    # ------------------------------------------------------------------
    filename = "input.txt"
    while ((getline line < filename) > 0) {
        ++rows
        map[rows] = line
        if (length(line) > cols) cols = length(line)
    }
    close(filename)

    # ------------------------------------------------------------------
    # 2. Initialise the grid for both parts.
    #    The grid is stored as an associative array:
    #      0 – clean (no entry)
    #      1 – infected (part 1)
    #      2 – weakened (part 2)
    #      3 – infected (part 2)
    #      4 – flagged (part 2)
    # ------------------------------------------------------------------
    for (y = 1; y <= rows; ++y) {
        line = map[y]
        for (x = 1; x <= cols; ++x) {
            c = substr(line, x, 1)
            if (c == "#") {
                # Part 1: infected
                grid1[key(x-1, y-1)] = 1
                # Part 2: infected (state 2)
                grid2[key(x-1, y-1)] = 2
            }
        }
    }

    # Starting position – centre of the input grid (0‑based coordinates)
    startX = int(cols / 2)
    startY = int(rows / 2)

    # ------------------------------------------------------------------
    # 3. Part 1 – 10 000 bursts, simple infection model
    # ------------------------------------------------------------------
    x = startX; y = startY; dir = 0   # 0=up,1=right,2=down,3=left
    infections1 = 0

    for (i = 1; i <= 10000; ++i) {
        k = key(x, y)
        if (k in grid1) {
            # Infected – turn right
            dir = (dir + 1) % 4
            # Clean the node
            delete grid1[k]
        } else {
            # Clean – turn left
            dir = (dir + 3) % 4
            # Infect the node
            grid1[k] = 1
            ++infections1
        }

        # Move forward
        if (dir == 0)      y--
        else if (dir == 1)  x++
        else if (dir == 2)  y++
        else                x--
    }

    # ------------------------------------------------------------------
    # 4. Part 2 – 10 000 000 bursts, evolved infection model
    # ------------------------------------------------------------------
    x = startX; y = startY; dir = 0   # reset position and direction
    infections2 = 0

    for (i = 1; i <= 10000000; ++i) {
        k = key(x, y)
        state = (k in grid2) ? grid2[k] : 0   # default = clean

        # Decide turn based on current state
        if (state == 0) {          # clean
            dir = (dir + 3) % 4    # turn left
        } else if (state == 1) {    # weakened
            # no turn
        } else if (state == 2) {    # infected
            dir = (dir + 1) % 4    # turn right
        } else {                    # flagged
            dir = (dir + 2) % 4    # reverse
        }

        # Update node state
        if (state == 0) {            # clean -> weakened
            grid2[k] = 1
        } else if (state == 1) {      # weakened -> infected
            grid2[k] = 2
            ++infections2
        } else if (state == 2) {      # infected -> flagged
            grid2[k] = 3
        } else {                      # flagged -> clean
            delete grid2[k]
        }

        # Move forward
        if (dir == 0)      y--
        else if (dir == 1)  x++
        else if (dir == 2)  y++
        else                x--
    }

    # ------------------------------------------------------------------
    # 5. Output results
    # ------------------------------------------------------------------
    print "Part 1:", infections1
    print "Part 2:", infections2
}
