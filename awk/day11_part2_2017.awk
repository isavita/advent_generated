
#!/usr/bin/awk -f
# Hex Ed – Advent of Code Day 11
# Reads the comma‑separated path from a file named "input.txt"
# and prints:
#   1) the fewest steps required to reach the final position
#   2) the maximum distance ever reached during the walk

# ------------------------------------------------------------
# Helper: absolute value
function abs(x) { return (x < 0 ? -x : x) }

# Hex distance using axial coordinates (q,r)
# distance = max(|q|, |r|, |q+r|)
function hexDist(q, r) {
    d1 = abs(q)
    d2 = abs(r)
    d3 = abs(q + r)
    # return the largest of the three
    return (d1 > d2 ? (d1 > d3 ? d1 : d3) : (d2 > d3 ? d2 : d3))
}

# ------------------------------------------------------------
BEGIN {
    # start at origin
    q = 0; r = 0
    maxDist = 0

    # read the whole line from input.txt
    while ((getline line < "input.txt") > 0) {
        # split the line on commas
        n = split(line, steps, ",")
        for (i = 1; i <= n; i++) {
            dir = steps[i]

            # move according to the direction
            if (dir == "n")  { r-- }
            else if (dir == "ne") { q++; r-- }
            else if (dir == "se") { q++ }
            else if (dir == "s")  { r++ }
            else if (dir == "sw") { q--; r++ }
            else if (dir == "nw") { q-- }
            # compute distance after this step
            cur = hexDist(q, r)
            if (cur > maxDist) maxDist = cur
        }
    }

    # final distance from start
    finalDist = hexDist(q, r)

    # output results
    print finalDist
    print maxDist
}
