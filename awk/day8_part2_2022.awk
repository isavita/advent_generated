
#!/usr/bin/awk -f
# Advent of Code – Day 8 (Treetop Tree House)
# Reads the grid from a file named “input.txt” and prints:
#   line 1 – number of trees visible from outside the grid (part 1)
#   line 2 – highest scenic score of any tree (part 2)

# ----------------------------------------------------------------------
# If the script is run without an explicit filename, force awk to read
# "input.txt".  This satisfies the requirement “reads input from a file
# called input.txt”.
BEGIN {
    if (ARGC == 1) {
        ARGV[1] = "input.txt"
        ARGC++
    }
}
# ----------------------------------------------------------------------
# Read each line, store every digit in a two‑dimensional array `g`.
{
    rows = NR                     # total number of rows
    cols = length($0)             # assume all lines have equal length
    for (c = 1; c <= cols; c++) {
        g[NR, c] = substr($0, c, 1) + 0   # store as a number
    }
}
# ----------------------------------------------------------------------
END {
    visible = 0
    maxScore = 0

    for (r = 1; r <= rows; r++) {
        for (c = 1; c <= cols; c++) {
            h = g[r, c]

            # ---------- part 1 : visibility ----------
            if (r == 1 || r == rows || c == 1 || c == cols) {
                visible++                         # edge trees are always visible
            } else {
                vis = 0

                # up
                blocked = 0
                for (k = r - 1; k >= 1; k--) {
                    if (g[k, c] >= h) { blocked = 1; break }
                }
                if (!blocked) vis = 1

                # down
                if (!vis) {
                    blocked = 0
                    for (k = r + 1; k <= rows; k++) {
                        if (g[k, c] >= h) { blocked = 1; break }
                    }
                    if (!blocked) vis = 1
                }

                # left
                if (!vis) {
                    blocked = 0
                    for (k = c - 1; k >= 1; k--) {
                        if (g[r, k] >= h) { blocked = 1; break }
                    }
                    if (!blocked) vis = 1
                }

                # right
                if (!vis) {
                    blocked = 0
                    for (k = c + 1; k <= cols; k++) {
                        if (g[r, k] >= h) { blocked = 1; break }
                    }
                    if (!blocked) vis = 1
                }

                if (vis) visible++
            }

            # ---------- part 2 : scenic score ----------
            # viewing distance upwards
            up = 0
            for (k = r - 1; k >= 1; k--) {
                up++
                if (g[k, c] >= h) break
            }

            # downwards
            down = 0
            for (k = r + 1; k <= rows; k++) {
                down++
                if (g[k, c] >= h) break
            }

            # leftwards
            left = 0
            for (k = c - 1; k >= 1; k--) {
                left++
                if (g[r, k] >= h) break
            }

            # rightwards
            right = 0
            for (k = c + 1; k <= cols; k++) {
                right++
                if (g[r, k] >= h) break
            }

            score = up * down * left * right
            if (score > maxScore) maxScore = score
        }
    }

    printf "%d\n%d\n", visible, maxScore
}
