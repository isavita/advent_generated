
#!/usr/bin/awk -f
# ------------------------------------------------------------
#  Advent of Code 2017 – Day 24: Electromagnetic Moat
#  Find the strength of the strongest possible bridge.
# ------------------------------------------------------------

# Global variables
#   a[i], b[i]   – ports of component i (1‑based)
#   used[i]     – 1 if component i is already used in the current bridge
#   n           – number of components
#   max_strength – strongest bridge found so far

# ----------------------------------------------------------------
# Recursive depth‑first search.
#   cur   – the type of the free port at the current end of the bridge
#   sum   – total strength of the bridge built so far
# ----------------------------------------------------------------
function dfs(cur, sum,   i, nxt) {
    # Update the best strength seen so far
    if (sum > max_strength) max_strength = sum

    # Try every unused component that can connect to the current port
    for (i = 1; i <= n; ++i) {
        if (used[i]) continue
        if (a[i] == cur || b[i] == cur) {
            used[i] = 1
            nxt = (a[i] == cur) ? b[i] : a[i]   # the new free port
            dfs(nxt, sum + a[i] + b[i])
            used[i] = 0
        }
    }
}

# ----------------------------------------------------------------
# Main entry point
# ----------------------------------------------------------------
BEGIN {
    # ----------------------------------------------------------------
    # Read the component list from the file "input.txt"
    # ----------------------------------------------------------------
    while ((getline line < "input.txt") > 0) {
        split(line, p, "/")
        a[++n] = p[1] + 0
        b[n]   = p[2] + 0
    }
    close("input.txt")

    # Initialise globals
    max_strength = 0
    for (i = 1; i <= n; ++i) used[i] = 0

    # Start building from the zero‑pin port
    dfs(0, 0)

    # Output the answer
    print max_strength
}
