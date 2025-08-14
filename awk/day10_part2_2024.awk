
#!/usr/bin/awk -f
# ------------------------------------------------------------
#  Count all increasing paths 0→1→…→9 in a grid of digits.
#  Input is read from the file "input.txt".
# ------------------------------------------------------------

BEGIN {
    # ----- read the whole grid --------------------------------
    while ((getline line < "input.txt") > 0) {
        rows[++nr] = line
    }
    if (nr == 0) exit

    nc = length(rows[1])               # number of columns

    for (r = 1; r <= nr; ++r) {
        line = rows[r]
        for (c = 1; c <= nc; ++c)
            grid[r, c] = substr(line, c, 1) + 0   # store as number
    }

    # direction vectors: down, up, right, left
    dr[1] =  1; dc[1] =  0
    dr[2] = -1; dc[2] =  0
    dr[3] =  0; dc[3] =  1
    dr[4] =  0; dc[4] = -1
}

# ------------------------------------------------------------
#  Depth‑first search with memoisation.
# ------------------------------------------------------------
function dfs(r, c,   key, h, sum, i, nr2, nc2) {
    key = r SUBSEP c
    if (key in dp) return dp[key]

    h = grid[r, c]
    if (h == 9) { dp[key] = 1; return 1 }

    sum = 0
    for (i = 1; i <= 4; ++i) {
        nr2 = r + dr[i];  nc2 = c + dc[i]
        if (nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc) continue
        if (grid[nr2, nc2] == h + 1) sum += dfs(nr2, nc2)
    }
    dp[key] = sum
    return sum
}

END {
    total = 0
    for (r = 1; r <= nr; ++r)
        for (c = 1; c <= nc; ++c)
            if (grid[r, c] == 0) total += dfs(r, c)

    print total
}
