
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
/^#/ {
    r++
    w = split($0, a, "")
    for (i = 1; i <= w; i++) {
        g[r, i] = a[i]
        if (a[i] == "@") { rr = r; rc = i }
    }
}
!/^#/ && NF {
    n = split($0, a, "")
    for (i = 1; i <= n; i++) {
        dr = (a[i] == "^" ? -1 : a[i] == "v" ? 1 : 0)
        dc = (a[i] == "<" ? -1 : a[i] == ">" ? 1 : 0)
        if (!dr && !dc) continue
        tr = rr + dr; tc = rc + dc
        while (g[tr, tc] == "O") { tr += dr; tc += dc }
        if (g[tr, tc] == ".") {
            if (g[rr + dr, rc + dc] == "O") g[tr, tc] = "O"
            g[rr, rc] = "."
            rr += dr; rc += dc
            g[rr, rc] = "@"
        }
    }
}
END {
    for (i = 1; i <= r; i++) {
        for (j = 1; j <= w; j++) {
            if (g[i, j] == "O") s += (i - 1) * 100 + (j - 1)
        }
    }
    print s
}

