function flow(x, y, lb, rb, i, lx, rx) {
    if (y > max_y) return 1
    if (g[x,y] == "#" || g[x,y] == "~") return 0
    if (g[x,y] == "|") return 1
    g[x,y] = "|"
    if (flow(x, y + 1)) return 1
    lb = 0; lx = x
    while (1) {
        if (g[lx - 1,y] == "#") { lb = 1; break }
        lx--; g[lx,y] = "|"
        if (flow(lx, y + 1)) break
    }
    rb = 0; rx = x
    while (1) {
        if (g[rx + 1,y] == "#") { rb = 1; break }
        rx++; g[rx,y] = "|"
        if (flow(rx, y + 1)) break
    }
    if (lb && rb) {
        for (i = lx; i <= rx; i++) g[i,y] = "~"
        return 0
    }
    return 1
}
BEGIN {
    FS = "[=, .]+"
    min_y = 1e9; max_y = 0
    while ((getline < "input.txt") > 0) {
        if ($1 == "x") {
            x = $2; y1 = $4; y2 = $5
            if (y1 < min_y) min_y = y1
            if (y2 > max_y) max_y = y2
            for (y = y1; y <= y2; y++) g[x,y] = "#"
        } else if ($1 == "y") {
            y = $2; x1 = $4; x2 = $5
            if (y < min_y) min_y = y
            if (y > max_y) max_y = y
            for (x = x1; x <= x2; x++) g[x,y] = "#"
        }
    }
    flow(500, 0)
    for (k in g) {
        if (g[k] == "~") {
            split(k, c, SUBSEP)
            if (c[2] >= min_y && c[2] <= max_y) a++
        }
    }
    print a + 0
}