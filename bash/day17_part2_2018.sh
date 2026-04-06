
awk -F'[=, .]+' '
function flow(x, y, lb, rb, lx, rx, i) {
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
        for (i = lx; i <= rx; i++) {
            if (g[i,y] != "~") {
                g[i,y] = "~"
                if (y >= min_y) ans++
            }
        }
        return 0
    }
    return 1
}
BEGIN { min_y = 1e9; max_y = 0 }
{
    if ($1 == "x") {
        x = $2; for (y = $4; y <= $5; y++) {
            g[x,y] = "#"; if (y < min_y) min_y = y; if (y > max_y) max_y = y
        }
    } else {
        y = $2; if (y < min_y) min_y = y; if (y > max_y) max_y = y
        for (x = $4; x <= $5; x++) g[x,y] = "#"
    }
}
END {
    flow(500, 0)
    print ans + 0
}' input.txt
