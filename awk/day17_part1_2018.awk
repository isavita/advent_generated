
#!/usr/bin/awk -f
BEGIN {
    FS = "[=, .]+"
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    if ($1 == "x") {
        x = $2; y1 = $4; y2 = $5
        for (y = y1; y <= y2; y++) clay[x, y] = 1
    } else {
        y = $2; x1 = $4; x2 = $5
        for (x = x1; x <= x2; x++) clay[x, y] = 1
        y1 = y2 = y
    }
    if (min_y == "" || y1 < min_y) min_y = y1
    if (y2 > max_y) max_y = y2
}
function f(x, y,   l, r, i) {
    if (y > max_y || clay[x, y] || s[x, y]) return
    if (v[x, y] && !(clay[x, y+1] || s[x, y+1])) return
    v[x, y] = 1
    if (!clay[x, y+1] && !s[x, y+1]) f(x, y+1)
    if (clay[x, y+1] || s[x, y+1]) {
        l = x; while (!clay[l-1, y] && (clay[l-1, y+1] || s[l-1, y+1])) { l--; v[l, y] = 1 }
        r = x; while (!clay[r+1, y] && (clay[r+1, y+1] || s[r+1, y+1])) { r++; v[r, y] = 1 }
        if (clay[l-1, y] && clay[r+1, y]) {
            for (i = l; i <= r; i++) s[i, y] = 1
            f(x, y - 1)
        } else {
            if (!clay[l-1, y]) f(l-1, y)
            if (!clay[r+1, y]) f(r+1, y)
        }
    }
}
END {
    f(500, 0)
    for (k in v) {
        split(k, c, SUBSEP)
        if (c[2] >= min_y && c[2] <= max_y) n++
    }
    print n
}
