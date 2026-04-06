
#!/bin/bash

awk '
{
    for (i = 1; i <= length($0); i++)
        if (substr($0, i, 1) == "#") e[i, NR] = 1
}
END {
    for (r = 0; r < 10; r++) {
        delete m; delete c
        for (p in e) {
            split(p, q, SUBSEP); x = q[1]; y = q[2]; h = 0
            for (dx = -1; dx <= 1; dx++) {
                for (dy = -1; dy <= 1; dy++)
                    if ((dx || dy) && (x + dx, y + dy) in e) { h = 1; break }
                if (h) break
            }
            if (!h) continue
            for (d = 0; d < 4; d++) {
                idx = (r + d) % 4; ok = 0
                if (idx == 0) { if (!((x - 1, y - 1) in e || (x, y - 1) in e || (x + 1, y - 1) in e)) { nx = x; ny = y - 1; ok = 1 } }
                else if (idx == 1) { if (!((x - 1, y + 1) in e || (x, y + 1) in e || (x + 1, y + 1) in e)) { nx = x; ny = y + 1; ok = 1 } }
                else if (idx == 2) { if (!((x - 1, y - 1) in e || (x - 1, y) in e || (x - 1, y + 1) in e)) { nx = x - 1; ny = y; ok = 1 } }
                else if (idx == 3) { if (!((x + 1, y - 1) in e || (x + 1, y) in e || (x + 1, y + 1) in e)) { nx = x + 1; ny = y; ok = 1 } }
                if (ok) { v = nx SUBSEP ny; m[p] = v; c[v]++; break }
            }
        }
        for (p in m) {
            v = m[p]
            if (c[v] == 1) { delete e[p]; e[v] = 1 }
        }
    }
    x0 = y0 = 1e9; x1 = y1 = -1e9; n = 0
    for (p in e) {
        split(p, q, SUBSEP); x = q[1] + 0; y = q[2] + 0; n++
        if (x < x0) x0 = x; if (x > x1) x1 = x
        if (y < y0) y0 = y; if (y > y1) y1 = y
    }
    print (x1 - x0 + 1) * (y1 - y0 + 1) - n
}' input.txt
