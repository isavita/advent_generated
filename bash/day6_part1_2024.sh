
#!/bin/bash
awk '
{
    w = length($0)
    for (x = 1; x <= w; x++) {
        c = substr($0, x, 1)
        grid[NR, x] = c
        if (c ~ /[\^>v<]/) {
            cx = x; cy = NR
            cd = (c == "^" ? 0 : (c == ">" ? 1 : (c == "v" ? 2 : 3)))
        }
    }
}
END {
    h = NR
    dx[0] = 0; dy[0] = -1
    dx[1] = 1; dy[1] = 0
    dx[2] = 0; dy[2] = 1
    dx[3] = -1; dy[3] = 0
    while (cx >= 1 && cx <= w && cy >= 1 && cy <= h) {
        if (!vis[cy, cx]++) count++
        nx = cx + dx[cd]
        ny = cy + dy[cd]
        if (grid[ny, nx] == "#") {
            cd = (cd + 1) % 4
        } else {
            cx = nx
            cy = ny
        }
    }
    print count
}' input.txt
