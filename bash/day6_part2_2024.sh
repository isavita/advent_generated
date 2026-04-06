
#!/bin/bash
awk '
BEGIN {
    h = 0
    while ((getline line < "input.txt") > 0) {
        h++
        w = length(line)
        for (i = 1; i <= w; i++) {
            char = substr(line, i, 1)
            grid[h, i] = char
            if (char == "^") { sy = h; sx = i; sd = 0 }
            else if (char == ">") { sy = h; sx = i; sd = 1 }
            else if (char == "v") { sy = h; sx = i; sd = 2 }
            else if (char == "<") { sy = h; sx = i; sd = 3 }
        }
    }

    # Trace original path to find candidate obstacle positions
    y = sy; x = sx; d = sd
    while (y >= 1 && y <= h && x >= 1 && x <= w) {
        if (!(y == sy && x == sx)) path[y, x] = 1
        dy = (d == 0 ? -1 : (d == 2 ? 1 : 0))
        dx = (d == 1 ? 1 : (d == 3 ? -1 : 0))
        ny = y + dy; nx = x + dx
        if (grid[ny, nx] == "#") d = (d + 1) % 4
        else { y = ny; x = nx }
    }

    ans = 0
    for (pos in path) {
        split(pos, p, SUBSEP)
        oy = p[1]; ox = p[2]
        grid[oy, ox] = "#"
        if (has_loop(sy, sx, sd, h, w)) ans++
        grid[oy, ox] = "."
    }
    print ans
}

function has_loop(y, x, d, h, w,    seen, dy, dx, ny, nx) {
    delete seen
    while (y >= 1 && y <= h && x >= 1 && x <= w) {
        if (seen[y, x, d]++) return 1
        dy = (d == 0 ? -1 : (d == 2 ? 1 : 0))
        dx = (d == 1 ? 1 : (d == 3 ? -1 : 0))
        ny = y + dy; nx = x + dx
        if (grid[ny, nx] == "#") d = (d + 1) % 4
        else { y = ny; x = nx }
    }
    return 0
}
'
