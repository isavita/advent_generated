#!/usr/bin/awk -f
BEGIN {
    FS = "[ \t]+"
    n = 1
    X[0] = 0
    Y[0] = 0

    while ((getline line < "input.txt") > 0) {
        if (line ~ /^[ \t]*$/) continue
        split(line, parts, FS)
        dir = parts[1]
        len = (parts[2] + 0)

        dx = 0; dy = 0
        if (dir == "U") { dx = 0; dy = -1 }
        else if (dir == "L") { dx = -1; dy = 0 }
        else if (dir == "D") { dx = 0; dy = 1 }
        else if (dir == "R") { dx = 1; dy = 0 } else { next }

        X[n] = X[n-1] + dx * len
        Y[n] = Y[n-1] + dy * len
        n++
    }
    close("input.txt")

    area2 = 0
    for (i = 0; i < n; i++) {
        nxt = (i + 1) % n
        area2 += X[i] * Y[nxt] - Y[i] * X[nxt]
    }
    if (area2 < 0) area2 = -area2
    shoelace = int(area2 / 2)

    per = 0
    for (i = 0; i < n; i++) {
        nxt = (i + 1) % n
        dx = X[i] - X[nxt]
        dy = Y[i] - Y[nxt]
        if (dx < 0) dx = -dx
        if (dy < 0) dy = -dy
        per += dx + dy
    }
    per2 = int(per / 2)

    ans = shoelace + per2 + 1
    print ans
    exit
}