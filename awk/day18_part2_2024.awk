
#!/usr/bin/awk -f
# ------------------------------------------------------------
# 71×71 grid, corrupted cells are read from input.txt (x,y per line)
# Part 1 – shortest path after the first ≤1024 corrupted points
# Part 2 – first point that makes the path impossible
# ------------------------------------------------------------

# ---------- read all points ----------
BEGIN {
    maxX = 70; maxY = 70
    file = "input.txt"
    n = 0
    while ((getline line < file) > 0) {
        split(line, p, ",")
        xs[n] = p[1] + 0
        ys[n] = p[2] + 0
        n++
    }
    close(file)

    # ---------- Part 1 ----------
    limit = (n < 1024 ? n : 1024)
    delete blocked
    for (i = 0; i < limit; i++) {
        key = xs[i] "," ys[i]
        blocked[key] = 1
    }
    p1 = bfs()
    print p1

    # ---------- Part 2 ----------
    delete blocked
    for (i = 0; i < n; i++) {
        key = xs[i] "," ys[i]
        blocked[key] = 1
        if (bfs() == -1) {
            print xs[i] "," ys[i]
            exit
        }
    }
}

# ---------- Breadth‑first search ----------
function bfs(   head, tail, qx, qy, key, nx, ny, nkey, d, dist) {
    # visited holds distance from start; -1 = unvisited
    delete visited
    head = 0; tail = 0
    qx[tail] = 0; qy[tail] = 0; dist[tail] = 0; tail++

    visited["0,0"] = 0

    while (head < tail) {
        x = qx[head]; y = qy[head]; d = dist[head]; head++

        if (x == maxX && y == maxY) return d

        # four neighbours
        nx = x + 1; ny = y
        if (valid(nx, ny) && !( (key = nx "," ny) in visited )) {
            visited[key] = d + 1
            qx[tail] = nx; qy[tail] = ny; dist[tail] = d + 1; tail++
        }
        nx = x - 1; ny = y
        if (valid(nx, ny) && !( (key = nx "," ny) in visited )) {
            visited[key] = d + 1
            qx[tail] = nx; qy[tail] = ny; dist[tail] = d + 1; tail++
        }
        nx = x; ny = y + 1
        if (valid(nx, ny) && !( (key = nx "," ny) in visited )) {
            visited[key] = d + 1
            qx[tail] = nx; qy[tail] = ny; dist[tail] = d + 1; tail++
        }
        nx = x; ny = y - 1
        if (valid(nx, ny) && !( (key = nx "," ny) in visited )) {
            visited[key] = d + 1
            qx[tail] = nx; qy[tail] = ny; dist[tail] = d + 1; tail++
        }
    }
    return -1
}

# ---------- cell validity ----------
function valid(x, y,   key) {
    if (x < 0 || x > maxX || y < 0 || y > maxY) return 0
    key = x "," y
    return !(key in blocked)
}
