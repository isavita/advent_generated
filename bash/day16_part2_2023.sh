
#!/bin/bash
awk '
{
    grid[NR-1] = $0
    W = length($0)
    H = NR
}
function sim(sx, sy, sdx, sdy,    head, tail, qx, qy, qdx, qdy, x, y, dx, dy, nx, ny, di, id, c, count, energized, visited) {
    head = 0; tail = 1
    qx[0] = sx; qy[0] = sy; qdx[0] = sdx; qdy[0] = sdy
    count = 0
    while (head < tail) {
        x = qx[head]; y = qy[head]; dx = qdx[head]; dy = qdy[head]; head++
        nx = x + dx; ny = y + dy
        if (nx < 0 || nx >= W || ny < 0 || ny >= H) continue
        di = (dx == 1 ? 0 : (dx == -1 ? 1 : (dy == 1 ? 2 : 3)))
        id = (ny * W + nx) * 4 + di
        if (visited[id]) continue
        visited[id] = 1
        if (!energized[ny * W + nx]++) count++
        c = substr(grid[ny], nx + 1, 1)
        if (c == ".") {
            qx[tail] = nx; qy[tail] = ny; qdx[tail] = dx; qdy[tail] = dy; tail++
        } else if (c == "/") {
            qx[tail] = nx; qy[tail] = ny; qdx[tail] = -dy; qdy[tail] = -dx; tail++
        } else if (c == "\\") {
            qx[tail] = nx; qy[tail] = ny; qdx[tail] = dy; qdy[tail] = dx; tail++
        } else if (c == "|") {
            if (dx != 0) {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = 0; qdy[tail] = 1; tail++
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = 0; qdy[tail] = -1; tail++
            } else {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = dx; qdy[tail] = dy; tail++
            }
        } else if (c == "-") {
            if (dy != 0) {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = 1; qdy[tail] = 0; tail++
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = -1; qdy[tail] = 0; tail++
            } else {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = dx; qdy[tail] = dy; tail++
            }
        }
    }
    return count
}
END {
    max_e = 0
    for (i = 0; i < W; i++) {
        v = sim(i, -1, 0, 1); if (v > max_e) max_e = v
        v = sim(i, H, 0, -1); if (v > max_e) max_e = v
    }
    for (i = 0; i < H; i++) {
        v = sim(-1, i, 1, 0); if (v > max_e) max_e = v
        v = sim(W, i, -1, 0); if (v > max_e) max_e = v
    }
    print max_e
}' input.txt

