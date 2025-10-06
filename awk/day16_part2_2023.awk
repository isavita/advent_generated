#!/usr/bin/awk -f
BEGIN {
    fname = "input.txt"
    i = 0
    while ((getline line < fname) > 0) {
        grid[i] = line
        if (i == 0) width = length(line)
        i++
    }
    height = i
    close(fname)
    STAMP = 0
    slash = "/"
    backslash = "\\"
}

function dir_to_index(dx, dy,   idx) {
    if (dx == 1 && dy == 0) idx = 0
    else if (dx == -1 && dy == 0) idx = 1
    else if (dx == 0 && dy == 1) idx = 2
    else if (dx == 0 && dy == -1) idx = 3
    else idx = -1
    return idx
}

function simulate_beam(sx, sy, sdx, sdy,   stamp, head, tail, x, y, dx, dy, nx, ny, ndx, ndy, cell, id, dir_idx) {
    STAMP++
    stamp = STAMP
    head = 0
    tail = 0
    qx[tail] = sx; qy[tail] = sy; qdx[tail] = sdx; qdy[tail] = sdy; tail++
    delete energ
    while (head < tail) {
        x = qx[head]; y = qy[head]; dx = qdx[head]; dy = qdy[head]; head++
        nx = x + dx; ny = y + dy
        if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue
        dir_idx = dir_to_index(dx, dy)
        id = ny "|" nx "|" dir_idx
        if (visited[id] == stamp) continue
        visited[id] = stamp
        energ[ny "|" nx] = 1
        cell = substr(grid[ny], nx+1, 1)
        if (cell == ".") {
            qx[tail] = nx; qy[tail] = ny; qdx[tail] = dx; qdy[tail] = dy; tail++
        } else if (cell == slash) {
            ndx = -dy; ndy = -dx
            qx[tail] = nx; qy[tail] = ny; qdx[tail] = ndx; qdy[tail] = ndy; tail++
        } else if (cell == backslash) {
            ndx = dy; ndy = dx
            qx[tail] = nx; qy[tail] = ny; qdx[tail] = ndx; qdy[tail] = ndy; tail++
        } else if (cell == "|") {
            if (dx != 0) {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = 0; qdy[tail] = 1; tail++
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = 0; qdy[tail] = -1; tail++
            } else {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = dx; qdy[tail] = dy; tail++
            }
        } else if (cell == "-") {
            if (dy != 0) {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = 1; qdy[tail] = 0; tail++
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = -1; qdy[tail] = 0; tail++
            } else {
                qx[tail] = nx; qy[tail] = ny; qdx[tail] = dx; qdy[tail] = dy; tail++
            }
        }
    }
    count = 0
    for (r = 0; r < height; r++) {
        for (c = 0; c < width; c++) {
            if (energ[r "|" c] != "") count++
        }
    }
    return count
}

END {
    max_energy = 0

    # Top edge: entering downward
    for (x = 0; x < width; x++) {
        val = simulate_beam(x, -1, 0, 1)
        if (val > max_energy) max_energy = val
    }

    # Bottom edge: entering upward
    for (x = 0; x < width; x++) {
        val = simulate_beam(x, height, 0, -1)
        if (val > max_energy) max_energy = val
    }

    # Left edge: entering rightwards
    for (y = 0; y < height; y++) {
        val = simulate_beam(-1, y, 1, 0)
        if (val > max_energy) max_energy = val
    }

    # Right edge: entering leftwards
    for (y = 0; y < height; y++) {
        val = simulate_beam(width, y, -1, 0)
        if (val > max_energy) max_energy = val
    }

    print max_energy
}