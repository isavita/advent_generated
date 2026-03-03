
function can_move2(x, y, dy) {
    if (vgrid[y, x] == ".") return 1
    if (vgrid[y, x] == "#") return 0
    if (vgrid[y, x] == "[") return can_move2(x, y + dy, dy) && can_move2(x + 1, y + dy, dy)
    if (vgrid[y, x] == "]") return can_move2(x - 1, y + dy, dy) && can_move2(x, y + dy, dy)
}
function do_move2(x, y, dy, x2) {
    if (vgrid[y, x] == ".") return
    x2 = x; if (vgrid[y, x] == "]") x2 = x - 1
    do_move2(x2, y + dy, dy)
    do_move2(x2 + 1, y + dy, dy)
    vgrid[y + dy, x2] = "["; vgrid[y + dy, x2 + 1] = "]"
    vgrid[y, x2] = "."; vgrid[y, x2 + 1] = "."
}
BEGIN {
    h = 0; is_map = 0
    while ((getline line < "input.txt") > 0) {
        if (length(line) == 0) { is_map = 1; continue }
        if (!is_map) {
            w = length(line)
            for (i = 1; i <= w; i++) orig[h, i - 1] = substr(line, i, 1)
            h++
        } else {
            moves = moves line
        }
    }
    gsub(/[ \n\r]/, "", moves)
    n_m = split(moves, m, "")
    dirs["^", 0] = 0; dirs["^", 1] = -1
    dirs["v", 0] = 0; dirs["v", 1] = 1
    dirs["<", -1] = -1; dirs["<", 0] = -1; dirs["<", 1] = 0 
    dirs["<", 0] = -1; dirs["<", 1] = 0
    dirs[">", 0] = 1; dirs[">", 1] = 0

    for (y = 0; y < h; y++) for (x = 0; x < w; x++) {
        vgrid[y, x] = orig[y, x]
        if (vgrid[y, x] == "@") { rx = x; ry = y }
    }
    for (i = 1; i <= n_m; i++) {
        dx = dirs[m[i], 0]; dy = dirs[m[i], 1]
        nx = rx + dx; ny = ry + dy
        tx = nx; ty = ny
        while (vgrid[ty, tx] == "O") { tx += dx; ty += dy }
        if (vgrid[ty, tx] == ".") {
            while (tx != nx || ty != ny) {
                vgrid[ty, tx] = vgrid[ty - dy, tx - dx]
                tx -= dx; ty -= dy
            }
            vgrid[ny, nx] = "."; vgrid[ry, rx] = "."; ry = ny; rx = nx; vgrid[ry, rx] = "@"
        }
    }
    for (y = 0; y < h; y++) for (x = 0; x < w; x++) if (vgrid[y, x] == "O") s1 += 100 * y + x
    print s1

    for (k in vgrid) delete vgrid[k]
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            c = orig[y, x]
            if (c == "#") { vgrid[y, 2 * x] = "#"; vgrid[y, 2 * x + 1] = "#" }
            if (c == "O") { vgrid[y, 2 * x] = "["; vgrid[y, 2 * x + 1] = "]" }
            if (c == ".") { vgrid[y, 2 * x] = "."; vgrid[y, 2 * x + 1] = "." }
            if (c == "@") { vgrid[y, 2 * x] = "@"; vgrid[y, 2 * x + 1] = "."; rx = 2 * x; ry = y }
        }
    }
    for (i = 1; i <= n_m; i++) {
        dx = dirs[m[i], 0]; dy = dirs[m[i], 1]
        nx = rx + dx; ny = ry + dy
        if (dy == 0) {
            tx = nx; ty = ny
            while (vgrid[ty, tx] == "[" || vgrid[ty, tx] == "]") { tx += dx; ty += dy }
            if (vgrid[ty, tx] == ".") {
                while (tx != nx || ty != ny) {
                    vgrid[ty, tx] = vgrid[ty - dy, tx - dx]
                    tx -= dx; ty -= dy
                }
                vgrid[ny, nx] = "."; vgrid[ry, rx] = "."; ry = ny; rx = nx; vgrid[ry, rx] = "@"
            }
        } else {
            if (vgrid[ny, nx] == ".") {
                vgrid[ry, rx] = "."; ry = ny; rx = nx; vgrid[ry, rx] = "@"
            } else if (vgrid[ny, nx] == "[" || vgrid[ny, nx] == "]") {
                if (can_move2(nx, ny, dy)) {
                    do_move2(nx, ny, dy)
                    vgrid[ny, nx] = "."; vgrid[ry, rx] = "."; ry = ny; rx = nx; vgrid[ry, rx] = "@"
                }
            }
        }
    }
    for (y = 0; y < h; y++) for (x = 0; x < 2 * w; x++) if (vgrid[y, x] == "[") s2 += 100 * y + x
    print s2
}

