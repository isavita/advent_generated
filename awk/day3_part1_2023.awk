#!/usr/bin/awk -f
function check_adjacent(xx, yy) {
    for (dy = -1; dy <= 1; dy++) {
        ny = yy + dy
        if (ny < 1 || ny > rows) continue
        for (dx = -1; dx <= 1; dx++) {
            nx = xx + dx
            if (dx == 0 && dy == 0) continue
            if (nx < 1 || nx > cols) continue
            ch = substr(grid[ny], nx, 1)
            if (ch ~ /[0-9]/) continue
            if (ch == ".") continue
            return 1
        }
    }
    return 0
}
function is_adjacent_to_symbol(xx, yy, len) {
    for (i = 0; i < len; i++) {
        if (check_adjacent(xx + i, yy)) return 1
    }
    return 0
}
BEGIN {
    fname = "input.txt"
    rows = 0
    while ((getline line < fname) > 0) {
        if (rows == 0) {
            cols = length(line)
        } else if (length(line) != cols) {
            print "Error: inconsistent line length" > "/dev/stderr"
            exit 1
        }
        rows++
        grid[rows] = line
    }
    if (rows == 0) { print 0; exit 0 }
    sum = 0
    for (y = 1; y <= rows; y++) {
        x = 1
        while (x <= cols) {
            key = y "," x
            if (visited[key] != 1 && substr(grid[y], x, 1) ~ /[0-9]/) {
                len = 0
                num = 0
                for (k = x; k <= cols; k++) {
                    ch = substr(grid[y], k, 1)
                    if (ch ~ /[0-9]/) { num = num * 10 + (ch + 0); len++ } else { break }
                }
                if (len > 0 && is_adjacent_to_symbol(x, y, len)) sum += num
                for (i2 = 0; i2 < len; i2++) visited[y "," (x + i2)] = 1
                x += len
                continue
            }
            x++
        }
    }
    print sum
}