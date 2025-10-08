
BEGIN {
    while ((getline line < "input.txt") > 0) {
        rows[++R] = line
        for (i = 1; i <= length(line); i++) {
            c = substr(line, i, 1)
            grid[R "," i] = c
            if (c ~ /[0-9]/) {
                num = num * 10 + (c + 0)
                if (!start) start = i
                end = i
            } else if (num) {
                parts[++N] = num
                for (j = start; j <= end; j++) {
                    partmap[R "," j] = N
                }
                num = start = end = 0
            }
        }
        if (num) {
            parts[++N] = num
            for (j = start; j <= end; j++) {
                partmap[R "," j] = N
            }
            num = start = end = 0
        }
        C = length(line)
    }
    sum = 0
    for (y = 1; y <= R; y++) {
        for (x = 1; x <= C; x++) {
            if (grid[y "," x] == "*") {
                delete seen
                cnt = 0
                for (dy = -1; dy <= 1; dy++) {
                    for (dx = -1; dx <= 1; dx++) {
                        if (dy == 0 && dx == 0) continue
                        ny = y + dy
                        nx = x + dx
                        idx = partmap[ny "," nx]
                        if (idx && !seen[idx]) {
                            seen[idx] = 1
                            neighbors[++cnt] = idx
                        }
                    }
                }
                if (cnt == 2) {
                    sum += parts[neighbors[1]] * parts[neighbors[2]]
                }
            }
        }
    }
    print sum
}
