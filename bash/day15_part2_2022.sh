
awk -F'[^0-9-]+' '
function abs(v) { return v < 0 ? -v : v }
NF > 4 {
    sx[n] = $2
    sy[n] = $3
    dist[n] = abs($2 - $4) + abs($3 - $5)
    n++
}
END {
    m = 4000000
    for (x = 0; x <= m; x++) {
        for (y = 0; y <= m; ) {
            hit = 0
            for (i = 0; i < n; i++) {
                dy = dist[i] - abs(sx[i] - x)
                if (dy >= 0) {
                    y_min = sy[i] - dy
                    y_max = sy[i] + dy
                    if (y >= y_min && y <= y_max) {
                        y = y_max + 1
                        hit = 1
                        break
                    }
                }
            }
            if (!hit) {
                printf "%.0f\n", x * 4000000 + y
                exit
            }
        }
    }
}' input.txt
