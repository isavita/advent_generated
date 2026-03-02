
function abs(x) { return x < 0 ? -x : x }

BEGIN {
    FS = "[^0-9-]+"
    ARGV[1] = "input.txt"
    ARGC = 2
}

NF > 4 {
    sx[n] = $2
    sy[n] = $3
    dist[n] = abs($2 - $4) + abs($3 - $5)
    n++
}

END {
    maxcoord = 4000000
    for (x = 0; x <= maxcoord; x++) {
        for (y = 0; y <= maxcoord; ) {
            detected = 0
            for (i = 0; i < n; i++) {
                dx = abs(sx[i] - x)
                dy = dist[i] - dx
                if (dy >= 0) {
                    y_min = sy[i] - dy
                    y_max = sy[i] + dy
                    if (y >= y_min && y <= y_max) {
                        y = y_max + 1
                        detected = 1
                        break
                    }
                }
            }
            if (!detected) {
                printf "%.0f\n", x * 4000000 + y
                exit
            }
        }
    }
}
