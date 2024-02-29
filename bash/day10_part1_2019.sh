#!/bin/bash

awk '
function visible_asteroids(x, y,     i, j, dx, dy, gcd, count) {
    delete angles
    for (i = 1; i <= rows; i++) {
        for (j = 1; j <= cols; j++) {
            if (asteroids[i, j] && !(i == y && j == x)) {
                dx = j - x
                dy = i - y
                if (dx == 0) {
                    angle = dy < 0 ? -3.141592653589793 / 2 : 3.141592653589793 / 2
                } else if (dy == 0) {
                    angle = dx < 0 ? 3.141592653589793 : 0
                } else {
                    angle = atan2(dy, dx)
                }
                angles[angle]
            }
        }
    }
    return length(angles)
}

function find_best_location(asteroids,     max_count, x, y, count) {
    max_count = 0
    for (y = 1; y <= rows; y++) {
        for (x = 1; x <= cols; x++) {
            if (asteroids[y, x]) {
                count = visible_asteroids(x, y)
                if (count > max_count) {
                    max_count = count
                }
            }
        }
    }
    return max_count
}

BEGIN {
    FS=""
}

NR == 1 {
    cols = NF
}

{
    for (i = 1; i <= NF; i++) {
        asteroids[NR, i] = ($i == "#")
    }
    rows = NR
}

END {
    max_count = find_best_location(asteroids)
    print max_count
}' input.txt