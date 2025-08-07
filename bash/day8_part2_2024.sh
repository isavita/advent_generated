
#!/usr/bin/env bash
# Bash script that reads a grid from input.txt and prints the number of
# antinode cells as defined in the problem statement.

awk '
BEGIN { FS="" }

# Read the grid line by line
{
    h++                     # current row number
    w = length($0)            # width of the grid (assumed constant)
    for (i = 1; i <= w; i++) {
        c = substr($0, i, 1)
        if (c != ".") {
            count[c]++
            coords[c, count[c]] = h "," i
        }
    }
}

END {
    total = 0

    # Process each frequency (character)
    for (c in count) {
        if (count[c] < 2) continue

        for (i = 1; i < count[c]; i++) {
            split(coords[c, i], p1, ",")
            y1 = p1[1]; x1 = p1[2]

            for (j = i + 1; j <= count[c]; j++) {
                split(coords[c, j], p2, ",")
                y2 = p2[1]; x2 = p2[2]

                dy = y2 - y1
                dx = x2 - x1
                g  = gcd(abs(dy), abs(dx))
                if (g == 0) continue

                stepY = dy / g
                stepX = dx / g

                # Forward direction from the first antenna
                x = x1; y = y1
                while (x >= 1 && x <= w && y >= 1 && y <= h) {
                    key = y "x" x
                    if (!(key in visited)) {
                        visited[key] = 1
                        total++
                    }
                    x += stepX
                    y += stepY
                }

                # Backward direction from the first antenna
                x = x1 - stepX; y = y1 - stepY
                while (x >= 1 && x <= w && y >= 1 && y <= h) {
                    key = y "x" x
                    if (!(key in visited)) {
                        visited[key] = 1
                        total++
                    }
                    x -= stepX
                    y -= stepY
                }
            }
        }
    }

    print total
}

# Helper functions
function gcd(a, b) {
    while (b != 0) {
        t = b
        b = a % b
        a = t
    }
    return a
}
function abs(x) { return x < 0 ? -x : x }
' input.txt
