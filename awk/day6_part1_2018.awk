
BEGIN {
    maxX = 0
    maxY = 0
    while ((getline < "input.txt") > 0) {
        split($0, coords, ", ")
        x = int(coords[1])
        y = int(coords[2])
        if (x > maxX) {
            maxX = x
        }
        if (y > maxY) {
            maxY = y
        }
        points[++n] = x " " y
    }
    close("input.txt")

    for (i = 0; i <= maxX+1; i++) {
        for (j = 0; j <= maxY+1; j++) {
            minDist = maxX + maxY
            for (k = 1; k <= n; k++) {
                split(points[k], point, " ")
                dist = abs(point[1]-i) + abs(point[2]-j)
                if (dist < minDist) {
                    minDist = dist
                    grid[i, j] = k
                } else if (dist == minDist) {
                    grid[i, j] = -1
                }
            }
            if (grid[i, j] != -1) {
                if (i == 0 || j == 0 || i == maxX+1 || j == maxY+1) {
                    infinite[grid[i, j]] = 1
                }
                areas[grid[i, j]]++
            }
        }
    }

    maxArea = 0
    for (i = 1; i <= n; i++) {
        if (!infinite[i] && areas[i] > maxArea) {
            maxArea = areas[i]
        }
    }
    print maxArea
}

function abs(x) {
    return x < 0 ? -x : x
}
