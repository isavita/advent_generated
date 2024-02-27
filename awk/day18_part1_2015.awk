
#!/usr/bin/awk -f

BEGIN {
    gridSize = 100
    steps = 100

    for (i = 0; i < gridSize; ++i) {
        for (j = 0; j < gridSize; ++j) {
            grid[i, j] = 0
        }
    }
}

{
    for (i = 1; i <= length($0); ++i) {
        grid[i - 1, FNR - 1] = substr($0, i, 1) == "#" ? 1 : 0
    }
}

END {
    for (s = 0; s < steps; ++s) {
        for (x = 0; x < gridSize; ++x) {
            for (y = 0; y < gridSize; ++y) {
                on = countOnNeighbors(x, y)
                newGrid[x, y] = grid[x, y] && (on == 2 || on == 3) || !grid[x, y] && on == 3
            }
        }

        for (x = 0; x < gridSize; ++x) {
            for (y = 0; y < gridSize; ++y) {
                grid[x, y] = newGrid[x, y]
            }
        }
    }

    onCount = 0
    for (x = 0; x < gridSize; ++x) {
        for (y = 0; y < gridSize; ++y) {
            if (grid[x, y]) {
                onCount++
            }
        }
    }

    print onCount
}

function countOnNeighbors(x, y) {
    on = 0
    for (dx = -1; dx <= 1; dx++) {
        for (dy = -1; dy <= 1; dy++) {
            if (dx == 0 && dy == 0) {
                continue
            }
            nx = x + dx
            ny = y + dy
            if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx, ny]) {
                on++
            }
        }
    }
    return on
}
