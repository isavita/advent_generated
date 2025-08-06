
#!/usr/bin/env bash
# 100×100 Game‑of‑Life with the four corners permanently lit.
# Reads the initial state from input.txt and prints the number of lights on
# after 100 steps.

awk -v steps=100 '
BEGIN { gridSize = 100 }

# Read the initial grid
{
    y = NR - 1
    for (x = 0; x < gridSize; x++) {
        c = substr($0, x + 1, 1)
        grid[x, y] = (c == "#") ? 1 : 0
    }
}

END {
    # Force the four corners to be on
    grid[0, 0] = 1
    grid[0, gridSize - 1] = 1
    grid[gridSize - 1, 0] = 1
    grid[gridSize - 1, gridSize - 1] = 1

    for (step = 1; step <= steps; step++) {
        delete new
        for (x = 0; x < gridSize; x++) {
            for (y = 0; y < gridSize; y++) {
                on = 0
                for (dx = -1; dx <= 1; dx++) {
                    for (dy = -1; dy <= 1; dy++) {
                        if (dx == 0 && dy == 0) continue
                        nx = x + dx
                        ny = y + dy
                        if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx, ny])
                            on++
                    }
                }
                if (grid[x, y]) {
                    new[x, y] = (on == 2 || on == 3) ? 1 : 0
                } else {
                    new[x, y] = (on == 3) ? 1 : 0
                }
            }
        }
        # corners stay on
        new[0, 0] = 1
        new[0, gridSize - 1] = 1
        new[gridSize - 1, 0] = 1
        new[gridSize - 1, gridSize - 1] = 1

        # copy new to grid
        for (x = 0; x < gridSize; x++)
            for (y = 0; y < gridSize; y++)
                grid[x, y] = new[x, y]
    }

    # Count lights that are on
    count = 0
    for (x = 0; x < gridSize; x++)
        for (y = 0; y < gridSize; y++)
            if (grid[x, y]) count++

    print count
}
' input.txt
