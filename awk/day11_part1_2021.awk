#!/usr/bin/awk

BEGIN {
    # Initialize the octopus grid
    for (i = 1; i <= 10; i++) {
        getline < "input.txt"
        split($0, row, "")
        for (j = 1; j <= 10; j++) {
            grid[i, j] = row[j]
        }
    }

    # Initialize the flash count
    flash_count = 0

    # Simulate 100 steps
    for (step = 1; step <= 100; step++) {
        # Increase energy levels
        for (i = 1; i <= 10; i++) {
            for (j = 1; j <= 10; j++) {
                grid[i, j]++
            }
        }

        # Flash and increase adjacent energy levels
        flashed = 1
        while (flashed) {
            flashed = 0
            for (i = 1; i <= 10; i++) {
                for (j = 1; j <= 10; j++) {
                    if (grid[i, j] > 9) {
                        flash_count++
                        flashed = 1
                        grid[i, j] = 0
                        for (x = i - 1; x <= i + 1; x++) {
                            for (y = j - 1; y <= j + 1; y++) {
                                if (x >= 1 && x <= 10 && y >= 1 && y <= 10) {
                                    if (grid[x, y] != 0) {
                                        grid[x, y]++
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    # Print the total flash count
    print flash_count
}