#!/usr/bin/awk -f

function power_level(x, y, serial) {
    rack_id = x + 10
    power = rack_id * y
    power += serial
    power *= rack_id
    return int((power / 100) % 10) - 5
}

BEGIN {
    getline serial < "input.txt"
    for (x = 1; x <= 300; x++) {
        for (y = 1; y <= 300; y++) {
            grid[x, y] = power_level(x, y, serial)
        }
    }

    for (x = 1; x <= 300; x++) {
        for (y = 1; y <= 300; y++) {
            prefix_sum[x, y] = grid[x, y] + prefix_sum[x-1, y] + prefix_sum[x, y-1] - prefix_sum[x-1, y-1]
        }
    }

    max_power = -9999
    max_x = max_y = size = 0

    for (s = 1; s <= 300; s++) {
        for (x = 1; x <= 300 - s + 1; x++) {
            for (y = 1; y <= 300 - s + 1; y++) {
                total_power = prefix_sum[x + s - 1, y + s - 1] - prefix_sum[x - 1, y + s - 1] - prefix_sum[x + s - 1, y - 1] + prefix_sum[x - 1, y - 1]
                if (total_power > max_power) {
                    max_power = total_power
                    max_x = x
                    max_y = y
                    size = s
                }
            }
        }
    }

    print max_x "," max_y "," size
}