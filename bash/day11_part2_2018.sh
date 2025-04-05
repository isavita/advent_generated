
#!/bin/bash

main() {
    local serial_number
    read serial_number < input.txt

    awk -v serial_number="$serial_number" '
    BEGIN {
        max_power = -999999 # Initialize with a very small number
        max_x = -1
        max_y = -1
        max_size = -1

        # Calculate cumulative grid (summed-area table) directly
        for (x = 1; x <= 300; x++) {
            for (y = 1; y <= 300; y++) {
                rack_id = x + 10
                power_level = rack_id * y
                power_level += serial_number
                power_level *= rack_id
                # Integer division for hundreds digit, then modulo 10
                power_level = int(power_level / 100) % 10
                power_level -= 5

                # cumulative[x, y] = power_level + cumulative[x-1, y] + cumulative[x, y-1] - cumulative[x-1, y-1]
                # Awk auto-initializes non-existent array elements to 0, handling boundaries
                cumulative[x, y] = power_level + cumulative[x-1, y] + cumulative[x, y-1] - cumulative[x-1, y-1]
            }
        }

        # Find the square with the largest total power
        for (size = 1; size <= 300; size++) {
            # Loop boundaries: x goes from 1 to 300-size+1
            for (x = 1; x <= 300 - size + 1; x++) {
                 # Loop boundaries: y goes from 1 to 300-size+1
                for (y = 1; y <= 300 - size + 1; y++) {
                    # Coordinates for the cumulative lookup formula
                    x1 = x - 1
                    y1 = y - 1
                    x2 = x + size - 1
                    y2 = y + size - 1

                    # Calculate total power for the square using the cumulative grid
                    total_power = cumulative[x2, y2] - cumulative[x1, y2] - cumulative[x2, y1] + cumulative[x1, y1]

                    # Check if this square has more power
                    if (total_power > max_power) {
                        max_power = total_power
                        max_x = x
                        max_y = y
                        max_size = size
                    }
                }
            }
        }

        # Print the result
        print max_x "," max_y "," max_size
    }
    '
}

main
