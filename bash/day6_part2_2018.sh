
#!/bin/bash

# Reads coordinates from input.txt, calculates the largest finite area
# and the size of the region containing locations with a total distance
# less than 10000 to all coordinates.

main() {
    awk '
    # Function to calculate absolute value
    function abs(v) {
        return v < 0 ? -v : v
    }

    # Function to calculate Manhattan distance
    function manhattan(x1, y1, x2, y2) {
        return abs(x1 - x2) + abs(y1 - y2)
    }

    # Read coordinates and find bounds
    {
        split($0, coord, /, /)
        x = coord[1]
        y = coord[2]
        coords_x[NR] = x
        coords_y[NR] = y
        num_coords = NR
        if (NR == 1 || x > max_x) max_x = x
        if (NR == 1 || y > max_y) max_y = y
    }

    # Process after reading all coordinates
    END {
        region_size = 0
        # Iterate through the grid
        for (x = 0; x <= max_x; x++) {
            for (y = 0; y <= max_y; y++) {
                min_dist = -1
                closest_idx = -1
                tied = 0
                total_dist = 0

                # Calculate distances to each coordinate
                for (i = 1; i <= num_coords; i++) {
                    dist = manhattan(x, y, coords_x[i], coords_y[i])
                    total_dist += dist

                    # Determine closest coordinate
                    if (min_dist == -1 || dist < min_dist) {
                        min_dist = dist
                        closest_idx = i
                        tied = 0
                    } else if (dist == min_dist) {
                        tied = 1
                    }
                }

                # Accumulate region size (Part 2)
                if (total_dist < 10000) {
                    region_size++
                }

                # Accumulate areas and identify infinite ones (Part 1)
                if (!tied && closest_idx != -1) {
                    areas[closest_idx]++
                    # Mark areas touching the boundary as infinite
                    if (x == 0 || x == max_x || y == 0 || y == max_y) {
                        infinite[closest_idx] = 1
                    }
                }
            }
        }

        # Find the largest finite area
        largest_area = 0
        for (i = 1; i <= num_coords; i++) {
            if (!(i in infinite) && areas[i] > largest_area) {
                largest_area = areas[i]
            }
        }

        # Print the results
        print largest_area
        print region_size
    }
    ' input.txt
}

# Ensure input file exists
if [[ ! -f "input.txt" ]]; then
    echo "Error: input.txt not found." >&2
    exit 1
fi

main
