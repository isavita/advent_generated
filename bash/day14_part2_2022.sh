
#!/bin/bash

main() {
  awk '
    function min(a, b) { return a < b ? a : b }
    function max(a, b) { return a > b ? a : b }

    # Process input lines to draw rocks and find max_y
    {
        # Split line like "x1,y1 -> x2,y2 -> ..." into coordinate pairs
        gsub(/ -> /, " ")
        n = split($0, coords, " ")

        # Get first point
        split(coords[1], p, ",")
        px = p[1]; py = p[2]
        if (py > max_y) max_y = py
        grid[py, px] = 1 # Mark rock/sand with 1

        # Process subsequent points to draw lines
        for (i = 2; i <= n; i++) {
            split(coords[i], p, ",")
            cx = p[1]; cy = p[2]
            if (cy > max_y) max_y = cy

            if (cx == px) { # Vertical line
                for (y = min(py, cy); y <= max(py, cy); y++) {
                    grid[y, cx] = 1
                }
            } else { # Horizontal line (cy == py)
                for (x = min(px, cx); x <= max(px, cx); x++) {
                    grid[cy, x] = 1
                }
            }
            px = cx; py = cy
        }
    }

    # After processing all input: simulate sand falling
    END {
        floor = max_y + 2
        sand_count = 0

        # Loop until sand blocks the source (0, 500)
        while (grid[0, 500] == "") {
            y = 0; x = 500 # Start sand at source

            # Simulate one grain falling
            while (1) {
                next_y = y + 1

                # Check floor
                if (next_y == floor) {
                    break # Sand rests at (x, y) just above floor
                }

                # Check below
                if (grid[next_y, x] == "") {
                    y = next_y
                    continue
                }

                # Check down-left
                if (grid[next_y, x - 1] == "") {
                    y = next_y
                    x = x - 1
                    continue
                }

                # Check down-right
                if (grid[next_y, x + 1] == "") {
                    y = next_y
                    x = x + 1
                    continue
                }

                # Sand comes to rest
                break
            }

            # Mark sand resting position
            grid[y, x] = 1
            sand_count++

            # Early exit if source got blocked (already checked by outer while loop condition)
        }
        print sand_count
    }
  ' input.txt
}

main
