
#!/bin/bash

main() {
  awk -F'[=<>,\ ]+' '
    {
        # Store initial positions and velocities using line number (NR) as index
        px[NR] = $2
        py[NR] = $3
        vx[NR] = $5
        vy[NR] = $6
    }
    END {
        width = 101
        height = 103

        # Simulate 100 steps
        for (step = 0; step < 100; step++) {
            for (i = 1; i <= NR; i++) {
                # Update position with velocity, apply modulo
                px[i] = (px[i] + vx[i]) % width
                py[i] = (py[i] + vy[i]) % height

                # Adjust if modulo result is negative (awk % can yield negative)
                if (px[i] < 0) px[i] += width
                if (py[i] < 0) py[i] += height
            }
        }

        # Count robots in quadrants
        q1 = 0; q2 = 0; q3 = 0; q4 = 0
        for (i = 1; i <= NR; i++) {
            x = px[i]
            y = py[i]

            # Skip robots on the dividing lines
            if (x == 50 || y == 51) continue

            # Assign to quadrants
            if      (x < 50 && y < 51) q1++
            else if (x > 50 && y < 51) q2++
            else if (x < 50 && y > 51) q3++
            else if (x > 50 && y > 51) q4++
        }

        # Print the final product
        print q1 * q2 * q3 * q4
    }
  ' input.txt
}

main
