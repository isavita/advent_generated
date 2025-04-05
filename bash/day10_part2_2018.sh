
#!/bin/bash

# Solution using awk for efficiency

main() {
    awk -F '[^-.0-9]+' '
    # Read initial positions and velocities
    NF >= 5 {
        px[NR] = $2
        py[NR] = $3
        vx[NR] = $4
        vy[NR] = $5
        n = NR # Store number of points
    }

    END {
        time = 0
        while (1) {
            # Find min/max coordinates for this time step
            min_x = px[1]; max_x = px[1]
            min_y = py[1]; max_y = py[1]
            for (i = 2; i <= n; i++) {
                if (px[i] < min_x) min_x = px[i]
                if (px[i] > max_x) max_x = px[i]
                if (py[i] < min_y) min_y = py[i]
                if (py[i] > max_y) max_y = py[i]
            }

            width = max_x - min_x
            height = max_y - min_y

            # Check if the dimensions meet the criteria
            if (width >= 0 && width < 100 && height >= 0 && height < 10) {
                print time
                exit # Found the time, exit awk
            }

            # Update point positions for the next time step
            for (i = 1; i <= n; i++) {
                px[i] += vx[i]
                py[i] += vy[i]
            }

            time++
        }
    }
    ' input.txt
}

# Ensure input file exists
if [[ ! -f "input.txt" ]]; then
    echo "Error: input.txt not found." >&2
    exit 1
fi

main
