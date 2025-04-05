
#!/bin/bash

main() {
    awk '
    BEGIN { FS = "[= .,]+" }
    {
        action = $1
        xs = $3; xe = $4
        ys = $6; ye = $7
        zs = $9; ze = $10

        if (xs < -50 || xe > 50 || ys < -50 || ye > 50 || zs < -50 || ze > 50) {
            next
        }

        # Ensure bounds are within -50..50 for the loops
        xs = (xs < -50) ? -50 : xs
        xe = (xe > 50) ? 50 : xe
        ys = (ys < -50) ? -50 : ys
        ye = (ye > 50) ? 50 : ye
        zs = (zs < -50) ? -50 : zs
        ze = (ze > 50) ? 50 : ze

        val = (action == "on") ? 1 : 0

        for (x = xs; x <= xe; x++) {
            for (y = ys; y <= ye; y++) {
                for (z = zs; z <= ze; z++) {
                    # Use offset coords for array index
                    grid[x+50, y+50, z+50] = val
                }
            }
        }
    }
    END {
        count = 0
        for (idx in grid) {
            count += grid[idx]
        }
        print count
    }
    ' < input.txt
}

main
