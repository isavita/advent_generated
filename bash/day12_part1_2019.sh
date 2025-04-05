
#!/bin/bash

main() {
    awk '
    function abs(v) { return (v < 0 ? -v : v) }

    # Parse input and initialize arrays
    {
        gsub(/[<>xyz= ]/, ""); # Remove formatting characters
        split($0, coords, ",");
        mx[NR] = coords[1];
        my[NR] = coords[2];
        mz[NR] = coords[3];
        vx[NR] = 0;
        vy[NR] = 0;
        vz[NR] = 0;
        num_moons = NR;
    }

    END {
        STEPS = 1000;

        for (step = 1; step <= STEPS; ++step) {
            # Update Velocities based on gravity
            for (i = 1; i <= num_moons; ++i) {
                for (j = i + 1; j <= num_moons; ++j) {
                    # Axis X
                    if (mx[i] < mx[j]) { dx=1 } else if (mx[i] > mx[j]) { dx=-1 } else { dx=0 }
                    vx[i] += dx; vx[j] -= dx;
                    # Axis Y
                    if (my[i] < my[j]) { dy=1 } else if (my[i] > my[j]) { dy=-1 } else { dy=0 }
                    vy[i] += dy; vy[j] -= dy;
                    # Axis Z
                    if (mz[i] < mz[j]) { dz=1 } else if (mz[i] > mz[j]) { dz=-1 } else { dz=0 }
                    vz[i] += dz; vz[j] -= dz;
                }
            }

            # Update Positions based on velocity
            for (i = 1; i <= num_moons; ++i) {
                mx[i] += vx[i];
                my[i] += vy[i];
                mz[i] += vz[i];
            }
        }

        # Calculate Total Energy
        total_energy = 0;
        for (i = 1; i <= num_moons; ++i) {
            pot = abs(mx[i]) + abs(my[i]) + abs(mz[i]);
            kin = abs(vx[i]) + abs(vy[i]) + abs(vz[i]);
            total_energy += pot * kin;
        }
        print total_energy;
    }
    ' input.txt
}

main
