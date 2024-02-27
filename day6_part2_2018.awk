
#!/usr/bin/awk -f

function abs(x) {return x < 0 ? -x : x}

BEGIN {
    while ((getline line < "input.txt") > 0) {
        split(line, coords, ",")
        x = coords[1]
        y = coords[2]
        coordinates[length(coordinates)] = x " " y
    }

    close("input.txt")

    for (i = 0; i <= 500; i++) {
        for (j = 0; j <= 500; j++) {
            totalDistance = 0

            for (k in coordinates) {
                split(coordinates[k], coord, " ")
                totalDistance += abs(i - coord[1]) + abs(j - coord[2])
            }

            if (totalDistance < 10000) {
                regionSize++
            }
        }
    }

    print regionSize
}
