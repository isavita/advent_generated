BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, coords, ",")
        cubes[coords[1] "," coords[2] "," coords[3]] = 1
    }

    for (cube in cubes) {
        split(cube, coords, ",")
        x = coords[1]
        y = coords[2]
        z = coords[3]
        exposedSides = 6

        for (d = -1; d <= 1; d += 2) {
            if ((x + d "," y "," z) in cubes) exposedSides--
            if ((x "," y + d "," z) in cubes) exposedSides--
            if ((x "," y "," z + d) in cubes) exposedSides--
        }
        surfaceArea += exposedSides
    }
    print surfaceArea
}