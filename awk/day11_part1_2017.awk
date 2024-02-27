
    #!/usr/bin/awk -f

    function abs(x) {
        return x < 0 ? -x : x
    }

    function max(a, b) {
        return a > b ? a : b
    }

    function distance(x, y, z) {
        return (abs(x) + abs(y) + abs(z)) / 2
    }

    BEGIN {
        FS=","
        getline input < "input.txt"
        split(input, directions, ",")
        x = 0
        y = 0
        z = 0
        maxDistance = 0
    }

    {
        for (i=1; i<=NF; i++) {
            dir = $i
            if (dir == "n") {
                y++
                z--
            } else if (dir == "ne") {
                x++
                z--
            } else if (dir == "se") {
                x++
                y--
            } else if (dir == "s") {
                y--
                z++
            } else if (dir == "sw") {
                x--
                z++
            } else if (dir == "nw") {
                x--
                y++
            }

            curDistance = distance(x, y, z)
            maxDistance = max(maxDistance, curDistance)
        }
    }

    END {
        print distance(x, y, z)
    }
    