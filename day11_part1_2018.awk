
{
    serial = $0
}

BEGIN {
    gridSize = 300
    for (i = 0; i < gridSize; i++) {
        for (j = 0; j < gridSize; j++) {
            grid[i, j] = 0
        }
    }
}

function abs(x) {
    return x < 0 ? -x : x
}

function powerLevel(x, y, serial) {
    rackID = x + 11
    power = rackID * (y + 1)
    power += serial
    power *= rackID
    power = int(power / 100) % 10
    power -= 5
    return power
}

END {
    for (y = 0; y < gridSize; y++) {
        for (x = 0; x < gridSize; x++) {
            grid[y, x] = powerLevel(x, y, serial)
        }
    }

    maxPower = -2**31
    for (y = 0; y < gridSize-2; y++) {
        for (x = 0; x < gridSize-2; x++) {
            totalPower = 0
            for (dy = 0; dy < 3; dy++) {
                for (dx = 0; dx < 3; dx++) {
                    totalPower += grid[y+dy, x+dx]
                }
            }
            if (totalPower > maxPower) {
                maxPower = totalPower
                maxX = x+1
                maxY = y+1
            }
        }
    }

    print maxX "," maxY
}
