BEGIN {
    Open = "."
    Trees = "|"
    Lumberyard = "#"
    Size = 50
    getline < "input.txt"
    for (i = 1; i <= Size; i++) {
        grid[i] = $0
        getline < "input.txt"
    }
}

function transform() {
    for (i = 1; i <= Size; i++) {
        for (j = 1; j <= Size; j++) {
            newGrid[i, j] = nextAcreState(i, j)
        }
    }
    for (i = 1; i <= Size; i++) {
        for (j = 1; j <= Size; j++) {
            grid[i] = substr(grid[i], 1, j-1) newGrid[i, j] substr(grid[i], j+1)
        }
    }
}

function nextAcreState(i, j) {
    acre = substr(grid[i], j, 1)
    if (acre == Open && countAdjacent(i, j, Trees) >= 3) return Trees
    if (acre == Trees && countAdjacent(i, j, Lumberyard) >= 3) return Lumberyard
    if (acre == Lumberyard) {
        if (countAdjacent(i, j, Lumberyard) >= 1 && countAdjacent(i, j, Trees) >= 1) return Lumberyard
        return Open
    }
    return acre
}

function countAdjacent(i, j, acreType) {
    count = 0
    for (x = -1; x <= 1; x++) {
        for (y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) continue
            ni = i + x
            nj = j + y
            if (ni > 0 && ni <= Size && nj > 0 && nj <= Size && substr(grid[ni], nj, 1) == acreType) count++
        }
    }
    return count
}

function countResources() {
    wooded = 0
    lumberyards = 0
    for (i = 1; i <= Size; i++) {
        for (j = 1; j <= Size; j++) {
            acre = substr(grid[i], j, 1)
            if (acre == Trees) wooded++
            if (acre == Lumberyard) lumberyards++
        }
    }
    return wooded * lumberyards
}

BEGIN {
    for (minute = 1; minute <= 10; minute++) {
        transform()
    }
    print countResources()
}