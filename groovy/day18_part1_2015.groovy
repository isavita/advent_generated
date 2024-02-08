def input = new File("input.txt").readLines()

def grid = input.collect { it.toList() }

def countNeighbors = { x, y ->
    def count = 0
    for (i in -1..1) {
        for (j in -1..1) {
            if (!(i == 0 && j == 0) && x + i >= 0 && x + i < 100 && y + j >= 0 && y + j < 100 && grid[y + j][x + i] == '#') {
                count++
            }
        }
    }
    count
}

100.times {
    def nextGrid = grid.collect { it.clone() }

    for (int y = 0; y < 100; y++) {
        for (int x = 0; x < 100; x++) {
            def neighbors = countNeighbors(x, y)
            if (grid[y][x] == '#' && !(neighbors == 2 || neighbors == 3)) {
                nextGrid[y][x] = '.'
            } else if (grid[y][x] == '.' && neighbors == 3) {
                nextGrid[y][x] = '#'
            }
        }
    }

    grid = nextGrid
}

println grid.flatten().count { it == '#' }