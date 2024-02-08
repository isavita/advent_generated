
def grid = new File("input.txt").readLines().collect { line -> line.collect { it as int } as int[] }

def step = 0
while (true) {
    step++
    def flashes = simulateStep(grid)
    if (flashes == 100) {
        break
    }
}

println step

def simulateStep(grid) {
    def flashes = 0
    def flashed = [:]

    grid.eachWithIndex { row, y ->
        row.eachWithIndex { energy, x ->
            grid[y][x]++
        }
    }

    grid.eachWithIndex { row, y ->
        row.eachWithIndex { energy, x ->
            if (grid[y][x] > 9) {
                flashes += flash(grid, x, y, flashed)
            }
        }
    }

    flashed.each { coords, _ ->
        grid[coords[1]][coords[0]] = 0
    }

    return flashes
}

def flash(grid, x, y, flashed) {
    if (flashed[[x, y]]) {
        return 0
    }

    flashed[[x, y]] = true
    def flashes = 1
    def directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

    directions.each { dir ->
        def newX = x + dir[0]
        def newY = y + dir[1]
        if (newX >= 0 && newX < grid[0].size() && newY >= 0 && newY < grid.size()) {
            grid[newY][newX]++
            if (grid[newY][newX] > 9) {
                flashes += flash(grid, newX, newY, flashed)
            }
        }
    }

    return flashes
}
