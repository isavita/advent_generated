
def data = new File("input.txt").text.trim().split("\n")
def grid = data.collect { it as char[] }

println findSafeStep(grid)

def findSafeStep(grid) {
    def step = 0
    while (true) {
        def eastMoved = moveEast(grid)
        def southMoved = moveSouth(grid)
        step++

        if (!eastMoved && !southMoved) {
            break
        }
    }
    return step
}

def moveEast(grid) {
    def moved = false
    def height = grid.size()
    def width = grid[0].size()

    def oldPositions = new char[height][width]
    for (int i = 0; i < height; i++) {
        oldPositions[i] = new char[width]
    }

    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (grid[y][x] == '>') {
                def nextX = (x + 1) % width
                if (grid[y][nextX] == '.') {
                    oldPositions[y][x] = '.'
                    grid[y][nextX] = '>'
                    x++
                    moved = true
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions)

    return moved
}

def moveSouth(grid) {
    def moved = false
    def height = grid.size()
    def width = grid[0].size()

    def oldPositions = new char[height][width]
    for (int i = 0; i < height; i++) {
        oldPositions[i] = new char[width]
    }

    for (int x = 0; x < width; x++) {
        for (int y = 0; y < height; y++) {
            if (grid[y][x] == 'v') {
                def nextY = (y + 1) % height
                if (grid[nextY][x] == '.') {
                    oldPositions[y][x] = '.'
                    grid[nextY][x] = 'v'
                    y++
                    moved = true
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions)

    return moved
}

def freeEmptyPositions(grid, oldPostion) {
    for (int y = 0; y < grid.size(); y++) {
        for (int x = 0; x < grid[0].size(); x++) {
            if (oldPostion[y][x] == '.') {
                grid[y][x] = '.'
            }
        }
    }
}
