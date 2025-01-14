
def tiltNorth(grid) {
    def rows = grid.size()
    def cols = grid[0].size()
    for (int col = 0; col < cols; col++) {
        int nextEmpty = 0
        for (int row = 0; row < rows; row++) {
            if (grid[row][col] == 'O') {
                if (row != nextEmpty) {
                    grid[nextEmpty][col] = 'O'
                    grid[row][col] = '.'
                }
                nextEmpty++
            } else if (grid[row][col] == '#') {
                nextEmpty = row + 1
            }
        }
    }
}

def tiltWest(grid) {
    def rows = grid.size()
    def cols = grid[0].size()
    for (int row = 0; row < rows; row++) {
        int nextEmpty = 0
        for (int col = 0; col < cols; col++) {
            if (grid[row][col] == 'O') {
                if (col != nextEmpty) {
                    grid[row][nextEmpty] = 'O'
                    grid[row][col] = '.'
                }
                nextEmpty++
            } else if (grid[row][col] == '#') {
                nextEmpty = col + 1
            }
        }
    }
}

def tiltSouth(grid) {
    def rows = grid.size()
    def cols = grid[0].size()
    for (int col = 0; col < cols; col++) {
        int nextEmpty = rows - 1
        for (int row = rows - 1; row >= 0; row--) {
            if (grid[row][col] == 'O') {
                if (row != nextEmpty) {
                    grid[nextEmpty][col] = 'O'
                    grid[row][col] = '.'
                }
                nextEmpty--
            } else if (grid[row][col] == '#') {
                nextEmpty = row - 1
            }
        }
    }
}

def tiltEast(grid) {
    def rows = grid.size()
    def cols = grid[0].size()
    for (int row = 0; row < rows; row++) {
        int nextEmpty = cols - 1
        for (int col = cols - 1; col >= 0; col--) {
            if (grid[row][col] == 'O') {
                if (col != nextEmpty) {
                    grid[row][nextEmpty] = 'O'
                    grid[row][col] = '.'
                }
                nextEmpty--
            } else if (grid[row][col] == '#') {
                nextEmpty = col - 1
            }
        }
    }
}

def calculateLoad(grid) {
    def rows = grid.size()
    def load = 0
    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < grid[0].size(); col++) {
            if (grid[row][col] == 'O') {
                load += rows - row
            }
        }
    }
    return load
}

def cycle(grid) {
    tiltNorth(grid)
    tiltWest(grid)
    tiltSouth(grid)
    tiltEast(grid)
}

def gridToString(grid) {
    grid.collect { it.join() }.join('\n')
}

def solvePart1(grid) {
    def copy = grid.collect { it.clone() }
    tiltNorth(copy)
    return calculateLoad(copy)
}

def solvePart2(grid) {
    def seen = [:]
    def loads = []
    def copy = grid.collect { it.clone() }
    def targetCycles = 1000000000
    
    for (int i = 0; i < targetCycles; i++) {
        cycle(copy)
        def gridString = gridToString(copy)
        if (seen.containsKey(gridString)) {
            def firstSeen = seen[gridString]
            def cycleLength = i - firstSeen
            def remainingCycles = targetCycles - i - 1
            def offset = remainingCycles % cycleLength
            return loads[firstSeen + offset]
        } else {
            seen[gridString] = i
            loads << calculateLoad(copy)
        }
    }
    return calculateLoad(copy)
}

def main() {
    def grid = new File('input.txt').readLines().collect { it.toList() }
    println "Part 1: ${solvePart1(grid)}"
    println "Part 2: ${solvePart2(grid)}"
}

main()
