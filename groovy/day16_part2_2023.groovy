
def grid = new File('input.txt').readLines()
def rows = grid.size()
def cols = grid[0].size()

def solve = { startRow, startCol, startDir ->
    def energized = new HashSet<Tuple>()
    def visited = new HashSet<Tuple>()
    def queue = new LinkedList<Tuple>()
    queue.add([startRow, startCol, startDir])

    while (!queue.isEmpty()) {
        def (row, col, dir) = queue.poll()

        if (row < 0 || row >= rows || col < 0 || col >= cols || visited.contains([row, col, dir])) {
            continue
        }

        visited.add([row, col, dir])
        energized.add([row, col])

        def tile = grid[row][col]
        switch (tile) {
            case '.':
                switch (dir) {
                    case 'R': queue.add([row, col + 1, 'R']); break
                    case 'L': queue.add([row, col - 1, 'L']); break
                    case 'U': queue.add([row - 1, col, 'U']); break
                    case 'D': queue.add([row + 1, col, 'D']); break
                }
                break
            case '/':
                switch (dir) {
                    case 'R': queue.add([row - 1, col, 'U']); break
                    case 'L': queue.add([row + 1, col, 'D']); break
                    case 'U': queue.add([row, col + 1, 'R']); break
                    case 'D': queue.add([row, col - 1, 'L']); break
                }
                break
            case '\\':
                switch (dir) {
                    case 'R': queue.add([row + 1, col, 'D']); break
                    case 'L': queue.add([row - 1, col, 'U']); break
                    case 'U': queue.add([row, col - 1, 'L']); break
                    case 'D': queue.add([row, col + 1, 'R']); break
                }
                break
            case '|':
                switch (dir) {
                    case 'R':
                    case 'L':
                        queue.add([row - 1, col, 'U'])
                        queue.add([row + 1, col, 'D'])
                        break
                    case 'U': queue.add([row - 1, col, 'U']); break
                    case 'D': queue.add([row + 1, col, 'D']); break
                }
                break
            case '-':
                switch (dir) {
                    case 'U':
                    case 'D':
                        queue.add([row, col - 1, 'L'])
                        queue.add([row, col + 1, 'R'])
                        break
                    case 'R': queue.add([row, col + 1, 'R']); break
                    case 'L': queue.add([row, col - 1, 'L']); break
                }
                break
        }
    }
    return energized.size()
}

// Part 1
println solve(0, 0, 'R')

// Part 2
def maxEnergized = 0
for (int i = 0; i < cols; i++) {
    maxEnergized = Math.max(maxEnergized, solve(0, i, 'D'))
    maxEnergized = Math.max(maxEnergized, solve(rows - 1, i, 'U'))
}
for (int i = 0; i < rows; i++) {
    maxEnergized = Math.max(maxEnergized, solve(i, 0, 'R'))
    maxEnergized = Math.max(maxEnergized, solve(i, cols - 1, 'L'))
}
println maxEnergized
