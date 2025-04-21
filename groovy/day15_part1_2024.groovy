
def pushBoxes(gridRef, r, c, dr, dc) {
    def nr = r + dr
    def nc = c + dc
    if (gridRef[nr][nc] == '#') {
        return false
    }
    if (gridRef[nr][nc] == 'O') {
        if (!pushBoxes(gridRef, nr, nc, dr, dc)) {
            return false
        }
    }
    if (gridRef[nr][nc] == '.') {
        gridRef[nr][nc] = 'O'
        gridRef[r][c] = '.'
        return true
    }
    return false
}

def main() {
    def inputFile = new File("input.txt")
    def lines = inputFile.readLines()

    def gridLines = []
    def moveLines = []
    def readingMap = true

    lines.each { line ->
       if (readingMap) {
           if (line.contains("#")) {
               gridLines << line
           } else if (gridLines.size() > 0) {
               readingMap = false
               moveLines << line
           } else {
               moveLines << line
           }
       } else {
           moveLines << line
       }
    }

    def grid = gridLines.collect { it.toList() }
    def moves = moveLines.join().trim()

    def robotR, robotC
    grid.eachWithIndex { row, r ->
        row.eachWithIndex { cell, c ->
            if (cell == '@') {
                robotR = r
                robotC = c
            }
        }
    }

    def dirs = [
        '^': [-1, 0],
        'v': [1, 0],
        '<': [0, -1],
        '>': [0, 1]
    ]

    moves.toList().each { move ->
        def d = dirs[move]
        def nr = robotR + d[0]
        def nc = robotC + d[1]

        if (grid[nr][nc] == '#') {
            return
        }

        def pushSucceeded = false
        if (grid[nr][nc] == 'O') {
            pushSucceeded = pushBoxes(grid, nr, nc, d[0], d[1])
            if (!pushSucceeded) {
                return
            }
        }

        if (grid[nr][nc] == '.' || grid[nr][nc] == 'O') {
             grid[robotR][robotC] = '.'
             grid[nr][nc] = '@'
             robotR = nr
             robotC = nc
        }
    }

    def sum = 0
    grid.eachWithIndex { row, r ->
        row.eachWithIndex { cell, c ->
            if (cell == 'O') {
                sum += r * 100 + c
            }
        }
    }

    println sum
}

main()
