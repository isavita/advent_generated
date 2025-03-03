
def solve() {
    def grid = new File("input.txt").readLines().collect { it.toList() }
    def startRow = -1
    def startCol = -1

    grid.eachWithIndex { row, i ->
        row.eachWithIndex { cell, j ->
            if (cell == 'S') {
                startRow = i
                startCol = j
                return  // breaks inner loop
            }
        }
        if (startRow != -1) {
            return // breaks outer loop
        }
    }

    // Determine possible starting directions
    def possibleDirections = []
    if (startRow > 0 && ['|', '7', 'F'].contains(grid[startRow - 1][startCol])) {
        possibleDirections << [-1, 0] // North
    }
    if (startRow < grid.size() - 1 && ['|', 'L', 'J'].contains(grid[startRow + 1][startCol])) {
        possibleDirections << [1, 0]   // South
    }
    if (startCol > 0 && ['-', 'L', 'F'].contains(grid[startRow][startCol - 1])) {
        possibleDirections << [0, -1]  // West
    }
    if (startCol < grid[0].size() - 1 && ['-', 'J', '7'].contains(grid[startRow][startCol + 1])) {
        possibleDirections << [0, 1]   // East
    }

    // Function to get the next position
    def getNextPosition = { row, col, prevRow, prevCol ->
        def currentChar = grid[row][col]
        
        if (currentChar == '|') {
            if (prevRow < row) return [row + 1, col] // coming from north
            else return [row - 1, col]              // coming from south
        } else if (currentChar == '-') {
            if (prevCol < col) return [row, col + 1] // coming from west
            else return [row, col - 1]              // coming from east
        } else if (currentChar == 'L') {
            if (prevRow < row) return [row, col + 1] // coming from north
            else return [row - 1, col]              // coming from east
        } else if (currentChar == 'J') {
            if (prevRow < row) return [row, col - 1] // coming from north
            else return [row - 1, col]              // coming from west
        } else if (currentChar == '7') {
            if (prevRow > row) return [row, col - 1] // coming from south
            else return [row + 1, col]              // coming from east
        } else if (currentChar == 'F') {
            if (prevRow > row) return [row, col + 1] // coming from south
            else return [row + 1, col]              // coming from west
        }
        return null // Should not happen if we are on the loop
    }

    // Perform DFS/Loop traversal starting from the identified directions
    def maxDistance = 0
    possibleDirections.each { dir ->
        def currentRow = startRow + dir[0]
        def currentCol = startCol + dir[1]
        def prevRow = startRow
        def prevCol = startCol
        def distance = 1

        while (grid[currentRow][currentCol] != 'S') {
            def nextPos = getNextPosition(currentRow, currentCol, prevRow, prevCol)
            prevRow = currentRow
            prevCol = currentCol
            currentRow = nextPos[0]
            currentCol = nextPos[1]
            distance++
            if(currentRow < 0 || currentRow >= grid.size() || currentCol < 0 || currentCol >= grid[0].size()) {
                distance = -1
                break
            }
        }
        maxDistance = Math.max(maxDistance, distance)
    }

    println maxDistance / 2 //Farthest point is half way around the loop
}

solve()
