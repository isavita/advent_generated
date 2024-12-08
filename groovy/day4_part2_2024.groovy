
def checkMAS(grid, x, y, dx, dy) {
    def word = "MAS"
    (0..<word.length()).every { i ->
        def newX = x + dx * i
        def newY = y + dy * i
        return (newX >= 0 && newY >= 0 && newX < grid.size() && newY < grid[0].size() && grid[newX][newY] == word[i])
    } || (0..<word.length()).every { i ->
        def newX = x + dx * i
        def newY = y + dy * i
        return (newX >= 0 && newY >= 0 && newX < grid.size() && newY < grid[0].size() && grid[newX][newY] == word[word.length() - 1 - i])
    }
}

def checkXMAS(grid, x, y) {
    checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1) ||
            checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1)
}

def countXMASPatterns(grid) {
    (1..<grid.size() - 1).sum { i ->
        (1..<grid[i].size() - 1).count { j ->
            grid[i][j] == 'A' && checkXMAS(grid, i, j)
        }
    }
}


def file = new File("input.txt")
def grid = file.readLines().findAll { it.trim() != "" }

println "X-MAS patterns appear ${countXMASPatterns(grid)} times in the word search"

