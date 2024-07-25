
def countVisibleTrees(grid) {
    int visibleCount = 0
    int rows = grid.size()
    int cols = grid[0].size()

    // Check visibility for each tree in the grid
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            int height = grid[r][c]
            boolean visible = false
            
            // Check visibility from the left
            if (c == 0 || (0..<c).every { grid[r][it] < height }) visible = true
            // Check visibility from the right
            if (c == cols - 1 || ((c + 1)..<cols).every { grid[r][it] < height }) visible = true
            // Check visibility from the top
            if (r == 0 || (0..<r).every { grid[it][c] < height }) visible = true
            // Check visibility from the bottom
            if (r == rows - 1 || ((r + 1)..<rows).every { grid[it][c] < height }) visible = true
            
            if (visible) visibleCount++
        }
    }
    
    return visibleCount
}

def readInput(fileName) {
    def lines = new File(fileName).readLines()
    return lines.collect { it.collect { it as int } }
}

def main() {
    def grid = readInput("input.txt")
    def visibleTrees = countVisibleTrees(grid)
    println visibleTrees
}

main()
