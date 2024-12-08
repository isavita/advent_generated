
def findXMAS(grid) {
    def count = 0
    def word = "XMAS"
    def rows = grid.size()
    def cols = grid[0].size()

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            // Check all 8 directions
            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    if (dx == 0 && dy == 0) continue

                    boolean found = true
                    for (int k = 0; k < word.length(); k++) {
                        int x = i + dx * k
                        int y = j + dy * k
                        if (x < 0 || x >= rows || y < 0 || y >= cols || grid[x][y] != word[k]) {
                            found = false
                            break
                        }
                    }
                    if (found) count++
                }
            }
        }
    }
    return count
}


def grid = new File("input.txt").readLines().collect { it.toCharArray() }
println findXMAS(grid)
