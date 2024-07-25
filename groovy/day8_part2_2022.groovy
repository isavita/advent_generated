
def readInput(fileName) {
    new File(fileName).readLines().collect { it.collect { it as Integer } }
}

def isVisible(trees, row, col) {
    def height = trees[row][col]
    def directions = [
        [0, -1], // left
        [0, 1],  // right
        [-1, 0], // up
        [1, 0]   // down
    ]
    
    for (dir in directions) {
        int r = row, c = col
        while (true) {
            r += dir[0]
            c += dir[1]
            if (r < 0 || r >= trees.size() || c < 0 || c >= trees[0].size()) {
                break // out of bounds
            }
            if (trees[r][c] >= height) {
                return false // blocked
            }
        }
    }
    return true // visible
}

def countVisibleTrees(trees) {
    def visibleCount = 0
    for (int row = 0; row < trees.size(); row++) {
        for (int col = 0; col < trees[0].size(); col++) {
            if (row == 0 || row == trees.size() - 1 || col == 0 || col == trees[0].size() - 1 || isVisible(trees, row, col)) {
                visibleCount++
            }
        }
    }
    return visibleCount
}

def scenicScore(trees, row, col) {
    def height = trees[row][col]
    def scores = [0, 0, 0, 0] // left, right, up, down
    def directions = [
        [0, -1], // left
        [0, 1],  // right
        [-1, 0], // up
        [1, 0]   // down
    ]
    
    for (int i = 0; i < directions.size(); i++) {
        int r = row, c = col
        while (true) {
            r += directions[i][0]
            c += directions[i][1]
            if (r < 0 || r >= trees.size() || c < 0 || c >= trees[0].size()) {
                break // out of bounds
            }
            scores[i]++
            if (trees[r][c] >= height) {
                break // blocked
            }
        }
    }
    return scores.inject(1) { acc, score -> acc * score }
}

def findHighestScenicScore(trees) {
    def maxScore = 0
    for (int row = 0; row < trees.size(); row++) {
        for (int col = 0; col < trees[0].size(); col++) {
            maxScore = Math.max(maxScore, scenicScore(trees, row, col))
        }
    }
    return maxScore
}

def main() {
    def trees = readInput("input.txt")
    def visibleCount = countVisibleTrees(trees)
    def highestScore = findHighestScenicScore(trees)
    
    println "Number of visible trees: $visibleCount"
    println "Highest scenic score: $highestScore"
}

main()
