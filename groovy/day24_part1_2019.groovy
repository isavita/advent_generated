def input = new File("input.txt").text.tokenize('\n')

def getBiodiversityRating(grid) {
    def rating = 0
    def index = 0
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (grid[i][j] == '#') {
                rating += 2 ** index
            }
            index++
        }
    }
    return rating
}

def seenLayouts = [:]
def currentLayout = input
while (!seenLayouts.containsKey(currentLayout.join())) {
    seenLayouts[currentLayout.join()] = true
    def newLayout = []
    for (int i = 0; i < 5; i++) {
        def row = ''
        for (int j = 0; j < 5; j++) {
            def adjacentBugs = 0
            if (i > 0 && currentLayout[i-1][j] == '#') adjacentBugs++
            if (i < 4 && currentLayout[i+1][j] == '#') adjacentBugs++
            if (j > 0 && currentLayout[i][j-1] == '#') adjacentBugs++
            if (j < 4 && currentLayout[i][j+1] == '#') adjacentBugs++
            if (currentLayout[i][j] == '#' && adjacentBugs != 1) {
                row += '.'
            } else if (currentLayout[i][j] == '.' && (adjacentBugs == 1 || adjacentBugs == 2)) {
                row += '#'
            } else {
                row += currentLayout[i][j]
            }
        }
        newLayout.add(row)
    }
    currentLayout = newLayout
}

println getBiodiversityRating(currentLayout)