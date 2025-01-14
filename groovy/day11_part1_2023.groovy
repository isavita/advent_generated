
def buildGrid(input, empty) {
    def width = input[0].length()
    def height = input.size()
    def data = [:]
    for (y in 0..<height) {
        for (x in 0..<width) {
            if (input[y][x] != empty) {
                data[[x, y]] = input[y][x]
            }
        }
    }
    [width, height, data]
}

def getEmptyRows(grid) {
    def emptyRows = []
    for (y in 0..<grid[1]) {
        def isEmpty = true
        for (x in 0..<grid[0]) {
            if (grid[2][[x, y]]) {
                isEmpty = false
                break
            }
        }
        if (isEmpty) {
            emptyRows << y
        }
    }
    emptyRows
}

def getEmptyCols(grid) {
    def emptyCols = []
    for (x in 0..<grid[0]) {
        def isEmpty = true
        for (y in 0..<grid[1]) {
            if (grid[2][[x, y]]) {
                isEmpty = false
                break
            }
        }
        if (isEmpty) {
            emptyCols << x
        }
    }
    emptyCols
}

def calculateOffsets(emptyIndexes, bound) {
    def offsets = (0..<bound).collect { 0 }
    for (idx in emptyIndexes) {
        for (i in idx + 1..<offsets.size()) {
            offsets[i]++
        }
    }
    offsets
}

def expandGrid(grid, expansionFactor) {
    def emptyCols = getEmptyCols(grid)
    def emptyRows = getEmptyRows(grid)
    def numLinesToAdd = expansionFactor - 1
    def newWidth = grid[0] + emptyCols.size() * numLinesToAdd
    def newHeight = grid[1] + emptyRows.size() * numLinesToAdd
    def newData = [:]
    def dXs = calculateOffsets(emptyCols, grid[0])
    def dYs = calculateOffsets(emptyRows, grid[1])
    for (y in 0..<grid[1]) {
        for (x in 0..<grid[0]) {
            if (grid[2][[x, y]]) {
                def newX = x + dXs[x] * numLinesToAdd
                def newY = y + dYs[y] * numLinesToAdd
                newData[[newX, newY]] = grid[2][[x, y]]
            }
        }
    }
    [newWidth, newHeight, newData]
}

def calculateLength(c1, c2) {
    Math.abs(c2[0] - c1[0]) + Math.abs(c2[1] - c1[1])
}

def solve(input) {
    def grid = buildGrid(input, '.')
    def expandedGrid = expandGrid(grid, 2)
    def res = 0
    def alreadySeen = [] as Set
    def coords = expandedGrid[2].keySet()
    for (c1 in coords) {
        for (c2 in alreadySeen) {
            res += calculateLength(c1, c2)
        }
        alreadySeen << c1
    }
    res
}

def input = new File("input.txt").readLines()
println solve(input)
