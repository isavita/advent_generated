
def file = new File("input.txt")
def input = file.text.trim()
def matrix = parseInput(input)
def originCol = 0
matrix[0].eachWithIndex { c, i ->
    if (c == "+") {
        originCol = i
    }
    matrix[matrix.size() - 1][i] = "#"
}

def ans = 0
while (!dropSand(matrix, originCol)) {
    ans++
    if (matrix[0][originCol] == "o") {
        break
    }
}

println ans

def parseInput(input) {
    def coordSets = []
    def lowestCol = Integer.MAX_VALUE
    def highestRow = 0
    input.readLines().each { line ->
        def rawCoords = line.split(" -> ")
        def coords = []
        rawCoords.each { rawCoord ->
            def rawNums = rawCoord.split(",")
            def col = rawNums[0] as Integer
            def row = rawNums[1] as Integer
            coords << [col, row]

            lowestCol = Math.min(lowestCol, col)
            highestRow = Math.max(highestRow, row)
        }
        coordSets << coords
    }

    def ExtraLeftSpace = 200

    def highestCol = 0
    coordSets.eachWithIndex { s, index ->
        s.each { set ->
            set[0] -= lowestCol - ExtraLeftSpace
            highestCol = Math.max(highestCol, set[0])
        }
    }

    def matrix = []
    (highestRow + 3).times {
        matrix << []
    }
    matrix.each { row ->
        (highestCol + ExtraLeftSpace * 2).times {
            row << ""
        }
    }

    coordSets.each { set ->
        (1..set.size() - 1).each { i ->
            def cols = [set[i - 1][0], set[i][0]].sort()
            def rows = [set[i - 1][1], set[i][1]].sort()

            if (cols[0] == cols[1]) {
                (rows[0]..rows[1]).each { r ->
                    matrix[r][cols[0]] = "#"
                }
            } else if (rows[0] == rows[1]) {
                (cols[0]..cols[1]).each { c ->
                    matrix[rows[0]][c] = "#"
                }
            }
        }
    }

    def originCol = 500 - lowestCol + ExtraLeftSpace
    matrix[0][originCol] = "+"

    matrix.each { row ->
        row.eachWithIndex { cell, j ->
            if (cell == "") {
                row[j] = "."
            }
        }
    }

    return matrix
}

def dropSand(matrix, originCol) {
    def r = 0, c = originCol

    while (r < matrix.size() - 1) {
        def below = matrix[r + 1][c]
        def diagonallyLeft = matrix[r + 1][c - 1]
        def diagonallyRight = matrix[r + 1][c + 1]
        if (below == ".") {
            r++
        } else if (diagonallyLeft == ".") {
            r++
            c--
        } else if (diagonallyRight == ".") {
            r++
            c++
        } else {
            matrix[r][c] = "o"
            return false
        }
    }

    return true
}
