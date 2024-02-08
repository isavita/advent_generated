
def matrix = new File("input.txt").readLines().collect{ it as char[] }
def sum = sumOfPartNumbers(matrix)
println sum

int sumOfPartNumbers(matrix) {
    int sum = 0
    def visited = new boolean[matrix.size()][matrix[0].size()]

    matrix.eachWithIndex { row, y ->
        row.eachWithIndex { _, x ->
            if (!visited[y][x] && Character.isDigit(matrix[y][x])) {
                def (number, length) = extractNumber(matrix, x, y)
                if (isAdjacentToSymbol(matrix, x, y, length)) {
                    sum += number
                }
                for (int i = 0; i < length; i++) {
                    visited[y][x+i] = true
                }
            }
        }
    }
    return sum
}

def extractNumber(matrix, x, y) {
    def numberStr = ""
    while (x < matrix[y].size() && Character.isDigit(matrix[y][x])) {
        numberStr += matrix[y][x++]
    }
    def number = numberStr as int
    return [number, numberStr.size()]
}

def isAdjacentToSymbol(matrix, x, y, length) {
    for (int i = 0; i < length; i++) {
        if (checkAdjacent(matrix, x+i, y)) {
            return true
        }
    }
    return false
}

def checkAdjacent(matrix, x, y) {
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            def adjX = x+dx, adjY = y+dy
            if (adjY >= 0 && adjY < matrix.size() && adjX >= 0 && adjX < matrix[adjY].size()) {
                if (!Character.isDigit(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.') {
                    return true
                }
            }
        }
    }
    return false
}
