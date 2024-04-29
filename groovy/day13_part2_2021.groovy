def file = new File("input.txt")
def points = [:]
def folds = []
def readingPoints = true // Declare and initialize readingPoints here

file.eachLine { line ->
    if (line == "") {
        readingPoints = false
    } else if (readingPoints) {
        def (x, y) = line.split(",").collect { it.toInteger() }
        points[x, y] = ""
    } else {
        def (axis, val) = line.split(" ")[2].split("=")
        def fold = [val.toInteger(), 0]
        if (axis == "x") {
            folds << fold
        } else {
            folds << [0, val.toInteger()]
        }
    }
}

folds.eachWithIndex { fold, i ->
    def newPoints = [:]
    points.each { point, value ->
        def newPoint = [point[0], point[1]]
        if (fold[0] != 0 && point[0] > fold[0]) {
            newPoint[0] = fold[0] - (point[0] - fold[0])
        } else if (fold[1] != 0 && point[1] > fold[1]) {
            newPoint[1] = fold[1] - (point[1] - fold[1])
        }
        newPoints[newPoint] = ""
    }
    points = newPoints
    if (i == 0) {
        println "Number of dots visible after first fold: ${points.size()}"
    }
}

def maxX = points.keySet().collect { it[0] }.max()
def maxY = points.keySet().collect { it[1] }.max()

def grid = new char[maxY + 1][maxX + 1]
for (int i = 0; i < grid.size(); i++) {
    for (int j = 0; j < grid[i].size(); j++) {
        grid[i][j] = ' '
    }
}

points.each { point, value ->
    grid[point[1]][point[0]] = '#'
}

grid.each { println new String(it) }