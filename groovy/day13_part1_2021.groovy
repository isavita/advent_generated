
def file = new File("input.txt")
def points = [:]
def folds = []

def readingPoints = true
file.eachLine { line ->
    if (line == "") {
        readingPoints = false
        return
    }
    if (readingPoints) {
        def coords = line.split(",")
        def x = coords[0] as Integer
        def y = coords[1] as Integer
        points[[x, y]] = true
    } else {
        folds.add(line)
    }
}

def fold = folds[0].tokenize()[2] // "fold along x=5" -> "x=5"
def axisValue = fold.split("=")
def axis = axisValue[0]
def value = axisValue[1] as Integer

def newPoints = [:]
if (axis == "x") {
    points.each { point, _ ->
        if (point[0] > value) {
            point[0] = 2*value - point[0]
        }
        newPoints[point] = true
    }
} else {
    points.each { point, _ ->
        if (point[1] > value) {
            point[1] = 2*value - point[1]
        }
        newPoints[point] = true
    }
}

println newPoints.size()
