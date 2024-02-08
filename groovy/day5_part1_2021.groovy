
def inputFile = new File("input.txt")
def grid = [:]

inputFile.eachLine { line ->
    def parts = line.split(" -> ")
    def startCoords = parts[0].split(",")
    def endCoords = parts[1].split(",")

    def x1 = startCoords[0] as int
    def y1 = startCoords[1] as int
    def x2 = endCoords[0] as int
    def y2 = endCoords[1] as int

    if (x1 == x2) {
        if (y1 > y2) {
            y1 = y1 ^ y2
            y2 = y1 ^ y2
            y1 = y1 ^ y2
        }
        (y1..y2).each { y -> grid[[x1, y]] = (grid[[x1, y]] ?: 0) + 1 }
    } else if (y1 == y2) {
        if (x1 > x2) {
            x1 = x1 ^ x2
            x2 = x1 ^ x2
            x1 = x1 ^ x2
        }
        (x1..x2).each { x -> grid[[x, y1]] = (grid[[x, y1]] ?: 0) + 1 }
    }
}

def overlapCount = grid.count { k, v -> v > 1 }
println overlapCount
