
def input = new File("input.txt").readLines()

def coordinates = input.collect { line ->
    def (x, y) = line.split(", ").collect { it as int }
    [x: x, y: y]
}

def minX = coordinates.min { it.x }.x
def minY = coordinates.min { it.y }.y
def maxX = coordinates.max { it.x }.x
def maxY = coordinates.max { it.y }.y

def areas = [:].withDefault { 0 }
def infiniteAreas = [:].withDefault { false }

for (int x = minX; x <= maxX; x++) {
    for (int y = minY; y <= maxY; y++) {
        def closest = [:].withDefault { Integer.MAX_VALUE }

        coordinates.eachWithIndex { coord, index ->
            def distance = Math.abs(coord.x - x) + Math.abs(coord.y - y)
            if (distance < closest.min) {
                closest = [index: index, min: distance]
            } else if (distance == closest.min) {
                closest = [index: -1, min: distance]
            }
        }

        if (closest.index != -1) {
            areas[closest.index]++
            if (x == minX || x == maxX || y == minY || y == maxY) {
                infiniteAreas[closest.index] = true
            }
        }
    }
}

def largestArea = areas.findAll { k, v -> !infiniteAreas[k] }.max { it.value }.value

println largestArea
