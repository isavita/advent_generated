
def file = new File("input.txt")
def heightmap = file.readLines().collect { it.collect { Integer.parseInt(it) } }

def basinSizes = []
def visited = [:]

heightmap.eachWithIndex { row, y ->
    row.eachWithIndex { height, x ->
        if (isLowPoint(heightmap, x, y)) {
            def size = exploreBasin(heightmap, x, y, visited)
            basinSizes.add(size)
        }
    }
}

basinSizes.sort { a, b -> b <=> a }
def result = basinSizes[0] * basinSizes[1] * basinSizes[2]
println result

def isLowPoint(heightmap, x, y) {
    def height = heightmap[y][x]
    if (x > 0 && heightmap[y][x-1] <= height) return false
    if (x < heightmap[y].size() - 1 && heightmap[y][x+1] <= height) return false
    if (y > 0 && heightmap[y-1][x] <= height) return false
    if (y < heightmap.size() - 1 && heightmap[y+1][x] <= height) return false
    return true
}

def exploreBasin(heightmap, x, y, visited) {
    if (visited[[x, y]] || heightmap[y][x] == 9) return 0
    visited[[x, y]] = true
    def size = 1

    def directions = [[0, -1], [-1, 0], [0, 1], [1, 0]]
    directions.each { dir ->
        def newX = x + dir[0]
        def newY = y + dir[1]
        if (newX >= 0 && newX < heightmap[0].size() && newY >= 0 && newY < heightmap.size()) {
            size += exploreBasin(heightmap, newX, newY, visited)
        }
    }
    return size
}
