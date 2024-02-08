def input = new File("input.txt").readLines()

def acreMap = input.collect { it.toList() }

def getAdjacent = { i, j ->
    def adjacent = []
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) continue
            if (i + x < 0 || j + y < 0 || i + x >= acreMap.size() || j + y >= acreMap[0].size()) continue
            adjacent.add(acreMap[i + x][j + y])
        }
    }
    adjacent
}

def transformAcre = { i, j ->
    def current = acreMap[i][j]
    def adjacent = getAdjacent(i, j)
    switch (current) {
        case '.':
            if (adjacent.count { it == '|' } >= 3) return '|'
            break
        case '|':
            if (adjacent.count { it == '#' } >= 3) return '#'
            break
        case '#':
            if (!(adjacent.contains('|') && adjacent.contains('#'))) return '.'
            break
    }
    return current
}

def transformMap = {
    def newMap = []
    for (int i = 0; i < acreMap.size(); i++) {
        newMap[i] = []
        for (int j = 0; j < acreMap[i].size(); j++) {
            newMap[i][j] = transformAcre(i, j)
        }
    }
    return newMap
}

(1..10).each {
    acreMap = transformMap()
}

def wooded = acreMap.sum { it.count { it == '|' } }
def lumberyards = acreMap.sum { it.count { it == '#' } }

println wooded * lumberyards