
def cleaningRobot(input) {
    def grid = input.readLines().collect { it.split('') }
    def graph = []

    grid.eachWithIndex { row, r ->
        row.eachWithIndex { cell, c ->
            if (cell.isNumber()) {
                def distancesFromPOI = bfsGetEdgeWeights(grid, [r, c])
                def index = cell.toInteger()
                graph[index] = distancesFromPOI
            }
        }
    }

    return dfs(graph, 0, [0: true], false)
}

def bfsGetEdgeWeights(grid, start) {
    def poiToDistance = [(grid[start[0]][start[1]]): 0]
    def queue = [[start[0], start[1], 0]]
    def visited = [:]

    while (queue) {
        def front = queue.remove(0)
        def (row, col, distance) = front

        if (visited["$row,$col"]) continue
        visited["$row,$col"] = true

        if (grid[row][col].isNumber()) {
            poiToDistance[grid[row][col]] = distance
        }

        [[0, -1], [0, 1], [1, 0], [-1, 0]].each { d ->
            def (nextRow, nextCol) = [row + d[0], col + d[1]]
            if (grid[nextRow][nextCol] != "#") {
                queue << [nextRow, nextCol, distance + 1]
            }
        }
    }

    def distances = new int[poiToDistance.size()]
    poiToDistance.each { numStr, dist ->
        distances[numStr.toInteger()] = dist
    }
    return distances
}

def dfs(graph, entryIndex, visited, returnToZero) {
    if (visited.size() == graph.size()) {
        return returnToZero ? graph[entryIndex][0] : 0
    }

    def minDistance = Integer.MAX_VALUE
    graph[entryIndex].eachWithIndex { val, i ->
        if (!visited[i]) {
            visited[i] = true
            def dist = val + dfs(graph, i, visited, returnToZero)
            minDistance = Math.min(minDistance, dist)
            visited.remove(i)
        }
    }

    return minDistance
}

def readFile(path) {
    new File(path).text.trim()
}

def input = readFile("input.txt")
println cleaningRobot(input)
